---
title: "Building a Pipeline to Store Flight Info Using RSQLite, httr and jsonlite"
author: ''
date: '2021-05-09'
slug: building-a-pipeline
categories: []
tags: []
type: ''
subtitle: ''
image: ''
---



#### *This is the "how the sausage is made" post of this subject, detailing how I created a process to save ADS-B data from OpenSky Network into an SQLite database. If you just want to read the article about UAV operations, [head here](https://www.charlesmercieca.com/post/tracking-frontexs-newest-uav/).*

## Step 1: Sketch up the plan

Going into this project, I knew I wanted to use OpenSky Network's API to query the activity of the UAV in question, but it became clear I needed some way to run an automatic process and store data locally because the two services I'd be using had some limitations. I could either use the State Vectors function to retrieve the actual ADS-B broadcast from the aircraft, however this only contained 1 hour of data before it would overwrite itself, meaning I would have to deploy something to execute the process in real time when the aircraft took off.

Alternatively, I could query [Tracks by Aircraft](https://opensky-network.org/apidoc/rest.html#track-by-aircraft) which contained less granular data but which persisted for 30 days. This meant that I could use a simple script scheduled to run say every two weeks and get most of my data. I opted for this structure, which involved downloading data about flights, then creating a mini process to see what had changed since last save, and append these new data points to an SQLite database.

## Step 2: Create two helper functions

Thinly wrapping the `httr`and `jsonlite` libraries into two separate functions streamlined things considerably. The first one creates a data frame consisting of the aircraft's ICAO 24 bit transponder code, callsign used and departure and arrival airport and times.


```r
getFlights <- function(icao24, begin, end, period){
  
  #Calculates range as seconds between begin & end points, then divides by seconds in a day... 
  range <- (as.numeric(end)- as.numeric(begin)) / 86400
  
  #...and rounds up to iterations needed for the below for loop.
  iterations <- ceiling(range/period)
  
  #Create empty data frame to hold results
  df.total <- data.frame()
  
  for(i in 1:iterations){
    
    path <- glue("https://{username}:{password}@opensky-network.org/api/flights/aircraft?")
    
    request <- GET(url = path, 
                   query = list(
                     icao24 = icao24,
                     begin = as.numeric(begin),
                     end = as.numeric(begin) + (86400 * as.numeric(period))
                   ))
    
    response <- content(request, as = "text", encoding = "UTF-8") 
    table <- data.frame(fromJSON(response, flatten = TRUE))
    df.total = rbind(df.total, table)
    begin <- begin + (86400 * as.numeric(period))
  }
  return(df.total)
}
```

This function is identical to the one [I used to query Air Malta flights in October.](https://www.charlesmercieca.com/post/impact-of-covid19-on-air-malta-s-operations/)

The other function does roughly the same but for track points along a flight, does some general data cleaning and computes traversed distance and estimated ground speed:


```r
get_tracks <- function(icao24, first_seen){
  path <- glue("https://{username}:{password}@opensky-network.org/api/tracks/all?")
  request <- GET(url = path,
                 query = list(
                   icao24 = icao24,
                   time = first_seen))
  
  response <- content(request, as = "text", encoding = "UTF-8")
  
  table <- data.frame(fromJSON(response, flatten = TRUE)) %>%
    mutate(flight = first_seen) %>% 
    rename(Lat = "path.2",
           Lon = "path.3",
           Alt = "path.4",
           Course = "path.5",
           OnGround = "path.6") %>%
    mutate(time = lubridate::as_datetime(path.1),
           Alt = Alt * 3.2808) %>%
    select(flight, callsign, time, Lat, Lon, Alt, Course, OnGround) %>% 
    mutate(Lat2 = lag(Lat),
           Lon2 = lag(Lon),
           t = difftime(time, lag(time), units = "hours")) %>%
    rowwise() %>%
    mutate(d = geosphere::distHaversine(p1 = c(Lat, Lon), p2 = c(Lat2, Lon2))*0.000539957) %>%
    ungroup() %>%
    mutate(knots = d / as.numeric(t))
  return(table)
}
```

Creating the two initial data frames was as easy as running the functions with hardset parameters.

## Step 3: Initializing an SQLite database

Incredibly, installing SQLite through R is as easy as installing the `RSQLite` package. Initializing the database is a one-liner, and so is writing the data frame.


```r
library(RSQLite)
library(DBI)

mydb <- dbConnect(RSQLite::SQLite(), "heron-flights.sqlite")

dbWriteTable(mydb, "heron_flights", flights)
```

## Step 4: Creating a boatload of parameters

The beauty of this approach is that we can query data from the last saved state of our database to use in the process. So for example, when we re-run the script, we can dynamically have it only check for data in the period between the last loaded point and the time now:


```r
WD <- "directory where SQLite DB is"

mydb <- dbConnect(RSQLite::SQLite(), paste0(WD, "/heron-flights.sqlite"))

LAST_LOAD <- dbGetQuery(mydb, 'SELECT max(lastSeen) FROM heron_flights') %>% 
  pull()

TIME_NOW <- as.numeric(as.POSIXct(Sys.time()))
```

Other things I parameterized were my access credentials, the transponder code we are looking for, and two summaries from the database before we append to it:


```r
ICAO_24 = "a11111"

username <- "Get Your Own"
password <- "It's Free :)"

prevFlights <- dbGetQuery(mydb, 'SELECT firstSeen FROM heron_flights')
Nrow_Tracks <- dbGetQuery(mydb, 'SELECT COUNT(1) FROM tracks') %>% pull()
```

## Step 5: Create a pipeline that appends data

The actual pipeline itself isn't as impressively long as I secretly hoped, but the `DBI` library's append mode makes this sort of thing a breeze:


```r
#Get list of flights by airframe since last save date

flights <- getFlights(icao24 = ICAO_24, 
                      begin = LAST_LOAD+1, 
                      end = TIME_NOW, 
                      period = 20) %>% 
  select(icao24, firstSeen, estDepartureAirport, lastSeen, estArrivalAirport, callsign) %>% 
  mutate(time = lubridate::as_datetime(firstSeen))

#Write result to 'heron_flights' SQLite table

dbWriteTable(mydb, "heron_flights", flights, append = TRUE)

#Use flights table to query tracks, calculate groundspeed etc.

tracks <- flights %>% 
  mutate(tracks = pmap(list(icao24, firstSeen), get_tracks)) %>% 
  select(tracks) %>% 
  unnest()

#Save to 'tracks' SQLite table
dbWriteTable(mydb, "tracks", tracks, append = TRUE)
```

## Step 6: Keeping track of changes

This part actually occurred to me later after I wrote a few queries manually to check that things were working, but I wrapped two quick checks in a print statement to see which flights were added and how many track points were inserted, before casually disconnecting the database.


```r
#Check
print(
  paste("The following records have been added to heron_flights",
        setdiff(flights %>% select(firstSeen), 
                prevFlights)))

NrowTracksNew <- dbGetQuery(mydb, 'SELECT COUNT(1) FROM tracks') %>% pull()
print(paste((NrowTracksNew - Nrow_Tracks), 
            "additional track points have been inserted."))


#Disconnect SQLite DB
dbDisconnect(mydb)
```

At this stage, even setting up a task scheduler job to run every two weeks should have it covered!

---
title: "Who would have been elected if we had the gender reform bill in the past?"
author: ''
date: '2021-04-18'
slug: gender-reform-bill
categories: []
tags: []
type: ''
subtitle: ''
image: ''
---

<script src="{{< blogdown/postref >}}index.en_files/htmlwidgets/htmlwidgets.js"></script>

<script src="{{< blogdown/postref >}}index.en_files/jquery/jquery.min.js"></script>

<link href="{{< blogdown/postref >}}index.en_files/datatables-css/datatables-crosstalk.css" rel="stylesheet" />

<script src="{{< blogdown/postref >}}index.en_files/datatables-binding/datatables.js"></script>

<link href="{{< blogdown/postref >}}index.en_files/dt-core/css/jquery.dataTables.min.css" rel="stylesheet" />
<link href="{{< blogdown/postref >}}index.en_files/dt-core/css/jquery.dataTables.extra.css" rel="stylesheet" />

<script src="{{< blogdown/postref >}}index.en_files/dt-core/js/jquery.dataTables.min.js"></script>

<link href="{{< blogdown/postref >}}index.en_files/nouislider/jquery.nouislider.min.css" rel="stylesheet" />

<script src="{{< blogdown/postref >}}index.en_files/nouislider/jquery.nouislider.min.js"></script>

<link href="{{< blogdown/postref >}}index.en_files/selectize/selectize.bootstrap3.css" rel="stylesheet" />

<script src="{{< blogdown/postref >}}index.en_files/selectize/selectize.min.js"></script>

<link href="{{< blogdown/postref >}}index.en_files/crosstalk/css/crosstalk.css" rel="stylesheet" />

<script src="{{< blogdown/postref >}}index.en_files/crosstalk/js/crosstalk.min.js"></script>

With Parliament’s Gender Quota bill [passing it’s third reading](https://timesofmalta.com/articles/view/more-women-in-parliament-as-house-approves-bill-on-gender-parity.864786), it’s only a President’s signature away from becoming law. Whether the mechanism has the desired effect or not can only be assessed in the future following a few Parliamentary cycles.

What we can do is play around with data and entertain what some previous Parliaments might have of looked like had these rules already been in place. This sort of [backtesting](https://en.wikipedia.org/wiki/Backtesting) is quite common in some domains, so adopting it to policy decisions shouldn’t be that much of a leap.

## How does the Gender Quota Bill work?

The best explainer I could find on how the reform will work is [from this public consultation document dated from March 2019.](https://meae.gov.mt/en/Public_Consultations/OPM/Documents/FINAL%20-%20CONSULTATION%20DOCUMENT%20-%20WEB%20VERSION.pdf) I’m not sure if the final version passed in Parliament is exactly identical.

In any case, the gist is in pages 41 to 47, where the process is proposed as:

1.  Allow the usual election counting, constitutional adjustment and casual elections to take place as they currently do.

2.  If two parties make it to parliament, calculate the proportion of MP’s by gender.

3.  If one gender has less than 40% of the seats, trigger the clause.

4.  Calculate the number of seats needed by the underrepresented gender to reach the 40% quota. Although the required seats to reach 40% might be more than 12, the maximum number of seats allocated can be 12, equally split between the parties.

5.  Fill these seats using a ranked list that prioritizes candidates of the under-represented gender who suffered from “wasted” votes in their district. This is a bit more complex than one assumes at first, since two ways are proposed. The first is rank by left over votes that can’t be transferred once 5 MP’s are elected from a district - this seems straight forward. The second entails using the leftover votes of the over-represented gender and holding “casual” elections transferring these votes to the under-represented gender. We can’t do this using the summarized data.

6.  If this list is exhausted, and seats remain available, parties can co-opt their choices.

## The Data

Like in other projects, we’ll use the [Malta Elections dataset](https://www.um.edu.mt/projects/maltaelections/elections/parliamentary) originally started by Professor John C. Lane and currently hosted by the University of Malta. The data here spans from 1921 to 2013, allowing us to simulate all but the last legislature.

## Step 1: Find in which Legislatures the Mechanism is applicable

For a legislature to be applicable it needs to have a gender imbalance of 60% and consist of only two parties. We can calculate those as follows:

``` r
library(readxl)
library(tidyverse)

elections <- read_excel("C:/Users/Charles Mercieca/Documents/RProjects/Parliament Gender Reform/Input/elections.xls")

prop_women <- elections %>% 
  filter(SEATED != 0,
         PARTY != 99) %>% 
  mutate(SEX = SEX-1) %>% 
  group_by(election = YEAR) %>% 
  summarise(proportion_women = mean(SEX))

parties <- elections %>%
  filter(WIN != 0,
         PARTY != 99) %>% 
  group_by(YEAR) %>% 
  distinct(PARTY) %>% 
  group_by(election = YEAR) %>% 
  tally(name = "Parties Elected to Parliament")

legislatures <- prop_women %>% 
  inner_join(parties) %>% 
  mutate(activate_mechanism = proportion_women < 0.4 &
           `Parties Elected to Parliament` == 2)

legislatures %>% tail(10)
```

    ## # A tibble: 10 x 4
    ##    election proportion_women `Parties Elected to Parliament` activate_mechanism
    ##       <dbl>            <dbl>                           <int> <lgl>             
    ##  1     1971           0.0357                               2 TRUE              
    ##  2     1976           0.0429                               2 TRUE              
    ##  3     1981           0.0290                               2 TRUE              
    ##  4     1987           0.0278                               2 TRUE              
    ##  5     1992           0.0145                               2 TRUE              
    ##  6     1996           0.0571                               2 TRUE              
    ##  7     1998           0.0909                               2 TRUE              
    ##  8     2003           0.0870                               2 TRUE              
    ##  9     2008           0.0870                               2 TRUE              
    ## 10     2013           0.130                                2 TRUE

The first condition always has been true for every General Election so far. Since women’s suffrage was only established in time for 1947 election, and before that no women contested, 6 legislatures spanning 1921-1945 are out.

The 1951, 1953 and 1962 elections also resulted in more than 2 parties being elected, which leaves us with 11 elections to play with.

``` r
ggplot(legislatures, aes(x = election, y = proportion_women))+
geom_line()+
geom_point(aes(col = activate_mechanism))+
scale_y_continuous(labels = scales::percent)+
  theme_light()
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-2-1.png" width="672" />

## Step 2: Calculate Additional Seats

Slightly more involved is how to calculate the additional seats, which I’ve implemented in a function below following the maths from page 43 of the document.

``` r
additional_seats <- function(gender_under, gender_over){
  #Calculate extra seats to be added to Parliament
  additional_seats <- ((0.4 * gender_over) - gender_under) /
    0.6
  
  #Check to see that MP's remain an odd number and round down if not
  if (floor(additional_seats)+(gender_under+gender_over) / 2){
    floor(additional_seats) - 1
  }
  
  else {
    floor(additional_seats)
  }
}
```

The document also has two fictional scenarios with which we can test our function.

``` r
#Scenario 1: "the under-represented sex secured nine seats from a total of 67"..."x = 29.67 which is rounded down to 28"

additional_seats(9, 67) == 28
```

    ## [1] TRUE

``` r
#Scenario 2: "If the under-represented sex obtained 23 seats from a total of 69 seats, the total number of additional seats assigned to this under-represented group is 6"
additional_seats(23, 69) == 6
```

    ## [1] TRUE

Now we need to parse out how many men and women were elected or seated in casual elections throughout the legislatures, and then apply our function, accounting for the fact that if the maximum number of additional seats is limited to 12.

``` r
eligible_years <- legislatures %>% 
  filter(activate_mechanism == T,
         election >= 1947) %>% 
  pull(election)

seats <- elections %>% 
  filter(YEAR %in% eligible_years,
         SEATED != 0,
         PARTY != 99) %>%
  group_by(YEAR, SEX) %>% 
  tally() %>% 
  pivot_wider(names_from = SEX, values_from = n) %>% 
  rename(men = "1", women = "2") %>% 
  mutate(calculated_seats = additional_seats(women, men),
         adjusted_seats = if_else(calculated_seats > 12, 
                                  12, 
                                  calculated_seats))
seats
```

    ## # A tibble: 12 x 5
    ## # Groups:   YEAR [12]
    ##     YEAR   men women calculated_seats adjusted_seats
    ##    <dbl> <int> <int>            <dbl>          <dbl>
    ##  1  1955    42     1               25             12
    ##  2  1966    49     2               28             12
    ##  3  1971    54     2               31             12
    ##  4  1976    67     3               38             12
    ##  5  1981    67     2               40             12
    ##  6  1987    70     2               42             12
    ##  7  1992    68     1               42             12
    ##  8  1996    66     4               36             12
    ##  9  1998    60     6               29             12
    ## 10  2003    63     6               31             12
    ## 11  2008    63     6               31             12
    ## 12  2013    60     9               24             12

## Step 3: Compile List

Now we need a fair way to fill those 12 seats. I did struggle to understand all the gist of the documentation here, but my understanding is the safest bets are “hanging” candidates of the minority gender.

It’s actually pretty trivial to calculated the “wasted” votes from our dataset, we just need to find candidates whose maximum count was equivalent to their last count like this:

``` r
wasted_votes <- elections %>% 
  filter(YEAR %in% eligible_years,
         SEATED == 0,
         TOPS == LAST,
         PARTY != 99)

w <- wasted_votes %>% 
  group_by(YEAR, Dist) %>% 
  summarise(max(LAST))
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

From here I thought it was fairly straightforward: find these candidates, filter for the ones that are women and we’d have most of the list. But it turns out that only three of those candidates with wasted votes are women.

``` r
wasted_votes %>% 
  filter(SEX == 2) %>% 
  select(YEAR, NAME, Dist)
```

    ## # A tibble: 3 x 3
    ##    YEAR NAME           Dist
    ##   <dbl> <chr>         <dbl>
    ## 1  2003 Law, Rita         2
    ## 2  2008 Dalli, Helena     3
    ## 3  2008 Law, Rita         3

Which is probably why the document mentions that vote transferring mechanism. But given the document doesn’t delve too deeply into the details here and reverse engineering transfers might be too bold of an undertaking for a weekend project, the below is a rough approximation using the highest votes a candidate saw (i.e. the `TOPS` variable).

For the 1st elected candidate in a district, this will be the first count, for the 2nd elected the second and so on. Since by default the candidates we’re looking into were not elected, higher `TOPS` counts indicate a higher preference to transfer votes to that candidate.

``` r
#Create filter list for candidates elected in other districts in same year (e.g. Claudette Buttigieg in 2013)

elected_seperately <- elections %>% 
  filter(SEATED != 0) %>% 
  distinct(YEAR, NAME)

#Select best performing women among non-elected candidates nation wide
candidates <- elections %>% 
  filter(YEAR %in% eligible_years,
         SEATED == 0, #Filter out elects, including adjustment and casual elec.
         PARTY %in% c(13, 15), #PN & PL
         SEX == 2) %>%  #Women
  mutate(PARTY = case_when(PARTY == "13" ~ "PL",
                           PARTY == "15" ~ "PN")) %>%
  anti_join(elected_seperately) %>% 
  select(YEAR, NAME, PARTY, Dist, CT1, TOPS) %>% 
  group_by(YEAR, PARTY, Dist) %>% 
  arrange(YEAR, PARTY, desc(TOPS)) %>% 
  group_by(YEAR, PARTY) %>% 
  slice(1:6) %>% 
  mutate(YEAR = as.factor(YEAR),
         PARTY = as.factor(PARTY)) %>% 
  arrange(desc(YEAR))
```

    ## Joining, by = c("NAME", "YEAR")

We can pipe this into an interactive HTML table using the `DT` package:

``` r
library(DT)

datatable(candidates, filter = 'top', options = list(
  pageLength = 24, autoWidth = TRUE))
```

<div id="htmlwidget-1" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"filter":"top","filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;1955&quot;,&quot;1976&quot;,&quot;1981&quot;,&quot;1987&quot;,&quot;1992&quot;,&quot;1996&quot;,&quot;1998&quot;,&quot;2003&quot;,&quot;2008&quot;,&quot;2013&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;PL&quot;,&quot;PN&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"1\" data-max=\"13\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"14\" data-max=\"2463\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none; position: absolute; width: 200px;\">\n      <div data-min=\"15\" data-max=\"3344\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89"],["2013","2013","2013","2013","2013","2013","2013","2013","2013","2013","2013","2013","2008","2008","2008","2008","2008","2008","2008","2008","2008","2008","2008","2003","2003","2003","2003","2003","2003","2003","2003","2003","2003","2003","2003","1998","1998","1998","1998","1998","1998","1998","1998","1998","1998","1998","1998","1996","1996","1996","1996","1996","1996","1996","1996","1996","1996","1992","1992","1992","1992","1992","1992","1987","1987","1987","1987","1987","1987","1987","1987","1981","1981","1981","1981","1981","1981","1981","1981","1976","1976","1976","1976","1976","1955","1955","1955","1955","1955"],["Law, Rita","Bland Mintoff, Joan k/a Yana","Vella Cuschieri, Joanne","Bland Mintoff, Joan k/a Yana","Law, Rita","Vella Cuschieri, Joanne","Comodini Cachia, Therese","Farrugia, Shirley","Galea, Caroline","Agius, Anna","Brownrigg, Ingrid","Galea, Graziella","Law, Rita","Vella, Monica","Law, Rita","Vassallo, Lorna","Vassallo, Lorna","Portelli, Marthese","D'Amato, Helen","Mifsud Bonnici, Paula","Farrugia, Shirley","Galea, Caroline","Chetcuti, Janice","Law, Rita","Ellul Bonnici, Sharon","Ellul Bonnici, Sharon","Attard Montalto, Doreen","Spiteri Debono, Myriam","Vassallo, Lorna","Schiavone, Anna","Wille Piscopo, Elizabeth","Sciberras, Nadine","Mifsud Bonnici, Paula","Zahra, Helga","Zahra, Helga","Spiteri Debono, Miriam","Cini, Simone","Cini, Simone","Spiteri Debono, Miriam","Camilleri, Maria","Miceli, Pauline","Micallef Leyson, Marisa","Delicata, Marselle","Drake, Joanne","Buttigieg, Alexandra","Catania, Anna","Pullicino Orlando, Marlene","Spiteri Debono, Myriam","Law, Rita","Zrinzo, Sylvia","Buttigieg, Maria Stella","Delicata, Marselle","Catania, Anna","Cristina, Dolores","Sciberras, Miriam","Borg (Bonavia), Doris","Vella, Mary Carmen","Sant, Carmen","Dalli, Helena","Spiteri Debono, Myriam","Catania, Anna","Schembri Orland, Lorraine","Ruggier, Maria","Darmenia Brincat, Cettina","Bonaci, Evelyn","Spiteri Debono, Myriam","Spiteri Coleiro, Carmen","Spiteri Coleiro, Carmen","Spiteri Debono, Myriam","Ruggier, Maria","Agius Ferrante, Anne","Bonaci, Evelyn","Spiteri Debono, Myriam","Agius Ferrante, Anne","Zammit, Grace","Farrugia, Agnes K/A Inez","Borg, Antoinette","Davies, Lucy","Darmanin, Cecilia (Cilia)","Darmenia Brincat, Cettina","Zammit, Grace","Darmanin, Cecilia Sive Cilla","Davies, Lucy","Darmanin, Cecilia Sive Cilla","De Trafford Strickland, Cecilia,Hon.","Ganado, Mary Anne","Aquilina, Emmanuela","Muscat Manduca, Maria Louisa","Grech, Carmela"],["PL","PL","PL","PL","PL","PL","PN","PN","PN","PN","PN","PN","PL","PL","PL","PL","PL","PN","PN","PN","PN","PN","PN","PL","PL","PL","PL","PL","PL","PN","PN","PN","PN","PN","PN","PL","PL","PL","PL","PL","PL","PN","PN","PN","PN","PN","PN","PL","PL","PL","PL","PN","PN","PN","PN","PN","PN","PL","PL","PL","PN","PN","PN","PL","PL","PL","PL","PL","PL","PN","PN","PL","PL","PN","PN","PN","PN","PN","PN","PL","PN","PN","PN","PN","PL","PL","PL","PN","PN"],[3,2,12,1,2,10,6,11,4,10,3,12,3,13,2,12,7,13,5,1,11,4,5,2,2,4,7,1,12,5,3,5,1,3,5,1,2,4,8,12,11,9,8,11,7,12,5,8,2,1,13,8,12,10,3,8,4,8,3,7,12,10,7,7,8,11,11,12,2,11,10,8,12,11,3,7,11,5,1,7,3,1,5,2,4,4,8,6,2],[650,70,547,285,37,208,846,531,690,615,613,418,1306,548,536,355,85,1103,1433,1349,821,411,422,2463,1108,518,455,40,238,1244,315,258,197,204,178,38,371,798,80,234,227,587,121,135,394,315,311,66,471,37,214,291,567,458,295,73,237,1286,987,161,475,286,59,186,210,139,42,42,14,208,180,1401,456,268,93,64,71,21,26,865,82,24,28,15,304,334,31,1017,495],[789,695,681,515,273,251,1260,1087,835,760,639,420,3006,720,541,480,85,2320,1847,1605,1225,519,509,3344,1358,840,467,373,278,1960,395,300,231,222,186,1547,1207,987,590,262,247,673,514,489,470,366,343,1219,1116,1036,298,1105,741,514,331,257,244,1960,1328,171,543,300,59,493,448,158,43,42,31,213,211,1898,593,287,107,82,72,28,27,1448,82,36,32,15,376,356,31,1064,565]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>YEAR<\/th>\n      <th>NAME<\/th>\n      <th>PARTY<\/th>\n      <th>Dist<\/th>\n      <th>CT1<\/th>\n      <th>TOPS<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":24,"autoWidth":true,"columnDefs":[{"className":"dt-right","targets":[4,5,6]},{"orderable":false,"targets":0}],"order":[],"orderClasses":false,"orderCellsTop":true,"lengthMenu":[10,24,25,50,100]}},"evals":[],"jsHooks":[]}</script>

Or alternatively:

``` r
ggplot(candidates, aes(fct_reorder(NAME, TOPS), TOPS, fill = PARTY))+
  geom_col()+
  coord_flip()+
  facet_wrap(~YEAR, 
             scales = "free", 
             ncol = 2)+
  ylab("Highest Vote Count Candidate Achieved Before Transfer")+
  xlab("")+
  theme_light()+
  theme(legend.position="top")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-10-1.png" width="672" />

We stumble into two areas of added complexity here:

1.  Sometimes the adjustment downward to ensure an odd overall parliament will result in an odd list (i.e. 11 candidates) How would this be split among the two parties?

2.  Well performing candidates perform well in multiple districts. Taking 2013 as an example, Yana Bland Mintoff, Rita Law and Joanne Vella Cuschieri all did well in two districts (hence we captured them twice). In some cases it would probably entail having casual elections of casual elections using the results of casual elections, which are several orders of abstractions above the result on election day.

Interestingly, a number of politicians that would have of made it to Parliament are those that did eventually make it in future years: Therese Comodini Cachia in 2013 (elected in 2017), Paula Mifsud Bonnici and Marthese Portelli in 2008 (both elected in 2013), Marlene Farrugia in 1998 and Helena Dalli in 1992 (subsequently elected in 1996).

Another noteworthy instance is in the case of Rita Law, where the mechanism would mean that someone who made it to Parliament in 1998 but subsequently did not despite relatively solid performances would have of returned to the MP benches.

As an aside, one of the 1955 candidates that would have of made it is Cecilia Strickland, sister of Mabel and daughter of Gerald, who eventually left Malta and settled in Surrey.

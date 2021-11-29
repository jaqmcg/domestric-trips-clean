Domestic Trips Clean
================
Jack McGrath
29/11/2021

# Domestic Trips Clean

In this short exercise I will attempt to clean the data from the
domestic trips csv that is located in the data folder so that it is
ready for exploration and plotting.

## Loading Libraries

``` r
library(tidyverse)
library(lubridate)
```

## Importing Data

``` r
data <- read_csv("data/domestic-trips.csv")
data
```

    ## # A tibble: 6,812 x 1
    ##    `SuperWEB2(tm)`                                                              
    ##    <chr>                                                                        
    ##  1 "Domestic Overnight Trips"                                                   
    ##  2 "Quarter and Stopover state/region/SA2 by Stopover reason"                   
    ##  3 "Counting: Overnight trips (000)"                                            
    ##  4 "Filters:"                                                                   
    ##  5 "Default Summation,Overnight trips (000)"                                    
    ##  6 ",\"Stopover reason\",\"Holiday\",\"Visiting friends and relatives\",\"Busin~
    ##  7 "Quarter,Stopover state/region/SA2,"                                         
    ##  8 "March quarter 1998,Sydney,828.3171185,818.3232867,524.9231427,117.3920809,0~
    ##  9 ",\"Blue Mountains\",103.7609092,70.5420568,18.7583009,2.4819126,0,0,0,195.5~
    ## 10 ",\"Capital Country\",99.2024433,100.0013013,42.712127,19.89227,0,2.4819126,~
    ## # ... with 6,802 more rows

It seems as if R is having trouble importing this CSV. When I open up
excel to view the spreadsheet I find out why. There is a bunch of
metadata in the header of the CSV. The data that we are actually
interested in starts on line 12 and spans for the first 6 columns. Let’s
use some read\_csv parameters to help import the data so that we can
clean it.

## Clean Data

``` r
cn <- c("Quarter", "Region", "Holiday", "Visiting", "Business", "Other")
data <- read_csv("data/domestic-trips.csv", skip = 11, col_names = cn,
                           n_max = 6804, col_select = cn)
data
```

    ## # A tibble: 6,804 x 6
    ##    Quarter            Region                 Holiday Visiting Business  Other
    ##    <chr>              <chr>                    <dbl>    <dbl>    <dbl>  <dbl>
    ##  1 March quarter 1998 Sydney                   828.     818.     525.  117.  
    ##  2 <NA>               Blue Mountains           104.      70.5     18.8   2.48
    ##  3 <NA>               Capital Country           99.2    100.      42.7  19.9 
    ##  4 <NA>               Central Coast            279.     149.      24.2   2.93
    ##  5 <NA>               Central NSW              170.     143.      99.8  11.7 
    ##  6 <NA>               Hunter                   329.     266.      69.2  10.8 
    ##  7 <NA>               New England North West   115.     143.      60.3  11.7 
    ##  8 <NA>               North Coast NSW          828.     416.      55.4  42.4 
    ##  9 <NA>               Outback NSW               24.5     21.3     48.7   2.07
    ## 10 <NA>               Riverina                  46.5     71.8     82.4  14.6 
    ## # ... with 6,794 more rows

Now our data looks much nicer. Since our data starts at row 12 I used
the skip parameter to skip the first 11 rows. I also set a column names
variable to ensure that our columns are named appropriate and reflect
the raw data. To make sure the rows that I want are selected I made sure
to only select the rows that are in the column names vector. I also made
sure to set a limit to the number of rows to import as there is
copyright information on the last row.

The first thing that I noticed was that there are a lot of NAs in the
quarter column. It appears that when the name of a quarter is entered
into a row that this quarter applies for all rows below it until another
value is observed. There also appears to be quarters named “Total” which
count the total observations for each quarter. We want to get rid of
this as this is easy computable and irrelevant. Let’s fix these issues.

``` r
filled_quarter <- data %>% 
  fill(Quarter, .direction = "down") %>% 
  filter(Quarter != "Total")
filled_quarter
```

    ## # A tibble: 6,720 x 6
    ##    Quarter            Region                 Holiday Visiting Business  Other
    ##    <chr>              <chr>                    <dbl>    <dbl>    <dbl>  <dbl>
    ##  1 March quarter 1998 Sydney                   828.     818.     525.  117.  
    ##  2 March quarter 1998 Blue Mountains           104.      70.5     18.8   2.48
    ##  3 March quarter 1998 Capital Country           99.2    100.      42.7  19.9 
    ##  4 March quarter 1998 Central Coast            279.     149.      24.2   2.93
    ##  5 March quarter 1998 Central NSW              170.     143.      99.8  11.7 
    ##  6 March quarter 1998 Hunter                   329.     266.      69.2  10.8 
    ##  7 March quarter 1998 New England North West   115.     143.      60.3  11.7 
    ##  8 March quarter 1998 North Coast NSW          828.     416.      55.4  42.4 
    ##  9 March quarter 1998 Outback NSW               24.5     21.3     48.7   2.07
    ## 10 March quarter 1998 Riverina                  46.5     71.8     82.4  14.6 
    ## # ... with 6,710 more rows

Looks great. One thing I noticed while scrolling through this table was
that some regions have significantly higher holiday, visiting, etc
values than the other regions. When taking a closer look at these
specific regions I notice that these are actually states within
Australia whereas the other regions are cities. What seems to be
happening is that all the cities within a region are grouped in order
and the total for this state follows. To make sure that values are not
double counted I will attempt to add a state column and put these cities
in their respective state. I will also remove the rows that have the
totals for the state. To do this I will do a case statement and set the
state column to be the name of the state and fill the rest of the
columns upward.

``` r
states <- c("New South Wales", "Victoria", "Queensland", "South Australia",
            "Western Australia", "Tasmania", "Northern Territory", "ACT")
state_trips <- filled_quarter %>% mutate(State = case_when(
  Region %in% states ~ Region,
  TRUE ~ NA_character_)) %>% 
  fill(State, .direction = "up") %>% 
  filter(Region != State)
state_trips
```

    ## # A tibble: 6,080 x 7
    ##    Quarter            Region         Holiday Visiting Business  Other State     
    ##    <chr>              <chr>            <dbl>    <dbl>    <dbl>  <dbl> <chr>     
    ##  1 March quarter 1998 Sydney           828.     818.     525.  117.   New South~
    ##  2 March quarter 1998 Blue Mountains   104.      70.5     18.8   2.48 New South~
    ##  3 March quarter 1998 Capital Count~    99.2    100.      42.7  19.9  New South~
    ##  4 March quarter 1998 Central Coast    279.     149.      24.2   2.93 New South~
    ##  5 March quarter 1998 Central NSW      170.     143.      99.8  11.7  New South~
    ##  6 March quarter 1998 Hunter           329.     266.      69.2  10.8  New South~
    ##  7 March quarter 1998 New England N~   115.     143.      60.3  11.7  New South~
    ##  8 March quarter 1998 North Coast N~   828.     416.      55.4  42.4  New South~
    ##  9 March quarter 1998 Outback NSW       24.5     21.3     48.7   2.07 New South~
    ## 10 March quarter 1998 Riverina          46.5     71.8     82.4  14.6  New South~
    ## # ... with 6,070 more rows

Great. Now we have removed the double counted values but haven’t lost
our ability to do analysis by state.

Right now our data is in the “long” format. To be able to aggregate the
data, analyse more thoroughly and create plots we need to pivot this
data into the “long” format.

``` r
tidy_trips <- state_trips %>% pivot_longer(cols = Holiday:Other, 
                                            names_to = "Purpose", 
                                            values_to = "Trips")
tidy_trips
```

    ## # A tibble: 24,320 x 5
    ##    Quarter            Region          State           Purpose   Trips
    ##    <chr>              <chr>           <chr>           <chr>     <dbl>
    ##  1 March quarter 1998 Sydney          New South Wales Holiday  828.  
    ##  2 March quarter 1998 Sydney          New South Wales Visiting 818.  
    ##  3 March quarter 1998 Sydney          New South Wales Business 525.  
    ##  4 March quarter 1998 Sydney          New South Wales Other    117.  
    ##  5 March quarter 1998 Blue Mountains  New South Wales Holiday  104.  
    ##  6 March quarter 1998 Blue Mountains  New South Wales Visiting  70.5 
    ##  7 March quarter 1998 Blue Mountains  New South Wales Business  18.8 
    ##  8 March quarter 1998 Blue Mountains  New South Wales Other      2.48
    ##  9 March quarter 1998 Capital Country New South Wales Holiday   99.2 
    ## 10 March quarter 1998 Capital Country New South Wales Visiting 100.  
    ## # ... with 24,310 more rows

While the data is now ready for aggregation and plotting, I wish to see
if there are any trends in the number of trips as time increases. To do
this I will need to parse the quarter column into a date format. This
way I can perform timeseries analysis while still having the option to
have the date as a factor if needed.

``` r
final_timed_trips <- tidy_trips %>% mutate(Quarter = parse_date(Quarter, 
                                                        format = "%B quarter %Y"))
final_timed_trips
```

    ## # A tibble: 24,320 x 5
    ##    Quarter    Region          State           Purpose   Trips
    ##    <date>     <chr>           <chr>           <chr>     <dbl>
    ##  1 1998-03-01 Sydney          New South Wales Holiday  828.  
    ##  2 1998-03-01 Sydney          New South Wales Visiting 818.  
    ##  3 1998-03-01 Sydney          New South Wales Business 525.  
    ##  4 1998-03-01 Sydney          New South Wales Other    117.  
    ##  5 1998-03-01 Blue Mountains  New South Wales Holiday  104.  
    ##  6 1998-03-01 Blue Mountains  New South Wales Visiting  70.5 
    ##  7 1998-03-01 Blue Mountains  New South Wales Business  18.8 
    ##  8 1998-03-01 Blue Mountains  New South Wales Other      2.48
    ##  9 1998-03-01 Capital Country New South Wales Holiday   99.2 
    ## 10 1998-03-01 Capital Country New South Wales Visiting 100.  
    ## # ... with 24,310 more rows

## Data Exploration

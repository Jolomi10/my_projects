Ramen_Ratings
================
Jolomi Akperi
2022-08-23

I’m so excited about this project because I absolutely LOVE ramen!!! For
this project, I want to carry out some analysis to find out the top
ramen brands, bottom ramen brands, countries with the most ramen brands,
and the customer’s favorite flavor.

I’ll begin with setting my working directory and reading the ramen data
set into this markdown. (This is a step I can never forget because when
I was new to markdowns, It just never occured to me to load the data set
unto markdown again even though it was present in my working
environment. I kept getting errors until in consulted a friend who
explained what the problem was.)

``` r
setwd("C:/Users/JOLOMI AKPERI/OneDrive/Documents/my_projects")                    
```

``` r
ramen_ratings <- read.csv('ramen-ratings.csv')
```

#### Installing Packages

``` r
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
```

``` r
install.packages("janitor", repos = "http://cran.us.r-project.org")
library(janitor)
```

``` r
install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)
```

### Cleaning the Data

Now let’s take a look at the data to see if it needs any cleaning.
(Trust me, you don’t want to skip this step. Dirty data mostly produces
inaccurate results. Now don’t want that do we?)

``` r
str(ramen_ratings)
```

    ## 'data.frame':    2580 obs. of  7 variables:
    ##  $ Review..: int  2580 2579 2578 2577 2576 2575 2574 2573 2572 2571 ...
    ##  $ Brand   : chr  "New Touch" "Just Way" "Nissin" "Wei Lih" ...
    ##  $ Variety : chr  "T's Restaurant Tantanmen " "Noodles Spicy Hot Sesame Spicy Hot Sesame Guan-miao Noodles" "Cup Noodles Chicken Vegetable" "GGE Ramen Snack Tomato Flavor" ...
    ##  $ Style   : chr  "Cup" "Pack" "Cup" "Pack" ...
    ##  $ Country : chr  "Japan" "Taiwan" "USA" "Taiwan" ...
    ##  $ Stars   : chr  "3.75" "1" "2.25" "2.75" ...
    ##  $ Top.Ten : chr  "" "" "" "" ...

The first thing I observed is that the column names begin with capital
letters. There’s absolutely no problem with that, but personally, I
prefer it when the column names I’m working with are in lowercase all
through. Let’s fix that real quick.

``` r
ramen_ratings <- (clean_names(ramen_ratings))
```

Now, lets change the data type of the stars column to numeric

``` r
ramen_ratings$stars <- as.numeric(ramen_ratings$stars)
```

``` r
str(ramen_ratings)
```

    ## 'data.frame':    2580 obs. of  7 variables:
    ##  $ review : int  2580 2579 2578 2577 2576 2575 2574 2573 2572 2571 ...
    ##  $ brand  : chr  "New Touch" "Just Way" "Nissin" "Wei Lih" ...
    ##  $ variety: chr  "T's Restaurant Tantanmen " "Noodles Spicy Hot Sesame Spicy Hot Sesame Guan-miao Noodles" "Cup Noodles Chicken Vegetable" "GGE Ramen Snack Tomato Flavor" ...
    ##  $ style  : chr  "Cup" "Pack" "Cup" "Pack" ...
    ##  $ country: chr  "Japan" "Taiwan" "USA" "Taiwan" ...
    ##  $ stars  : num  3.75 1 2.25 2.75 3.75 4.75 4 3.75 0.25 2.5 ...
    ##  $ top_ten: chr  "" "" "" "" ...

Lets be sure we have no missing data

``` r
ramen_ratings %>% is.na() %>% colSums() %>% sort(decreasing=TRUE)
```

    ##   stars  review   brand variety   style country top_ten 
    ##       3       0       0       0       0       0       0

There are 3 missing values in the stars column. Let’s fix that.

``` r
ramen_ratings <- ramen_ratings %>% 
  na.omit()
```

``` r
ramen_ratings %>% is.na() %>% colSums() %>% sort(decreasing=TRUE)
```

    ##  review   brand variety   style country   stars top_ten 
    ##       0       0       0       0       0       0       0

PERFECT!

Since I have not observed any other anomalies, we’ll move on to the
analysis.

### Analysing Ramen Ratings

#### The Top Ramen Brands

Firstly, I will love to know the highest ramen rated brands so I’ll to
know which ones to look out for the next time I visit the store!

``` r
ramen_ratings %>%
  select(brand,country,stars) %>%
  group_by(brand,stars) %>%
  summarise(count=n()) %>%
  filter(stars>=4.5) %>%
  filter(count>5) %>%
  arrange(desc(count))
```

    ## # A tibble: 20 × 3
    ## # Groups:   brand [16]
    ##    brand            stars count
    ##    <chr>            <dbl> <int>
    ##  1 Nissin            5       76
    ##  2 MyKuali           5       22
    ##  3 Nissin            4.5     22
    ##  4 Nongshim          5       19
    ##  5 Indomie           5       16
    ##  6 Paldo             5       16
    ##  7 Mama              5       14
    ##  8 KOKA              5       10
    ##  9 Nissin            4.75    10
    ## 10 Nongshim          4.5      9
    ## 11 Samyang Foods     5        9
    ## 12 Mamee             5        8
    ## 13 Prima Taste       5        7
    ## 14 Sapporo Ichiban   5        7
    ## 15 A-Sha Dry Noodle  5        6
    ## 16 CarJEN            5        6
    ## 17 Indomie           4.5      6
    ## 18 MAMA              5        6
    ## 19 Maruchan          5        6
    ## 20 Myojo             4.5      6

The above brands are the definatly the ones to look out for as they have
the highest ratings.

#### Customers’ Favourite Ramen Flavors

It’ll be nice to know the variety of ramen flavors that have the highest
ratings. To achieve this, I selected the columns I felt were applicable
which are: brand, variety, and the number stars it received. I want to
see the customers that rated the flavors over 4.5 to show that they
really, really enjoyed that particular variety.

``` r
ramen_ratings %>%
  select(brand,variety,stars) %>%
  group_by(variety) %>%
  filter(stars>="4.5") %>%
  count(variety) %>%
  filter(n>1) %>%
  arrange(desc(n))
```

    ## # A tibble: 21 × 2
    ## # Groups:   variety [21]
    ##    variety                                   n
    ##    <chr>                                 <int>
    ##  1 Yakisoba                                  5
    ##  2 Curry Udon                                3
    ##  3 Kokomen Spicy Chicken                     3
    ##  4 Artificial Chicken                        2
    ##  5 Chef Creamy Tom Yam Flavour               2
    ##  6 Chef Curry Laksa Flavour                  2
    ##  7 Chef Lontong Flavour                      2
    ##  8 Chongqing Noodles Burning Dry Noodles     2
    ##  9 Curry Flavour Instant Noodles             2
    ## 10 Hakata Tonkotsu Ramen                     2
    ## # … with 11 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

Yakisoba had the highest number of ratings from 4.5 and above.This
doesn’t necessarily mean it is the customers’ favorite,but it’s certain
that it is really enjoyed by customers.

#### Country With the most Ramen Brands

Finally, I want to find out the country that produces the most ramen so
I can be sure to visit during my next vacation!

``` r
Country_ramen <- ramen_ratings %>% 
  select(variety,country) %>%
  group_by(country) %>%
  count(country) %>%
  arrange(desc(n))
Country_ramen
```

    ## # A tibble: 38 × 2
    ## # Groups:   country [38]
    ##    country         n
    ##    <chr>       <int>
    ##  1 Japan         352
    ##  2 USA           323
    ##  3 South Korea   307
    ##  4 Taiwan        224
    ##  5 Thailand      191
    ##  6 China         169
    ##  7 Malaysia      155
    ##  8 Hong Kong     137
    ##  9 Indonesia     126
    ## 10 Singapore     109
    ## # … with 28 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

I guess I’m heading to Japan!!

### Conclusion

This very data set was very interesting to me and I really enjoyed
working with it. I actually find it interesting that the USA has the
second highest number or ramen brands and the highest rated brand
(Nissin) is from the USA. Thats crazy.

Video Games
================
Jolomi Akperi
2022-08-24

![.](\Users\JOLOMI%20AKPERI\OneDrive\Documents\my_projects\pad.jpg)

This Project is going to be such an interesting one! Video games were
such a big part of my childhood as it was something my brothers and I
bonded over.Before our very eyes, the video game industry has grown so
much the evolution is nothing short of mind-blowing. For this analysis,
I’ll be considering sales data of PS4 and Xbox one video games.I’ll like
to discover which video games, genre and publishers have the highest
ratings at a global level. It’ll also be nice to know which year between
2014 - 2018 produced the highest rated video games for both PS4 and Xbox
One.

## Installing Packages

``` r
install.packages("tidyverse", repos = "http://cran.us.r-project.org/src/contrib/PACKAGES")
library(tidyverse)
```

``` r
install.packages("janitor", repos = "http://cran.us.r-project.org/src/contrib/PACKAGES")
library(janitor)
```

## Loading Datasets

``` r
ps4_game_sales <- read.csv("video_games/PS4_GamesSales.csv")
View(ps4_game_sales)
```

``` r
xboxone_game_sales <- read.csv("video_games/XboxOne_GameSales.csv")
View(xboxone_game_sales)
```

## Cleaning the Data

I’ll go on to clean the two data sets to ensure that I get accurate
results from this analysis.

``` r
# I'll view the structure of the data sets to understand to familiarize myself with it.
str(ps4_game_sales)
```

    ## 'data.frame':    1034 obs. of  9 variables:
    ##  $ Game         : chr  "Grand Theft Auto V" "Call of Duty: Black Ops 3" "Red Dead Redemption 2" "Call of Duty: WWII" ...
    ##  $ Year         : chr  "2014" "2015" "2018" "2017" ...
    ##  $ Genre        : chr  "Action" "Shooter" "Action-Adventure" "Shooter" ...
    ##  $ Publisher    : chr  "Rockstar Games" "Activision" "Rockstar Games" "Activision" ...
    ##  $ North.America: num  6.06 6.18 5.26 4.67 1.27 1.26 4.49 3.64 3.11 2.91 ...
    ##  $ Europe       : num  9.71 6.05 6.21 6.21 8.64 7.95 3.93 3.39 3.83 3.97 ...
    ##  $ Japan        : num  0.6 0.41 0.21 0.4 0.15 0.12 0.21 0.32 0.19 0.27 ...
    ##  $ Rest.of.World: num  3.02 2.44 2.26 2.12 1.73 1.61 1.7 1.41 1.36 1.34 ...
    ##  $ Global       : num  19.4 15.1 13.9 13.4 11.8 ...

``` r
str(xboxone_game_sales)
```

    ## 'data.frame':    613 obs. of  10 variables:
    ##  $ Pos          : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Game         : chr  "Grand Theft Auto V" "Call of Duty: Black Ops 3" "Call of Duty: WWII" "Red Dead Redemption 2" ...
    ##  $ Year         : chr  "2014" "2015" "2017" "2018" ...
    ##  $ Genre        : chr  "Action" "Shooter" "Shooter" "Action-Adventure" ...
    ##  $ Publisher    : chr  "Rockstar Games" "Activision" "Activision" "Rockstar Games" ...
    ##  $ North.America: num  4.7 4.63 3.75 3.76 3.23 3.25 3.37 2.94 2.94 2.91 ...
    ##  $ Europe       : num  3.25 2.04 1.91 1.47 1.71 1.49 1.26 1.62 1.49 1.44 ...
    ##  $ Japan        : num  0.01 0.02 0 0 0 0.01 0.02 0.02 0.03 0 ...
    ##  $ Rest.of.World: num  0.76 0.68 0.57 0.54 0.49 0.48 0.48 0.45 0.45 0.44 ...
    ##  $ Global       : num  8.72 7.37 6.23 5.77 5.43 5.22 5.13 5.03 4.92 4.79 ...

From what I can observe, the data types are appropriate. Awesome! But I
always prefer to have the column names in lowercase and seprated with
underscores. I’ll fix that real quick.

``` r
ps4_game_sales <- clean_names(ps4_game_sales)
xboxone_game_sales <- clean_names(xboxone_game_sales)
```

``` r
str(ps4_game_sales)
```

    ## 'data.frame':    1034 obs. of  9 variables:
    ##  $ game         : chr  "Grand Theft Auto V" "Call of Duty: Black Ops 3" "Red Dead Redemption 2" "Call of Duty: WWII" ...
    ##  $ year         : chr  "2014" "2015" "2018" "2017" ...
    ##  $ genre        : chr  "Action" "Shooter" "Action-Adventure" "Shooter" ...
    ##  $ publisher    : chr  "Rockstar Games" "Activision" "Rockstar Games" "Activision" ...
    ##  $ north_america: num  6.06 6.18 5.26 4.67 1.27 1.26 4.49 3.64 3.11 2.91 ...
    ##  $ europe       : num  9.71 6.05 6.21 6.21 8.64 7.95 3.93 3.39 3.83 3.97 ...
    ##  $ japan        : num  0.6 0.41 0.21 0.4 0.15 0.12 0.21 0.32 0.19 0.27 ...
    ##  $ rest_of_world: num  3.02 2.44 2.26 2.12 1.73 1.61 1.7 1.41 1.36 1.34 ...
    ##  $ global       : num  19.4 15.1 13.9 13.4 11.8 ...

``` r
str(xboxone_game_sales)
```

    ## 'data.frame':    613 obs. of  10 variables:
    ##  $ pos          : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ game         : chr  "Grand Theft Auto V" "Call of Duty: Black Ops 3" "Call of Duty: WWII" "Red Dead Redemption 2" ...
    ##  $ year         : chr  "2014" "2015" "2017" "2018" ...
    ##  $ genre        : chr  "Action" "Shooter" "Shooter" "Action-Adventure" ...
    ##  $ publisher    : chr  "Rockstar Games" "Activision" "Activision" "Rockstar Games" ...
    ##  $ north_america: num  4.7 4.63 3.75 3.76 3.23 3.25 3.37 2.94 2.94 2.91 ...
    ##  $ europe       : num  3.25 2.04 1.91 1.47 1.71 1.49 1.26 1.62 1.49 1.44 ...
    ##  $ japan        : num  0.01 0.02 0 0 0 0.01 0.02 0.02 0.03 0 ...
    ##  $ rest_of_world: num  0.76 0.68 0.57 0.54 0.49 0.48 0.48 0.45 0.45 0.44 ...
    ##  $ global       : num  8.72 7.37 6.23 5.77 5.43 5.22 5.13 5.03 4.92 4.79 ...

Much better!

Next, I’ll check for blanks in both datasets (N/A)

``` r
 ps4_game_sales%>% 
  is.na() %>% 
  colSums() %>% 
  sort(decreasing=TRUE)
```

    ##          game          year         genre     publisher north_america 
    ##             0             0             0             0             0 
    ##        europe         japan rest_of_world        global 
    ##             0             0             0             0

``` r
xboxone_game_sales %>% 
  is.na() %>%
  colSums() %>% 
  sort(decreasing=TRUE)
```

    ##           pos          game          year         genre     publisher 
    ##             0             0             0             0             0 
    ## north_america        europe         japan rest_of_world        global 
    ##             0             0             0             0             0

Perfect! Our data is squeeky clean!

## Video Game Analysis

### Video games with the Highest Global Rating

#### PS4

``` r
top_ps4_games <- ps4_game_sales %>%
  select(game,genre,global) %>%
  group_by(game,global) %>%
  filter(global>=10) %>%
  arrange(desc(global))

ggplot(data = top_ps4_games) +
  geom_point(mapping = aes(x = game, y = global, color = game))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Highest rated PS4 video game", subtitle = "2014-2019")
```

![](video_game_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

As we can clearly see, Grand Theft Auto V had the highest global rating
among all PS4 games put out between 2014-2019.

#### Xbox One

``` r
top_xboxone_games <- xboxone_game_sales %>%
  select(game,genre,global) %>%
  group_by(game,global) %>%
  filter(global>=5) %>%
  arrange(desc(global))

ggplot(data = top_xboxone_games) +
  geom_point(mapping = aes(x = game, y = global, color = game))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Highest rated xbox one video game", subtitle = "2014-2019")
```

![](video_game_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> From
this plot, I can tell that Grand Theft Auto V is also the highest rated
video game for Xbox between 2014 and 2018.

Is it safe to say Grand Theft Auto V was the customers’ favorite between
2014 and 2018 since it has the highest ratings for both consoles?

It is also very interesting to see that the Xbox version of this same
Grand Theft Auto 5 has significantly lower ratings than the PS4 version
does.

### Genre With The Highest Ratings

#### PS4

To achieve this, I ran the code below to count the number of PS4 game
genres that have global ratings above 6. This will help us determine the
video game genres that had the highest ratings all over the world from
2014 - 2018. global)

``` r
top_ps4_genre <- ps4_game_sales %>%
  select(game,genre,global) %>%
  group_by(genre) %>%
  filter(global>="5") %>%
  count(genre) %>%
  filter(n>1) %>%
  arrange(desc(n))
```

![](\Users\JOLOMI%20AKPERI\OneDrive\Documents\my_projects\ps4_top_genre.png)
From this bar chart I created with Tableau, we can see that shooting
games had the highest ratings overall from 2014 - 2018. Even though a
game with the shooter genre isn’t the highest rated, the shooter genre
has the highest ratings regardless for PS4 games. The action genre
follows closely behind.

#### Xbox One

Applying the same method to determine the genre with the highest ratings
for PS4, I’ll do the same for xbox.But since xbox games generally have
lower ratings than PS4 games, I’ll apply the filter to select genres
have 3 and above as their global rating.

``` r
top_xboxone_genre <- xboxone_game_sales %>%
  select(game,genre,global) %>%
  group_by(genre) %>%
  filter(global>="3") %>%
  count(genre) %>%
  filter(n>1) %>%
  arrange(desc(n))
```

![](\Users\JOLOMI%20AKPERI\OneDrive\Documents\my_projects\xbox_one_genre.png)

From the bar chart above we can see that the shooter video game genre is
by far the highest rated for the Xbox One console.

### Publishers With the Highest Number of Games

#### PS4

Next up, I will determine the publisher who put out the most PS4 games
between 2014 and 2018.

``` r
ps4_publishers <- ps4_game_sales %>% 
  select(publisher,game) %>%
  group_by(publisher) %>%
  count(publisher)%>%
  arrange(desc(n))
```

![](\Users\JOLOMI%20AKPERI\OneDrive\Documents\my_projects\ps4_publishers.png)

Namco Bandai published the most PS4 games

#### Xbox

I’ll repeat the same process for Xbox One.

``` r
xbox_publishers <- xboxone_game_sales %>% 
  select(publisher,game) %>%
  group_by(publisher) %>%
  count(publisher)%>%
  arrange(desc(n))
```

![](\Users\JOLOMI%20AKPERI\OneDrive\Documents\my_projects\xbox_pub.png)

Microsoft Studios put out the most Xbox games.

I can see from these two bar charts that Namco Bandai Games (the
publisher with the highest number of games produced in 2014-2018)
publishes both PS4 games and Xbox games but produced more of PS4 games
than Xbox game.

On the other hand, Microsoft Studios put out the most Xbox games and did
not put out a single PS4 game. This is simply because Microsoft owns
Xbox. They definately won’t make games for the competition.At the same
time, Sony Interactive Entertainment owns PlayStation and does not have
a single Xbox game.

Also, I find it interesting that even though Sony Interactive
Entertainment owns PlayStation, they are not the publisher with the most
PS4 games.

### Year With the Highest Rated Games

To determine the year with the highest rating, I ran the code below to
count the number of release years that have global ratings above 5 (for
PS4) and 3(for Xbox One) since Xbox One generally has lower ratings.
This will help us determine the release year that had the highest rated
games all over the world from 2014 - 2018.

#### PS4

``` r
ps4_year <- ps4_game_sales %>%
  select(year,genre,global) %>%
  group_by(year) %>%
  filter(global>="5") %>%
  count(year) %>%
  filter(n>1) %>%
  arrange(desc(n))
```

#### Xbox One

``` r
xbox_year <- xboxone_game_sales %>%
  select(year,genre,global) %>%
  group_by(year) %>%
  filter(global>="3") %>%
  count(year) %>%
  filter(n>1) %>%
  arrange(desc(n))
```

![](\Users\JOLOMI%20AKPERI\OneDrive\Documents\my_projects\global_yearly.png)

Both Xbox One and PS4 have 2014 as the year in which they produced their
highest rated games between 2014 and 2018. I can observe that there has
been a decline in the ratings of video games with every year that
passes. This is interesting because aren’t the games meant to get better
as the years go by? But who knows, a game could be technologically
advanced but not interesting.

### Conclusion

It is interesting that video game ratings kept reducing as the years
went by. Isn’t meant to get better as the years go by? But who knows, a
game could be technologically advanced but not interesting.
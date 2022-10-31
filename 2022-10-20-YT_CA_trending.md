YouTube trending data (2) - Canada
================

## US, Canada & Japan

### Background:

This is just for practice and for fun and thus, will only focus on the
US, Canada and Japan YouTube channels. This will be divided into 3 parts
so it won’t be too long.

### Dataset

Dataset link: [YouTube Trending Video Dataset (updated
daily)](https://www.kaggle.com/datasets/rsrishav/youtube-trending-video-dataset)<br>

List of YouTube videos that have been on daily trending list from
2020-08-12 to 2022-10-02.

### Want to know:

1.  Channels & videos that on trending (top 50 \# of days on trending)
2.  Channels & videos with most views (top 100)
3.  Paid from views (top 20)

### YouTube Channel category

ID - Category name<br> 1 - Film & Animation<br> 2 - Autos<br> 10 -
Music<br> 15 - Pets & Animals<br> 17 - Sports<br> 19 - Travel
&Events<br> 20 - Gaming<br> 21 - Vblogging<br> 22 - Blogs<br> 23 -
Comedy<br> 24 - Entertainment<br> 25 - News & Politics<br> 26 - Howto &
Style<br> 27 - Education<br> 28 - Science & Technology<br> 29 -
Nonprofits & Activism<br> 43 - Shows<br>

## Methods

### 1. Data preparation

1-1. Load libraries

``` r
library(tidyr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ dplyr   1.0.10
    ## ✔ tibble  3.1.8      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ✔ purrr   0.3.4      
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(tibble)
library(dbplyr)
```

    ## 
    ## Attaching package: 'dbplyr'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     ident, sql

``` r
library(utils)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

1-2. Import datasets

``` r
CA <- read_csv("/Users/Linda/Desktop/RStudio/Kaggle/YouTube/CA_youtube_trending_data.csv")
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 157143 Columns: 16
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): video_id, title, channelId, channelTitle, tags, thumbnail_link, de...
    ## dbl  (5): categoryId, view_count, likes, dislikes, comment_count
    ## lgl  (2): comments_disabled, ratings_disabled
    ## dttm (2): publishedAt, trending_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### 2. Data cleaning

2-1. Check for duplicates

``` r
sum(duplicated(CA))  # 83 duplicate
```

    ## [1] 109

2-2. Remove duplicates

``` r
# use distinct() to remove duplicates
CA <- distinct(CA)

# check if it's removed
sum(duplicated(CA))
```

    ## [1] 0

2.3 - Extract columns we need

We will only look at these columns: title, channel_title, category_id,
trending_date, tags, views, likes, dislikes, comment_count

``` r
# select columns we need for analysis
# add a "category" column
CA_data <- CA %>%
  select(title, channelTitle, categoryId, trending_date, tags, view_count, likes, dislikes, comment_count) %>%
  mutate("category" = as.factor(categoryId)) %>%
  mutate(category = recode(category, "1" = "Film", "2" = "Autos", "10" = "Music", "15" = "Pets", 
                           "17" = "Sports", "19" = "Travel", "20" = "Gaming", "22" = "Blogs",
                           "23" = "Comedy", "24" = "Entertainment", "25" = "News", 
                           "26" = "HowToD", "27" = "Education", "28" = "Science", "29" = "NPO"))

# convert trending date to the date format
CA_data$trending_date<- as.Date(CA_data$trending_date, format = "%y-%m-%d")

# create a "trending _year" column
CA_data <- CA_data %>%
  mutate(trending_year = format(trending_date, format = '%Y'))

glimpse(CA_data)
```

    ## Rows: 157,034
    ## Columns: 11
    ## $ title         <chr> "Diljit Dosanjh: CLASH (Official) Music Video | G.O.A.T.…
    ## $ channelTitle  <chr> "Diljit Dosanjh", "jacksepticeye", "Apex Legends", "Braw…
    ## $ categoryId    <dbl> 10, 24, 20, 22, 26, 27, 17, 17, 22, 24, 24, 10, 24, 10, …
    ## $ trending_date <date> 2020-08-12, 2020-08-12, 2020-08-12, 2020-08-12, 2020-08…
    ## $ tags          <chr> "clash diljit dosanjh|diljit dosanjh|diljit dosanjh goat…
    ## $ view_count    <dbl> 9140911, 2038853, 2381688, 1514614, 1123889, 1050143, 75…
    ## $ likes         <dbl> 296541, 353797, 146740, 156914, 45803, 89192, 8278, 1655…
    ## $ dislikes      <dbl> 6180, 2628, 2794, 5857, 964, 855, 331, 4198, 1860, 5759,…
    ## $ comment_count <dbl> 30059, 40222, 16549, 35331, 2198, 6455, 2441, 15777, 705…
    ## $ category      <fct> Music, Entertainment, Gaming, Blogs, HowToD, Education, …
    ## $ trending_year <chr> "2020", "2020", "2020", "2020", "2020", "2020", "2020", …

2-4. Check for NAs

We check NA now but not at the beginning because we don’t need to care
about NAs in the columns that we don’t use.

``` r
sum(is.na(CA_data)) # 0 NAs
```

    ## [1] 0

No NAs in the columns in our final version of dataset.

## 3. Data analysis

### 3-1. All categories

We first take a look at all categories and all videos, and check out
general trend between 2020 to present.

#### 3-1-1. Number times of trending

Which category is most popular (i.e., on trending most frequent) each
year between 2020 and present?

``` r
# All category and all videos
# no. times of trending of each category
CA_channel_trending <- CA_data %>%
  group_by(trending_year, category) %>%
  summarise(chtrending_n = n(),
            ch_views_sum = sum(view_count),
            ch_views_avg = ch_views_sum / chtrending_n) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'trending_year'. You can override using the
    ## `.groups` argument.

``` r
# No. times of trending & total views
ggplot(CA_channel_trending, aes(y = category, x = chtrending_n, fill = ch_views_sum)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'No. times of trending of each category (total views)',
       x = 'No. of times on Trending',
       y = 'Category')
```

![](2022-10-20-YT_CA_trending_files/figure-gfm/p01_trending_category_total_views-1.png)<!-- -->

Entertainment & Gaming videos became popular from 2021. They both on
trending most frequent and also have higher total views. Note that
although music is the third on trending, it has higher or comparable
total views to Entertainment and Gaming.

``` r
# No. times of trending & average views per trending time
ggplot(CA_channel_trending, aes(y = category, x = chtrending_n, fill = ch_views_avg)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'No. times of trending of each category (average views)',
       x = 'No. of times on Trending',
       y = 'Category')
```

![](2022-10-20-YT_CA_trending_files/figure-gfm/p02_trending_category_avg_views-1.png)<!-- -->

If look at average views per trending time, music still has higher or
comparable total views to Entertainment and Gaming.

What are the channels on trending for \>200 times?

``` r
# no. times of trending of each channel
CA_channel_trending <- CA_data %>%
  group_by(trending_year, channelTitle) %>%
  summarise(chtrending_n = n(),
            ch_views_sum = sum(view_count)) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'trending_year'. You can override using the
    ## `.groups` argument.

``` r
# Channels on Trending for >200 times
CA_channel_200trending <- CA_channel_trending %>%
  arrange(desc(chtrending_n)) %>%
  filter(chtrending_n > 200)

# Trending times do not correlate with # views
ggplot(CA_channel_200trending, aes(y = channelTitle, x = chtrending_n, fill = ch_views_sum)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Channels on trending >200 times',
       x = 'No. of times on trending',
       y = 'YouTube Channels')
```

![](2022-10-20-YT_CA_trending_files/figure-gfm/p03_trending_channels-1.png)<!-- -->

Sports channels are on trending most frequent from 2020 to 2022.

Note that both of MrBeast’s channels “MrBeast” and “MrBeast Gaming” were
on trending \>400 times in 2021.

Which videos are on trending for \>30 days?

``` r
# Videos on Trending for >30 days
CAvideo_trending <- CA_data %>%
  group_by(title) %>%
  summarise(vtrending_n = n(),
            video_views_sum = sum(view_count),
            category = first(category),
            channelTitle = last(channelTitle)) %>%
  as.data.frame()

CA_video_30trending <- CAvideo_trending %>%
  arrange(desc(vtrending_n)) %>%
  filter(vtrending_n > 30)

CA_video_30trending
```

    ##              title vtrending_n video_views_sum category channelTitle
    ## 1 Starlink Mission         101        78914625  Science       SpaceX

SpaceX’s Starlink Mission is the one on Trending most frequent.

#### 3-1-2. Highest views

Now, we only select the top 100 videos with highest views.

``` r
# rank Canada videos with top 100 highest views from 2020 to 2022
# total 100 videos
CA_channel_top100 <- CA_data %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# rank Canada channels with top 100 highest views from 2020 to 2022
# total 300 videos = 100 videos per year
CA_channel_top100_year <- CA_data %>%
  group_by(trending_year) %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# Category of highest views
CA_category_views <- CA_channel_top100_year %>%
  group_by(category, trending_year) %>%
  summarise(n_cat_views = n(),
            avg_views = sum(view_count) / n_cat_views) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'category'. You can override using the
    ## `.groups` argument.

``` r
# Top 100 views videos are Music and Entertainment
ggplot(CA_category_views, aes(y = category, x = n_cat_views, fill = avg_views)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Category of videos with top 100 views (2020 - present)',
       x = '# of trending videos in top 100 views',
       y = 'YouTube Category',
       color = "Avg views")
```

![](2022-10-20-YT_CA_trending_files/figure-gfm/p03_channels_top100views-1.png)<!-- -->

In 2020, most popular videos are music, then shift to Entertainmen in
2021. Two most popular categories in 2022 are News and music.

``` r
# Channels with videos of top 100 views: Higher views do not correlate with more likes
ggplot(CA_channel_top100, aes(y = title, x = view_count, alpha = likes)) +
  geom_point(color = "blue") +
  theme_linedraw(base_family = "NanumGothic") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Top 100 Canadian YouTube videos with higest views',
       x = '# of views',
       y = 'YouTube videos')
```

![](2022-10-20-YT_CA_trending_files/figure-gfm/p04_channels_top100views-1.png)<!-- -->

Black Pink has the highest views.

FFUNTV are mostly shorts!

Big Hit Labels is the former name of HYBE LABELS, which is the company
that BTS is belonged to.

``` r
# Top 100 viewed videos: Higher views do not correlate with more likes
ggplot(CA_channel_top100, aes(y = title, x = view_count, alpha = likes)) +
  geom_point(color = "blue") +
  theme_linedraw(base_family = "NanumGothic") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Top 100 Canadian YouTube videos with higest views',
       x = '# of views',
       y = 'YouTube videos')
```

![](2022-10-20-YT_CA_trending_files/figure-gfm/p05_videos_top100views-1.png)<!-- -->

4 / 15 videos of highest views are BTS, Black Pink and shorts.

### 3-1-3. Paid by ad views

To estimate how much each channel get paid.

“A good rule of thumb is assuming that only half of your views across
the board will be monetized. That said, somewhere around \$5-7 per 1,000
views would be the average across all industries.”

Ref: [How Much Money Do You Get Per View on YouTube? (2022
Stats)](https://www.thinkific.com/blog/youtube-money-per-view/)

``` r
## Paid by views of each channel
# All category
CAchannel_pay <- CA_data %>%
  group_by(channelTitle) %>%
  summarise(view_sum = sum(view_count),
            USD_pay_in_k = (view_sum /1000 * 5)/100 ) %>%
  arrange(desc(USD_pay_in_k)) %>%
  slice(1:20)

ggplot(CAchannel_pay, aes(y = channelTitle, x = USD_pay_in_k, fill = view_sum)) +
  geom_col() +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Canadian YouTube channels earned (per 1000 views)',
       x = 'USD (per 1000 dollars)',
       y = 'Category',
       fill = "Total views")
```

![](2022-10-20-YT_CA_trending_files/figure-gfm/p06_channels_paid-1.png)<!-- -->

MrBeast earns lots of money!

### 3-2. Channels with excluded categories

Because channels with high views are music, movies, sports, TV shows, we
want to exclude those to see what types of channels give higher views.

``` r
# list of commercials to be excluded
comm_ch_list <- c("HYBE LABELS", "starshipTV", "Stone Music Entertainment", 
                  "NPR Music", "Navrattan Music", "Strange Music Inc",
                  "Zee Music Company", "Desi Music Factory", "MTV",
               "Paramount Pictures", "Warner Bros. Pictures", "Sony Pictures Entertainment",
               "Universal Pictures", "Magnolia Pictures & Magnet Releasing",
               "Orion Pictures", "Marvel Entertainment", "JYP Entertainment", "Big Hit Labels",
               "Apple", "amazon",  "T-Mobile", "Samsung Mobile USA", "Google")

comm_title_list <- c("Music Video", "Official Video", "Official Music Video", "Official Lyric Video",
                  "Official Audio", "Oficial", "Official MV", "Official Teaser", "Shorts", "shorts",
                  "SHORTS", "Trailer", "TRAILER", "Teaser Video", "TEASER", "M/V", "MV")

## Exclude music, films, sports, etc
CA_nocomm <- CA_data %>%
  filter(categoryId != 2 & categoryId != 17 & categoryId != 25 & categoryId != 43) %>%
  filter(!grepl(paste(comm_title_list, collapse = "|"), title)) %>%
  filter(!grepl(paste(comm_ch_list, collapse = "|"), channelTitle)) %>%
  filter(!grepl('VEVO|Vevo', channelTitle))
```

#### 3-2-1. Number times of trending of each category

Which category is most popular? (i.e., on trending most often)

``` r
# No. times of trending of each category
CA_nocomm_trending<- CA_nocomm %>%
  group_by(category, trending_year) %>%
  summarise(n_personal_trending = n(),
            avg_views = sum(view_count) / n_personal_trending) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'category'. You can override using the
    ## `.groups` argument.

``` r
ggplot(CA_nocomm_trending, aes(y = category, x = n_personal_trending, fill = avg_views)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'All Canadian YouTube videos',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = 'No. of times on Trending',
       y = 'Category',
       fill = "Avg views")
```

![](2022-10-20-YT_CA_trending_files/figure-gfm/p07_ex-category-trending-avg-views-1.png)<!-- -->

Gaming and Entertainment (personal, not commercial) became very popular
since 2021.

### 3-2-2. Highest view

What are the categories with most highest views?

``` r
# Personal videos of top 100 videos of each year
# total = 300 videos
CA_nocomm_top100_year <- CA_nocomm %>%
  group_by(trending_year) %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# Category of highest views
CA_nocomm_top100_catyear <- CA_nocomm_top100_year %>%
  group_by(category, trending_year) %>%
  summarise(n_per_cat_views = n(),
            avg_percat_views = sum(view_count) / n_per_cat_views) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'category'. You can override using the
    ## `.groups` argument.

``` r
ggplot(CA_nocomm_top100_catyear, aes(y = category, x = n_per_cat_views, fill = avg_percat_views)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'No. times of trending of Canadian YouTube videos with top 100 higest views',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = 'No. times on Trending',
       y = 'Category',
       fill = "Avg views")
```

![](2022-10-20-YT_CA_trending_files/figure-gfm/p08_ex-category-top100views-1.png)<!-- -->

Entertainment is the one with highest views from 2020 to 2022.

What are the channels of top 100 videos with highest views?

``` r
# Personal videos of top 45 videos of each year
# total = 135 videos
CA_nocomm_top50_year <- CA_nocomm %>%
  group_by(trending_year) %>%
  arrange(desc(view_count)) %>%
  slice(1:45)

# Channels of top 50 views videos of each year
# exclude shorts & commercials
ggplot(CA_nocomm_top50_year, aes(y = channelTitle, x = view_count, alpha = likes, color = trending_year)) +
  geom_point() +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)) + 
  facet_grid(. ~trending_year) +
  theme_linedraw(base_family = "NanumGothic") +
  labs(title = 'Top 45 Canadian YouTube Channels with higest views of each year',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = '# of views',
       y = 'YouTube channels',
       color = "Trending year")
```

![](2022-10-20-YT_CA_trending_files/figure-gfm/p09_ex-channels-top100views-1.png)<!-- -->

MrBeast has the highest views in 2021

What are the vidoes with highest views?

``` r
# Top 100 highest views of total excl. videos
# total = 100 videos in 3 years
CA_nocomm_top100_view <-CA_nocomm %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# Videos of top 100 highest views
# No. times on trending
CA_nocomm_top100_view_ntrending <- CA_nocomm_top100_view %>%
  group_by(title) %>%
  summarise(channelTitle = first(channelTitle),
            v_ntrending = n(),
            sum_view = sum(view_count)) %>%
  arrange(desc(v_ntrending))

View(CA_nocomm_top100_view_ntrending)
```

#### 3-2-3. Paid by ad views

Which channels make most money?

``` r
CA_nocomm_paytop20 <- CA_nocomm %>%
  group_by(channelTitle) %>%
  summarise(personal_views = sum(view_count),
            personal_pay_in_k = (personal_views / 1000 * 5)/1000) %>%
  arrange(desc(personal_pay_in_k)) %>%
  slice(1:20)

ggplot(CA_nocomm_paytop20, aes(y = channelTitle, x = personal_pay_in_k, fill = personal_views)) +
  geom_col() +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Canadian YouTube channels earned (per 1000 views)',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = 'USD (per 1000 dollars)',
       y = 'Category',
       fill = "Total views")
```

![](2022-10-20-YT_CA_trending_files/figure-gfm/p10_ex-channels-paid-1.png)<!-- -->

YouTube trending video data (3) - Japan
================

## US, Canada & Japan

### Background:

This is just for practice and for fun and thus, will only focus on US,
Canada and Japan YouTube channels. This will be divided into 3 parts so
it won’t be too long.

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
Music<br> 15 - Pets & Animals<br> 17 - Sports<br> 19 - Travel &
Events<br> 20 - Gaming<br> 21 - Vblogging<br> 22 - Blogs<br> 23 -
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
JP <- read_csv("/Users/Linda/Desktop/RStudio/Kaggle/YouTube/JP_youtube_trending_data.csv")
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 157186 Columns: 16
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
sum(duplicated(JP))  # 149 duplicate
```

    ## [1] 149

2-2. Remove duplicates

``` r
# use distinct() to remove duplicates
JP <- distinct(JP)

# check if it's removed
sum(duplicated(JP))
```

    ## [1] 0

2.3 - Extract columns we need

We will only look at these columns: title, channel_title, category_id,
trending_date, tags, views, likes, dislikes, comment_count

``` r
# select columns we need for analysis
# add a "category" column
JP_data <- JP %>%
  select(title, channelTitle, categoryId, trending_date, tags, view_count, likes, dislikes, comment_count) %>%
  mutate("category" = as.factor(categoryId)) %>%
  mutate(category = recode(category, "1" = "Film", "2" = "Autos", "10" = "Music", "15" = "Pets", 
                           "17" = "Sports", "19" = "Travel", "20" = "Gaming", "22" = "Blogs",
                           "23" = "Comedy", "24" = "Entertainment", "25" = "News", 
                           "26" = "HowToD", "27" = "Education", "28" = "Science", "29" = "NPO"))

# convert trending date to the date format
JP_data$trending_date<- as.Date(JP_data$trending_date, format = "%y-%m-%d")

# create a "trending _year" column
JP_data <- JP_data %>%
  mutate(trending_year = format(trending_date, format = '%Y'))

glimpse(JP_data)
```

    ## Rows: 157,037
    ## Columns: 11
    ## $ title         <chr> "皆からの色々な質問に何も隠さず答える！びっくりさせたら…
    ## $ channelTitle  <chr> "タナカガ", "(パーソル パ・リーグTV公式)PacificLeagueTV"…
    ## $ categoryId    <dbl> 22, 17, 23, 20, 1, 26, 10, 22, 10, 20, 24, 20, 10, 24, 2…
    ## $ trending_date <date> 2020-08-12, 2020-08-12, 2020-08-12, 2020-08-12, 2020-08…
    ## $ tags          <chr> "[None]", "パーソルパリーグTV|パリーグTV|パシフィックリ…
    ## $ view_count    <dbl> 778499, 1161952, 1980557, 2381688, 442524, 431031, 60000…
    ## $ likes         <dbl> 34811, 18514, 63961, 146742, 14388, 6096, 714306, 8627, …
    ## $ dislikes      <dbl> 667, 259, 692, 2794, 73, 123, 15176, 134, 572, 163, 420,…
    ## $ comment_count <dbl> 3939, 4115, 6216, 16557, 1420, 607, 31040, 1781, 826, 14…
    ## $ category      <fct> Blogs, Sports, Comedy, Gaming, Film, HowToD, Music, Blog…
    ## $ trending_year <chr> "2020", "2020", "2020", "2020", "2020", "2020", "2020", …

2-4. Check for NAs

We check NA now but not at the beginning because we don’t need to care
about NAs in the columns that we don’t use.

``` r
sum(is.na(JP_data)) # 0 NAs
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
JP_channel_trending <- JP_data %>%
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
ggplot(JP_channel_trending, aes(y = category, x = chtrending_n, fill = ch_views_sum)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'No. times of trending of each category (total views)',
       x = 'No. times on Trending',
       y = 'Category')
```

![](2022-10-29-YT-JP-trending_files/figure-gfm/p01_trending-category-total-views-1.png)<!-- -->

Unlike US and Canada, which music is very outstanding, Entertainment is
the most popular category in Japan from 2020 to 2022. Music is rather
low. Blogs and Gaming is a big higher than Music.

``` r
# No. times of trending & average views per trending time
ggplot(JP_channel_trending, aes(y = category, x = chtrending_n, fill = ch_views_avg)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'No. times of trending of each category (average views)',
       x = 'No. of times on Trending',
       y = 'Category')
```

![](2022-10-29-YT-JP-trending_files/figure-gfm/p02_trending-category-avg-views-1.png)<!-- -->

If look at average views per trending time, entertainment still has
higher or comparable total views to Entertainment and Gaming.

What are the channels on trending for \>250 times?

``` r
# no. times of trending of each channel
JP_channel_trending <- JP_data %>%
  group_by(trending_year, channelTitle) %>%
  summarise(chtrending_n = n(),
            ch_views_sum = sum(view_count)) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'trending_year'. You can override using the
    ## `.groups` argument.

``` r
# Channels on Trending for >250 times
JP_channel_250trending <- JP_channel_trending %>%
  arrange(desc(chtrending_n)) %>%
  filter(chtrending_n > 250)

# Fig 03 - Trending times do not correlate with # views
ggplot(JP_channel_250trending, aes(y = channelTitle, x = chtrending_n, fill = ch_views_sum)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme_linedraw(base_family = "HiraKakuProN-W3") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Channels on trending >250 times',
       x = 'No. of times on trending',
       y = 'YouTube Channels',
       fill = 'Total Views')
```

![](2022-10-29-YT-JP-trending_files/figure-gfm/p03_trending-channels-1.png)<!-- -->

Although MrBeast’s channels “MrBeast” and “MrBeast Gaming” are very
popular in North American and are on trending \>400 times, it is not
popular at all in Japan.

Which videos are on trending for \>20 times?

``` r
# Videos on Trending for >20 times
JPvideo_trending <- JP_data %>%
  group_by(title) %>%
  summarise(vtrending_n = n(),
            video_views_sum = sum(view_count),
            category = first(category),
            channelTitle = last(channelTitle)) %>%
  as.data.frame()

JP_video_20trending <- JPvideo_trending %>%
  arrange(desc(vtrending_n)) %>%
  filter(vtrending_n > 20)

JP_video_20trending
```

    ##                                                        title vtrending_n
    ## 1                                                     ご報告          51
    ## 2                                         ご報告があります。          30
    ## 3    Ｐ丸様が授業をした結果WWWWWWWWW【Ｐ丸様。】【すとぷり】          21
    ## 4 あなたが落としたのはこのアホですか？【ジェル】【すとぷり】          21
    ## 5                                               彼氏について          21
    ## 6                                               試合を終えて          21
    ##   video_views_sum      category                channelTitle
    ## 1        72161477         Blogs                社畜OLちえ丸
    ## 2         8694165 Entertainment カズチャンネル/Kazu Channel
    ## 3        28959973 Entertainment            ジェルちゃんねる
    ## 4        26185568 Entertainment                     P丸様。
    ## 5        41091834 Entertainment            中町綾チャンネル
    ## 6        54215996 Entertainment     朝倉未来 Mikuru Asakura

The video on trending most frequent is a …. 社畜OL的生活頻道。

#### 3-1-2. Highest views

Now, we only select the top 100 videos with highest views.

``` r
# rank Japan videos with top 100 highest views from 2020 to 2022
# total 100 videos
JP_channel_top100 <- JP_data %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# rank Japan videos with top 100 highest views from 2020 to 2022
# total 300 videos = 100 videos per year
JP_channel_top100_year <- JP_data %>%
  group_by(trending_year) %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# Category of highest views
JP_category_views <- JP_channel_top100_year %>%
  group_by(category, trending_year) %>%
  summarise(n_cat_views = n(),
            avg_views = sum(view_count) / n_cat_views) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'category'. You can override using the
    ## `.groups` argument.

``` r
# Top 100 views videos are Music and Entertainment
ggplot(JP_category_views, aes(y = category, x = n_cat_views, fill = avg_views)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  theme_linedraw(base_family = "HiraKakuProN-W3") +
  labs(title = 'Category of videos with top 100 views (2020 - present)',
       x = '# of trending videos in top 100 views',
       y = 'YouTube Category')
```

![](2022-10-29-YT-JP-trending_files/figure-gfm/p04_trending-category-top100-1.png)<!-- -->

From 2020 to 2022, most popular videos are all music.

``` r
# Channels with videos of top 100 views: Higher views do not correlate with more likes
ggplot(JP_channel_top100, aes(y = channelTitle, x = view_count, alpha = likes)) +
  geom_point(color = "blue") +
  theme_linedraw(base_family = "HiraKakuProN-W3") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Japan YouTube Channels with higest top 100 views',
       x = '# of viewss',
       y = 'YouTube Channels')
```

![](2022-10-29-YT-JP-trending_files/figure-gfm/p05_trending-channels-top100-1.png)<!-- -->

Most popular channels with highest views are BLACK PINK and BTS.

Big Hit Labels is the former name of HYBE LABELS, which is the company
that BTS is belonged to.

``` r
# Top 100 viewed videos: Higher views do not correlate with more likes
ggplot(JP_channel_top100, aes(y = title, x = view_count, alpha = likes)) +
  geom_point(color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = -1)) + 
  theme_bw(base_family = "NanumGothic") +
  labs(title = 'Top 100 Japan YouTube videos with higest views',
       x = '# of views',
       y = 'YouTube videos')
```

![](2022-10-29-YT-JP-trending_files/figure-gfm/p06_top100-viewed-videos-1.png)<!-- -->

9 / 15 videos of highest views are BTS and Black PINK!

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
JPchannel_pay <- JP_data %>%
  group_by(channelTitle) %>%
  summarise(view_sum = sum(view_count),
            USD_pay_in_k = (view_sum /1000 * 5)/100 ) %>%
  arrange(desc(USD_pay_in_k)) %>%
  slice(1:20)

ggplot(JPchannel_pay, aes(y = channelTitle, x = USD_pay_in_k, fill = view_sum)) +
  geom_col() +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme_linedraw(base_family = "HiraKakuProN-W3") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Amount of top view channel got paid',
       x = 'USD (per 1000 dollars)',
       y = 'Category')
```

![](2022-10-29-YT-JP-trending_files/figure-gfm/p07_top20-paid-1.png)<!-- -->

Different from the US and Canada, MrBeast is not popular in Japan. SM
TOMN, JYP Entertainment and HYBE LABELS are Korean agency companies.

### 3-2. Channels with excluded categories

Because channels with high views are music, movies, sports, TV shows, we
want to exclude those to see what types of channels give higher views.

``` r
## Exclude music, films, sports, etc
# list of commercials to be excluded
comm_ch_list <- c("HYBE LABELS", "starshipTV", "Stone Music Entertainment", 
                  "NPR Music", "Navrattan Music", "Strange Music Inc",
                  "Zee Music Company", "Desi Music Factory", "MTV",
                  "Paramount Pictures", "Warner Bros. Pictures", "Sony Pictures Entertainment",
                  "Universal Pictures", "Magnolia Pictures & Magnet Releasing",
                  "Orion Pictures", "Marvel Entertainment", "JYP Entertainment", "Big Hit Labels",
                  "Apple", "amazon",  "T-Mobile", "Samsung Mobile USA", "Google")

comm_title_list <- c("Music Video", "Official Video", "Official Music Video", "Official Lyric Video",
                     "Official Audio", "Official MV", "Official Teaser", "Shorts", "shorts",
                     "SHORTS", "Trailer", "TRAILER", "Teaser Video", "TEASER", "M/V", "MV", "Video Oficial")

## Exclude music, films, sports, etc
JP_nocomm <- JP_data %>%
  filter(categoryId != 2 & categoryId != 17 & categoryId != 25 & categoryId != 43) %>%
  filter(!grepl(paste(comm_title_list, collapse = "|"), title)) %>%
  filter(!grepl(paste(comm_ch_list, collapse = "|"), channelTitle)) %>%
  filter(!grepl('VEVO|Vevo', channelTitle))
```

#### 3-2-1. Number times of trending of each category

Which category is most popular? (i.e., on trending most often)

``` r
# No. times of trending of each category
JP_personal_trending <- JP_nocomm %>%
  group_by(category, trending_year) %>%
  summarise(n_personal_trending = n(),
            avg_views = sum(view_count) / n_personal_trending) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'category'. You can override using the
    ## `.groups` argument.

``` r
ggplot(JP_personal_trending, aes(y = category, x = n_personal_trending, fill = avg_views)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Top 100 Japan YouTube videos with higest views',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = 'No. of times on Trending',
       y = 'Category',
       fill = "Average views")
```

![](2022-10-29-YT-JP-trending_files/figure-gfm/p08_ex-trending-category-avg-views-1.png)<!-- -->

Gaming and Entertainment (personal, not commercial) became very popular
since 2021.

### 3-2-2. Highest view

What are the categories with most highest views?

``` r
# Personal videos of top 100 videos of each year
# total = 300 videos
JP_nocomm_top100_year <- JP_nocomm %>%
  group_by(trending_year) %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# Category of highest views
JP_personal_top100_bycat <- JP_nocomm_top100_year %>%
  group_by(category, trending_year) %>%
  summarise(n_per_cat_views = n(),
            avg_percat_views = sum(view_count) / n_per_cat_views) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'category'. You can override using the
    ## `.groups` argument.

``` r
ggplot(JP_personal_top100_bycat, aes(y = category, x = n_per_cat_views, fill = avg_percat_views)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'No. times of trending of Japan YouTube videos with top 100 higest views',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = 'No. times on Trending',
       y = 'Category',
       fill = "Average views")
```

![](2022-10-29-YT-JP-trending_files/figure-gfm/p09_ex-category-top100views-1.png)<!-- -->

Entertainment is the one with highest views from 2020 to 2022.

What are the channels of top 100 videos with highest views?

``` r
# Fig 10 - Channels of top 100 views videos
# exclude shorts & commercials
ggplot(JP_nocomm_top100_year, aes(y = channelTitle, x = view_count, alpha = likes, color = trending_year)) +
  geom_point() +
  facet_grid(.~trending_year) +
  theme_linedraw(base_family = "HiraKakuProN-W3") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Top 100 Japan YouTube videos with higest views',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = '# of views',
       y = 'YouTube channels',
       color = "Trending year")
```

![](2022-10-29-YT-JP-trending_files/figure-gfm/p10_ex-trending-channels-top100views-1.png)<!-- -->

Black Pink has the highest views in 2021 & 2022.

What are the vidoes with highest views?

``` r
# Top 100 highest views of total excl. videos
# total = 100 videos in 3 years
JP_nocomm_top100_view <-JP_nocomm %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# Videos of top 100 highest views
# No. times on trending
JP_nocomm_top100_view_ntrending <- JP_nocomm_top100_view %>%
  group_by(title) %>%
  summarise(channelTitle = first(channelTitle),
            v_ntrending = n(),
            sum_view = sum(view_count)) %>%
  arrange(desc(v_ntrending))

View(JP_nocomm_top100_view_ntrending)
```

#### 3-2-3. Paid by ad views

Which channels make most money?

``` r
JP_personal_paytop20 <- JP_nocomm %>%
  group_by(channelTitle) %>%
  summarise(personal_views = sum(view_count),
            personal_pay_in_k = (personal_views / 1000 * 5)/1000) %>%
  arrange(desc(personal_pay_in_k)) %>%
  slice(1:20)

ggplot(JP_personal_paytop20, aes(y = channelTitle, x = personal_pay_in_k, fill = personal_views)) +
  geom_col() +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme_linedraw(base_family = "HiraKakuProN-W3") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(title = 'Japan YouTube channels earned (per 1000 views)',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = 'USD (per 1000 dollars)',
       y = 'Category',
       fill = "Views")
```

![](2022-10-29-YT-JP-trending_files/figure-gfm/p12_ex-top20-paid-1.png)<!-- -->

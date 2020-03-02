AssignmentFour
================
ad699
26/02/2020

``` r
library(tidyverse)
Data8 <- read_tsv("/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")


Data8 <- Data8 %>%
    select(pidp, h_age_dv, h_payn_dv, h_gor_dv)

Stable <- read_tsv("/Users/alexdennis/Documents/DA-III/data/UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")

Stable <- Stable %>%
    select(pidp, sex_dv, ukborn, plbornc)

Data <- Data8 %>% left_join(Stable, "pidp")

rm(Data8, Stable)

Data <- Data %>%
    mutate(sex_dv = ifelse(sex_dv == 1, "male",
                       ifelse(sex_dv == 2, "female", NA))) %>%
    mutate(h_payn_dv = ifelse(h_payn_dv < 0, NA, h_payn_dv)) %>%
    mutate(h_gor_dv = recode(h_gor_dv,
                     `-9` = NA_character_,
                     `1` = "North East",
                     `2` = "North West",
                     `3` = "Yorkshire",
                     `4` = "East Midlands",
                     `5` = "West Midlands",
                     `6` = "East of England",
                     `7` = "London",
                     `8` = "South East",
                     `9` = "South West",
                     `10` = "Wales",
                     `11` = "Scotland",
                     `12` = "Northern Ireland")) %>%
    mutate(placeBorn = case_when(
            ukborn  == -9 ~ NA_character_,
            ukborn < 5 ~ "UK",
            plbornc == 5 ~ "Ireland",
            plbornc == 18 ~ "India",
            plbornc == 19 ~ "Pakistan",
            plbornc == 20 ~ "Bangladesh",
            plbornc == 10 ~ "Poland",
            plbornc == 27 ~ "Jamaica",
            plbornc == 24 ~ "Nigeria",
            TRUE ~ "other")
    )
  
Data 
```

    ## # A tibble: 39,293 x 8
    ##       pidp h_age_dv h_payn_dv h_gor_dv      sex_dv ukborn plbornc placeBorn
    ##      <dbl>    <dbl>     <dbl> <chr>         <chr>   <dbl>   <dbl> <chr>    
    ##  1   22445       31     2000  London        female      1      -9 UK       
    ##  2   29925       39     1200  London        female      1      -9 UK       
    ##  3   76165       33     2200  West Midlands female      1      -9 UK       
    ##  4  223725       41       NA  South East    male        1      -9 UK       
    ##  5  280165       37     2263  South East    female      1      -9 UK       
    ##  6  333205       26     1550  West Midlands female      1      -9 UK       
    ##  7  665045       34      772. West Midlands male        1      -9 UK       
    ##  8  813285       46       NA  North West    male        1      -9 UK       
    ##  9 1587125       50       NA  North East    female     -9      -9 <NA>     
    ## 10 1697285       43     1171  East Midlands male        1      -9 UK       
    ## # … with 39,283 more rows

Graph 1: Univariate Distribution (n Respondents/Net Monthly Pay)
================================================================

``` r
Data.1 <- Data %>%
  filter(!is.na(h_payn_dv)) %>%
  select(h_payn_dv)

ggplot(Data.1, aes(h_payn_dv)) +
  geom_freqpoly() +
  labs(
    x = "Net monthly pay",
    y = "Number of respondents"
  )
```

![](AssignmentFour_files/figure-markdown_github/unnamed-chunk-2-1.png?raw=true)

*Interpretation*
----------------

The most common net monthly pay is ~£1700; then the number of people with monthly net pay greater than this, sharply declines. Furthermore, at the left-most part of the graph, it shows some individuals that make a monthly *loss*. This shows the importance of the word *net*.

Graph 2: Non-Parametric association between age and monthly earnings for men and women
======================================================================================

``` r
Data.2 <- Data %>%
  filter(!is.na(h_payn_dv)) %>%
  filter(!is.na(sex_dv)) %>%
  filter(!is.na(h_age_dv))

ggplot(Data.2, aes(h_age_dv, h_payn_dv)) + 
  geom_smooth(aes(linetype = sex_dv), colour = "black", size = 0.75) +
  labs(
    x = "Age",
    y = "Monthly earnings"
  ) +
  lims(
    x = c(16, 65)
  )
```

![](AssignmentFour_files/figure-markdown_github/unnamed-chunk-3-1.png?raw=true)

*Interpretation*
----------------

This graph makes it clear that, at all ages, men have higher monthly earnings than women, however, they both have quadratic curvilinear lines: they both peak between 40 and 50, and then tail off after this point. The 'female' line is less uniformally curvilinear, than that of its 'male' counterpart, with the distortion starting to take place in the late 20s. This can possibly be explained by the affect of childbirth (and subsequently staying at home to care for the child), on female earning potential.

Graph 3: Faceted bar chart
==========================

``` r
Data.3 <- Data %>%
  filter(!is.na(h_payn_dv)) %>%
  filter(!is.na(placeBorn)) %>%
  group_by(sex_dv, placeBorn) %>%
  summarise(median = median(h_payn_dv))

ggplot(data = Data.3, aes(x = sex_dv, y = median)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ placeBorn) +
  labs(
      x = "Sex",
      y = "Median monthly net pay"
    )
```

![](AssignmentFour_files/figure-markdown_github/unnamed-chunk-4-1.png?raw=true)

*Interpretation*
----------------

This graphic demonstrates that male median monthly net pay (for those who live in the UK but born in the above countries) is higher than that of its female counterparts. This is true across all 9 groupings, suggesting a pervasiveness of this phenomenon. Furthermore, it shows a disparity in pay between people with differing countries of birth. Those born in Bangladesh earn far less than those born in the UK, yet those born in Ireland earn more.

Graph 4: Heat map
=================

``` r
Data.4 <- Data %>%
  filter(!is.na(h_gor_dv)) %>%
  filter(!is.na(placeBorn)) %>%
  filter(!is.na(h_age_dv)) %>%
  group_by(placeBorn, h_gor_dv) %>%
  summarise(mean = mean(h_age_dv))

ggplot(data = Data.4, aes(x = h_gor_dv, y = placeBorn)) +
  geom_raster(aes(fill = mean)) +
  labs(
    y = "Country of birth", 
    x = "Region", 
    fill = "Mean age"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank()
        ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](AssignmentFour_files/figure-markdown_github/unnamed-chunk-5-1.png?raw=true)

*Interpretation*
----------------

The graph is helpful in discerning intra- and inter-group mean age-profile differences. Intra-group differences can be seen, for example, with Nigerian-born individuals (between those in Scotland, and those in Wales, say). However, the n for these samples is important if we are to trust the data. In terms of inter-group differences, an example is: the higher average age of Jamaican-born individuals in the UK, compared to Pakistan-born individuals in the UK. The visualisation of these two types of differences, can aid one in understanding the trends of immigration over time.

Graph 5: Population pyramid
===========================

``` r
Data.5 <- Data %>%
  filter(!is.na(h_age_dv)) %>%
  filter(!is.na(sex_dv)) %>%
  select(h_age_dv, sex_dv)
 
ggplot(Data.5, aes(x = h_age_dv, fill = sex_dv)) +
  geom_bar(data=subset(Data, sex_dv=="female")) + 
  geom_bar(data=subset(Data, sex_dv=="male"), aes(y=..count..*(-1))) +
  coord_flip() +
  labs(y = "n",
       x = "Age",
       fill = "Sex") +
  theme_bw() +
  scale_colour_manual(values = c("red", "blue"),
                       
                       aesthetics = c("colour", "fill"))
```

![](AssignmentFour_files/figure-markdown_github/unnamed-chunk-6-1.png?raw=true)

*Interpretation*
----------------

The distributions of age, for both male and female, is similar. Most individuals of both sexes are in the 50-75 age bracket, with the n diminishing very quickly after this point. This graph may be helpful in instructing one on the historical childbirth trends of those living in the UK. However, the similarity in age patterns could possibly be attributed to the amount of couples in the dataset. Usually, couples are of similar age, and so this could explain the similarity of distribution.

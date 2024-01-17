---
title: My First Post!
author: ''
date: '2024-01-17'
slug: my-first-post
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2024-01-17T18:03:03-03:00'
featured: no
image: 
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---



## Analyzing Sectoral ETFs with R:

## Introduction

Welcome to my first blog post! As a quant data scientist, I often find myself working with financial data, and one of my favorite tools for this is R. In this post, I'm going to walk you through downloading ETF prices, specifically sectoral ETFs, from Yahoo Finance, and then we'll compute daily, monthly, and yearly returns. We'll be using two powerful packages: `tidyquant` and `quantmod`. Let's get started!

### Setting Up Your Environment

First, ensure that you have the necessary packages installed, the load these packages into your R session:


```r
library(tidyquant)
library(quantmod)
library(tidyverse)
library(ggridges)
library(knitr)
library(PerformanceAnalytics)
```

### Downloading Sectoral ETF Data

Let's start by downloading some sectoral ETF data. For this demonstration, we'll use a few ETFs as examples and select only the adjusted prices. You can replace these with ETFs of your choice. By using the 'tq_get' function, the resulting object will be a tidy long table with all symbols listed in one column.


```r
# Define the ETF tickers
etf_symbols <- c("XLF", "XLV", "XLE", "XLI", "XLY", "XLP", "XLK", "XLU", "XLRE", "XLC", "XLB")

# Download data using tidyquant
etf_data <- tq_get(etf_symbols, from = "2000-01-01", to = today()) %>% 
    select(symbol, date, adjusted)
kable(etf_data %>% slice(1:5))
```



|symbol |date       | adjusted|
|:------|:----------|--------:|
|XLF    |2000-01-03 | 11.48061|
|XLF    |2000-01-04 | 10.97873|
|XLF    |2000-01-05 | 10.89247|
|XLF    |2000-01-06 | 11.37083|
|XLF    |2000-01-07 | 11.55904|

Alternatively, you can use the 'getSymbols' function from the quantmod package and then merge all the tickers into one xts object, resulting in a wide-format table. For the sake of simplicity, let's stick with the tidy approach (etf_data).


```r
getSymbols(etf_symbols, src = "yahoo", from = "2020-01-01", to = "2023-01-01")
# Merge all ETFs into one xts object
merged_adjusted_prices <- do.call(merge, lapply(etf_symbols, function(sym) {
  Ad(get(sym))
}))
```

### Computing Returns

Now, let's compute the daily, monthly, and yearly logarithmic returns. We'll use the tidyquant package for this task.


```r
daily_returns <- etf_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "daily_return")

monthly_returns <- etf_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               type = "log",
               col_rename = "monthly_return")

yearly_returns <- etf_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               type = "log",
               col_rename = "yearly_return")
```

### Visualization and Analysis

With the computed returns, you can now create various visualizations and perform further analysis. Lets get some fun using ggplot and ggridges libraries.


```r
yearly_returns %>% 
   ggplot(aes(x =  as.factor(year(date)), y = yearly_return, fill = symbol)) +
      geom_col() +
      theme_minimal() +
      facet_wrap(~symbol)+
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(breaks = seq(2000,2025, by=5))+
      labs(title = "", y = "Yearly Returns", x = "") +
      theme(legend.position = "none", axis.text.x = element_text(angle = 65, hjust = 1)) 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />


```r
yearly_returns %>%
    ggplot(aes(x = symbol, y = yearly_return, fill = symbol)) +
    geom_boxplot() +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "", x = "", y = "Yearly Return") +
    theme(legend.position = "none")  
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />


```r
yearly_returns %>%
  ggplot(aes(x = yearly_return, y = symbol, fill = symbol)) +
  geom_density_ridges() +
  scale_fill_viridis_d() +
  scale_x_continuous(labels = scales::percent_format(), breaks = seq(-1, 1, by = 0.2)) +
  labs(title = "Density of Yearly Returns for Each ETF") +
  theme_ridges() +  
  theme(legend.position = "none",axis.title.y = element_blank(),axis.title.x = element_blank())
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

*Let's dive deeper into our analysis by displaying a comprehensive table that will showcase summary statistics for each sector ETF.*


```r
finance_summary_stats <- yearly_returns %>%
  group_by(symbol) %>%
  summarise(
    Mean = mean(yearly_return, na.rm = TRUE),
    SD = sd(yearly_return, na.rm = TRUE),
    Min = min(yearly_return, na.rm = TRUE),
    Max = max(yearly_return, na.rm = TRUE),
    Skewness = PerformanceAnalytics::skewness(yearly_return, na.rm = TRUE),
    Kurtosis = PerformanceAnalytics::kurtosis(yearly_return, na.rm = TRUE)
  )
```


```r
# Display the table
kable(finance_summary_stats)
```



|symbol |      Mean|        SD|        Min|       Max|   Skewness|   Kurtosis|
|:------|---------:|---------:|----------:|---------:|----------:|----------:|
|XLB    | 0.0679753| 0.2033694| -0.5806915| 0.3931084| -1.2289761|  2.1881065|
|XLC    | 0.0619817| 0.3060285| -0.4720782| 0.4240962| -0.6586318| -0.6683947|
|XLE    | 0.0692086| 0.2522587| -0.4935640| 0.4966754| -0.4584431| -0.4768523|
|XLF    | 0.0471762| 0.2403684| -0.7963367| 0.3040031| -1.7366809|  3.9850207|
|XLI    | 0.0713439| 0.1870549| -0.4898416| 0.3403836| -1.2463748|  1.5689834|
|XLK    | 0.0610893| 0.2921158| -0.5712391| 0.4448036| -0.8483779| -0.1469061|
|XLP    | 0.0690208| 0.1183949| -0.2243200| 0.2424281| -0.7828004|  0.2054434|
|XLRE   | 0.0549470| 0.1818922| -0.3044793| 0.3791280| -0.1116763|  0.3127526|
|XLU    | 0.0668234| 0.1619749| -0.3412266| 0.2526806| -1.1977022|  0.9041963|
|XLV    | 0.0753812| 0.1238054| -0.2654003| 0.3464881| -0.3805919|  1.0532241|
|XLY    | 0.0813638| 0.2201552| -0.4505491| 0.3558028| -0.8953782|  0.1077713|

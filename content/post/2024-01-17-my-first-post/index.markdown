---
title: My first post
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
```
### Downloading Sectoral ETF Data
Let's start by downloading some sectoral ETF data. For this demonstration, we'll use a few ETFs as examples and select only the adjusted prices. You can replace these with ETFs of your choice. By using the 'tq_get' function, the resulting object will be a tidy long table with all symbols listed in one column.


```r
# Define the ETF tickers
etf_symbols <- c("XLF", "XLV", "XLE", "XLI", "XLY", "XLP", "XLK", "XLU", "XLRE", "XLC", "XLB")

# Download data using tidyquant
etf_data <- tq_get(etf_symbols, from = "2000-01-01", to = today()) %>% 
    select(symbol, date, adjusted)
etf_data %>% slice(1:5)
```

```
## # A tibble: 5 × 3
##   symbol date       adjusted
##   <chr>  <date>        <dbl>
## 1 XLF    2000-01-03     11.5
## 2 XLF    2000-01-04     11.0
## 3 XLF    2000-01-05     10.9
## 4 XLF    2000-01-06     11.4
## 5 XLF    2000-01-07     11.6
```

Alternatively, you can use the 'getSymbols' function from the quantmod package and then merge all the tickers into one xts object, resulting in a wide-format table. For the sake of simplicity, let's stick with the tidy approach (etf_data).

```r
getSymbols(etf_symbols, src = "yahoo", from = "2020-01-01", to = "2023-01-01")
# Merge all ETFs into one xts object
merged_adjusted_prices <- do.call(merge, lapply(etf_symbols, function(sym) {
  Ad(get(sym))
}))
```

### Computing Returns
Now, let's compute the daily, monthly, and yearly logarithmic returns. We'll use tidyquant and tidyverse packages for this task.


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
    theme(legend.position = "none")  # Remove the legend if not needed
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
  theme(legend.position = "none")  
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

Lets show some summary stats!

```r
finance_summary_stats <- yearly_returns %>%
  group_by(symbol) %>%
  summarise(
    Mean = mean(yearly_return, na.rm = TRUE),
    Median = median(yearly_return, na.rm = TRUE),
    SD = sd(yearly_return, na.rm = TRUE),
    Min = min(yearly_return, na.rm = TRUE),
    Max = max(yearly_return, na.rm = TRUE),
  )
```



```r
calculate_cagr <- function(returns) {
  n_years <- length(unique(year(returns$date)))
  start_value <- 1
  end_value <- prod(1 + returns$yearly_return)
  (end_value / start_value)^(1 / n_years) - 1
}

finance_summary_stats <- yearly_returns %>%
  group_by(symbol) %>%
  summarise(
    Mean = calculate_cagr(cur_data()),  # Pass the current grouped data
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
|XLB    | 0.0436908| 0.2033694| -0.5806913| 0.3931086| -1.2289742|  2.1881015|
|XLC    | 0.0160389| 0.3060284| -0.4720780| 0.4240961| -0.6586318| -0.6683946|
|XLE    | 0.0364349| 0.2522587| -0.4935646| 0.4966754| -0.4584441| -0.4768491|
|XLF    | 0.0006374| 0.2403683| -0.7963361| 0.3040028| -1.7366793|  3.9850133|
|XLI    | 0.0519205| 0.1870547| -0.4898414| 0.3403836| -1.2463759|  1.5689864|
|XLK    | 0.0110787| 0.2921157| -0.5712390| 0.4448036| -0.8483769| -0.1469082|
|XLP    | 0.0622515| 0.1183949| -0.2243206| 0.2424280| -0.7828050|  0.2054524|
|XLRE   | 0.0400574| 0.1818922| -0.3044793| 0.3791279| -0.1116777|  0.3127557|
|XLU    | 0.0529942| 0.1619749| -0.3412265| 0.2526809| -1.1977010|  0.9041941|
|XLV    | 0.0681611| 0.1238054| -0.2654001| 0.3464880| -0.3805929|  1.0532224|
|XLY    | 0.0555992| 0.2201553| -0.4505493| 0.3558030| -0.8953793|  0.1077749|


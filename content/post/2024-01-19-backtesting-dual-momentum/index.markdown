---
title: Backtesting Dual Momentum
author: ''
date: '2024-01-19'
slug: backtesting-dual-momentum
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2024-01-19T16:29:26-03:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---



## Backtesting the Dual Momentum Strategy in R: A Comprehensive Analysis

## Introduction
In this blog post, we examine the Dual Momentum strategy, a concept popularized by Gary Antonacci. This strategy, rooted in the broader realm of momentum investing, has gained considerable attention in both academic circles and practical application. We delve into an R-based backtest of this strategy, offering insights into its underlying principles, historical performance, and recent challenges.

### Momentum Investing and Dual Momentum: Theoretical Foundations
Momentum investing is based on the principle that securities demonstrating strong performance in the recent past will likely continue their trajectory in the short to medium term. Pioneering studies, such as those by Jegadeesh and Titman (1993), provide empirical support for this concept, revealing that stocks with high returns over preceding months often outperform the market.

Gary Antonacci's Dual Momentum strategy integrates two distinct types of momentum:

1. Relative Momentum: This component involves comparing the performance of various assets, selecting the one with the strongest return over a specified period (commonly 12 months).
2. Absolute Momentum: This aspect compares the chosen asset's performance against a risk-free benchmark. If the asset underperforms, the strategy shifts towards safer asset classes, typically bonds.

Let's get to work!!!

### Setting Up Your Environment
First, ensure that you have the necessary packages installed, then load these packages into your R session:

```r
library(tidyverse)
library(tidyquant)
library(timetk)
library(knitr)
library(viridis)
```
### Data Retrieval 
We use tq_get to fetch historical prices for ETFs like SPY (S&P 500), AGG (Aggregate Bond), and EFA (Developed World ex-US). Additionally, we retrieve 1-3 Month T-Bill rates as our risk-free rate. The use of pad_by_time and fill for the T-Bill rates ensures data completeness, an essential step for accurate return calculation.


```r
tickers <- c("SPY", "AGG", "EFA")

etf_prices <- tq_get(tickers,
                 from = "2000-01-01",
                 to = "2024-01-19",
                 get = "stock.prices")

t_bill_rates <- tq_get("DGS1MO", get = "economic.data", from = "2000-01-01", to = Sys.Date())

tbills<- t_bill_rates %>% select(date, price)
tbills <- tbills %>% pad_by_time( date, .by = "day") %>% fill(price, .direction = "down")
```
### Monthly Returns Computation
The script meticulously calculates monthly returns for each ETF, adjusting for lag and considering a 12-month look-back period, aligning with the Dual Momentum strategy's requirements. 


```r
n_lag <- 12

monthly_rets<- t_bill_rates %>%
    tq_transmute(select     = price, 
                 mutate_fun = to.period, 
                 period     = "months",
                 col_rename = "return") %>% 
    mutate(return = return/100)

risk_free_tlt <- monthly_rets %>% 
    mutate(monthret = return/12) %>% 
    mutate(cumrets = cumprod(1+monthret)) %>%
    mutate(tbill_12mon_ret = ((cumrets / lag(cumrets, n_lag)) - 1)) %>% 
    select(date, tbill_12mon_ret)

etf_monthly_prices <- etf_prices %>%
    group_by(symbol) %>% 
        tq_transmute(select     = adjusted, 
                     mutate_fun = to.period, 
                     period     = "months") %>% 
    ungroup()
    
bond_returns <- 
    etf_monthly_prices %>% 
    filter(symbol == "AGG") %>%
    mutate(bond_return =  ((adjusted / lag(adjusted, 1)) - 1)) %>% 
    select(-adjusted, -symbol) %>% 
    na.omit()

equities_ex_us_returns <- 
    etf_monthly_prices %>% 
    filter(symbol == "EFA") %>% 
    mutate(ex_us_return =  ((adjusted / lag(adjusted)) - 1),
           ex_us_12mon_ret = ((adjusted / lag(adjusted, n_lag)) - 1)) %>%
    select(-adjusted, -symbol) %>% 
    na.omit()

sp_500_returns <- 
    etf_monthly_prices %>% 
    filter(symbol == "SPY") %>% 
    mutate(spy_return =  ((adjusted / lag(adjusted)) - 1),
           spy_12mon_ret = ((adjusted / lag(adjusted, n_lag)) - 1)) %>% 
    select(-adjusted, -symbol)
```
In the following step, we consolidate all of our time series returns into a single tibble. This consolidated tibble will be utilized for generating the strategy signals.

```r
returns_list <- list(sp_500_returns, risk_free_tlt, equities_ex_us_returns, bond_returns)

returns_tbl <- reduce(returns_list, ~left_join(.x, .y, by = "date")) %>% na.omit() 
```
### Strategy Implementation
We compare the 12-month lagged returns of SPY against EFA and the T-Bill rate, allocating the portfolio to the asset with the highest past return, or to bonds if both SPY and EFA underperform the T-Bill rate.


```r
strat_returns_tbl <- returns_tbl %>%
    mutate(
        Dual_Momentum = case_when( 
            date == first(date) ~ NA,
            lag(spy_12mon_ret) >= lag(ex_us_12mon_ret) & lag(spy_12mon_ret) >= lag(tbill_12mon_ret) ~ spy_return,
            lag(spy_12mon_ret) >= lag(ex_us_12mon_ret) & lag(spy_12mon_ret) < lag(tbill_12mon_ret) ~ bond_return,
            lag(spy_12mon_ret) < lag(ex_us_12mon_ret) & lag(ex_us_12mon_ret) < lag(tbill_12mon_ret) ~ bond_return,
            TRUE ~ ex_us_return
        )) %>% 
    na.omit() %>% 
    select(date, SPY = spy_return, Dual_Momentum)
```
### Computing Metrics and Visualizing Resuts
After calculating the Profit and Loss (P&L) curves for both the strategy and our benchmark (SPY), we offer an intuitive visual representation of both time series. Performance metrics provide a clear tabular view of key investment metrics such as returns and downside risks. Furthermore, returns for each year will be displayed to facilitate ease of comparison.


```r
strat_growth <- 
    strat_returns_tbl %>% 
    mutate(Dual_Momentum = cumprod(1 + Dual_Momentum),
           SPY = cumprod(1 + SPY)) %>% 
    select(date, Dual_Momentum, SPY) %>%
    pivot_longer(cols= -date, names_to="strat",values_to="growth")

strat_growth  %>%  
    ggplot(aes(x = date, y = growth, colour = factor(strat))) +
    geom_line() +
    labs(title = "Portfolio Growth Over Time",
         subtitle = "Comparative Growth of Investment Strategies",
         x = "",
         y = "",
         colour = "Strategy") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank()) +
    scale_y_continuous(n.breaks = 10, labels = scales::dollar) +
    scale_color_viridis(discrete = TRUE, option = "A", alpha=1, begin = 0.2, end = 0.6)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

```r
return_metrics <- strat_returns_tbl %>% 
    select(date, Dual_Momentum, SPY)  %>%
    pivot_longer(cols =  -date, names_to="Strategies", values_to="Returns") %>% 
    group_by(Strategies) %>% 
    tq_performance(Ra = Returns,
                   performance_fun = table.Stats) %>% 
    pivot_longer(cols = -Strategies, names_to = "Metrics", values_to = "Values") %>% 
    pivot_wider(names_from = Strategies, values_from = Values) %>% 
    mutate(Dual_Momentum = round(Dual_Momentum,3) %>% scales::percent(),
           SPY = round(SPY,3) %>% scales::percent()) %>% 
    filter( Metrics %in% c("GeometricMean","Maximum","Median","Minimum","Stdev"))

kable(return_metrics, align = "ccc", caption = "Monthly Return Metrics")
```



Table: <span id="tab:unnamed-chunk-7"></span>Table 1: Monthly Return Metrics

|    Metrics    | Dual_Momentum |   SPY   |
|:-------------:|:-------------:|:-------:|
| GeometricMean |     0.7%      |  0.80%  |
|    Maximum    |     10.9%     | 12.70%  |
|    Median     |     0.9%      |  1.40%  |
|    Minimum    |    -12.5%     | -16.50% |
|     Stdev     |     3.5%      |  4.30%  |

```r
downside_metrics <- strat_returns_tbl %>% 
    select(date, Dual_Momentum, SPY)  %>%
    pivot_longer(cols=-date, names_to="Strategies", values_to="Returns") %>% 
    group_by(Strategies) %>% 
    tq_performance(Ra = Returns,
                   performance_fun = table.DownsideRisk) %>% 
    pivot_longer(cols = -Strategies, names_to = "Metrics", values_to = "Values") %>% 
    pivot_wider(names_from = Strategies, values_from = Values) %>% 
    mutate(Dual_Momentum = round(Dual_Momentum,3) %>% scales::percent(),
           SPY = round(SPY,3) %>% scales::percent()) 

kable(downside_metrics, align = "ccc", caption = "Monthly Downside Metrics")
```



Table: <span id="tab:unnamed-chunk-8"></span>Table 2: Monthly Downside Metrics

|          Metrics           | Dual_Momentum |  SPY   |
|:--------------------------:|:-------------:|:------:|
|   DownsideDeviation(0%)    |     2.3%      |  2.8%  |
| DownsideDeviation(MAR=10%) |     2.7%      |  3.2%  |
|  DownsideDeviation(Rf=0%)  |     2.3%      |  2.8%  |
|       GainDeviation        |     2.1%      |  2.5%  |
|     HistoricalES(95%)      |     -7.4%     | -9.5%  |
|     HistoricalVaR(95%)     |     -5.3%     | -7.0%  |
|       LossDeviation        |     2.4%      |  3.2%  |
|      MaximumDrawdown       |     23.0%     | 50.8%  |
|      ModifiedES(95%)       |     -7.7%     | -10.1% |
|      ModifiedVaR(95%)      |     -5.4%     | -6.7%  |
|       SemiDeviation        |     2.6%      |  3.2%  |

```r
annual_returns <- strat_returns_tbl %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    mutate(Dual_Momentum = round(cumprod(Dual_Momentum+1)-1,3) %>% scales::percent(),
              SPY = round(cumprod(SPY+1)-1,3) %>% scales::percent()) %>% 
    select(year, Dual_Momentum, SPY) %>% 
    slice_tail()

kable(annual_returns, align = "ccc", caption = "Yearly Returns")
```



Table: <span id="tab:unnamed-chunk-9"></span>Table 3: Yearly Returns

| year | Dual_Momentum |  SPY   |
|:----:|:-------------:|:------:|
| 2003 |     11.5%     |  6.2%  |
| 2004 |     19.0%     | 10.7%  |
| 2005 |     13.3%     |  4.8%  |
| 2006 |     25.8%     | 15.8%  |
| 2007 |     10.0%     |  5.1%  |
| 2008 |    -2.80%     | -36.8% |
| 2009 |     5.50%     | 26.4%  |
| 2010 |     5.8%      | 15.1%  |
| 2011 |     0.9%      |  1.9%  |
| 2012 |    11.40%     | 16.0%  |
| 2013 |    17.40%     | 32.30% |
| 2014 |     13.5%     | 13.5%  |
| 2015 |    -6.60%     | 1.20%  |
| 2016 |     6.9%      | 12.00% |
| 2017 |    19.00%     | 21.70% |
| 2018 |     -7.6%     | -4.6%  |
| 2019 |    18.60%     | 31.2%  |
| 2020 |     2.6%      | 18.3%  |
| 2021 |     28.7%     | 28.7%  |
| 2022 |    -16.4%     | -18.2% |
| 2023 |     4.3%      | 26.2%  |

### Reflections on the Strategy's Performance
The recent underperformance of the Dual Momentum strategy, despite its strong historical track record, can be attributed to the broader challenges faced by Tactical Asset Allocation (TAA) strategies during the 2022 bear market. As observed, TAA strategies, in general, struggled with loss management during this period, a deviation from their historical performance in previous downturns. A key issue was the reliance on mid/long-duration US Treasuries as defensive assets, which, contrary to historical trends, failed to offset losses in risk assets. This unprecedented concurrent decline in both bonds and risk assets, not seen in nearly a century, highlights the evolving market dynamics and the need for strategies to adapt to changing market conditions. The Dual Momentum strategy, being a part of the TAA framework, was not immune to these market shifts, leading to its recent underperformance.

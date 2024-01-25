---
title: Accelerating Dual Momentum
author: ''
date: '2024-01-23'
slug: accelerating-dual-momentum
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2024-01-23T22:13:05-03:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index_files/plotly-binding/plotly.js"></script>
<script src="{{< blogdown/postref >}}index_files/typedarray/typedarray.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/jquery/jquery.min.js"></script>
<link href="{{< blogdown/postref >}}index_files/crosstalk/css/crosstalk.min.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/crosstalk/js/crosstalk.min.js"></script>
<link href="{{< blogdown/postref >}}index_files/plotly-htmlwidgets-css/plotly-htmlwidgets.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/plotly-main/plotly-latest.min.js"></script>

# Exploring the Accelerating Dual Momentum Strategy

## Introduction

The Accelerating Dual Momentum Strategy enhances the concept of the Dual Momentum strategy (shown in a previous post) by incorporating a more dynamic approach. It not only compares the relative strength of different assets but also considers the acceleration or deceleration in their price momentum. This is typically achieved by examining shorter-term price changes and integrating them into the asset selection process.

Unlike Gary Antonacci’s strategy (Dual Momentum), the rationale for focusing on small/mid-cap stocks arises due to the increasing correlation of US large-cap stocks with global large-cap ones, as they are predominantly multinational conglomerates. This correlation might obscure the actual underlying trends and fundamentals, hence the shift to small/mid-cap stocks. Similarly, for bonds, a total bond fund which often includes a substantial proportion of corporate bonds tends to have a higher correlation with stock performance compared to treasury bonds.

**Strategy Rules**

On the final trading day of each month, the strategy requires the computation of a ‘momentum score’ for two distinct assets: VFINX (S&P 500 Index Fund) and VINEX (Foreign Small/Mid Growth Index Fund). This momentum score is derived by averaging the total returns over three distinct time frames: 1-month, 3-month, and 6-month.

If the momentum score of VFINX exceeds that of VINEX and is also positive, the strategy dictates an allocation to VFINX at the month’s close. Conversely, if the VINEX momentum score surpasses that of VFINX and remains positive, the strategy prescribes an investment in VINEX at the month’s close. In scenarios where neither of the above conditions is satisfied, indicating a lack of positive momentum in both VFINX and VINEX, the strategy adopts a defensive posture. It transitions the entire portfolio to long-term U.S. Treasuries (VUSTX). The strategy is 100% allocated to one asset each month.

### Setting Up Your Environment

First, ensure that you have the necessary packages installed, then load these packages into your R session:

``` r
library(tidyverse)
library(tidyquant)
library(timetk)
library(knitr)
library(plotly)
library(viridis)
```

### Dowloading Data

``` r
tickers <- c("VFINX", "VUSTX", "VINEX")

etf_prices <- tq_get(tickers,
                     from = "1990-01-01",
                     to = "2024-12-31",
                     get = "stock.prices")

etf_monthly_prices <- etf_prices %>%
    group_by(symbol) %>% 
    tq_transmute(select     = adjusted, 
                 mutate_fun = to.period, 
                 period     = "months") %>% 
    ungroup()
```

### Monthly Returns Computation

Calculate monthly returns for each asset, adjusting for lag and considering a 1/3/6 month look-back period.

``` r
bond_returns <- 
    etf_monthly_prices %>% 
    filter(symbol == "VUSTX") %>%
    mutate(bond_return =  ((adjusted / lag(adjusted, 1)) - 1)) %>% 
    select(-adjusted, -symbol) %>% 
    na.omit()

equities_ex_us_returns <- 
    etf_monthly_prices %>% 
    filter(symbol == "VINEX") %>% 
    mutate(ex_us_return =  ((adjusted / lag(adjusted)) - 1),
           ex_us_6mon_ret = ((adjusted / lag(adjusted, 6)) - 1),
           ex_us_3mon_ret = ((adjusted / lag(adjusted, 3)) - 1)) %>%
    select(-adjusted, -symbol) %>% 
    na.omit()

VFINX_returns <- 
    etf_monthly_prices %>% 
    filter(symbol == "VFINX") %>% 
    mutate(VFINX_return =  ((adjusted / lag(adjusted)) - 1),
           VFINX_6mon_ret = ((adjusted / lag(adjusted, 6)) - 1),
           VFINX_3mon_ret = ((adjusted / lag(adjusted, 3)) - 1)) %>%
    select(-adjusted, -symbol)
```

In the following step, we consolidate all of our time series returns into a single tibble. This consolidated tibble will be utilized for generating the strategy signals.

``` r
returns_list <- list(VFINX_returns, equities_ex_us_returns, bond_returns)

returns_tbl <- reduce(returns_list, ~left_join(.x, .y, by = "date")) %>% 
    na.omit() 

returns_tbl <- returns_tbl %>% 
    mutate(VFINX_signal = VFINX_return+VFINX_3mon_ret+VFINX_6mon_ret,
           ex_us_signal = ex_us_return+ ex_us_3mon_ret+ ex_us_6mon_ret) %>% 
    select(date,VFINX_return,VFINX_signal,ex_us_return,ex_us_signal,bond_return)
```

### Strategy Implementation

We compare the composite momentum signal of VFINX against VINEX and cash position, allocating the portfolio to the asset with the highest past return, or to bonds if both VFINX and VINEX underperform thE cash position (0% return).

``` r
strat_returns_tbl <- returns_tbl %>%
    mutate(
        Acc_Dual_Momentum = case_when( 
            date == first(date) ~ NA,
            lag(VFINX_signal) > lag(ex_us_signal) & lag(VFINX_signal) > 0 ~ VFINX_return,
            lag(VFINX_signal) > lag(ex_us_signal) & lag(VFINX_signal) < 0 ~ bond_return,
            lag(VFINX_signal) < lag(ex_us_signal) & lag(ex_us_signal) < 0 ~ bond_return,
            TRUE ~ ex_us_return
        )) %>% 
    na.omit() %>% 
    select(date, VFINX = VFINX_return, Acc_Dual_Momentum)

strat_growth <- 
    strat_returns_tbl %>% 
    mutate(Acc_Dual_Momentum = cumprod(1 + Acc_Dual_Momentum),
           VFINX = cumprod(1 + VFINX)) %>% 
    select(date, Acc_Dual_Momentum, VFINX) %>%
    pivot_longer(cols= -date, names_to="strat",values_to="growth")
```

### Computing Metrics and Visualizing Resuts

After calculating the Profit and Loss (P&L) curves for both the strategy and our benchmark (VFINX), we offer an intuitive visual representation of both time series. Performance metrics provide a clear tabular view of key investment metrics such as returns and downside risks. Furthermore, returns for each year will be displayed to facilitate ease of comparison.

``` r
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
    scale_y_log10(breaks = c(1,3,9,27,81), labels = scales::dollar) +
    scale_color_viridis(discrete = TRUE, option = "A", alpha=1, begin = 0.2, end = 0.6)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

``` r
return_metrics <- strat_returns_tbl %>% 
    select(date, Acc_Dual_Momentum, VFINX)  %>%
    pivot_longer(cols =  -date, names_to="Strategies", values_to="Returns") %>% 
    group_by(Strategies) %>% 
    tq_performance(Ra = Returns,
                   performance_fun = table.Stats) %>% 
    pivot_longer(cols = -Strategies, names_to = "Metrics", values_to = "Values") %>% 
    pivot_wider(names_from = Strategies, values_from = Values) %>% 
    mutate(Acc_Dual_Momentum = round(Acc_Dual_Momentum,3) %>% scales::percent(),
           VFINX = round(VFINX,3) %>% scales::percent()) %>% 
    filter( Metrics %in% c("GeometricMean","Maximum","Median","Minimum","Stdev"))

kable(return_metrics, align = "ccc", caption = "Monthly Return Metrics")
```

|    Metrics    | Acc_Dual_Momentum | VFINX  |
|:-------------:|:-----------------:|:------:|
| GeometricMean |       1.3%        |  0.7%  |
|    Maximum    |       17.4%       | 12.8%  |
|    Median     |       1.7%        |  1.3%  |
|    Minimum    |      -12.7%       | -16.8% |
|     Stdev     |       4.3%        |  4.5%  |

<span id="tab:unnamed-chunk-7"></span>Table 1: Monthly Return Metrics

``` r
downside_metrics <- strat_returns_tbl %>% 
    select(date, Acc_Dual_Momentum, VFINX)  %>%
    pivot_longer(cols=-date, names_to="Strategies", values_to="Returns") %>% 
    group_by(Strategies) %>% 
    tq_performance(Ra = Returns,
                   performance_fun = table.DownsideRisk) %>% 
    pivot_longer(cols = -Strategies, names_to = "Metrics", values_to = "Values") %>% 
    pivot_wider(names_from = Strategies, values_from = Values) %>% 
    mutate(Acc_Dual_Momentum = round(Acc_Dual_Momentum,3) %>% scales::percent(),
           VFINX = round(VFINX,3) %>% scales::percent()) 

kable(downside_metrics, align = "ccc", caption = "Monthly Downside Metrics")
```

|          Metrics           | Acc_Dual_Momentum | VFINX  |
|:--------------------------:|:-----------------:|:------:|
|   DownsideDeviation(0%)    |       2.4%        |  3.0%  |
| DownsideDeviation(MAR=10%) |       2.8%        |  3.4%  |
|  DownsideDeviation(Rf=0%)  |       2.4%        |  3.0%  |
|       GainDeviation        |       3.0%        |  2.6%  |
|     HistoricalES(95%)      |       -8.0%       | -10.0% |
|     HistoricalVaR(95%)     |       -5.3%       | -7.9%  |
|       LossDeviation        |       2.6%        |  3.2%  |
|      MaximumDrawdown       |       33.4%       | 51.0%  |
|      ModifiedES(95%)       |       -7.8%       | -10.2% |
|      ModifiedVaR(95%)      |       -5.5%       | -7.2%  |
|       SemiDeviation        |       3.0%        |  3.4%  |

<span id="tab:unnamed-chunk-7"></span>Table 1: Monthly Downside Metrics

``` r
# Convert monthly returns to annual returns
annual_returns <- strat_returns_tbl %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    mutate(Acc_Dual_Momentum = round(cumprod(Acc_Dual_Momentum+1)-1,3) %>% scales::percent(),
           VFINX = round(cumprod(VFINX+1)-1,3) %>% scales::percent()
    ) %>% 
    select(year, Acc_Dual_Momentum, VFINX) %>% 
    slice_tail()

# Display the table
kable(annual_returns, align = "ccc", caption = "Yearly Returns")
```

| year | Acc_Dual_Momentum | VFINX  |
|:----:|:-----------------:|:------:|
| 1997 |       15.4%       | 15.4%  |
| 1998 |       14.6%       | 28.6%  |
| 1999 |       70.3%       | 21.1%  |
| 2000 |       11.9%       | -9.1%  |
| 2001 |       4.4%        | -12.0% |
| 2002 |       6.70%       | -22.2% |
| 2003 |       66.1%       | 28.5%  |
| 2004 |       35.1%       | 10.70% |
| 2005 |       20.6%       | 4.80%  |
| 2006 |      28.00%       | 15.60% |
| 2007 |       4.5%        | 5.40%  |
| 2008 |       10.2%       | -37.0% |
| 2009 |      29.00%       | 26.5%  |
| 2010 |       19.1%       | 14.9%  |
| 2011 |       16.2%       |  2.0%  |
| 2012 |      12.70%       | 15.8%  |
| 2013 |       25.9%       | 32.2%  |
| 2014 |       14.9%       | 13.5%  |
| 2015 |       0.1%        |  1.2%  |
| 2016 |       6.5%        | 11.80% |
| 2017 |       30.8%       | 21.7%  |
| 2018 |       -6.3%       | -4.5%  |
| 2019 |       15.3%       | 31.3%  |
| 2020 |       30.9%       | 18.2%  |
| 2021 |       24.3%       | 28.5%  |
| 2022 |      -29.9%       | -18.2% |
| 2023 |       17.5%       | 24.7%  |
| 2024 |        -3%        |   2%   |

<span id="tab:unnamed-chunk-8"></span>Table 2: Yearly Returns

``` r
plot_tbl<-strat_returns_tbl %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    mutate(Acc_Dual_Momentum = round(cumprod(Acc_Dual_Momentum+1)-1,3),
           VFINX = round(cumprod(VFINX+1)-1,3)) %>% 
    select(year, Acc_Dual_Momentum, VFINX) %>% 
    slice_tail() %>% 
    pivot_longer(-year, names_to = "Strategy", values_to = "Return")

p<-ggplot(plot_tbl, aes(x = year, y = Return, fill=Strategy)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Annual Returns", x = "", y = "") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank()) +
    scale_y_continuous(labels = scales::percent, n.breaks = 5) +
    scale_fill_viridis(discrete = TRUE, option = "A", alpha=1, begin = 0.2, end = 0.6)

ggplotly(p) %>% layout(legend = list(orientation = 'h', title=""))
```

<div class="plotly html-widget html-fill-item" id="htmlwidget-1" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"data":[{"orientation":"v","width":[0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181],"base":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0.063,0,0,0,-0.29899999999999999,0,-0.034000000000000002],"x":[1996.7750000000001,1997.7750000000001,1998.7750000000001,1999.7750000000001,2000.7750000000001,2001.7750000000001,2002.7750000000001,2003.7750000000001,2004.7750000000001,2005.7750000000001,2006.7750000000001,2007.7750000000001,2008.7750000000001,2009.7750000000001,2010.7750000000001,2011.7750000000001,2012.7750000000001,2013.7750000000001,2014.7750000000001,2015.7750000000001,2016.7750000000001,2017.7750000000001,2018.7750000000001,2019.7750000000001,2020.7750000000001,2021.7750000000001,2022.7750000000001,2023.7750000000001],"y":[0.154,0.14599999999999999,0.70299999999999996,0.11899999999999999,0.043999999999999997,0.067000000000000004,0.66100000000000003,0.35099999999999998,0.20599999999999999,0.28000000000000003,0.044999999999999998,0.10199999999999999,0.28999999999999998,0.191,0.16200000000000001,0.127,0.25900000000000001,0.14899999999999999,0.001,0.065000000000000002,0.308,0.063,0.153,0.309,0.24299999999999999,0.29899999999999999,0.17499999999999999,0.034000000000000002],"text":["year: 1997<br />Return: 0.154<br />Strategy: Acc_Dual_Momentum","year: 1998<br />Return: 0.146<br />Strategy: Acc_Dual_Momentum","year: 1999<br />Return: 0.703<br />Strategy: Acc_Dual_Momentum","year: 2000<br />Return: 0.119<br />Strategy: Acc_Dual_Momentum","year: 2001<br />Return: 0.044<br />Strategy: Acc_Dual_Momentum","year: 2002<br />Return: 0.067<br />Strategy: Acc_Dual_Momentum","year: 2003<br />Return: 0.661<br />Strategy: Acc_Dual_Momentum","year: 2004<br />Return: 0.351<br />Strategy: Acc_Dual_Momentum","year: 2005<br />Return: 0.206<br />Strategy: Acc_Dual_Momentum","year: 2006<br />Return: 0.280<br />Strategy: Acc_Dual_Momentum","year: 2007<br />Return: 0.045<br />Strategy: Acc_Dual_Momentum","year: 2008<br />Return: 0.102<br />Strategy: Acc_Dual_Momentum","year: 2009<br />Return: 0.290<br />Strategy: Acc_Dual_Momentum","year: 2010<br />Return: 0.191<br />Strategy: Acc_Dual_Momentum","year: 2011<br />Return: 0.162<br />Strategy: Acc_Dual_Momentum","year: 2012<br />Return: 0.127<br />Strategy: Acc_Dual_Momentum","year: 2013<br />Return: 0.259<br />Strategy: Acc_Dual_Momentum","year: 2014<br />Return: 0.149<br />Strategy: Acc_Dual_Momentum","year: 2015<br />Return: 0.001<br />Strategy: Acc_Dual_Momentum","year: 2016<br />Return: 0.065<br />Strategy: Acc_Dual_Momentum","year: 2017<br />Return: 0.308<br />Strategy: Acc_Dual_Momentum","year: 2018<br />Return: 0.063<br />Strategy: Acc_Dual_Momentum","year: 2019<br />Return: 0.153<br />Strategy: Acc_Dual_Momentum","year: 2020<br />Return: 0.309<br />Strategy: Acc_Dual_Momentum","year: 2021<br />Return: 0.243<br />Strategy: Acc_Dual_Momentum","year: 2022<br />Return: 0.299<br />Strategy: Acc_Dual_Momentum","year: 2023<br />Return: 0.175<br />Strategy: Acc_Dual_Momentum","year: 2024<br />Return: 0.034<br />Strategy: Acc_Dual_Momentum"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(59,15,112,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"Acc_Dual_Momentum","legendgroup":"Acc_Dual_Momentum","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181],"base":[0,0,0,-0.090999999999999998,-0.12,-0.222,0,0,0,0,0,-0.37,0,0,0,0,0,0,0,0,0,-0.044999999999999998,0,0,0,-0.182,0,0],"x":[1997.2249999999999,1998.2249999999999,1999.2249999999999,2000.2249999999999,2001.2249999999999,2002.2249999999999,2003.2249999999999,2004.2249999999999,2005.2249999999999,2006.2249999999999,2007.2249999999999,2008.2249999999999,2009.2249999999999,2010.2249999999999,2011.2249999999999,2012.2249999999999,2013.2249999999999,2014.2249999999999,2015.2249999999999,2016.2249999999999,2017.2249999999999,2018.2249999999999,2019.2249999999999,2020.2249999999999,2021.2249999999999,2022.2249999999999,2023.2249999999999,2024.2249999999999],"y":[0.154,0.28599999999999998,0.21099999999999999,0.090999999999999998,0.12,0.222,0.28499999999999998,0.107,0.048000000000000001,0.156,0.053999999999999999,0.37,0.26500000000000001,0.14899999999999999,0.02,0.158,0.32200000000000001,0.13500000000000001,0.012,0.11799999999999999,0.217,0.044999999999999998,0.313,0.182,0.28499999999999998,0.182,0.247,0.021000000000000001],"text":["year: 1997<br />Return: 0.154<br />Strategy: VFINX","year: 1998<br />Return: 0.286<br />Strategy: VFINX","year: 1999<br />Return: 0.211<br />Strategy: VFINX","year: 2000<br />Return: 0.091<br />Strategy: VFINX","year: 2001<br />Return: 0.120<br />Strategy: VFINX","year: 2002<br />Return: 0.222<br />Strategy: VFINX","year: 2003<br />Return: 0.285<br />Strategy: VFINX","year: 2004<br />Return: 0.107<br />Strategy: VFINX","year: 2005<br />Return: 0.048<br />Strategy: VFINX","year: 2006<br />Return: 0.156<br />Strategy: VFINX","year: 2007<br />Return: 0.054<br />Strategy: VFINX","year: 2008<br />Return: 0.370<br />Strategy: VFINX","year: 2009<br />Return: 0.265<br />Strategy: VFINX","year: 2010<br />Return: 0.149<br />Strategy: VFINX","year: 2011<br />Return: 0.020<br />Strategy: VFINX","year: 2012<br />Return: 0.158<br />Strategy: VFINX","year: 2013<br />Return: 0.322<br />Strategy: VFINX","year: 2014<br />Return: 0.135<br />Strategy: VFINX","year: 2015<br />Return: 0.012<br />Strategy: VFINX","year: 2016<br />Return: 0.118<br />Strategy: VFINX","year: 2017<br />Return: 0.217<br />Strategy: VFINX","year: 2018<br />Return: 0.045<br />Strategy: VFINX","year: 2019<br />Return: 0.313<br />Strategy: VFINX","year: 2020<br />Return: 0.182<br />Strategy: VFINX","year: 2021<br />Return: 0.285<br />Strategy: VFINX","year: 2022<br />Return: 0.182<br />Strategy: VFINX","year: 2023<br />Return: 0.247<br />Strategy: VFINX","year: 2024<br />Return: 0.021<br />Strategy: VFINX"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(222,73,104,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"VFINX","legendgroup":"VFINX","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":46.286425902864259,"r":7.9701120797011216,"b":30.12263279354362,"l":37.459526774595282},"font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402243},"title":{"text":"<b> Annual Returns <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":19.128268991282692},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1995.1550000000002,2025.8449999999998],"tickmode":"array","ticktext":["2000","2005","2010","2015","2020","2025"],"tickvals":[2000,2005,2010,2015,2020,2025],"categoryorder":"array","categoryarray":["2000","2005","2010","2015","2020","2025"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.9850560398505608,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":12.7521793275218},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.72455564360919278,"zeroline":false,"anchor":"y","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402243}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.42364999999999997,0.75664999999999993],"tickmode":"array","ticktext":["-25%","0%","25%","50%","75%"],"tickvals":[-0.25,0,0.25,0.5,0.74999999999999989],"categoryorder":"array","categoryarray":["-25%","0%","25%","50%","75%"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.9850560398505608,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":12.7521793275218},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.72455564360919278,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402243}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":12.7521793275218},"title":"","orientation":"h"},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"428c6b98102":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"428c6b98102","visdat":{"428c6b98102":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

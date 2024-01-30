---
title: Vigilant Asset Allocation
author: ''
date: '2024-01-30'
slug: vigilant-asset-allocation
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2024-01-30T19:10:59-03:00'
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

## Introduction

In this post we backtest the Vigilant Asset Allocation (VAA) strategy, ‘Keller, Wouter J. and Keuning, Jan Willem, Breadth Momentum and Vigilant Asset Allocation (VAA): Winning More by Losing Less (July 14, 2017)’. VAA is a dual-momentum based investment strategy with strong crash protection and a fast momentum filter. It combines absolute (trend-following) and relative (strength) momentum and introduces a unique approach to crash protection using breadth momentum at the universe level. We will implement the VAA4 strategy, that as the name suggests, uses a universe of only four “offensive” assets.

**Strategy Rules**

The VAA4 strategy includes both offensive (US, international, emerging market equities, and US aggregate bonds) and defensive assets (US corporate, intermediate-term, and short-term Treasuries). On the last trading day of each month, a “momentum score” is calculated for each asset using month-end data. This score is derived from a formula that weighs recent price changes over 1, 3, 6, and 12-month periods, thus assigning more weight to the most recent month’s price change. When the momentum score of all offensive assets is positive, we invest the entire portfolio in the offensive asset with the highest momentum score. Otherwise, if any of the offensive assets exhibit negative momentum score, we invest in the defensive asset (LQD, IEF or SHY) with the highest momentum.

### Setting Up Your Environment

First, ensure that you have the necessary packages installed, then load these packages into your R session:

``` r
library(tidyverse)
library(tidyquant)
library(knitr)
library(plotly)
library(viridis)
library(gt)
```

### Dowloading Data

``` r
tickers <- c("SPY", "EFA", "EEM", "AGG","LQD","IEF","SHY")

etf_prices <- tq_get(tickers,
                     from = "1990-01-01",
                     to = "2024-12-31",
                     get = "stock.prices")

# Tranform daily prices to monthly
etf_monthly_prices <- etf_prices %>%
    group_by(symbol) %>% 
    tq_transmute(select     = adjusted, 
                 mutate_fun = to.period, 
                 period     = "months") %>% 
    ungroup()
```

### Monthly Returns & Composite Momentum

Calculate monthly returns for each asset, considering the composite momentum score with 1/3/6/12 month look-back period.

``` r
# Define the offensive and defensive assets
offensive_tickers <- c("SPY", "EFA", "EEM", "AGG")
defensive_tickers <- c("LQD","IEF","SHY")

# Returns and Composite Momentum
etfs_mom <- etf_monthly_prices %>% 
        group_by(symbol) %>% 
            mutate(
            mom1 = ((adjusted / lag(adjusted)) - 1),
            mom3 = ((adjusted / lag(adjusted, 3)) - 1),
            mom6 = ((adjusted / lag(adjusted, 6)) - 1),
            mom12 = ((adjusted / lag(adjusted, 12)) - 1),
            mom_composite = (mom1*12+mom3*4+mom6*2+mom12)
        ) %>%
        select(date, symbol, mom1, mom_composite) %>% 
        na.omit() %>%  group_split()

# Rename columns
etfs_mom <- etfs_mom %>%
  map(~ {
    unique_symbol <- unique(.x$symbol)
    .x %>% rename(
      !!paste0("mom1_", unique_symbol) := mom1,
      !!paste0("mom_composite_", unique_symbol) := mom_composite
    )
  }) %>% 
    map(~ select(.x, -symbol))
```

In the following step, we consolidate all of our monthly time series into a single tibble. This consolidated tibble will be utilized for generating the strategy signals.

``` r
mom_tbl <- reduce(etfs_mom, ~left_join(.x, .y, by = "date")) %>% 
    na.omit() 
```

### Strategy Implementation

``` r
strat_returns_tbl <- 
    mom_tbl %>%
    mutate(
        defensive_swith = case_when(
            date == first(date) ~ NA,
            lag(if_any(contains(str_c("mom_composite_",offensive_tickers)), ~.x < 0))~ 1,
            TRUE ~ 0
            ),
        top_defensive = case_when(
            lag(mom_composite_IEF > mom_composite_LQD & mom_composite_SHY) ~ mom1_IEF,
            lag(mom_composite_LQD > mom_composite_IEF & mom_composite_SHY) ~ mom1_LQD,
            lag(mom_composite_SHY > mom_composite_IEF & mom_composite_LQD) ~ mom1_SHY,
             ),
        top_offensive = case_when(
            lag(mom_composite_SPY > mom_composite_EEM & mom_composite_EFA & mom_composite_AGG) ~ mom1_SPY,
            lag(mom_composite_EEM > mom_composite_SPY & mom_composite_EFA & mom_composite_AGG) ~ mom1_EEM,
            lag(mom_composite_EFA > mom_composite_SPY & mom_composite_EEM & mom_composite_AGG) ~ mom1_EFA,
            lag(mom_composite_AGG > mom_composite_SPY & mom_composite_EFA & mom_composite_EEM) ~ mom1_AGG
        
        )) %>% 
    na.omit() %>% 
    select(date, mom1_SPY, defensive_swith, contains("top")) %>% 
    mutate(
        VAA = case_when(
            defensive_swith == 0 ~ top_offensive,
            TRUE ~ top_defensive
        )
    )

strat_growth <- 
    strat_returns_tbl %>% 
    mutate(VAA = cumprod(1 + VAA),
           SPY = cumprod(1 + mom1_SPY)) %>% 
    select(date, VAA, SPY) %>%
    pivot_longer(cols= -date, names_to="strat",values_to="growth")
```

### Computing Metrics and Visualizing Resuts

After calculating the Profit and Loss (P&L) curves for both the strategy and our benchmark (SPY), we offer an intuitive visual representation of both time series. Performance metrics provide a clear tabular view of key investment metrics such as returns and downside risks. Furthermore, returns for each year will be displayed to facilitate ease of comparison.

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
    select(date, VAA, SPY=mom1_SPY)  %>%
    pivot_longer(cols =  -date, names_to="Strategies", values_to="Returns") %>% 
    group_by(Strategies) %>% 
    tq_performance(Ra = Returns,
                   performance_fun = table.Stats) %>% 
    pivot_longer(cols = -Strategies, names_to = "Metrics", values_to = "Values") %>% 
    pivot_wider(names_from = Strategies, values_from = Values) %>% 
    mutate(VAA = round(VAA,3) %>% scales::percent(),
           SPY = round(SPY,3) %>% scales::percent()) %>% 
    filter( Metrics %in% c("GeometricMean","Maximum","Median","Minimum","Stdev"))

gt(return_metrics, caption = "Monthly Return Metrics")
```

<div id="ghrtitfnwv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ghrtitfnwv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ghrtitfnwv thead, #ghrtitfnwv tbody, #ghrtitfnwv tfoot, #ghrtitfnwv tr, #ghrtitfnwv td, #ghrtitfnwv th {
  border-style: none;
}
&#10;#ghrtitfnwv p {
  margin: 0;
  padding: 0;
}
&#10;#ghrtitfnwv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ghrtitfnwv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ghrtitfnwv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ghrtitfnwv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ghrtitfnwv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ghrtitfnwv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ghrtitfnwv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ghrtitfnwv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ghrtitfnwv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ghrtitfnwv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ghrtitfnwv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ghrtitfnwv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ghrtitfnwv .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ghrtitfnwv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ghrtitfnwv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ghrtitfnwv .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ghrtitfnwv .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ghrtitfnwv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ghrtitfnwv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ghrtitfnwv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ghrtitfnwv .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ghrtitfnwv .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ghrtitfnwv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ghrtitfnwv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ghrtitfnwv .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ghrtitfnwv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ghrtitfnwv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ghrtitfnwv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ghrtitfnwv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ghrtitfnwv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ghrtitfnwv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ghrtitfnwv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ghrtitfnwv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ghrtitfnwv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ghrtitfnwv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ghrtitfnwv .gt_left {
  text-align: left;
}
&#10;#ghrtitfnwv .gt_center {
  text-align: center;
}
&#10;#ghrtitfnwv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ghrtitfnwv .gt_font_normal {
  font-weight: normal;
}
&#10;#ghrtitfnwv .gt_font_bold {
  font-weight: bold;
}
&#10;#ghrtitfnwv .gt_font_italic {
  font-style: italic;
}
&#10;#ghrtitfnwv .gt_super {
  font-size: 65%;
}
&#10;#ghrtitfnwv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ghrtitfnwv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ghrtitfnwv .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ghrtitfnwv .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ghrtitfnwv .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ghrtitfnwv .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ghrtitfnwv .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <caption>(#tab:unnamed-chunk-7)Monthly Return Metrics</caption>
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Metrics">Metrics</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="VAA">VAA</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SPY">SPY</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Metrics" class="gt_row gt_left">GeometricMean</td>
<td headers="VAA" class="gt_row gt_right">1.0%</td>
<td headers="SPY" class="gt_row gt_right">0.80%</td></tr>
    <tr><td headers="Metrics" class="gt_row gt_left">Maximum</td>
<td headers="VAA" class="gt_row gt_right">15.9%</td>
<td headers="SPY" class="gt_row gt_right">12.70%</td></tr>
    <tr><td headers="Metrics" class="gt_row gt_left">Median</td>
<td headers="VAA" class="gt_row gt_right">0.9%</td>
<td headers="SPY" class="gt_row gt_right">1.40%</td></tr>
    <tr><td headers="Metrics" class="gt_row gt_left">Minimum</td>
<td headers="VAA" class="gt_row gt_right">-8.0%</td>
<td headers="SPY" class="gt_row gt_right">-16.50%</td></tr>
    <tr><td headers="Metrics" class="gt_row gt_left">Stdev</td>
<td headers="VAA" class="gt_row gt_right">3.6%</td>
<td headers="SPY" class="gt_row gt_right">4.40%</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
downside_metrics <- strat_returns_tbl %>% 
    select(date, VAA, SPY=mom1_SPY)  %>%
    pivot_longer(cols=-date, names_to="Strategies", values_to="Returns") %>% 
    group_by(Strategies) %>% 
    tq_performance(Ra = Returns,
                   performance_fun = table.DownsideRisk) %>% 
    pivot_longer(cols = -Strategies, names_to = "Metrics", values_to = "Values") %>% 
    pivot_wider(names_from = Strategies, values_from = Values) %>% 
    mutate(VAA = round(VAA,3) %>% scales::percent(),
           SPY = round(SPY,3) %>% scales::percent()) 

kable(downside_metrics, align = "ccc", caption = "Monthly Downside Metrics")
```

|          Metrics           |  VAA  |  SPY   |
|:--------------------------:|:-----:|:------:|
|   DownsideDeviation(0%)    | 1.9%  |  2.9%  |
| DownsideDeviation(MAR=10%) | 2.3%  |  3.3%  |
|  DownsideDeviation(Rf=0%)  | 1.9%  |  2.9%  |
|       GainDeviation        | 2.9%  |  2.5%  |
|     HistoricalES(95%)      | -6.4% | -9.7%  |
|     HistoricalVaR(95%)     | -4.3% | -7.4%  |
|       LossDeviation        | 2.1%  |  3.2%  |
|      MaximumDrawdown       | 31.8% | 50.8%  |
|      ModifiedES(95%)       | -5.0% | -10.1% |
|      ModifiedVaR(95%)      | -4.1% | -6.8%  |
|       SemiDeviation        | 2.4%  |  3.3%  |

<span id="tab:unnamed-chunk-7"></span>Table 1: Monthly Downside Metrics

``` r
# Convert monthly returns to annual returns
annual_returns <- strat_returns_tbl %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    mutate(VAA = round(cumprod(VAA+1)-1,3) %>% scales::percent(),
           SPY = round(cumprod(mom1_SPY+1)-1,3) %>% scales::percent()
    ) %>% 
    select(year, VAA, SPY) %>% 
    slice_tail()

# Display the table
kable(annual_returns, caption = "Yearly Returns", align = 'ccc')
```

| year |   VAA   |  SPY   |
|:----:|:-------:|:------:|
| 2004 |  18.4%  |  9.0%  |
| 2005 |  25.0%  |  4.8%  |
| 2006 | 31.20%  | 15.8%  |
| 2007 |  33.0%  |  5.1%  |
| 2008 |  23.8%  | -36.8% |
| 2009 |  29.3%  | 26.4%  |
| 2010 | -1.70%  | 15.1%  |
| 2011 |  5.9%   |  1.9%  |
| 2012 | 24.70%  | 16.0%  |
| 2013 | 15.00%  | 32.30% |
| 2014 |  4.40%  | 13.5%  |
| 2015 | -4.50%  | 1.20%  |
| 2016 |  6.0%   | 12.00% |
| 2017 | 20.30%  | 21.70% |
| 2018 | 10.70%  | -4.6%  |
| 2019 |  8.7%   | 31.2%  |
| 2020 |  22.2%  | 18.3%  |
| 2021 |  0.40%  | 28.7%  |
| 2022 | -22.30% | -18.2% |
| 2023 |  1.0%   | 26.2%  |
| 2024 |   3%    |   3%   |

<span id="tab:unnamed-chunk-8"></span>Table 2: Yearly Returns

``` r
plot_tbl<-strat_returns_tbl %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    mutate(VAA = round(cumprod(VAA+1)-1,3),
           SPY = round(cumprod(mom1_SPY+1)-1,3)) %>% 
    select(year, VAA, SPY) %>% 
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
<script type="application/json" data-for="htmlwidget-1">{"x":{"data":[{"orientation":"v","width":[0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181],"base":[0,0,0,0,-0.36799999999999999,0,0,0,0,0,0,0,0,0,-0.045999999999999999,0,0,0,-0.182,0,0],"x":[2003.7750000000001,2004.7750000000001,2005.7750000000001,2006.7750000000001,2007.7750000000001,2008.7750000000001,2009.7750000000001,2010.7750000000001,2011.7750000000001,2012.7750000000001,2013.7750000000001,2014.7750000000001,2015.7750000000001,2016.7750000000001,2017.7750000000001,2018.7750000000001,2019.7750000000001,2020.7750000000001,2021.7750000000001,2022.7750000000001,2023.7750000000001],"y":[0.089999999999999997,0.048000000000000001,0.158,0.050999999999999997,0.36799999999999999,0.26400000000000001,0.151,0.019,0.16,0.32300000000000001,0.13500000000000001,0.012,0.12,0.217,0.045999999999999999,0.312,0.183,0.28699999999999998,0.182,0.26200000000000001,0.033000000000000002],"text":["year: 2004<br />Return: 0.090<br />Strategy: SPY","year: 2005<br />Return: 0.048<br />Strategy: SPY","year: 2006<br />Return: 0.158<br />Strategy: SPY","year: 2007<br />Return: 0.051<br />Strategy: SPY","year: 2008<br />Return: 0.368<br />Strategy: SPY","year: 2009<br />Return: 0.264<br />Strategy: SPY","year: 2010<br />Return: 0.151<br />Strategy: SPY","year: 2011<br />Return: 0.019<br />Strategy: SPY","year: 2012<br />Return: 0.160<br />Strategy: SPY","year: 2013<br />Return: 0.323<br />Strategy: SPY","year: 2014<br />Return: 0.135<br />Strategy: SPY","year: 2015<br />Return: 0.012<br />Strategy: SPY","year: 2016<br />Return: 0.120<br />Strategy: SPY","year: 2017<br />Return: 0.217<br />Strategy: SPY","year: 2018<br />Return: 0.046<br />Strategy: SPY","year: 2019<br />Return: 0.312<br />Strategy: SPY","year: 2020<br />Return: 0.183<br />Strategy: SPY","year: 2021<br />Return: 0.287<br />Strategy: SPY","year: 2022<br />Return: 0.182<br />Strategy: SPY","year: 2023<br />Return: 0.262<br />Strategy: SPY","year: 2024<br />Return: 0.033<br />Strategy: SPY"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(59,15,112,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"SPY","legendgroup":"SPY","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181,0.4499999999998181],"base":[0,0,0,0,0,0,-0.017000000000000001,0,0,0,0,-0.044999999999999998,0,0,0,0,0,0,-0.223,0,0],"x":[2004.2249999999999,2005.2249999999999,2006.2249999999999,2007.2249999999999,2008.2249999999999,2009.2249999999999,2010.2249999999999,2011.2249999999999,2012.2249999999999,2013.2249999999999,2014.2249999999999,2015.2249999999999,2016.2249999999999,2017.2249999999999,2018.2249999999999,2019.2249999999999,2020.2249999999999,2021.2249999999999,2022.2249999999999,2023.2249999999999,2024.2249999999999],"y":[0.184,0.25,0.312,0.33000000000000002,0.23799999999999999,0.29299999999999998,0.017000000000000001,0.058999999999999997,0.247,0.14999999999999999,0.043999999999999997,0.044999999999999998,0.059999999999999998,0.20300000000000001,0.107,0.086999999999999994,0.222,0.0040000000000000001,0.223,0.01,0.033000000000000002],"text":["year: 2004<br />Return: 0.184<br />Strategy: VAA","year: 2005<br />Return: 0.250<br />Strategy: VAA","year: 2006<br />Return: 0.312<br />Strategy: VAA","year: 2007<br />Return: 0.330<br />Strategy: VAA","year: 2008<br />Return: 0.238<br />Strategy: VAA","year: 2009<br />Return: 0.293<br />Strategy: VAA","year: 2010<br />Return: 0.017<br />Strategy: VAA","year: 2011<br />Return: 0.059<br />Strategy: VAA","year: 2012<br />Return: 0.247<br />Strategy: VAA","year: 2013<br />Return: 0.150<br />Strategy: VAA","year: 2014<br />Return: 0.044<br />Strategy: VAA","year: 2015<br />Return: 0.045<br />Strategy: VAA","year: 2016<br />Return: 0.060<br />Strategy: VAA","year: 2017<br />Return: 0.203<br />Strategy: VAA","year: 2018<br />Return: 0.107<br />Strategy: VAA","year: 2019<br />Return: 0.087<br />Strategy: VAA","year: 2020<br />Return: 0.222<br />Strategy: VAA","year: 2021<br />Return: 0.004<br />Strategy: VAA","year: 2022<br />Return: 0.223<br />Strategy: VAA","year: 2023<br />Return: 0.010<br />Strategy: VAA","year: 2024<br />Return: 0.033<br />Strategy: VAA"],"type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(222,73,104,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"VAA","legendgroup":"VAA","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":46.286425902864259,"r":7.9701120797011216,"b":30.12263279354362,"l":37.459526774595282},"font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402243},"title":{"text":"<b> Annual Returns <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":19.128268991282692},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[2002.5050000000001,2025.4949999999999],"tickmode":"array","ticktext":["2005","2010","2015","2020","2025"],"tickvals":[2005,2010,2015,2020,2025],"categoryorder":"array","categoryarray":["2005","2010","2015","2020","2025"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.9850560398505608,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":12.7521793275218},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.72455564360919278,"zeroline":false,"anchor":"y","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402243}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.40289999999999998,0.3649],"tickmode":"array","ticktext":["-40%","-20%","0%","20%"],"tickvals":[-0.40000000000000002,-0.20000000000000001,-5.5511151231257827e-17,0.20000000000000001],"categoryorder":"array","categoryarray":["-40%","-20%","0%","20%"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.9850560398505608,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":12.7521793275218},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.72455564360919278,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":15.940224159402243}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":12.7521793275218},"title":"","orientation":"h"},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"50f824f7103f":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"50f824f7103f","visdat":{"50f824f7103f":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

### Conclusion

As shown in previous posts and documented on [allocate smartly](https://allocatesmartly.com/tactical-asset-allocation-performance-during-the-2022-bear-market/), typical asset allocation strategies struggled to offset risks during the last bear market. While TAA historically switched effectively from offensive to defensive assets, the issue was with the defensive assets chosen, particularly mid/long-duration US Treasuries strongly correlated with the rest of the risky universe and were further punished in the context of rising interest rates.

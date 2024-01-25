---
title: Bank Telemarketing
author: ''
date: '2024-01-22'
slug: bank-telemarketing
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2024-01-22T10:41:11-03:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

## Introduction

In this post, we will focus on predicting the success of telemarketing calls aimed at selling long-term bank deposits, particularly in the context of a Portuguese banking institution. The primary classification goal is to ascertain whether a potential client will subscribe to a term deposit, denoted as the dependent variable ‘y’. This analysis draws inspiration from the seminal work of Moro, S., Cortez, P., & Rita, P. (2014) titled “A Data-Driven Approach to Predict the Success of Bank Telemarketing.” Understanding the factors that influence the success of telemarketing calls can empower companys to refine their communication strategies and resource allocation, potentially leading to heightened operational efficiency and enhanced customer satisfaction. *As we will see later on, we can benefit from the predictive analysis of our models by reducing the number of calls by significant amounts and successfully capturing almost the majority of the clients who will subscribe to a long-term deposit.*

### Overview of the Dataset

**Data Overview**

The dataset under examination encompasses data collected from 2008 to 2013, sourced from the UCI Machine Learning Repository. For the purposes of this analysis, we will engage with a reduced subset of the original database, which contains slightly over 4,000 records and 17 input variables, as opposed to the full dataset over 40,000 records.

**Data Structure**

The Output Variable (Target) in our study is binary, indicating whether the client subscribed to a term deposit (yes/no).
The input variables in our dataset can be categorized into several groups:
1. Client Data: This includes demographic and socio-economic information such as age, job, marital status, education, etc.
2. Current Campaign Data: Pertinent details of the ongoing marketing campaign, including the type of contact communication, the month and day of the week of the last contact, and the duration of the last contact, among others.
3. Other Attributes: This encompasses data such as the number of contacts made during the current campaign, the number of days that have passed since the client was last contacted in a previous campaign, and the outcome of the previous marketing campaign.

**Further Considerations**

As the authors of the referenced paper implemented a semi-automatic feature selection process, reducing the input variables from an initial pool of 150 to 17, the feature engineering approach that we will adopt shall be minimal, yet sufficient to ensure the effective functioning of our predictive models.

The attribute ‘duration’ highly affects the output target (e.g., if duration=0 then y=‘no’). Yet, the duration is not known before a call is performed. Also, after the end of the call ‘y’ is obviously known. Thus, this input should be removed if the intention is to have a realistic predictive model.

### Setting Up and Loading the Data

``` r
# Load libraries
library(tidymodels)
library(tidyverse)
library(knitr)
library(baguette)
library(stacks)
library(doParallel)
library(corrplot)
library(gt)
tidymodels_prefer()

# Load the data and remove the duration feature
data_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip"
download.file(data_url, destfile = "bank.zip")
unzip("bank.zip", files = "bank.csv", exdir = "data")
bank_data <- read_csv2("data/bank.csv")
bank_data <- bank_data %>% 
    mutate(across(where(is.character),as.factor)) %>% 
    select(-duration)
```

### EDA

**Features Overview**

``` r
gt(bank_data %>% slice(1:10))
```

<div id="dpujfqghmf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dpujfqghmf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#dpujfqghmf thead, #dpujfqghmf tbody, #dpujfqghmf tfoot, #dpujfqghmf tr, #dpujfqghmf td, #dpujfqghmf th {
  border-style: none;
}
&#10;#dpujfqghmf p {
  margin: 0;
  padding: 0;
}
&#10;#dpujfqghmf .gt_table {
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
&#10;#dpujfqghmf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#dpujfqghmf .gt_title {
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
&#10;#dpujfqghmf .gt_subtitle {
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
&#10;#dpujfqghmf .gt_heading {
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
&#10;#dpujfqghmf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dpujfqghmf .gt_col_headings {
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
&#10;#dpujfqghmf .gt_col_heading {
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
&#10;#dpujfqghmf .gt_column_spanner_outer {
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
&#10;#dpujfqghmf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#dpujfqghmf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#dpujfqghmf .gt_column_spanner {
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
&#10;#dpujfqghmf .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#dpujfqghmf .gt_group_heading {
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
&#10;#dpujfqghmf .gt_empty_group_heading {
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
&#10;#dpujfqghmf .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#dpujfqghmf .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#dpujfqghmf .gt_row {
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
&#10;#dpujfqghmf .gt_stub {
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
&#10;#dpujfqghmf .gt_stub_row_group {
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
&#10;#dpujfqghmf .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#dpujfqghmf .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#dpujfqghmf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dpujfqghmf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#dpujfqghmf .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#dpujfqghmf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dpujfqghmf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dpujfqghmf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#dpujfqghmf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#dpujfqghmf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#dpujfqghmf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dpujfqghmf .gt_footnotes {
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
&#10;#dpujfqghmf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dpujfqghmf .gt_sourcenotes {
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
&#10;#dpujfqghmf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dpujfqghmf .gt_left {
  text-align: left;
}
&#10;#dpujfqghmf .gt_center {
  text-align: center;
}
&#10;#dpujfqghmf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#dpujfqghmf .gt_font_normal {
  font-weight: normal;
}
&#10;#dpujfqghmf .gt_font_bold {
  font-weight: bold;
}
&#10;#dpujfqghmf .gt_font_italic {
  font-style: italic;
}
&#10;#dpujfqghmf .gt_super {
  font-size: 65%;
}
&#10;#dpujfqghmf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#dpujfqghmf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#dpujfqghmf .gt_indent_1 {
  text-indent: 5px;
}
&#10;#dpujfqghmf .gt_indent_2 {
  text-indent: 10px;
}
&#10;#dpujfqghmf .gt_indent_3 {
  text-indent: 15px;
}
&#10;#dpujfqghmf .gt_indent_4 {
  text-indent: 20px;
}
&#10;#dpujfqghmf .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="age">age</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="job">job</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="marital">marital</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="education">education</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="default">default</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="balance">balance</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="housing">housing</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="loan">loan</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="contact">contact</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="day">day</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="month">month</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="campaign">campaign</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="pdays">pdays</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="previous">previous</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="poutcome">poutcome</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="y">y</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="age" class="gt_row gt_right">30</td>
<td headers="job" class="gt_row gt_center">unemployed</td>
<td headers="marital" class="gt_row gt_center">married</td>
<td headers="education" class="gt_row gt_center">primary</td>
<td headers="default" class="gt_row gt_center">no</td>
<td headers="balance" class="gt_row gt_right">1787</td>
<td headers="housing" class="gt_row gt_center">no</td>
<td headers="loan" class="gt_row gt_center">no</td>
<td headers="contact" class="gt_row gt_center">cellular</td>
<td headers="day" class="gt_row gt_right">19</td>
<td headers="month" class="gt_row gt_center">oct</td>
<td headers="campaign" class="gt_row gt_right">1</td>
<td headers="pdays" class="gt_row gt_right">-1</td>
<td headers="previous" class="gt_row gt_right">0</td>
<td headers="poutcome" class="gt_row gt_center">unknown</td>
<td headers="y" class="gt_row gt_center">no</td></tr>
    <tr><td headers="age" class="gt_row gt_right">33</td>
<td headers="job" class="gt_row gt_center">services</td>
<td headers="marital" class="gt_row gt_center">married</td>
<td headers="education" class="gt_row gt_center">secondary</td>
<td headers="default" class="gt_row gt_center">no</td>
<td headers="balance" class="gt_row gt_right">4789</td>
<td headers="housing" class="gt_row gt_center">yes</td>
<td headers="loan" class="gt_row gt_center">yes</td>
<td headers="contact" class="gt_row gt_center">cellular</td>
<td headers="day" class="gt_row gt_right">11</td>
<td headers="month" class="gt_row gt_center">may</td>
<td headers="campaign" class="gt_row gt_right">1</td>
<td headers="pdays" class="gt_row gt_right">339</td>
<td headers="previous" class="gt_row gt_right">4</td>
<td headers="poutcome" class="gt_row gt_center">failure</td>
<td headers="y" class="gt_row gt_center">no</td></tr>
    <tr><td headers="age" class="gt_row gt_right">35</td>
<td headers="job" class="gt_row gt_center">management</td>
<td headers="marital" class="gt_row gt_center">single</td>
<td headers="education" class="gt_row gt_center">tertiary</td>
<td headers="default" class="gt_row gt_center">no</td>
<td headers="balance" class="gt_row gt_right">1350</td>
<td headers="housing" class="gt_row gt_center">yes</td>
<td headers="loan" class="gt_row gt_center">no</td>
<td headers="contact" class="gt_row gt_center">cellular</td>
<td headers="day" class="gt_row gt_right">16</td>
<td headers="month" class="gt_row gt_center">apr</td>
<td headers="campaign" class="gt_row gt_right">1</td>
<td headers="pdays" class="gt_row gt_right">330</td>
<td headers="previous" class="gt_row gt_right">1</td>
<td headers="poutcome" class="gt_row gt_center">failure</td>
<td headers="y" class="gt_row gt_center">no</td></tr>
    <tr><td headers="age" class="gt_row gt_right">30</td>
<td headers="job" class="gt_row gt_center">management</td>
<td headers="marital" class="gt_row gt_center">married</td>
<td headers="education" class="gt_row gt_center">tertiary</td>
<td headers="default" class="gt_row gt_center">no</td>
<td headers="balance" class="gt_row gt_right">1476</td>
<td headers="housing" class="gt_row gt_center">yes</td>
<td headers="loan" class="gt_row gt_center">yes</td>
<td headers="contact" class="gt_row gt_center">unknown</td>
<td headers="day" class="gt_row gt_right">3</td>
<td headers="month" class="gt_row gt_center">jun</td>
<td headers="campaign" class="gt_row gt_right">4</td>
<td headers="pdays" class="gt_row gt_right">-1</td>
<td headers="previous" class="gt_row gt_right">0</td>
<td headers="poutcome" class="gt_row gt_center">unknown</td>
<td headers="y" class="gt_row gt_center">no</td></tr>
    <tr><td headers="age" class="gt_row gt_right">59</td>
<td headers="job" class="gt_row gt_center">blue-collar</td>
<td headers="marital" class="gt_row gt_center">married</td>
<td headers="education" class="gt_row gt_center">secondary</td>
<td headers="default" class="gt_row gt_center">no</td>
<td headers="balance" class="gt_row gt_right">0</td>
<td headers="housing" class="gt_row gt_center">yes</td>
<td headers="loan" class="gt_row gt_center">no</td>
<td headers="contact" class="gt_row gt_center">unknown</td>
<td headers="day" class="gt_row gt_right">5</td>
<td headers="month" class="gt_row gt_center">may</td>
<td headers="campaign" class="gt_row gt_right">1</td>
<td headers="pdays" class="gt_row gt_right">-1</td>
<td headers="previous" class="gt_row gt_right">0</td>
<td headers="poutcome" class="gt_row gt_center">unknown</td>
<td headers="y" class="gt_row gt_center">no</td></tr>
    <tr><td headers="age" class="gt_row gt_right">35</td>
<td headers="job" class="gt_row gt_center">management</td>
<td headers="marital" class="gt_row gt_center">single</td>
<td headers="education" class="gt_row gt_center">tertiary</td>
<td headers="default" class="gt_row gt_center">no</td>
<td headers="balance" class="gt_row gt_right">747</td>
<td headers="housing" class="gt_row gt_center">no</td>
<td headers="loan" class="gt_row gt_center">no</td>
<td headers="contact" class="gt_row gt_center">cellular</td>
<td headers="day" class="gt_row gt_right">23</td>
<td headers="month" class="gt_row gt_center">feb</td>
<td headers="campaign" class="gt_row gt_right">2</td>
<td headers="pdays" class="gt_row gt_right">176</td>
<td headers="previous" class="gt_row gt_right">3</td>
<td headers="poutcome" class="gt_row gt_center">failure</td>
<td headers="y" class="gt_row gt_center">no</td></tr>
    <tr><td headers="age" class="gt_row gt_right">36</td>
<td headers="job" class="gt_row gt_center">self-employed</td>
<td headers="marital" class="gt_row gt_center">married</td>
<td headers="education" class="gt_row gt_center">tertiary</td>
<td headers="default" class="gt_row gt_center">no</td>
<td headers="balance" class="gt_row gt_right">307</td>
<td headers="housing" class="gt_row gt_center">yes</td>
<td headers="loan" class="gt_row gt_center">no</td>
<td headers="contact" class="gt_row gt_center">cellular</td>
<td headers="day" class="gt_row gt_right">14</td>
<td headers="month" class="gt_row gt_center">may</td>
<td headers="campaign" class="gt_row gt_right">1</td>
<td headers="pdays" class="gt_row gt_right">330</td>
<td headers="previous" class="gt_row gt_right">2</td>
<td headers="poutcome" class="gt_row gt_center">other</td>
<td headers="y" class="gt_row gt_center">no</td></tr>
    <tr><td headers="age" class="gt_row gt_right">39</td>
<td headers="job" class="gt_row gt_center">technician</td>
<td headers="marital" class="gt_row gt_center">married</td>
<td headers="education" class="gt_row gt_center">secondary</td>
<td headers="default" class="gt_row gt_center">no</td>
<td headers="balance" class="gt_row gt_right">147</td>
<td headers="housing" class="gt_row gt_center">yes</td>
<td headers="loan" class="gt_row gt_center">no</td>
<td headers="contact" class="gt_row gt_center">cellular</td>
<td headers="day" class="gt_row gt_right">6</td>
<td headers="month" class="gt_row gt_center">may</td>
<td headers="campaign" class="gt_row gt_right">2</td>
<td headers="pdays" class="gt_row gt_right">-1</td>
<td headers="previous" class="gt_row gt_right">0</td>
<td headers="poutcome" class="gt_row gt_center">unknown</td>
<td headers="y" class="gt_row gt_center">no</td></tr>
    <tr><td headers="age" class="gt_row gt_right">41</td>
<td headers="job" class="gt_row gt_center">entrepreneur</td>
<td headers="marital" class="gt_row gt_center">married</td>
<td headers="education" class="gt_row gt_center">tertiary</td>
<td headers="default" class="gt_row gt_center">no</td>
<td headers="balance" class="gt_row gt_right">221</td>
<td headers="housing" class="gt_row gt_center">yes</td>
<td headers="loan" class="gt_row gt_center">no</td>
<td headers="contact" class="gt_row gt_center">unknown</td>
<td headers="day" class="gt_row gt_right">14</td>
<td headers="month" class="gt_row gt_center">may</td>
<td headers="campaign" class="gt_row gt_right">2</td>
<td headers="pdays" class="gt_row gt_right">-1</td>
<td headers="previous" class="gt_row gt_right">0</td>
<td headers="poutcome" class="gt_row gt_center">unknown</td>
<td headers="y" class="gt_row gt_center">no</td></tr>
    <tr><td headers="age" class="gt_row gt_right">43</td>
<td headers="job" class="gt_row gt_center">services</td>
<td headers="marital" class="gt_row gt_center">married</td>
<td headers="education" class="gt_row gt_center">primary</td>
<td headers="default" class="gt_row gt_center">no</td>
<td headers="balance" class="gt_row gt_right">-88</td>
<td headers="housing" class="gt_row gt_center">yes</td>
<td headers="loan" class="gt_row gt_center">yes</td>
<td headers="contact" class="gt_row gt_center">cellular</td>
<td headers="day" class="gt_row gt_right">17</td>
<td headers="month" class="gt_row gt_center">apr</td>
<td headers="campaign" class="gt_row gt_right">1</td>
<td headers="pdays" class="gt_row gt_right">147</td>
<td headers="previous" class="gt_row gt_right">2</td>
<td headers="poutcome" class="gt_row gt_center">failure</td>
<td headers="y" class="gt_row gt_center">no</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
# Categorical Variables Proportions
kable(table(bank_data$y), align="ccc", col.names = c("Deposit","Frequency"))
```

| Deposit | Frequency |
|:-------:|:---------:|
|   no    |   4000    |
|   yes   |    521    |

``` r
glimpse(bank_data)

bank_data %>% select(where(is.factor)) %>% map(~prop.table(table(.)))
```

**Univariate Analysis**

``` r
# Distribution of Numerical Variables
bank_data %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(),names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = value, fill=variable)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~variable, scales = "free") + 
  theme_minimal()+
  theme(legend.position = "none")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

``` r
# Distribution of Categorical Variables
bank_data %>% 
  select(where(is.factor)) %>% 
  pivot_longer(cols = everything(),names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = value, fill=variable)) + 
  geom_bar() + 
  facet_wrap(~variable, scales = "free") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-2.png" width="672" />

**Correlation Analysis**

The absence of high inter-correlations suggests that each variable contributes unique information to the model, and there is no redundancy among the predictors.

``` r
cor_mat <- bank_data %>% select(where(is.numeric)) %>% cor()
corrplot(cor_mat, method="circle")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

**Missing Values Inspection**

Upon thorough inspection for missing values, it has been determined that the dataset does not exhibit any instances of missing observations

``` r
gt(summarise_all(bank_data, ~sum(is.na(.))))
```

<div id="tbckkyhrjl" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#tbckkyhrjl table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#tbckkyhrjl thead, #tbckkyhrjl tbody, #tbckkyhrjl tfoot, #tbckkyhrjl tr, #tbckkyhrjl td, #tbckkyhrjl th {
  border-style: none;
}
&#10;#tbckkyhrjl p {
  margin: 0;
  padding: 0;
}
&#10;#tbckkyhrjl .gt_table {
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
&#10;#tbckkyhrjl .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#tbckkyhrjl .gt_title {
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
&#10;#tbckkyhrjl .gt_subtitle {
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
&#10;#tbckkyhrjl .gt_heading {
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
&#10;#tbckkyhrjl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tbckkyhrjl .gt_col_headings {
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
&#10;#tbckkyhrjl .gt_col_heading {
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
&#10;#tbckkyhrjl .gt_column_spanner_outer {
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
&#10;#tbckkyhrjl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#tbckkyhrjl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#tbckkyhrjl .gt_column_spanner {
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
&#10;#tbckkyhrjl .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#tbckkyhrjl .gt_group_heading {
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
&#10;#tbckkyhrjl .gt_empty_group_heading {
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
&#10;#tbckkyhrjl .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#tbckkyhrjl .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#tbckkyhrjl .gt_row {
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
&#10;#tbckkyhrjl .gt_stub {
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
&#10;#tbckkyhrjl .gt_stub_row_group {
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
&#10;#tbckkyhrjl .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#tbckkyhrjl .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#tbckkyhrjl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tbckkyhrjl .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#tbckkyhrjl .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#tbckkyhrjl .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tbckkyhrjl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tbckkyhrjl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#tbckkyhrjl .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#tbckkyhrjl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#tbckkyhrjl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tbckkyhrjl .gt_footnotes {
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
&#10;#tbckkyhrjl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tbckkyhrjl .gt_sourcenotes {
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
&#10;#tbckkyhrjl .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tbckkyhrjl .gt_left {
  text-align: left;
}
&#10;#tbckkyhrjl .gt_center {
  text-align: center;
}
&#10;#tbckkyhrjl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#tbckkyhrjl .gt_font_normal {
  font-weight: normal;
}
&#10;#tbckkyhrjl .gt_font_bold {
  font-weight: bold;
}
&#10;#tbckkyhrjl .gt_font_italic {
  font-style: italic;
}
&#10;#tbckkyhrjl .gt_super {
  font-size: 65%;
}
&#10;#tbckkyhrjl .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#tbckkyhrjl .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#tbckkyhrjl .gt_indent_1 {
  text-indent: 5px;
}
&#10;#tbckkyhrjl .gt_indent_2 {
  text-indent: 10px;
}
&#10;#tbckkyhrjl .gt_indent_3 {
  text-indent: 15px;
}
&#10;#tbckkyhrjl .gt_indent_4 {
  text-indent: 20px;
}
&#10;#tbckkyhrjl .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="age">age</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="job">job</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="marital">marital</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="education">education</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="default">default</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="balance">balance</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="housing">housing</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="loan">loan</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="contact">contact</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="day">day</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="month">month</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="campaign">campaign</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="pdays">pdays</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="previous">previous</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="poutcome">poutcome</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="y">y</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="age" class="gt_row gt_right">0</td>
<td headers="job" class="gt_row gt_right">0</td>
<td headers="marital" class="gt_row gt_right">0</td>
<td headers="education" class="gt_row gt_right">0</td>
<td headers="default" class="gt_row gt_right">0</td>
<td headers="balance" class="gt_row gt_right">0</td>
<td headers="housing" class="gt_row gt_right">0</td>
<td headers="loan" class="gt_row gt_right">0</td>
<td headers="contact" class="gt_row gt_right">0</td>
<td headers="day" class="gt_row gt_right">0</td>
<td headers="month" class="gt_row gt_right">0</td>
<td headers="campaign" class="gt_row gt_right">0</td>
<td headers="pdays" class="gt_row gt_right">0</td>
<td headers="previous" class="gt_row gt_right">0</td>
<td headers="poutcome" class="gt_row gt_right">0</td>
<td headers="y" class="gt_row gt_right">0</td></tr>
  </tbody>
  &#10;  
</table>
</div>

### Modelling Steps Using Tidymodels

**Data Splitting**

``` r
set.seed(123)
data_split <- initial_split(bank_data, prop = 0.75, strata = y)
train_data <- training(data_split)
test_data <- testing(data_split)

set.seed(123)
train_folds <- vfold_cv(train_data, v = 10)
```

**Recipe**

Create a recipe for defining the steps required to transform your raw data into a format suitable for analysis.

``` r
normalized_rec <-
    recipe(y ~ ., data = train_data) %>%
    step_normalize(all_numeric_predictors()) %>% 
    step_dummy(all_nominal_predictors())
```

**Models Selection**

``` r
logistic_reg_spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
    set_engine("glmnet") %>%
    set_mode("classification")
nnet_spec <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
    set_engine("nnet", MaxNWts = 2600) %>%
    set_mode("classification")
svm_r_spec <-svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
    set_engine("kernlab") %>%
    set_mode("classification")
svm_p_spec <-svm_poly(cost = tune(), degree = tune()) %>%
    set_engine("kernlab") %>%
    set_mode("classification")
svm_l_spec <- svm_linear(cost = tune(), margin = tune()) %>%
    set_engine("kernlab") %>%
    set_mode("classification")
cart_spec <-decision_tree(cost_complexity = tune(), min_n = tune()) %>%
    set_engine("rpart") %>%
    set_mode("classification")
bag_cart_spec <-bag_tree() %>%
    set_engine("rpart", times = 50L) %>%
    set_mode("classification")
rf_spec <-rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
    set_engine("ranger") %>%
    set_mode("classification")
knn_spec <-nearest_neighbor(neighbors = tune()) %>%
 set_engine("kknn") %>%
 set_mode("classification")
xgb_spec <-boost_tree(tree_depth = tune(), learn_rate = tune(),
                      loss_reduction = tune(), min_n = tune(),
                      sample_size = tune(), trees =tune()) %>%
    set_engine("xgboost") %>%
    set_mode("classification")
```

**Workflow Creation**

``` r
all_workflows <-workflow_set(
    preproc = list(normalized = normalized_rec ),
    models = list(SVM_radial = svm_r_spec, 
                  SVM_poly = svm_p_spec,
                  SVM_linear = svm_l_spec,
                  neural_network = nnet_spec,
                  logistic_reg_spec,
                  CART = cart_spec,
                  CART_bagged = bag_cart_spec,
                  RF = rf_spec,
                  boosting = xgb_spec,
                  knn = knn_spec))

all_workflows <- all_workflows %>% 
    mutate(wflow_id = gsub("(normalized_)", "",
                           wflow_id))
```

**Parallel Processing**

Parallel processing is a prudent step before fitting models, especially when working with computationally intensive tasks like training multiple models or performing hyperparameter tuning

``` r
cl <- makePSOCKcluster(parallel::detectCores(logical = TRUE))
registerDoParallel(cl)
```

**Hyperparamter Tuninning**

``` r
tic()
grid_ctrl <-
    control_grid(
        save_pred = TRUE,
        parallel_over = "everything",
        save_workflow = TRUE,
        event_level = "second")

grid_results <-
    all_workflows %>%
    workflow_map(
        seed = 1503,
        resamples = train_folds,
        grid = 20,
        control = grid_ctrl,
        verbose = TRUE)
toc()
```

**Listing best model**

One of the most informative metrics for classification tasks, especially in imbalanced datasets, is the Receiver Operating Characteristic (ROC) Area Under Curve (AUC). This metric provides a comprehensive measure of model performance across various threshold settings, balancing the trade-off between sensitivity (true positive rate) and specificity (false positive rate). Upon listing and comparing models based on the ROC AUC metric, the analysis indicates that a XGBOOST model outperforms others with an AUC of 74.85% on the cross-validation data.

``` r
gt(grid_results %>%
    rank_results(rank_metric = "roc_auc") %>%
    filter(.metric == "roc_auc") %>%
    select(model, .config, roc_auc = mean, rank) %>% slice(1:20))
```

<div id="bdqouwplhb" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#bdqouwplhb table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#bdqouwplhb thead, #bdqouwplhb tbody, #bdqouwplhb tfoot, #bdqouwplhb tr, #bdqouwplhb td, #bdqouwplhb th {
  border-style: none;
}
&#10;#bdqouwplhb p {
  margin: 0;
  padding: 0;
}
&#10;#bdqouwplhb .gt_table {
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
&#10;#bdqouwplhb .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#bdqouwplhb .gt_title {
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
&#10;#bdqouwplhb .gt_subtitle {
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
&#10;#bdqouwplhb .gt_heading {
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
&#10;#bdqouwplhb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bdqouwplhb .gt_col_headings {
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
&#10;#bdqouwplhb .gt_col_heading {
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
&#10;#bdqouwplhb .gt_column_spanner_outer {
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
&#10;#bdqouwplhb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#bdqouwplhb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#bdqouwplhb .gt_column_spanner {
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
&#10;#bdqouwplhb .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#bdqouwplhb .gt_group_heading {
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
&#10;#bdqouwplhb .gt_empty_group_heading {
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
&#10;#bdqouwplhb .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#bdqouwplhb .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#bdqouwplhb .gt_row {
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
&#10;#bdqouwplhb .gt_stub {
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
&#10;#bdqouwplhb .gt_stub_row_group {
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
&#10;#bdqouwplhb .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#bdqouwplhb .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#bdqouwplhb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bdqouwplhb .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#bdqouwplhb .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#bdqouwplhb .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bdqouwplhb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bdqouwplhb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#bdqouwplhb .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#bdqouwplhb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#bdqouwplhb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bdqouwplhb .gt_footnotes {
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
&#10;#bdqouwplhb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bdqouwplhb .gt_sourcenotes {
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
&#10;#bdqouwplhb .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bdqouwplhb .gt_left {
  text-align: left;
}
&#10;#bdqouwplhb .gt_center {
  text-align: center;
}
&#10;#bdqouwplhb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#bdqouwplhb .gt_font_normal {
  font-weight: normal;
}
&#10;#bdqouwplhb .gt_font_bold {
  font-weight: bold;
}
&#10;#bdqouwplhb .gt_font_italic {
  font-style: italic;
}
&#10;#bdqouwplhb .gt_super {
  font-size: 65%;
}
&#10;#bdqouwplhb .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#bdqouwplhb .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#bdqouwplhb .gt_indent_1 {
  text-indent: 5px;
}
&#10;#bdqouwplhb .gt_indent_2 {
  text-indent: 10px;
}
&#10;#bdqouwplhb .gt_indent_3 {
  text-indent: 15px;
}
&#10;#bdqouwplhb .gt_indent_4 {
  text-indent: 20px;
}
&#10;#bdqouwplhb .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="model">model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=".config">.config</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="roc_auc">roc_auc</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="rank">rank</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="model" class="gt_row gt_left">boost_tree</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model01</td>
<td headers="roc_auc" class="gt_row gt_right">0.7484995</td>
<td headers="rank" class="gt_row gt_right">1</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model18</td>
<td headers="roc_auc" class="gt_row gt_right">0.7430474</td>
<td headers="rank" class="gt_row gt_right">2</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model06</td>
<td headers="roc_auc" class="gt_row gt_right">0.7429241</td>
<td headers="rank" class="gt_row gt_right">3</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model20</td>
<td headers="roc_auc" class="gt_row gt_right">0.7423235</td>
<td headers="rank" class="gt_row gt_right">4</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model02</td>
<td headers="roc_auc" class="gt_row gt_right">0.7411300</td>
<td headers="rank" class="gt_row gt_right">5</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model01</td>
<td headers="roc_auc" class="gt_row gt_right">0.7409096</td>
<td headers="rank" class="gt_row gt_right">6</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model04</td>
<td headers="roc_auc" class="gt_row gt_right">0.7408934</td>
<td headers="rank" class="gt_row gt_right">7</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model17</td>
<td headers="roc_auc" class="gt_row gt_right">0.7399115</td>
<td headers="rank" class="gt_row gt_right">8</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model11</td>
<td headers="roc_auc" class="gt_row gt_right">0.7396023</td>
<td headers="rank" class="gt_row gt_right">9</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model09</td>
<td headers="roc_auc" class="gt_row gt_right">0.7389904</td>
<td headers="rank" class="gt_row gt_right">10</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model03</td>
<td headers="roc_auc" class="gt_row gt_right">0.7381772</td>
<td headers="rank" class="gt_row gt_right">11</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model10</td>
<td headers="roc_auc" class="gt_row gt_right">0.7378032</td>
<td headers="rank" class="gt_row gt_right">12</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model07</td>
<td headers="roc_auc" class="gt_row gt_right">0.7361533</td>
<td headers="rank" class="gt_row gt_right">13</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model05</td>
<td headers="roc_auc" class="gt_row gt_right">0.7359897</td>
<td headers="rank" class="gt_row gt_right">14</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model14</td>
<td headers="roc_auc" class="gt_row gt_right">0.7356451</td>
<td headers="rank" class="gt_row gt_right">15</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model19</td>
<td headers="roc_auc" class="gt_row gt_right">0.7347003</td>
<td headers="rank" class="gt_row gt_right">16</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model08</td>
<td headers="roc_auc" class="gt_row gt_right">0.7338226</td>
<td headers="rank" class="gt_row gt_right">17</td></tr>
    <tr><td headers="model" class="gt_row gt_left">boost_tree</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model05</td>
<td headers="roc_auc" class="gt_row gt_right">0.7332005</td>
<td headers="rank" class="gt_row gt_right">18</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model12</td>
<td headers="roc_auc" class="gt_row gt_right">0.7317803</td>
<td headers="rank" class="gt_row gt_right">19</td></tr>
    <tr><td headers="model" class="gt_row gt_left">rand_forest</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model13</td>
<td headers="roc_auc" class="gt_row gt_right">0.7316423</td>
<td headers="rank" class="gt_row gt_right">20</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
autoplot(
    grid_results,
    rank_metric = "roc_auc", 
    metric = "roc_auc", 
    select_best = TRUE 
) +
    geom_text(aes(y = mean - 0.015, label = wflow_id), angle = 90,
              hjust = 1) +
    lims(y = c(0.5, 0.8)) +
    scale_x_continuous(breaks = 1:10) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(y = "ROC AUC")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />

**Finalizing best model**

We proceed by fitting the XGBOOST model to the entire training dataset.

``` r
best_results <-
    grid_results %>%
    extract_workflow_set_result("boosting") %>% 
    select_best(metric = "roc_auc")

boosting_test_results <-
    grid_results %>%
    extract_workflow("boosting") %>%
    finalize_workflow(best_results) %>%
    last_fit(split = data_split)
```

The model has an Area Under the Curve (AUC) of 73.81% on the test data. We observe that with 24.49% of the highest-ranked calls (as per the model’s prediction), we are able to detect 50% of the ‘yes’ cases – the clients who actually subscribed to a term deposit. While targeting 50% of the calls based on the model’s ranking allows us to identify 78.62% of the potential subscribers.

``` r
gt(collect_metrics(boosting_test_results))
```

<div id="rwwclghwsb" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rwwclghwsb table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#rwwclghwsb thead, #rwwclghwsb tbody, #rwwclghwsb tfoot, #rwwclghwsb tr, #rwwclghwsb td, #rwwclghwsb th {
  border-style: none;
}
&#10;#rwwclghwsb p {
  margin: 0;
  padding: 0;
}
&#10;#rwwclghwsb .gt_table {
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
&#10;#rwwclghwsb .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#rwwclghwsb .gt_title {
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
&#10;#rwwclghwsb .gt_subtitle {
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
&#10;#rwwclghwsb .gt_heading {
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
&#10;#rwwclghwsb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rwwclghwsb .gt_col_headings {
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
&#10;#rwwclghwsb .gt_col_heading {
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
&#10;#rwwclghwsb .gt_column_spanner_outer {
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
&#10;#rwwclghwsb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#rwwclghwsb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#rwwclghwsb .gt_column_spanner {
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
&#10;#rwwclghwsb .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#rwwclghwsb .gt_group_heading {
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
&#10;#rwwclghwsb .gt_empty_group_heading {
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
&#10;#rwwclghwsb .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#rwwclghwsb .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#rwwclghwsb .gt_row {
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
&#10;#rwwclghwsb .gt_stub {
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
&#10;#rwwclghwsb .gt_stub_row_group {
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
&#10;#rwwclghwsb .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#rwwclghwsb .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#rwwclghwsb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rwwclghwsb .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#rwwclghwsb .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#rwwclghwsb .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rwwclghwsb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rwwclghwsb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#rwwclghwsb .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#rwwclghwsb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#rwwclghwsb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rwwclghwsb .gt_footnotes {
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
&#10;#rwwclghwsb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rwwclghwsb .gt_sourcenotes {
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
&#10;#rwwclghwsb .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rwwclghwsb .gt_left {
  text-align: left;
}
&#10;#rwwclghwsb .gt_center {
  text-align: center;
}
&#10;#rwwclghwsb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#rwwclghwsb .gt_font_normal {
  font-weight: normal;
}
&#10;#rwwclghwsb .gt_font_bold {
  font-weight: bold;
}
&#10;#rwwclghwsb .gt_font_italic {
  font-style: italic;
}
&#10;#rwwclghwsb .gt_super {
  font-size: 65%;
}
&#10;#rwwclghwsb .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#rwwclghwsb .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#rwwclghwsb .gt_indent_1 {
  text-indent: 5px;
}
&#10;#rwwclghwsb .gt_indent_2 {
  text-indent: 10px;
}
&#10;#rwwclghwsb .gt_indent_3 {
  text-indent: 15px;
}
&#10;#rwwclghwsb .gt_indent_4 {
  text-indent: 20px;
}
&#10;#rwwclghwsb .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=".metric">.metric</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=".estimator">.estimator</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id=".estimate">.estimate</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=".config">.config</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers=".metric" class="gt_row gt_left">accuracy</td>
<td headers=".estimator" class="gt_row gt_left">binary</td>
<td headers=".estimate" class="gt_row gt_right">0.8868258</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model1</td></tr>
    <tr><td headers=".metric" class="gt_row gt_left">roc_auc</td>
<td headers=".estimator" class="gt_row gt_left">binary</td>
<td headers=".estimate" class="gt_row gt_right">0.7392214</td>
<td headers=".config" class="gt_row gt_left">Preprocessor1_Model1</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
boosting_test_results %>% collect_predictions() %>% 
    roc_curve(truth = y, .pred_yes, event_level = "second") %>%
    autoplot()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />

``` r
boosting_test_results %>% collect_predictions() %>% 
    gain_curve(truth = y, .pred_yes, event_level = "second") %>%
    autoplot()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-2.png" width="672" />

**Considering Model Stacking over individual model selection**

We consider an ensemble of various models from our grid, with a Lasso regression model serving as the meta-learner. This approach aims to harness the collective strengths of different models, thereby potentially achieving superior predictive performance compared to any single model. Upon implementation, the stacked model exhibits an AUC of 75.60%, an improvement over the best individual model’s AUC. Furthermore, the stacked model demonstrates a compelling efficiency in prioritizing calls. It detects 50% of the ‘yes’ cases by targeting only 19.10% of the highest-ranked calls. While extending the campaign to 50% of the ranked calls, it successfully identifies 79.39% of the potential subscribers.

``` r
# Create Stack Model Object
model_stack <-
    stacks() %>%
    add_candidates(grid_results)

# Train meta-learning model
set.seed(2001)
ens <- blend_predictions(model_stack, 
                         non_negative = TRUE, metric = metric_set(roc_auc),
                          penalty = 10^seq(-3, -0.5, length = 10))
```

``` r
# Plot optimal number of models
autoplot(ens)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-20-1.png" width="672" />

``` r
# Plot weights of the stack model
autoplot(ens, "weights") +
    geom_text(aes(x = weight + 0.01, label = model), hjust = 0) +
    theme_minimal() + 
    theme(legend.position = "none") +
    lims(x = c(-0.01, 10)) +
    labs(title = "")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-20-2.png" width="672" />

``` r
# Fit Member Models
ens_fit <- fit_members(ens)
```

``` r
#Test Stacking and obtain metrics
clas_metrics<- metric_set(roc_auc)

ens_test_pred <-
    predict(ens_fit, test_data, type="prob" ) %>%
    bind_cols(test_data %>% select(y),predict(ens_fit, test_data)) 

gt(ens_test_pred %>%
    clas_metrics(truth = y, .pred_yes,event_level = "second"))
```

<div id="ejgnwxutga" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ejgnwxutga table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ejgnwxutga thead, #ejgnwxutga tbody, #ejgnwxutga tfoot, #ejgnwxutga tr, #ejgnwxutga td, #ejgnwxutga th {
  border-style: none;
}
&#10;#ejgnwxutga p {
  margin: 0;
  padding: 0;
}
&#10;#ejgnwxutga .gt_table {
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
&#10;#ejgnwxutga .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ejgnwxutga .gt_title {
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
&#10;#ejgnwxutga .gt_subtitle {
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
&#10;#ejgnwxutga .gt_heading {
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
&#10;#ejgnwxutga .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ejgnwxutga .gt_col_headings {
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
&#10;#ejgnwxutga .gt_col_heading {
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
&#10;#ejgnwxutga .gt_column_spanner_outer {
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
&#10;#ejgnwxutga .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ejgnwxutga .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ejgnwxutga .gt_column_spanner {
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
&#10;#ejgnwxutga .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ejgnwxutga .gt_group_heading {
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
&#10;#ejgnwxutga .gt_empty_group_heading {
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
&#10;#ejgnwxutga .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ejgnwxutga .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ejgnwxutga .gt_row {
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
&#10;#ejgnwxutga .gt_stub {
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
&#10;#ejgnwxutga .gt_stub_row_group {
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
&#10;#ejgnwxutga .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ejgnwxutga .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ejgnwxutga .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ejgnwxutga .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ejgnwxutga .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ejgnwxutga .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ejgnwxutga .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ejgnwxutga .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ejgnwxutga .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ejgnwxutga .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ejgnwxutga .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ejgnwxutga .gt_footnotes {
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
&#10;#ejgnwxutga .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ejgnwxutga .gt_sourcenotes {
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
&#10;#ejgnwxutga .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ejgnwxutga .gt_left {
  text-align: left;
}
&#10;#ejgnwxutga .gt_center {
  text-align: center;
}
&#10;#ejgnwxutga .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ejgnwxutga .gt_font_normal {
  font-weight: normal;
}
&#10;#ejgnwxutga .gt_font_bold {
  font-weight: bold;
}
&#10;#ejgnwxutga .gt_font_italic {
  font-style: italic;
}
&#10;#ejgnwxutga .gt_super {
  font-size: 65%;
}
&#10;#ejgnwxutga .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ejgnwxutga .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ejgnwxutga .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ejgnwxutga .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ejgnwxutga .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ejgnwxutga .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ejgnwxutga .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=".metric">.metric</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=".estimator">.estimator</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id=".estimate">.estimate</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers=".metric" class="gt_row gt_left">roc_auc</td>
<td headers=".estimator" class="gt_row gt_left">binary</td>
<td headers=".estimate" class="gt_row gt_right">0.7559771</td></tr>
  </tbody>
  &#10;  
</table>
</div>

``` r
ens_test_pred %>% 
    roc_curve(truth = y, .pred_yes, event_level = "second") %>%
    autoplot()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-23-1.png" width="672" />

``` r
ens_test_pred %>% 
    conf_mat(truth = y, .pred_class) %>% 
    autoplot(type="heatmap") +
    scale_fill_gradient(low="white", high="#009194")  + 
    theme(axis.ticks = element_line(colour = "gray0"),
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_line(colour = NA),
    axis.title = element_text(size = 12,
        colour = "gray0"), axis.text = element_text(size = 10,
        face = "bold", colour = "gray0"),
    axis.text.x = element_text(size = 10,
        colour = "gray0"), axis.text.y = element_text(size = 10,
        colour = "gray0"), panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-23-2.png" width="672" />

``` r
gaincurve <- gain_curve(data = ens_test_pred, truth = y, .pred_yes, event_level = "second") 

autoplot(gaincurve)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-23-3.png" width="672" />

End parallel processing

``` r
stopCluster(cl)
```

### Conclusion

The gain curve insightfully reveals that with this approach, we can identify approximately half of the potential clients by reaching out to only a select percentage (19.1%) of the total call list. While extending the campaign to 50% of the ranked calls, it successfully identifies 79.39% of the potential subscribers. This level of efficiency in client targeting is a crucial factor in optimizing the cost policy of any marketing campaign. This approach enables a more judicious allocation of resources, ensuring that efforts and investments are concentrated on the most promising leads, thereby enhancing the overall cost-effectiveness and success rate of the campaigns.

Furthermore, there is ample scope for further refinement and improvement of the model. A deeper analysis, including more extensive exploratory data analysis and feature engineering, as well as a more thorough hyperparameter tuning, holds the potential to elevate the model’s performance even further. Additionally, conducting a sensitivity analysis to understand the impact and importance of each variable could provide invaluable insights. This analysis would not only enhance the model’s predictive accuracy but also offer strategic insights into the factors most influential in determining customer responses to telemarketing efforts.

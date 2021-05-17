## Script: 01_Explore_raw_data.R ------------------------------------


# 0. Working setup ---------------------------------
library(data.table)
library(ggplot2)
library(haven) # read .dta files

# 1. Load data ------------------------------------
institutions <- haven::read_dta('Data/Raw_Data/data/T1_data_packaging_practices.dta')

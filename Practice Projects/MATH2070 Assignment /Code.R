# MATH2070 Assignment in R
# Summer Holidays 2019/2020

# Raw Data imported as excel 
library(readxl)
> FinanceData <- read_excel("Desktop/FinanceData.xlsx")

# importing Tidyverse
library(tidyverse)

# Question 1
FinanceData %>%
  select(-"Date", -"Dow Jones Index", -"S&P 500", -"Dow Inc")





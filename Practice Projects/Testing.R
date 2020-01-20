library(readxl)
FinanceData <- read_excel("Desktop/GIT/R/Practice Projects/MATH2070 Assignment /FinanceData.xlsx")

# Importing libraries
library(tidyverse)
library(reshape2)
require(reshape2)
require(tidyverse)
library(matlib)
require(matlib)

# Renaming variables
FinanceData <- rename(FinanceData,
                      threem = "3M",
                      american_express = "American Express",
                      apple ="Apple",
                      boeing = "Boeing",
                      cat ="Caterpillar",
                      coke = "Coca-Cola",
                      chevron = "Chevron",
                      cisco = "Cisco",
                      exxon = "Exxon Mobil",
                      goldman = "Goldman Sachs",
                      home_depot = "Home Depot",
                      ibm = "IBM",
                      intel = "Intel",
                      j_and_j = "Johnson & Johnson",
                      jpmorgan = "JPMorgan Chase",
                      mcdonalds = "McDonald's",
                      merck = "Merck",
                      microsoft = "Microsoft",
                      nike = "Nike",
                      pfizer = "Pfizer",
                      procter = "Procter & Gamble",
                      travelers = "Travelers Companies Inc.",
                      united_heath = "United Health",
                      united_tech = "United Technologies",
                      verizon = "Verizon",
                      visa = "Visa",
                      walgreens = "Walgreens Boots Alliance",
                      walmart = "Walmart",
                      disney = "Walt Disney"
)

# removing 'bad' columns
df_1 <- FinanceData %>%
  select(-"Date", -"Dow Jones Index", -"S&P 500", -"Dow Inc")

# selecting three similar coolumns
df <- FinanceData %>% select(apple, boeing, american_express)

comparefunc <- function(x,y,z){
  if (x > y){value <- "ecks"}else{if(y > z){value <- "why"}else{value = "zed"}}
  return(value)
}

# The following give the same output
results <- apply(X = df, MARGIN = 1, FUN = function(w) comparefunc(w['apple'],w['boeing'],w['american_express']))
results_too <- mapply(comparefunc, df$apple, df$boeing, df$american_express)





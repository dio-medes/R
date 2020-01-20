#### "Melt and Cast The Shape of Your Data-Frame: Exercises" from 
# https://www.r-exercises.com/2018/06/22/melt-and-cast-the-shape-of-your-data-frame-exercises/

# Libraries
library(tidyverse)
library(data.table)

# Set up data frame for exercises

df <- data.frame(id = 1:2, q1 = c("A", "B"), q2 = c("C", "A"), stringsAsFactors = FALSE)

#### Exercise 1
ex_1 <- df %>%
  gather(., 'q1', 'q2', key = "question", value = "value")

#### Exercise 2
ex_2 <- ex_1 %>% 
  spread(., key = "question", value = "value")

#### Exercise 3 
ex_3 <- df %>% 
  gather(.,'q1', 'q2', key = "question", value = "value") %>% 
  spread(., key = "id", value = "value") %>% 
  rename(., "id_1" = "1", "id_2" = "2")

#### Exercise 4 # Using solutions
df2 <- data.frame(
  A = c("A1", "A12", "A31", "A4"), 
  B = c("B4", "C7", "C3", "B9"), 
  C = c("C3", "B16", "B3", "C4")
)
setDT(df2)
# first: adds a column "id" which is just row index
# second: melts the data set, where the variaable names become factors in a column,
# and "id" is used to identity the rows that are produced
df2l <- data.table::melt(df2[, id := .I], id.vars = "id")
# first: using dcast (long-to-wide). Using data df21, casts the data, rows identified by id,
# then spread across A, B and C using string select
# second: [, -c("id")] selects all but the id column
ex_4 <- data.table::dcast(df2l, id ~ substr(value, 1, 1))[, -c("id")]

#### Exercise 5 
df3 <- data.frame(
  Join_ID = rep(1:3, each = 2), 
  Type    = rep(c("a", "b"), 3), 
  v2      = c(8, 9, 7, 6, 5, 4)*10
)
setDT(df3)
df31 <- data.table::dcast(df3, Join_ID ~ Type, value.var = "v2") # then rename variables

# cleaner solution
df311 <- dcast(df3, Join_ID ~ paste0(Type, "_v2"), value.var = "v2")
setDT(df311)

#### Exercise 6
install.packages("AER")
library("AER")
data("Fertility")
setDT(Fertility)




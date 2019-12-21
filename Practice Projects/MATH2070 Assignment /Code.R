# MATH2070 Assignment in R
# Summer Holidays 2019/2020

# Raw Data imported as excel 
library(readxl)
FinanceData <- read_excel("Desktop/GIT/R/Practice Projects/MATH2070 Assignment /FinanceData.xlsx")

# importing Tidyverse
library(tidyverse)
library(reshape2)
require(reshape2)
require(tidyverse)

### Question 1 ###
FinanceData %>%
  select(-"Date", -"Dow Jones Index", -"S&P 500", -"Dow Inc") -> dataq;
  dataw <- log(dataq, base = exp(1))

# Making differenced-log data
diffunp <- function(input_column) {
  results <- c()
  for (i in 1:1697) {
    v <- input_column[i+1] - input_column[i]
    results <- rbind(results,v)
  }
return(results)
}
ykt <- as.tibble(cbind(apply(dataw,2,diffunp)))

# Making differenced-log data with dplyr (much cleaner!)
# The following two piped-sections give the same output

differenced <- dataw %>%
  mutate_at(vars("3M":"Walt Disney"), funs(.-lag(.)))

differenced_1 <- dataw %>%
  mutate_all(funs(.-lag(.)))

# Renaming variables

ykt <- rename(ykt,
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

FinanceDataTime <- rename(FinanceDataTime ,
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

# iii
FinanceData$Date <-as.character(FinanceData$Date)
match("2014-12-01",FinanceData$Date) # row 483
match("2016-09-01",FinanceData$Date) # row 925
match("2018-02-01",FinanceData$Date) # row 1281 

# For each sub period, we first find the correlation matrix, then melt it, then plot it.

cormat_1 <- round(cor(ykt[482:924,],method = c("pearson")),5)
head(cormat_1)

melted_cormat_1 <- melt(cormat_1)
head(melted_cormat_1)

ggplot(data = melted_cormat_1, aes(Var2, Var1, fill = value))+ #First Period
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 70, vjust = 1, 
                                   size = 10, hjust = 1))+
  coord_fixed()

cormat_2 <- round(cor(ykt[924:1280,]),5)
melted_cormat_2 <- melt(cormat_2)
head(melted_cormat_2)

ggplot(data = melted_cormat_2, aes(Var2, Var1, fill = value))+ #Second Period
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 70, vjust = 1, 
                                   size = 10, hjust = 1))+
  coord_fixed()

# Now we plot the DJIA over the whole period

FinanceDataTime <- cbind(c(1:1698),FinanceData)
names(FinanceDataTime)[1] <- "time"
ggplot(FinanceDataTime, aes(x=time,y=`Dow Jones Index`)) + geom_line()

#iv
# The following three lines cbind to corrdata the following process on each cormat:
# first extract the upper triangle of the correlation matrix by using the logical function upper.tri,
# then melting that into a vector.

corrdata <- cbind(melt(cormat_1[upper.tri(cormat_1)]),melt(cormat_2[upper.tri(cormat_2)]))
names(corrdata) <- c("first","second")
corrdata <- as.tibble(corrdata)

# Now we generate the layered histogram

ggplot(corrdata)+
  geom_histogram(mapping = aes(x = first),binwidth = 0.05, alpha = 0.5,
                 fill = 'blue')+
  geom_histogram(mapping = aes(x = second),binwidth = 0.05, alpha = 0.5,
                 fill = 'red')

# v and vi
# variances_ykt is a column vector of the variances of each company
# the lines after determine max and min variances
variances_ykt = as.tibble(sapply(ykt, var))
which.max(apply(variances_ykt,MARGIN=1,max)) # apple is max, index 3
which.min(apply(variances_ykt,MARGIN=1,min)) # coke is min, index 8

# finding max correlations
corrdatafull <- as.tibble(round(cor(ykt[1:1697,],method = c("pearson")),5))

applecokecorrs <- corrdatafull %>% 
  slice(c(3,8))  # selects the relevant rows

applecokecorrs[applecokecorrs == 1] <- 0 # removing correlations with self

which.max(apply(applecokecorrs[1,],MARGIN=2,max)) # microsoft, index 18
which.max(apply(applecokecorrs[2,],MARGIN=2,max)) # procter, index 21

# making and plotting normalised time-series

FinanceDataTime <- mutate(FinanceDataTime,
       norm_apple_price = apple*(100/apple[1]),
       norm_coke_price = coke*(100/coke[1]))
       
ggplot(FinanceDataTime)+
  geom_line(mapping = aes(x = time, y = norm_apple_price), colour = 'blue')+
  geom_line(mapping = aes(x = time, y = norm_coke_price), colour = 'red')+
  xlab("Normalised Price")+ ylab("# Days")
  



























  
  
  
  



# MATH2070 Assignment in R
# Summer Holidays 2019/2020

# Raw Data imported as excel 
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

### Question 1 ###
# Pre-question data prep

# removing 'bad' columns
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

# Making differenced-log data with dplyr instead (much cleaner!)
# The following two piped-sections give the same output
# the dot means 'whatever was piped into this' - it is a dummy argument/variable

#differenced <- dataw %>%
#  mutate_at(vars("3M":"Walt Disney"), funs(.-lag(.)))

#differenced_1 <- dataw %>%
#  mutate_all(funs(.-lag(.)))

# iii
# Determining relevant rows for splitting data into time periods
FinanceData$Date <-as.character(FinanceData$Date)
match("2014-12-01",FinanceData$Date) # row 483 -> corresponds to row 482 in the ykt data
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
variances_ykt <- as.tibble(sapply(ykt, var))
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
       norm_coke_price = coke*(100/coke[1]),
       norm_microsoft_price = microsoft*(100/microsoft[1]),
       norm_procter_price = procter*(100/procter[1])
       )

# Now we plot the data, using a molten data frame in order to get a nice legend in the ggplot
q1plotdata <- FinanceDataTime %>% select(norm_apple_price,norm_coke_price, norm_microsoft_price,norm_procter_price)
names(q1plotdata) <- labelsvec <- c("Apple", "Coke", "Microsoft", "Procter")
q1plotdata_melted <- cbind(melt(q1plotdata),c(1:1698))
names(q1plotdata_melted) <- c("Company","price","timevec")

ggplot(q1plotdata_melted, mapping = aes(x = timevec, y=price, colour = Company))+
  geom_line()+
  scale_colour_manual(values=c("deepskyblue4","firebrick","deepskyblue3","firebrick1"))+
  ylab("Normalised Price")+ xlab("# Days")+
  labs(title = "Question 1 Plot", subtitle = "Sub-questions (v) and (vi)")

### Question 2 ###

# This queston is concerned with the simple return rates of the raw prices. Recyclying the form of
# the function used in question 1, we have the following:

# Making simple return data
returnsfunc <- function(input_column) {
  results <- c()
  for (i in 1:1697) {
    v <- (input_column[i+1] - input_column[i])/input_column[i]
    results <- rbind(results,v)
  }
  return(results)
}
returnsdata <- as.tibble(cbind(apply(dataq,2,returnsfunc)))

# a
# Now we have to apply the n-unrestricted asset portfolio theorem
# Calculate the relevant constants and get the critical line as a function of t

S <- as.matrix(round(cov(returnsdata),6))

e <- c()
for (i in 1:29) {e[i] <- 1}
e <- as.vector(e)
e_t <- t(e)

r <- as.vector(apply(returnsdata,2,mean))
r_t <- t(r)

a_2 <- as.vector(e_t%*%inv(S)%*%e)
b_2 <- as.vector(r_t%*%inv(S)%*%e)
c_2 <- as.vector(r_t%*%inv(S)%*%r)
d_2 <- a_2*c_2-b_2^2

alpha_2 <- as.vector((1/a_2)*(inv(S)%*%e))
beta_2 <- as.vector((inv(S)%*%r) - (b_2/a_2)*(inv(S)%*%e))

dollar_invest <- as.tibble(t(as.matrix(200000*(alpha_2 + 0.2*beta_2))))
names(dollar_invest) <- names(dataq)

p_star <- as.tibble(cbind((b_2+d_2*0.2)/a_2,sqrt((1+d_2*0.2^2)/a_2)))
names(p_star) <- c("mu","sigma")

# plotting position, no idea what the arguments in geom_bar do atm, also names may be wrong (or in alphabetical order)
dollar_invest %>%
  gather(key = names(dataq), value = position) %>% 
ggplot(., aes(x = `names(dataq)`, y = position)) +
  geom_bar(
    stat = "identity", position = position_stack(),
    color = "white", fill = "lightblue"
  ) +
  coord_flip()

# b

# This question calls for the following in mu-sigma space:
# the 29 stocks, MVF with |t| < 0.35, 1000 randomly-generated feasible portfiolios, indifference curve of t=-0.2

# 29 stocks

stocks_ms_data <- as.tibble(cbind(apply(returnsdata,2,mean),sqrt(apply(returnsdata,2,var))))
names(stocks_ms_data) <- c("mu_stocks","sigma_stocks")

# MVF
# Using the theorem, we calculate the following:

mu_mvf <- function(t_mu) {
  (b_2+d_2*t_mu)/a_2
}
sigma_mvf <- function(t_sigma) {
  sqrt(((1+d_2*t_sigma^2)/a_2))
}

dummy_2 <- seq(from = -0.35, to = 0.35, by = 0.005) # Very obviously a matlab approach, should find elegant solution
mvf_data <- as.tibble(cbind(mu_mvf(dummy_2),sigma_mvf(dummy_2)))
names(mvf_data) <- c("mu","sigma")

# 1000 random feasible portfolios
set.seed(2)
counter <- 0
rand_data <- as.tibble(cbind(1:1000,1:1000))
names(rand_data) <- c("mu_r","sigma_r")
rand_feasible <- c(1:29)

# while loop method (optimised Ruth)
while (counter < 1000) {
  for (i in 1:29) {
    # Generate vector column with required element-wise bounds |x_i| < 0.2
    as.vector(rand_feasible[i] <- -20 + 40*runif(n = 1))
    }
    # Rescale to sum to 1, then scale to somewhere in [0,1]
    rand_feasible <- (1/sum(rand_feasible))*rand_feasible*runif(n = 1)
    
    # Find bounds on parabola. Sigma bound is constant, mu bound is a function of sigma (hyperbola)
    sigma_lower_bound <- 1/sqrt(a_2)
    mu_lower_bound = b_2/a_2 - sqrt((d_2/a_2) * (t(rand_feasible) %*% S %*% rand_feasible - (1/a_2)))
    mu_upper_bound = b_2/a_2 + sqrt((d_2/a_2) * (t(rand_feasible) %*% S %*% rand_feasible - (1/a_2)))
    
    # Check whether the current column satisfies all the required conditions (inside the bounds and sigma < 0.05 for each column)
    # If it satisfies all the conditions, add one to countvar
    if (
      sqrt(t(rand_feasible) %*% S %*% rand_feasible) >= sigma_lower_bound &
      sqrt(t(rand_feasible) %*% S %*% rand_feasible) <= 0.05 &
      t(rand_feasible) %*% r < mu_upper_bound &
      t(rand_feasible) %*% r > mu_lower_bound 
      ) {
    #then...
      counter <- counter+1
      rand_data[counter,1] <- rand_feasible %*% r # random mu
      rand_data[counter,2] <- sqrt(t(rand_feasible) %*% S %*% rand_feasible)
  }
}

# Indifference curve
dummy_2_b <- seq(from = 0, to = 0.05, by = 0.0005)
z_star <- -0.2*((b_2+d_2*0.2)/a_2) + (1/2)*((1+d_2*0.2^2)/a_2)
indiff_func <- function(x){
  (-1/0.2)*(z_star - x^2/2)
}
indiff_data <- as.tibble(cbind(dummy_2_b,indiff_func(dummy_2_b)))
names(indiff_data) <- c("input","curve")

# Question 2 plot

ggplot()+
  geom_point(rand_data, mapping = aes(x = sigma_r, y = mu_r), colour = "royalblue", alpha = 0.1)+
  geom_point(mvf_data, mapping = aes(x=sigma,y=mu),colour = "firebrick",alpha = 0.8)+
  geom_point(stocks_ms_data, mapping = aes(x = sigma_stocks, y = mu_stocks), colour = "seagreen1")+
  geom_point(indiff_data, mapping = aes(x = input, y=curve))+
  ylab("mu")+ xlab("sigma")+
  labs(title = "Question 2 Plot")

# R-centric solution for generating the random portfolios
# produces a random (mu,sigma) combo which satisfies all requirements (BROKEN & REALLY INEFFICIENT)
#randgen <- function(){
  #set up results tibble
  results <- as.matrix(cbind(1,1))
  
  # generates a random vector of elements that satisfy |x_i|< 20 & sums to somewhere in [0,1]
  rand_vec_fun <- function(x){
    x <- as.vector(-20 + 40*runif(n = 29))
    return(1/sum(x)*x*runif(n = 1))
  }
  v <- rand_vec_fun()
  
  # Calculate bounds (notice, depends on S)
  sigma_lower_bound <- 1/sqrt(a_2)
  mu_lower_bound <- b_2/a_2 - sqrt((d_2/a_2) * (t(v) %*% S %*% v - (1/a_2))) # these produce a error about 50% of the time
  mu_upper_bound <- b_2/a_2 + sqrt((d_2/a_2) * (t(v) %*% S %*% v - (1/a_2)))
  
  # Deal with this error in the random generation process (results only proceed if not sqrting a negative number)
  if (is.na(mu_lower_bound) == FALSE){
  
  # check each condition
  if (
    sqrt(t(v) %*% S %*% v) >= sigma_lower_bound &
    sqrt(t(v) %*% S %*% v) <= 0.05 &
    t(v) %*% r < mu_upper_bound &
    t(v) %*% r > mu_lower_bound 
  ) {
    #then...
    results[1] <- v %*% r # random mu
    results[2] <- sqrt(t(v) %*% S %*% v) #random sigma
  }
} else {results <- cbind(NA,NA)}
return(results)
}
# next part is supposed to output a matrix that looks like rand_data.
#portfolio <- function(){
  musigma <- matrix(nrow=1,ncol=2)
  n <- 0
  while(n < 10)
    new_gen <- t(as.matrix(replicate(1,randgen())))
    if (is.na(new_gen[1,1]) == FALSE) {
      musigma <- rbind(musigma,new_gen)
      n <- n+1}
return(musigma)
}

### Question 3 ###

# Need to solve alpha + t*beta <= 0 element wise.
# Pay attention to the sign of beta, which could flip the inequality
# The following function will depend on alpha_2 and beta_2


# The following code creates the tibble 'alphabeta'. Because the inequality flips, we create
# a factor variable 'category' to distinguish these cases. Remember, t >= 0. Then the following happens:
# For each row,
# 1. If category == lowerbound, go to 2. Else, go to 3.
# 2. If inequality_term  < 0, stock is always ss. Else (> 0), stock is ss above
# 3. If inequality_term  < 0, stock is never ss. Else (> 0), stock is ss below

alphabeta <- as.tibble(cbind(alpha_2,beta_2)) %>% 
  mutate(
    category = as.factor(ifelse(beta_2 < 0, "lowerbound", "upperbound")),
    inequality_term = -1*(alpha_2/beta_2),
    type = as.factor(ifelse(category == "lowerbound",
                  yes = ifelse(inequality_term < 0,"ALL","ss_above"),
                  no = ifelse(inequality_term < 0,"NONE","ss_below")
          )
          )
  )
row.names(alphabeta) <- names(dataq)
alphabeta <- rownames_to_column(alphabeta, var = 'stock')

### Question 4 ###

# a

# Copying the form from question 1 to get the variances.
# Then:
# 1. we give the stocks indexes and names
# 2. we arrange them so that the most risky is in position 1
# 3. create the fund category based on position in this arrangement

variances_returns <- as.tibble(sapply(returnsdata, var)) %>% 
  mutate(stock_index = 1:n(),
         stock = names(dataq)[stock_index]) %>% 
  arrange(desc(value)) %>% 
  mutate(fund = as.factor(if_else(value > value[10],'high',
                           if_else(value < value[19],'low','mid'))))

# Now we get the actual data for each fund

# Pull function from : https://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector
# This pulls a column from a tibble as a vector, used for getting names of stocks below

pull_func <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}

# Get data by doing row-wise sums of the funds
# The results are quite different to the matlab version, where we used row-wise MEANS instead of sums
# Edit: Sums appear to actually be the wrong method (stock amount is roughly continuous). I have left one of the
# sections commented because the row-wise sum operation code is pretty nice (but slow).

#highfund <- returnsdata %>% # this is quite inefficient
#  select(variances_returns %>% filter(fund == 'high') %>% pull_func(stock)) %>%
#  rowwise() %>%
#  do( (.) %>% as.data.frame %>% mutate(sum = sum(.)) ) %>% 
#  ungroup()
# After doing this for mid and low fund, run the following lines to make 'r'
# fundsdata <- as.tibble(cbind(highfund$sum,midfund$sum,lowfund$sum))
# r_funds <- as.vector(apply(fundsdata,2,mean))

# Getting fundsdata (3 columns) and r_funds using row-wise means
# (There must be a better way to get the relevant columns than the one used here)

highfund <- returnsdata %>%
  select(variances_returns %>% filter(fund == 'high') %>% pull_func(stock)) %>%
  mutate(fundvalue = rowMeans(.))

midfund <- returnsdata %>%
  select(variances_returns %>% filter(fund == 'mid') %>% pull_func(stock)) %>%
  mutate(fundvalue = rowMeans(.))

lowfund <- returnsdata %>%
  select(variances_returns %>% filter(fund == 'low') %>% pull_func(stock)) %>%
  mutate(fundvalue = rowMeans(.))

fundsdata <- as.tibble(cbind(highfund$fundvalue,midfund$fundvalue,lowfund$fundvalue))
S_funds <- as.matrix(cov(fundsdata)) 
r_funds <- as.vector(apply(fundsdata,2,mean))

# b, i

# As in the second question, we use the theorem to compute the relevant constants

e_funds <- as.vector(c(1,1,1))
e_funds_t <- t(e_funds)

r_funds_t <- t(r_funds)

a_4_funds <- as.vector(e_funds_t%*%inv(S_funds)%*%e_funds)
b_4_funds <- as.vector(r_funds_t%*%inv(S_funds)%*%e_funds)
c_4_funds <- as.vector(r_funds_t%*%inv(S_funds)%*%r_funds)
d_4_funds <- a_4_funds*c_4_funds-b_4_funds^2

alpha_4_funds <- as.vector((1/a_4_funds)*(inv(S_funds)%*%e_funds))
beta_4_funds <- as.vector((inv(S_funds)%*%r_funds) - (b_4_funds/a_4_funds)*(inv(S_funds)%*%e_funds))

dollar_invest_funds <- as.tibble(t(as.matrix(200000*(alpha_4_funds + 0.2*beta_4_funds))))

p_hat <- as.tibble(cbind((b_4_funds+d_4_funds*0.2)/a_4_funds,sqrt((1+d_4_funds*0.2^2)/a_4_funds)))
names(p_hat) <- c("mu","sigma")

# b, ii

# fund plotting data
funds_ms_data <- as.tibble(cbind(apply(fundsdata,2,mean),sqrt(apply(fundsdata,2,var))))
names(funds_ms_data) <- c("mu_funds","sigma_funds")

# MVF plotting data (still in bad matlab form)

mu_mvf <- function(t_mu) {
  (b_4_funds+d_4_funds*t_mu)/a_4_funds
}
sigma_mvf <- function(t_sigma) {
  sqrt(((1+d_4_funds*t_sigma^2)/a_4_funds))
}

dummy_4 <- seq(from = -0.35, to = 0.35, by = 0.005) # Very obviously a matlab approach, should find elegant solution
mvf_data_funds <- as.tibble(cbind(mu_mvf(dummy_4),sigma_mvf(dummy_4)))
names(mvf_data_funds) <- c("mu","sigma")


# Indifference curve
dummy_4_b <- seq(from = 0, to = 0.02, by = 0.0005)
z_star_funds <- round(-0.2*((b_4_funds+d_4_funds*0.2)/a_4_funds) + (1/2)*((1+d_4_funds*0.2^2)/a_4_funds), digits = 8)
indiff_func_funds <- function(x){
  (-1/0.2)*(z_star_funds - x^2/2)
}

indiff_data_funds <- as.tibble(cbind(dummy_4_b,indiff_func_funds(dummy_4_b)))
names(indiff_data_funds) <- c("input","curve")

# Now we plot, in order:
# MVF for funds, MVF for stocks, (mu,sigma) for funds, indiff curve for funds, optimal portfolio for 4,
# optimal portfolio for 2

ggplot()+
  geom_point(mvf_data_funds, mapping = aes(x=sigma,y=mu),colour = "firebrick",alpha = 0.3)+
  geom_point(mvf_data, mapping = aes(x=sigma,y=mu),colour = "firebrick1",alpha = 0.3)+
  geom_point(funds_ms_data, mapping = aes(x = sigma_funds, y = mu_funds), colour = "seagreen1", size = 5)+
  geom_point(indiff_data_funds, mapping = aes(x = input, y = curve))+
  geom_point(p_hat, mapping = aes(x = sigma, y = mu), colour = "blue", size = 5)+
  geom_point(p_star, mapping = aes(x = sigma, y = mu), colour = "blue", size = 5)+
  labs(title = "Question 4 Plot")

### Question 5 ###

rf <- 0.002906*0.01 #(pencentage x 1/100)

# Need to get S&P 500 column differenced returns
# pipe sp500 price column into applying returnsfunc along the column (column = 2),
# with returns function as in question 2

sp500_returns <- FinanceData %>% select('S&P 500') %>% 
  {apply(.,2,returnsfunc)}

# Mean and variance of sp500returns
sp500_returns_mu <- mean(sp500_returns)
sp500_returns_sigma2 <- var(sp500_returns)

# beta_stocks
# 100000000 times nicer than in matlab

beta_func <- function(input_column){
  cov <- cov(input_column,sp500_returns)
  value = cov/sp500_returns_sigma2
  return(value)
}

beta_stocks <- as.vector(t(apply(returnsdata, MARGIN = 2, FUN = beta_func)))
#beta_stocks_tibble <- as.tibble(t(apply(returnsdata, MARGIN = 2, FUN = beta_func)))

# beta_funds
# notice we can just reuse the function above, close to 10e30 times nicer than the matlab

beta_funds <- as.vector(apply(fundsdata, MARGIN = 2, FUN = beta_func))

# Plottable data with the stock and fund beta
plot5_stocks <- as.tibble(cbind(beta_stocks,stocks_ms_data$mu_stocks))
names(plot5_stocks) <- c("beta","mu")

plot5_funds <- as.tibble(cbind(beta_funds,as.vector(funds_ms_data$mu_funds)))
names(plot5_funds) <- c("beta","mu")

# Question 2 p_star beta (weighted sum of constituent elements)
x_2 <- alpha_2+0.2*beta_2
beta_p_star <- sum(x_2%*%beta_stocks)

# Question 4 p_hat beta
x_4_funds <- alpha_4_funds+0.2*beta_4_funds
beta_p_hat <- sum(x_4_funds*beta_funds)

# Plottable data for p_star and p_hat
plot5_star <- as.tibble(cbind(beta_p_star,p_star$mu))
names(plot5_star) <- c("beta","mu")
plot5_hat <- as.tibble(cbind(beta_p_hat,p_hat$mu))
names(plot5_hat) <- c("beta","mu")

# SML
SML <- function(x){
  rf+x*(sp500_returns_mu-rf)
}

# Plotabble risk-free asset
plot5_rf <- as.tibble(cbind(rf,0))
names(plot5_rf) <- c("rf","zero")

# Plottable market (sp500)
# Generating beta for sp500 in second argument of cbind instead of on a seperate line
plot5_sp500 <- as.tibble(cbind(sp500_returns_mu,apply(sp500_returns, MARGIN = 2, FUN = beta_func)))
names(plot5_sp500) <- c("mu","beta")

# Question 5 plot calls for: on mu-beta plane:
# SML, risk free asset, market portfolio, betas for the stocks, funds, and optimal portfolios

ggplot(data = data.frame(x = 0), mapping = aes(x = x))+xlab('betaa')+ylab('mu')+
  coord_cartesian(ylim = c(0, 4.6e-3))+
  stat_function(fun = SML) + xlim(0,2)+ # At this point I found out how to plot functions
  geom_point(plot5_rf, mapping = aes(x = zero, y = rf), colour = 'blue', size = 5)+
  geom_point(plot5_sp500, mapping = aes(x = beta, y = mu), colour = 'red', size = 5)+
  geom_point(plot5_stocks, mapping = aes(x = beta, y = mu), colour = 'orange', alpha = 0.5)+
  geom_point(plot5_funds, mapping = aes(x = beta, y = mu), colour = 'magenta',size = 5)+
  geom_point(plot5_star, mapping = aes(x = beta, y = mu), colour = 'red')+
  geom_point(plot5_hat, mapping = aes(x = beta, y = mu), colour = 'red')

# Now we have to find which funds are above the line, and which are below

compare_func <- function(x,y){
if(x > y){value <- "above"}else{value <- "below"}
  return(value)
}

# Option 1 - doesn't give names by default ####
check_sml_data <- as.tibble(cbind(stocks_ms_data$mu_stocks,SML(beta_stocks)))
names(check_sml_data) <- c("mu","sml")
check_sml <- mapply(compare_func, check_sml_data$mu, check_sml_data$sml)

# Option 2 - gives names but not very generalisable ####
check_sml_data <- as.tibble(cbind(stocks_ms_data$mu_stocks,SML(beta_stocks)))
names(check_sml_data) <- c('mu','sml')
check_sml_2 <- as.tibble(t(apply(check_sml_data, MARGIN = 1, FUN = compare_func(), y = check_sml_data$sml)))
names(this_is_where) <- names(returnsdata)

# Option 3 - using multi variable apply ####
check_sml_3 <- as.tibble(t(apply(X = check_sml_data, MARGIN = 1, FUN = function(z) compare_func(z['mu'],z['sml']))))
names(check_sml_3) <- names(returnsdata)

# Option 4 - using mutate and checking ####
check_sml_4 <- check_sml_data %>% 
  mutate(
    test = mu-sml,
    invest = as.factor(if_else(test > 0, "above", "below"))
    )
summary(check_sml_4) # 23 above, 6 below



























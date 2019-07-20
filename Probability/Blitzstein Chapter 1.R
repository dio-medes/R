# Chapter 1
v <- c(3,1,4,1,5,9)
length(v)

#Sampling
n <- 10; k <- 5
sample(n,k,replace=TRUE) #Sample with replacement from integers 1:n, size k
sample(n,n) #permutation
sample(letters,7) #letters is default alphabet vector. Gives random 7 letter 'word'
sample(4,3,replace=TRUE,prob = c(0.1,0.2,0.3,0.4)) 
#If sample without replacement, the probability of a 
#yet-to-be-picked element is proportional to it's given probability

#de Montmort
n <- 100 #100 cards
r <- replicate(10^4,sum(sample(n,n)==(1:n))) # sample(n,n)==(1:n) is a n-dim vector eg. (0,1,0,...,0). == is TRUE if ith element matches. TRUE is encoded as 1, FALSE as 0. Hence we get the vector (0,0,1,...,0) if there is a match on the third card. 
#sum adds up the elements of this n-vector. Then replicate does this 10^4 times.
# r is a vector of length 10^4 containing the number of matched cards from each experiment.
sum(r>=1)/10^4 # no. of successful experiments divided by the number of simulations.

#Birthday problem
#To calculate the probability of at least one match in a group of 23 people:
k <- 23
1-(prod((365-k+1):365)/365^k) # Counting the complement w/ naive def. approx = 0.5

#Now we /simulate/ the birthday problem.
s <- 10000
g <- 23 #prob of at least one match is 0.5
r <- replicate(s,max(tabulate(sample(365,g,replace=TRUE))))
sum(r>=2)/s

### Question 28: What is the probability that a n-letter word is a palindrome? ###

# This is straight forward to caluclate, though one has to consider
# even and odd cases.

# Number of simulations
simnum <- 10^3

# Number of letters
num_let <- 2

one_simulation <- function(){
  word <- replicate(num_let,sample(letters,1))
  if (num_let %% 2 == 0){                      # Even case
    v <- (word[1:(num_let/2)]==word[num_let:((num_let/2)+1)])}
  else {                                     # Odd case
    v <- word[1:floor(num_let/2)]==word[num_let:ceiling(num_let/2+1)]
  }
  check <- sum(v)                              # Checking for palindrome
  if (num_let %% 2 == 0){                      # Even case
    if (check==num_let/2){return(check)}
    else{check <- 0}
  } else{                                      # Odd case
    if (check==floor(num_let/2)){return(check)}
    else{check <- 0}
  }
  check 
}  
success = replicate(simnum,one_simulation())
Sim_prob <- sprintf("%.10f",sum(success)/simnum) # Gives floating point to 10 decimal places
true_prob <- 26^(ceiling(num_let/2))/26^(num_let)
cat("The simulation resulted in ", Sim_prob," but the exact probability is ", true_prob,".",sep = '')

### Question 33: 13-card hand, what is the probability of at least 3 of each suit. ###

# Insight: having at least 3 of each suit is the same as having 4 of one suit and 3 of the others.

# Number of simulations:
sim_num <- 10^4

# Number of cards in hand
hand_size <- 13

# One simulation
one_simulation <- function(){
  hand <- sample(1:52,13,replace = FALSE)
  if(sum(sum(hand >= 1 & hand < 14)==union(3,4))==1){                     # Yikes, this is ugly.
    if(sum(sum(hand > 13 & hand < 27)==union(3,4))==1){                   # I want to check if the sum of the logical vector
      if(sum(sum(hand > 26 & hand < 40)==union(3,4))==1){                 # is equal to 3 OR 4. Can't get the OR operator to work.
        if(sum(sum(hand > 39 & hand < 53)==union(3,4))==1){check <- 1}    # So, instead use union(), which returns a logical vector
    else{check <- 0}                                                      # of length 2. THEN sum this vector. If it's 3 or 4, second
  } else{check <- 0}                                                      # sum will be = 1. Then checks for this. There is 100% a 
  } else{check <- 0}                                                      # cleaner way to do this with some logical operator.
  } else{check <- 0}
check
}
result <- replicate(sim_num,one_simulation())
sim_prob <- sprintf("%.10f",sum(result)/sim_num) # Gives floating point to 10 decimal places
true_prob <- 0.1054
cat("The simulation resulted in ", sim_prob," but the exact probability is ", true_prob,".",sep = '')

### Question 32a: 5 card hand, simulating probability for flush ### 

# Calculation:
# true_prob <- 4*(choose(13,5))/choose(52,5)

# Simulation:

# Number of simulations
sim_num <- 10^3

one_sim <- function(){
hand <- sample(1:52,5,replace = FALSE)
A <- cbind(c(1, 14, 27, 40), c(13, 26, 39, 52)) # Suit dividers

flush <- function(i){ #Input element is called i inside the flush function.
logical_vector <- i >= A[,1] & i <= A[,2] # Outputs a logical vector
}
results <- rbind(sapply(hand, flush))     # Row binds the output vector into a matrix
check <- max(rowSums(results))==5
check
}
final_results <- replicate(sim_num,one_sim())
sim_prob <- sprintf("%.10f",sum(final_results)/sim_num) # Gives floating point to 10 decimal places
true_prob <- 0.001980792
cat(sim_num," simulations resulted in probability ", sim_prob," but the exact probability is ", true_prob,".",sep = '')








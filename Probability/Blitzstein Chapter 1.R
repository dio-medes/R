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

# Question 28.
# What is the probability that a n-letter word is a palindrome?
# This is straight forward to caluclate, though one has to consider
# even and odd cases. Now I'm looking to simulate & confirm these calculations.

# Number of simulations
simnum <- 10^4

# Number of letters
num_let <- 2

one_simulation <- function(){
  word <- replicate(num_let,sample(letters,1))
  if (num_let %% 2 == 0){                      # Even case
    v <- (word[1:(num_let/2)]==word[num_let:((num_let/2)+1)])}
    else {                                     # Odd case
      v <- word[1:floor(num_let/2)]==word[num_let:(floor(num_let/2)+1)]
    }
  check <- sum(v)
  check
}  
success = replicate(simnum,one_simulation())
prob <- sprintf("%.10f",sum(success)/simnum)
prob































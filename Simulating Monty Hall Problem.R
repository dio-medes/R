#Simulating Monty-hall
#16/7/19
cnt <-1
r <- c()
repeat {
cnt <- cnt+1
contestant <- sample(1:3,1) #Two random samples, one the contestant picks,
true_door <- sample(1:3,1)  #one has the prize. Blitzstein just always picks door 1 for simplicity.
forbidden_doors <- c(true_door,contestant)
allowed_doors <- c(setdiff(c(1,2,3), forbidden_doors))

samplefunction <- function(x){ # Thanks to user 'Pomber' from StackOverflow for this function
  if (length(x) <= 1){         # If m in sample(m,1) is a vector of length one, the sample function
    return(x)                  # will interpret this as 1:m. So, you have to check for this case.
  } else {
      return(sample(x,1))
  }
}

opened_door <- samplefunction(allowed_doors) #Door that Monty opens

#Always swap strategy
swapped_door <- c(setdiff(c(1,2,3),c(opened_door,contestant)))
swap_wins <- swapped_door == true_door #checks if swap worked
r <- c(r,swap_wins)                    #appends the result of each repeat onto the r vector
if (cnt > 200) {
   break
  }
}
winprob <- sum(r==TRUE)/cnt #naive definition of probability
winprob



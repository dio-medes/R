# Before trying to figure out how to simulate the n-door problem, going to try and extend
# code from the normal 3-door problem to a 4-door situation.
# 18/7/19
# rm(list=ls()) here for convenience.

# Contestant picks 1 door. Monty opens 2 doors. Find probability of swap win.
# Don't expect much except in terms of syntax to change.
# Going to try to clean up the result section.

choice <- sample(1:4,1)
true <- sample(1:4,1)
forbidden <- c(choice,true)
canopen <- c(setdiff(1:4,forbidden))

# Sample Function with correction again
#BetterSample <- function(x){
#  if (length(x) == 1){
#    return(x)}
#  else{ return(sample(x,2))
#  }
#}

opened <- sample(canopen,2,replace = FALSE)
swapdoor <- c(setdiff(1:4,c(choice,opened)))
result <- swapdoor == true

if (result == TRUE){
  print("Swap worked, ez clap")
  } else{
     print("you got boomed")}




  

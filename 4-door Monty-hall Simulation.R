# Before trying to figure out how to simulate the n-door problem, going to try and extend
# code from the normal 3-door problem to a 4-door situation.
# 18/7/19
# rm(list=ls()) here for convenience.

# Contestant picks 1 door. Monty opens 2 doors. Find probability of swap win.

# Number of Simulations:
n <- 100

#Repeating code the better way
do_once <- function()
{

choice <- sample(1:4,1)
true <- sample(1:4,1)
forbidden <- c(choice,true)
canopen <- c(setdiff(1:4,forbidden))

opened <- sample(canopen,2,replace = FALSE)
swapdoor <- c(setdiff(1:4,c(choice,opened)))
result <- swapdoor == true

}
success <- sum(replicate(n,do_once())) # TRUE encoded as 1, FALSE as 0.
prob<- success/n
prob



  

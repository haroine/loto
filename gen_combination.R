
## Generate played combinations

list_combinations <- data.frame(matrix(0,
                                       nrow=N_players, ncol=2))
names(list_combinations) <- c("combination","freq")
list_combinations$freq <- 0

first_free_index <- 1

for(i in 1:N_players) {
  random_combination <- sample((1:(max_numbers)), k_combination,
                               prob = probs_numbers)
  random_combination <- random_combination[order(random_combination)]
  random_combination_str <- paste(random_combination, collapse = "-")
  
  if(random_combination_str %in% list_combinations$combination) {
    index_current <- which(list_combinations$combination == random_combination_str)
    list_combinations[index_current,"freq"] <- 
      as.numeric(list_combinations[index_current,"freq"])+1
  } else {
    list_combinations[first_free_index,] <- c(random_combination_str,1)
    first_free_index <- first_free_index + 1
  }
  
}

list_combinations <- list_combinations[1:(first_free_index-1),]
list_combinations <- list_combinations[order(list_combinations$freq, 
                                             decreasing = T),]
row.names(list_combinations) <- NULL

played_combinations <- t(sapply(list_combinations$combination, function(x) {
  as.numeric(strsplit(x,"-")[[1]])
}
))

list_combinations <- cbind(list_combinations, played_combinations)

## Harmonic mean of the probabilities of drawing each number
list_combinations$prob_mean_h <- 0

prob_mean_h <- apply(list_combinations,1,function(x) {
  currentVecProb <- as.numeric(x[(1:(k_combination))+2])
  prod(probs_numbers[currentVecProb])**(1/(k_combination))
})

list_combinations$prob_mean_h <- prob_mean_h

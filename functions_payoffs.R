
## Compute payoffs

# Number of winners in each category

compute_n_winners <- function(list_combinations, payoffs, right_combination) {
 
  n_winners <- rep(0, length(payoffs))
  
  n_winners_table <- table(
    unlist(
      apply(list_combinations,1,function(x) {
    rep(sum(as.numeric(x[(1:(k_combination))+2]) %in% right_combination),
      as.numeric(x["freq"]))
  } )
    )
  )
  
  if(sum(n_winners_table) != N_players) {
    stop("Number of winners not OK!!")
  }
  
  for(k in 1:(length(payoffs))) {
    n_winners[k] <- n_winners_table[as.character(k-1)]
    if(is.na(n_winners[k])) {
      n_winners[k] <- 0
    }
  }
  
  return(n_winners)
}



compute_payoff_element <- function(x, payoffs, n_winners) {
  
  n_numbers_ok <- sum(x %in% right_combination)
  payoffs[n_numbers_ok+1] / n_winners[n_numbers_ok+1] - price_ticket
}

compute_payoff <- function(list_combinations, payoffs, right_combination) {
  n_winners <- compute_n_winners(list_combinations, payoffs, right_combination)
  
  apply(list_combinations[,(1:(k_combination))+2],1,compute_payoff_element,
        payoffs=payoffs, n_winners=n_winners)
}

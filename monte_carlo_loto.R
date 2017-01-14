N_simus <- 100

set.seed(10032)
gains_df <- foreach(n=1:N_simus, .combine=cbind) %do% {

  right_combination <- sample((1:(max_numbers)), k_combination)
  right_combination <- right_combination[order(right_combination)]
  right_combination_str <- paste(right_combination, collapse = "-")

  gains <- compute_payoff(list_combinations,
            payoffs = payoffs, right_combination = right_combination)

}

mean_gains <- rowMeans(gains_df)
mean_gains_trimmed_idx <- which(mean_gains <= 1)
mean_gains_trimmed <- mean_gains[mean_gains_trimmed_idx]

summary(mean_gains_trimmed)
plot(list_combinations[mean_gains_trimmed_idx,]$freq, mean_gains_trimmed)
plot(list_combinations[mean_gains_trimmed_idx,]$prob_mean_h, mean_gains_trimmed)


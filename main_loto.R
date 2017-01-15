library(foreach)
library(ggplot2)
source("functions_payoffs.R")

## Parameters

max_numbers <- 19
probs_numbers <- rep(1/max_numbers, max_numbers)

## Favored numbers (scenario 1)
favored_numbers <- 1:5
least_favored_numbers <- 14:19
probs_numbers[favored_numbers] <- probs_numbers[favored_numbers] * 2
probs_numbers[least_favored_numbers] <- probs_numbers[least_favored_numbers] / 2

## Favored numbers (scenario 2)
# probs_numbers[1] <- probs_numbers[1] * 2
# probs_numbers[19] <- probs_numbers[19] * 0.5
# probs_numbers[2:6] <- probs_numbers[2:6] * 1.4
# probs_numbers[14:18] <- probs_numbers[14:18] * 0.7

probs_numbers <- probs_numbers / sum(probs_numbers)

k_combination <- 6

N_players <- 1e4

## Vector of payoffs: cash gained for having 0 ... k
## numbers right (has to be positive)
price_ticket <- 2
total_prize <- 0.9 * price_ticket * N_players ## The the lotto company thus ensures a profit
payoffs <- rep(0, (k_combination+1))
payoffs <- total_prize*c(0,0,0.42,0.1,0.03,0.05,0.4) ## Loosely based on Euromillions

## Generate combinations
set.seed(10031)
source("gen_combination.R")

right_combination <- rep(0,k_combination)
gains <- rep(0,nrow(list_combinations))

## TODO: something better than nested for loops?
for(i1 in 1:(max_numbers-5)) {
  # print(right_combination)
  right_combination[1] <- i1
  for(i2 in (i1+1):(max_numbers-4)) {
    right_combination[2] <- i2
    for(i3 in (i2+1):(max_numbers-3)) {
      # print(right_combination)
      right_combination[3] <- i3
      for(i4 in (i3+1):(max_numbers-2)) {
        right_combination[4] <- i4
        for(i5 in (i4+1):(max_numbers-1)) {
          right_combination[5] <- i5
          for(i6 in (i5+1):max_numbers) {
            right_combination[6] <- i6
            print(right_combination)

            gains <- gains + compute_payoff(list_combinations, 
                      payoffs = payoffs, right_combination = right_combination)
            
          }
        }
      }
    }
  }
}

list_combinations$gains <- gains / choose(max_numbers,k_combination)

expected_return <- sum(list_combinations$gains*as.numeric(list_combinations$freq)) / N_players

plot_gains <- ggplot(list_combinations, aes(x=prob_mean_h)) +
  geom_point(aes(y=gains)) +
  geom_hline(yintercept = 0, color="red") +
  geom_hline(yintercept = expected_return, linetype="dashed",
             color="blue") +
  xlab("Frequency of combination") +
  ylab("Expected return") +
  annotate("text", c(0.03,0.035), c(0.05,expected_return+0.05), 
           label =c("","Expected return"), color="blue")

print(plot_gains)

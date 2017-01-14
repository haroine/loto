library(foreach)
source("functions_payoffs.R")

## Parameters

max_numbers <- 19
probs_numbers <- rep(1/max_numbers, max_numbers)
## Favored numbers
favored_numbers <- 1:5
# least_favored_numbers <- 41:49
least_favored_numbers <- 15:19
probs_numbers[favored_numbers] <- probs_numbers[favored_numbers] * 2
probs_numbers[least_favored_numbers] <- probs_numbers[least_favored_numbers] / 2
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

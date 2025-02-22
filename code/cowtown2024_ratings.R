# A script to compute team ratings from Cow Town 2024

library("tidyverse")
theme_set(theme_bw())

source("code/find_teams.R")
source("code/construct_model_matrix.R")
source("code/compute_team_rating.R")
source("code/compute_probabilities.R")


# Compute team ratings based on qualification rounds
quals <- read.csv("data/CowTown2024.csv")
Xq <- construct_model_matrix(quals)

ratings <- compute_team_rating(quals)
ratings[order(-ratings$rating), ]

sd <- 19.418134997194

# Compute probabilities
playoffs <- read.csv("data/CowTown2024Playoffs.csv")

Xp <- construct_model_matrix(playoffs)

cp <- compute_probabilities(playoffs, ratings, sd)



# Compare 
ggplot(cp, 
       aes(
         x = expected_difference,
         y = probability_red_win
       )) +
  geom_point() +
  labs(
    x = 'Expected Point Difference (red - blue)',
    y = 'Probability (red win)'
  )


ggplot(cp, 
       aes(
         x = expected_difference,
         y = RedScore - BlueScore
       )) +
  geom_point() +
  labs(
    x = 'Expected Point Difference (red - blue)',
    y = 'Actual Point Difference (red - blue)'
  )

cp <- cp |>
  mutate(
    ActualWin  = RedScore > BlueScore,
    PredictWin = expected_difference > 0
  ) 

table(cp$ActualWin, cp$PredictWin)

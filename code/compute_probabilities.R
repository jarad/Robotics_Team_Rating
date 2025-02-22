compute_probabilities <- function(d, r, sd) {
  X <- construct_model_matrix(d)
  
  # Limit ratings to teams in d
  rating <- r$rating[r$team %in% colnames(X)]
  
  # print(cat(dim(X), "\n", length(rating), "\n"))
  expected_difference <- X %*% rating
  
  probability_red_win <- pnorm(0, mean = -expected_difference, sd = sd)
  
  d$expected_difference <- expected_difference
  d$probability_red_win <- probability_red_win
  
  return(d)
}

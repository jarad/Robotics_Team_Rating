#' Compute team rating data
#' 
#' Compute team rating data from scores
#' 
#' @param d FRC qualification match data.frame columns are 
#'   MatchID
#'   Red1
#'   Red2
#'   Red3
#'   Blue1
#'   Blue2
#'   Blue3
#'   RedScore
#'   BlueScore
#' 
#' 
compute_team_rating <- function(d) {
  X <- construct_model_matrix(d)
  
  m <- lm(RedScore - BlueScore ~ X, data = d)
  print(paste("Expected score SD=", summary(m)$sigma))
  
  r <- coef(m)[-1] # remove intercept
  r[length(r)] <- 0 
  names(r) <- gsub("X", "", names(r), fixed = TRUE)

  d <- data.frame(team   = names(r), 
                  rating = r)
  rownames(d) <- NULL
  
  return(d)
}


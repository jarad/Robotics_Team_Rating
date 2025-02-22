#' Constructs the model matrix
#' 
#' One column for each team with 
construct_model_matrix <- function(d) {

  teams <- find_teams(d)
  
  columns <- list()
  for (i in 1:length(teams)) {
    columns[[i]] <- 
      (d$Red1  == teams[i]) + 
      (d$Red2  == teams[i]) + 
      (d$Red3  == teams[i]) -
      (d$Blue1 == teams[i]) - 
      (d$Blue2 == teams[i]) - 
      (d$Blue3 == teams[i])
  }
  
  d <- dplyr::bind_cols(columns)
  colnames(d) <- teams
  
  return(as.matrix(d))
}

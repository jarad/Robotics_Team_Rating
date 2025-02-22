find_teams <- function(d) {
  unique(c(d$Red1, d$Red2, d$Red3,
           d$Blue1, d$Blue2, d$Blue3)) |>
    sort()
}
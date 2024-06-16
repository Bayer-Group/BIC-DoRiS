#' helper function to get truth value per dose
#'
#' @param x numeric vector
#' @param delta vector with fuzzy logic threshold values

gettvpdose <- function(x, delta) {
  c("<" = max(min(x/(-delta),1),0),
    ">" = max(min(x/delta,1),0),
    "=" = 1 - (max(min(x/delta,1),0) + max(min(x/(-delta),1),0))
  )
}

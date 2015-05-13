# Utility functions -------------------------------------------------------

coalesce <- function(...){
  Reduce(function(x,y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}



safe_ifelse <- function(cond, yes, no){  
  if(cond){
    yes 
  } else no
}

#' @export
round_any_vec <- Vectorize(round_any, c("x"))
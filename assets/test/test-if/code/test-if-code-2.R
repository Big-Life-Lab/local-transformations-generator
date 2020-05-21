test <- function(a, d) {
  if (a == 0){
    b <- 0
    c <- 0
  } else if(d == 1) {
    e <- 1
  } else if(a == 3) {
    b <- 3
    c <- 3
  } else {
    b <- 4
    c <- 4
  }
  
  return(b + c + e)
}
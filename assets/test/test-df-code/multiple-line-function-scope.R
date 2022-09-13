a <- function(b, table) {
  c <- table[table$b == b, ]$c
  return(c)
}

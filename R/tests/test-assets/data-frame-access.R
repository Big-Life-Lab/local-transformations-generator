testOne <- Table[Table$col1 == 'Name', ]$out

testTwo <- Table[Table$col2 == testOne, ]$out

testThree <- Table['Age_cont', 'Mean_male']

row <- Table[Table$col2 == 'test', ]

col <- row$col1

test <- function(row_one, const, row_two) {
  a <- row_one$col1
  
  c <- row_two$col2
  
  return(a + b)
}
col2 <- test(row, 1, row)
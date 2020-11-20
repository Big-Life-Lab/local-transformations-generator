test <- function(row, const, row_two) {
  return(row$col3 + const + row_two$col4)
}

row <- table[table$col1 == a, ]
row_two <- table[table$col2 == 'val1', ]
row_three <- table[table$col3 == b, ]

col <- test(row, 1, row_two) + test(row, 1, row_three)

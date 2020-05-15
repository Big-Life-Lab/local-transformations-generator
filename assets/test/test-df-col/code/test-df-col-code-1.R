# Test for when the value being compared to is a constant
a <- table[table$col1 == "val", ]$col2

# Test for when the value being compared to is in a variable
b <- table[table$col1 == val, ]$col2
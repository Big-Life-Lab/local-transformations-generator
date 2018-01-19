testOne <- Table[Table$col1 == 'Name', ]$out
testTwo <- Table[Table$col2 == testOne, ]$out

testThree <- Table['Age_cont', 'Mean_male']
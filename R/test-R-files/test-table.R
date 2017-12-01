TableData <- read.csv(file.path(getwd(), 'R/test-R-files/table.csv'))

test <- TableData[TableData$col1==a & TableData$col2=='b', ]$out + 1
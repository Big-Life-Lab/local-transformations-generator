source(file.path(getwd(), 'R/tests/test-table.R'))
source(file.path(getwd(), 'R/tests/data-frame-access.R'))
source(file.path(getwd(), 'R/tests/mutation.R'))

testTransformationForTable()
testDataFrameAccess()
testMutation()

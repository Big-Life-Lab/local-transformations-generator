source(file.path(getwd(), 'R/tests/test-table.R'))
source(file.path(getwd(), 'R/tests/data-frame-access.R'))
source(file.path(getwd(), 'R/tests/mutation.R'))
source(file.path(getwd(), 'R/tests/constants.R'))

testTransformationForTable()
testDataFrameAccess()
testMutation()
testConstants()
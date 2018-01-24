source(file.path(getwd(), 'R/local-transformations-generator.R'))

testConstants <- function() {
  expectedPmml <- ' <LocalTransformations> <DerivedField name="stringConstant" optype="continuous"><Constant dataType="string">testString</Constant></DerivedField> </LocalTransformations>'
  
  actualPmml <- getPmmlStringFromRFile(file.path('R/tests/test-assets/constants.R'), TRUE)
  
  if(expectedPmml != actualPmml) {
    print('Actual Value')
    print(actualPmml)
    print('Expected Value')
    print(expectedPmml)
    stop('Incorrectly generated Pmml string')
  } else {
    print('constants.R: Correctly generated Pmml string')
  }
}
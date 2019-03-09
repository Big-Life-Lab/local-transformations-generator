source(file.path(getwd(), 'R/local-transformations-generator.R'))

testTransformationForNull <- function() {
  expectedPmml <- '<PMML>  <LocalTransformations> <DerivedField name=\"juice_cont\" optype=\"continuous\"><Apply function=\"ifelse\"><Apply function=\"is.na\"><FieldRef field=\"fv_juice1\"/></Apply><Constant dataType=\"NULL\">NULL</Constant><Apply function=\"/\"><FieldRef field=\"fv_juice1\"/><Constant dataType=\"double\">7</Constant></Apply></Apply></DerivedField> </LocalTransformations></PMML>'
  
  actualPmml <- getPmmlStringFromRFile(file.path(getwd(), 'R/tests/test-assets/null.R'), TRUE)
  
  if(actualPmml != expectedPmml) {
    print(glue::glue('Actual Value'))
    print(actualPmml)
    print(glue::glue('Expected Value'))
    print(expectedPmml)
    stop('testTransformationForNull test failed')
  } else {
    print('test-null.R: testTransformationForNull test success')
  }
}
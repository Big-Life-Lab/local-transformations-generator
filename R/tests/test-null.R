source(file.path(getwd(), 'R/local-transformations-generator.R'))

testTransformationForNull <- function() {
  expectedPmml <- ' <LocalTransformations> <DerivedField name=\"test\" optype=\"continuous\"><Constant dataType=\"NULL\">NULL</Constant></DerivedField> </LocalTransformations>'
  
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
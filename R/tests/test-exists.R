source(file.path(getwd(), 'R/local-transformations-generator.R'))

testTransformationForExists <- function() {
  expectedPmml <- '<PMML>  <LocalTransformations> <DerivedField name="test" optype="continuous"><Apply function="exists"><FieldRef field="testOne"/></Apply></DerivedField> </LocalTransformations></PMML>'
  
  actualPmml <- getPmmlStringFromRFile(file.path(getwd(), 'R/tests/test-assets/exists.R'), TRUE)
  
  if(actualPmml != expectedPmml) {
    print(glue::glue('Actual Value'))
    print(actualPmml)
    print(glue::glue('Expected Value'))
    print(expectedPmml)
    stop('testTransformationForExists test failed')
  } else {
    print('test-exists.R: testTransformationForExists test success')
  }
}
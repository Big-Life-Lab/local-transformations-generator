source(file.path(getwd(), 'R/local-transformations-generator.R'))

testColonOperator <- function() {
  expectedPmml <- '<PMML>  <LocalTransformations> <DerivedField name=\"test\" optype=\"continuous\"><Apply function=\"isIn\"><FieldRef field=\"a\"/><Apply function=\"colonOperator\"><Constant dataType=\"double\">6</Constant><Constant dataType=\"double\">9</Constant></Apply></Apply></DerivedField> </LocalTransformations></PMML>'
  
  actualPmml <- getPmmlStringFromRFile(file.path(getwd(), 'R/tests/test-assets/colon-operator.R'), TRUE)
  
  if(actualPmml != expectedPmml) {
    print(glue::glue('Actual Value'))
    print(actualPmml)
    print(glue::glue('Expected Value'))
    print(expectedPmml)
    stop('testTransformationForColonOperator test failed')
  } else {
    print('test-colon-operator.R: testTransformationForColonOperator test success')
  }
}
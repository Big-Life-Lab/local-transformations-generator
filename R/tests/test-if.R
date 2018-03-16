source(file.path(getwd(), 'R/local-transformations-generator.R'))

testIf <- function() {
  expectedPmml <- ' <LocalTransformations> <DerivedField name=\"test\"><Apply function=\"if\"><Apply function=\"equal\"><FieldRef field=\"cond\"/><Constant dataType=\"boolean\">true</Constant></Apply><Constant dataType=\"double\">1</Constant><Constant dataType=\"double\">3</Constant></Apply></DerivedField><DerivedField name=\"testOne\"><Apply function=\"if\"><Apply function=\"equal\"><FieldRef field=\"cond\"/><Constant dataType=\"boolean\">false</Constant></Apply><Constant dataType=\"double\">2</Constant><FieldRef field=\"testOne\"/></Apply></DerivedField> </LocalTransformations>'
  
  actualPmml <- getPmmlStringFromRFile(file.path(getwd(), 'R/tests/test-assets/if.R'), TRUE)
  
  if(actualPmml != expectedPmml) {
    print(glue::glue('Actual Value'))
    print(actualPmml)
    print(glue::glue('Expected Value'))
    print(expectedPmml)
    stop('testTransformationForIf test failed')
  } else {
    print('test-if.R: testTransformationForIf test success')
  }
}
source(file.path(getwd(), 'R/local-transformations-generator.R'))

testIf <- function() {
  expectedPmml <- '<PMML>  <LocalTransformations> <DerivedField name="Diabetes_cat"><Apply function="if"><Apply function="equal"><FieldRef field="cond_SQ010"/><Constant dataType="string">Yes</Constant></Apply><Constant dataType="double">0</Constant><Constant dataType="double">1</Constant></Apply></DerivedField><DerivedField name="HeartDis_cat"><Apply function="if"><Apply function="equal"><FieldRef field="cond_SQ010"/><Constant dataType="string">Yes</Constant></Apply><Constant dataType="double">0</Constant><Constant dataType="double">1</Constant></Apply></DerivedField><DerivedField name="testOne"><Apply function="if"><Apply function="equal"><FieldRef field="cond_SQ010"/><Constant dataType="string">No</Constant></Apply><Constant dataType="double">1</Constant><FieldRef field="testOne"/></Apply></DerivedField> </LocalTransformations></PMML>'
  
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
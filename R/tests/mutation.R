source(file.path(getwd(), 'R/local-transformations-generator.R'))

testMutation <- function() {
  expectedValue <- ' <LocalTransformations> <DerivedField name="testOne" optype="continuous"><Constant dataType="double">1</Constant></DerivedField><DerivedField name="testOne_Mutated_1" optype="continuous"><FieldRef field="testOne"/></DerivedField><DerivedField name="testTwo" optype="continuous"><FieldRef field="testOne_Mutated_1"/></DerivedField> </LocalTransformations>'
  actualValue <- getPmmlStringFromRFile(file.path(getwd(), 'R/tests/test-assets/mutation.R'), TRUE)
  
  if(expectedValue != actualValue) {
    print('Actual Value:')
    print(actualValue)
    print('Expected Value:')
    print(expectedValue)
    stop('Incorrect PMML string generated')
  } else {
    print('testMutation: PMML string correctly generated')
  }
}
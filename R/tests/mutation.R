source(file.path(getwd(), 'R/local-transformations-generator.R'))

testMutation <- function() {
  expectedValue <- '<PMML>  <LocalTransformations> <DerivedField name=\"ALWDWKY_Mutated_1\" optype=\"continuous\"><FieldRef field=\"ALWDWKY\"/></DerivedField><DerivedField name=\"ALWDWKY_Mutated_2\" optype=\"continuous\"><Apply function=\"+\"><FieldRef field=\"ALWDWKY_Mutated_1\"/><Constant dataType=\"double\">1</Constant></Apply></DerivedField><DerivedField name=\"SMKDSTY\" optype=\"continuous\"><FieldRef field=\"ALWDWKY_Mutated_2\"/></DerivedField> </LocalTransformations></PMML>'
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
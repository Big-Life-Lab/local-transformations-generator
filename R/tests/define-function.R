source(file.path(getwd(), 'R/local-transformations-generator.R'))

testDefineFunction <- function() {
  parameterFields <-  '<ParameterField name="testOne" dataType="double"/>'
  firstDefineFunction <- glue::glue('<DefineFunction name="testFunction(testTwo)"> {parameterFields}<Apply function="+"><FieldRef field="testOne"/><Constant dataType="double">1</Constant></Apply></DefineFunction>')
  secondDefineFunction <- glue::glue('<DefineFunction name="testFunction"> {parameterFields}<Apply function="testFunction(testTwo)"><FieldRef field="testOne"/></Apply></DefineFunction>')
  expectedPmml <- paste('<PMML>  <LocalTransformations> ', firstDefineFunction, secondDefineFunction, ' </LocalTransformations></PMML>', sep='')
  
  actualPmml <- getPmmlStringFromRFile(file.path(getwd(), 'R/tests/test-assets/function.R'), srcFile=TRUE)
  
  if(expectedPmml != actualPmml) {
    print('Actual Value')
    print(actualPmml)
    print('Expected Value')
    print(expectedPmml)
    stop('Incorrectly generated PMML string')
  } else {
    print('test-function.R: PMML string correctly generated')
  }
}

source(file.path(getwd(), 'R/local-transformations-generator.R'))

getDefineFunctionForDefault <- function(argName, defaultValue, defaultValueType, defaultValuePmml = NA) {
  if(is.na(defaultValuePmml)) {
    return(glue::glue("<Apply function=\"if\"><Apply function=\"equal\"><FieldRef field=\"{argName}\"/><Constant dataType=\"NA\">NA</Constant></Apply><Constant dataType=\"{defaultValueType}\">{defaultValue}</Constant><FieldRef field=\"{argName}\"/></Apply>"))  
  } else {
    return(glue::glue("<Apply function=\"if\"><Apply function=\"equal\"><FieldRef field=\"{argName}\"/><Constant dataType=\"NA\">NA</Constant></Apply>{defaultValuePmml}<FieldRef field=\"{argName}\"/></Apply>"))  
  }
}

testDefaultFunctionArg <- function() {
  parameterFields <- "<ParameterField name=\"argOne\" dataType=\"double\"/> <ParameterField name=\"argTwo\" dataType=\"double\"/>"
  defaultValueArgOne <- "<Apply function=\"+\"><Constant dataType=\"double\">1</Constant><Constant dataType=\"double\">1</Constant></Apply>"
  defineFunctionDefaultArgOne <- glue::glue("<DefineFunction name=\"default(argOne)\"> {parameterFields}{getDefineFunctionForDefault('argOne', NA, NA, defaultValueArgOne)}</DefineFunction>")
  defineFunctionDefaultArgTwo <- glue::glue("<DefineFunction name=\"default(argTwo)\"> {parameterFields}{getDefineFunctionForDefault('argTwo', 'a', 'string')}</DefineFunction>")
  defineFunctionTestFunc <- glue::glue("<DefineFunction name=\"testFunc\"> {parameterFields}<Apply function=\"+\"><Apply function=\"default(argOne)\"><FieldRef field=\"argOne\"/><FieldRef field=\"argTwo\"/></Apply><Apply function=\"default(argTwo)\"><FieldRef field=\"argOne\"/><FieldRef field=\"argTwo\"/></Apply></Apply></DefineFunction>")
  
  expectedPmml <- paste(' <LocalTransformations> ', glue::glue('{defineFunctionDefaultArgOne}{defineFunctionDefaultArgTwo}{defineFunctionTestFunc}'), ' </LocalTransformations>', sep='')
  actualPmml <- getPmmlStringFromRFile(file.path(getwd(), 'R/tests/test-assets/default-function-arg.R'), TRUE)
  
  if(expectedPmml != actualPmml) {
    print("Expected Pmml")
    print(expectedPmml)
    print("Actual Pmml")
    print(actualPmml)
    
    stop("default-function-arg-test.R: Test failed")
  } else {
    print("default-function-arg-test.R: Test passed")
  }
}
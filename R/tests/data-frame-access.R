source(file.path(getwd(), 'R/local-transformations-generator.R'))

testDataFrameAccess <- function() {
  expectedPmmlForFirstLine <- "<DerivedField name=\"testOne\" optype=\"continuous\"><MapValues outputColumn=\"out\"> <FieldColumnPair column=\"col1\" constant=\"Name\"/><TableLocator location=\"taxonomy\" name=\"Table\"/></MapValues></DerivedField>"
  expectedPmmlForSecondLine <- "<DerivedField name=\"testTwo\" optype=\"continuous\"><MapValues outputColumn=\"out\"> <FieldColumnPair column=\"col2\" field=\"testOne\"/><TableLocator location=\"taxonomy\" name=\"Table\"/></MapValues></DerivedField>"
  expectedPmmlForThirdLine <- '<DerivedField name="testThree" optype="continuous"><MapValues outputColumn="Mean_male"><FieldColumnPair column="index" constant="Age_cont"/><TableLocator location=\"taxonomy\" name=\"Table\"/></MapValues></DerivedField>'
  expectedPmmlForCol <- '<DerivedField name="col" optype="continuous"><MapValues outputColumn="col1"> <FieldColumnPair column="col2" constant="test"/><TableLocator location=\"taxonomy\" name=\"Table\"/></MapValues></DerivedField>'
  expectedPmmlForFunctionCol <- ' <DefineFunction name=\"test_col2(a)\"> <ParameterField name=\"const\" dataType=\"double\"/><MapValues outputColumn=\"col1\"> <FieldColumnPair column=\"col2\" constant=\"test\"/><TableLocator location=\"taxonomy\" name=\"Table\"/></MapValues></DefineFunction><DefineFunction name=\"test_col2(c)\"> <ParameterField name=\"const\" dataType=\"double\"/><MapValues outputColumn=\"col2\"> <FieldColumnPair column=\"col2\" constant=\"test\"/><TableLocator location=\"taxonomy\" name=\"Table\"/></MapValues></DefineFunction><DefineFunction name=\"test_col2\"> <ParameterField name=\"const\" dataType=\"double\"/><Apply function=\"+\"><Apply function=\"test_col2(a)\"><FieldRef field=\"const\"/></Apply><Apply function=\"test_col2(b)\"><FieldRef field=\"const\"/></Apply></Apply></DefineFunction> <DerivedField name=\"col2\" optype=\"continuous\"><Apply function=\"test_col2\"><Constant dataType=\"double\">1</Constant></Apply></DerivedField>'
  expectedPmml <- paste('<PMML>  <LocalTransformations> ', expectedPmmlForFirstLine, expectedPmmlForSecondLine, expectedPmmlForThirdLine, expectedPmmlForCol, expectedPmmlForFunctionCol, ' </LocalTransformations></PMML>', sep = '')
  
  actualPmml <- getPmmlStringFromRFile(file.path(getwd(), 'R/tests/test-assets/data-frame-access.R'), TRUE)
  
  if(actualPmml != expectedPmml) {
    print('Actual Value')
    print(actualPmml)
    print('Expected Value')
    print(expectedPmml)
    stop('Incorrectly generated pmml for data frame access')
  } else {
    print('data-frame-access.R: Pmml string correctly generated')
  }
}
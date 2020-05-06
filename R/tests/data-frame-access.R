source(file.path(getwd(), 'R/local-transformations-generator.R'))

testDataFrameAccess <- function() {
  expectedPmmlForFirstLine <- "<DerivedField name=\"testOne\" optype=\"continuous\"><MapValues outputColumn=\"out\"> <FieldColumnPair column=\"col1\" constant=\"Name\"/><TableLocator location=\"taxonomy\" name=\"Table\"/></MapValues></DerivedField>"
  expectedPmmlForSecondLine <- "<DerivedField name=\"testTwo\" optype=\"continuous\"><MapValues outputColumn=\"out\"> <FieldColumnPair column=\"col2\" field=\"testOne\"/><TableLocator location=\"taxonomy\" name=\"Table\"/></MapValues></DerivedField>"
  expectedPmmlForThirdLine <- '<DerivedField name="testThree" optype="continuous"><MapValues outputColumn="Mean_male"><FieldColumnPair column="index" constant="Age_cont"/><TableLocator location=\"taxonomy\" name=\"Table\"/></MapValues></DerivedField>'
  expectedPmmlForCol <- '<DerivedField name="col" optype="continuous"><MapValues outputColumn="col1"> <FieldColumnPair column="col2" constant="test"/><TableLocator location=\"taxonomy\" name=\"Table\"/></MapValues></DerivedField>'
  expectedPmml <- paste('<PMML>  <LocalTransformations> ', expectedPmmlForFirstLine, expectedPmmlForSecondLine, expectedPmmlForThirdLine, expectedPmmlForCol, ' </LocalTransformations></PMML>', sep = '')
  
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
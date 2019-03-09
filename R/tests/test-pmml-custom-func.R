source(file.path(getwd(), 'R/local-transformations-generator.R'))

testTransformationForPmmlCustomFunc <- function() {
  expectedPmml <- '<PMML>  <Taxonomy name=\"reference_table\"><InlineTable><row><index>1</index><variable>Juice_cont</variable><mean>10</mean><sd>15</sd></row><row><index>2</index><variable>Potatoes_cont</variable><mean>20</mean><sd>25</sd></row></InlineTable></Taxonomy> <LocalTransformations> <DerivedField name=\"zJuice\" optype=\"continuous\"><Apply function=\"zScore\"><Constant dataType=\"double\">10</Constant><Constant dataType=\"double\">15</Constant><FieldRef field=\"Juice_cont\"/></Apply></DerivedField> </LocalTransformations></PMML>'
  
  actualPmml <- getPmmlStringFromRFile(file.path(getwd(), 'R/tests/test-assets/pmml-custom-func.R'), TRUE)
  
  if(actualPmml != expectedPmml) {
    print(glue::glue('Actual Value'))
    print(actualPmml)
    print(glue::glue('Expected Value'))
    print(expectedPmml)
    stop('testTransformationForPmmlCustomFunc test failed')
  } else {
    print('test-pmml-custom-func.R: testTransformationForPmmlCustomFunc test success')
  }
}
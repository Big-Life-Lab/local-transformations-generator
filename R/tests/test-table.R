source(file.path(getwd(), 'R/local-transformations-generator.R'))

testTransformationForTable <- function() {
  expectedPmml <- '<PMML>  <Taxonomy name=\"table\"><InlineTable><row><index>1</index><col1>a</col1><col2>b</col2><col3>b</col3><out>t</out></row><row><index>2</index><col1>a</col1><col2>c</col2><col3>c</col3><out>u</out></row><row><index>3</index><col1>1</col1><col2>2</col2><col3>3</col3><out>v</out></row></InlineTable></Taxonomy> <Taxonomy name=\"table_Mutated_1\"><InlineTable><row><index>1</index><col1>a</col1><col2>b</col2><col3>b</col3><out>t</out></row><row><index>2</index><col1>a</col1><col2>c</col2><col3>c</col3><out>u</out></row><row><index>3</index><col1>1</col1><col2>2</col2><col3>3</col3><out>v</out></row></InlineTable></Taxonomy> <LocalTransformations>  </LocalTransformations></PMML>'
  
  actualPmml <- getPmmlStringFromRFile(file.path(getwd(), 'R/tests/test-assets/table.R'), TRUE)

  if(actualPmml != expectedPmml) {
    print(glue::glue('Actual Value'))
    print(actualPmml)
    print(glue::glue('Expected Value'))
    print(expectedPmml)
    stop('testTransformationForTable test failed')
  } else {
    print('test-table.R: testTransformationForTable test success')
  }
}
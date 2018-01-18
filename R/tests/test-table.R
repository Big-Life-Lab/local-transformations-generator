source(file.path(getwd(), 'R/local-transformations-generator.R'))

testTransformationForTable <- function() {
  expectedPmml <- ' <Taxonomy name="table"><InlineTable><row><col1>a</col1><col2>b</col2><col3>b</col3><out>t</out></row><row><col1>a</col1><col2>c</col2><col3>c</col3><out>u</out></row><row><col1>1</col1><col2>2</col2><col3>3</col3><out>v</out></row></InlineTable></Taxonomy> <LocalTransformations>  </LocalTransformations>'
  
  actualPmml <- getPmmlStringFromRFile(file.path(getwd(), 'R/tests/test-assets/table.R'), TRUE)
  
  if(actualPmml != expectedPmml) {
    print(glue::glue('Actual Value'))
    print(actualPmml)
    print(glue::glue('Expected Value'))
    print(expectedPmml)
    stop('testTransformationForTable test failed')
  } else {
    print('testTransformationForTable test success')
  }
}
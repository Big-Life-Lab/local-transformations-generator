context("Testing converting data frames")

test_that("Data frames from read.csv are correctly generated", {
  code <- '
source(file.path(getwd(), "table-code-2.R"))

table <- read.csv(file.path(getwd(), "table.csv"))
'

 table_code_2 <- '
table <- read.csv(file.path(getwd(), "table.csv"))
'

 table <- data.frame(
                     col1 = c('a', 'a', '1'),
                     col2 = c('b', 'c', '2'),
                     col3 = c('b', 'c', '3'),
                     out = c('t', 'u', 'v')
                     )

  expected_pmml <- '<PMML>
<Taxonomy name="table">
<InlineTable>
<row><index>1</index><col1>a</col1><col2>b</col2><col3>b</col3><out>t</out></row>
<row><index>2</index><col1>a</col1><col2>c</col2><col3>c</col3><out>u</out></row>
<row><index>3</index><col1>1</col1><col2>2</col2><col3>3</col3><out>v</out></row>
</InlineTable>
</Taxonomy>
<Taxonomy name="table_Mutated_1">
<InlineTable>
<row><index>1</index><col1>a</col1><col2>b</col2><col3>b</col3><out>t</out></row>
<row><index>2</index><col1>a</col1><col2>c</col2><col3>c</col3><out>u</out></row>
<row><index>3</index><col1>1</col1><col2>2</col2><col3>3</col3><out>v</out></row>
</InlineTable>
</Taxonomy>
<LocalTransformations/>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml,
    files = list(
                 'table-code-2' = list(
                                       type = 'R',
                                       contents = table_code_2
                                       ),
                 'table' = list(
                                type = 'CSV',
                                contents = table
                                )
                 )

  )
})

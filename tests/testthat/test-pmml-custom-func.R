context("Testing converting PMML Custom Functions")

test_that("z-score functions outside functions are correctly generated", {
  code <- '
    reference_table <- read.csv(file.path(getwd(), "reference-table.csv"))

    #Test One
    # @pmml_custom_func(z_score, Juice_cont, reference_table)
    #Test One
    zJuice <- zScore.fun(Juice_cont)
  '

  reference_table <- data.frame(
    variable = c("Juice_cont", "Potatoes_cont"),
    mean = c(10, 20),
    sd = c(15, 25)
  )

  expected_pmml <- '<PMML>
<Taxonomy name="reference_table">
<InlineTable>
<row><index>1</index><variable>Juice_cont</variable><mean>10</mean><sd>15</sd></row>
<row><index>2</index><variable>Potatoes_cont</variable><mean>20</mean><sd>25</sd></row>
</InlineTable>
</Taxonomy>
<LocalTransformations>
<DerivedField name="zJuice" optype="continuous">
<Apply function="zScore">
<Constant dataType="double">10</Constant>
<Constant dataType="double">15</Constant>
<FieldRef field="Juice_cont"/>
</Apply>
</DerivedField>
</LocalTransformations></PMML>'

  test_utils_run_generate_pmml_test(
    code, 
    expected_pmml,
    files = list('reference-table' = list(type = "CSV", contents = reference_table))
  )
})

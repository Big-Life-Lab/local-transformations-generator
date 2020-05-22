context("Testing converting PMML Custom Functions")

test_that("z-score functions outside functions are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '
<PMML>
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
</LocalTransformations></PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-pmml-custom-func/code/test-pmml-custom-func-code-1.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})
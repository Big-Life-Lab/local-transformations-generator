context("Testing sourcing other files")

test_that("PMML is correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
<LocalTransformations>
<DerivedField name="b" optype="continuous">
<Constant dataType="double">3</Constant>
</DerivedField>
<DerivedField name="a" optype="continuous">
<Constant dataType="double">1</Constant>
</DerivedField>
</LocalTransformations></PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-source/code/test-source-code-1.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})
context("Testing PMML functions")

test_that("Exists function is correctly converted", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
<LocalTransformations>
<DerivedField name="test" optype="continuous">
<Apply function="exists">
<FieldRef field="testOne"/>
</Apply></DerivedField>
</LocalTransformations>
</PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-pmml-functions/code/test-exists-code.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})
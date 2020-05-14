context("Testing converting constants")

test_that("Constant expressions outside functions are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<Constant dataType="string">string</Constant>
</DerivedField>
</LocalTransformations>
</PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-constants/code/test-constants-code-1.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})

test_that("Constant expressions inside functions are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
<LocalTransformations>
<DefineFunction name="b">
<Constant dataType="double">1</Constant>
</DefineFunction>
</LocalTransformations>
</PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-constants/code/test-constants-code-2.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})
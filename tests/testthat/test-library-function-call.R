context("Testing calling a function from a library")

test_that("PMML is correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<Apply function="dplyr.filter">
<FieldRef field="b"/>
</Apply>
</DerivedField>
</LocalTransformations></PMML>'

  test_utils_test_code_file("test-library-function-call/test-library-function-call-1.R", expected_pmml)
})

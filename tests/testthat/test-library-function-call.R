context("Testing calling a function from a library")

test_that("PMML is correctly generated", {
  code <- 'a <- dplyr::filter(b)'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<Apply function="dplyr.filter">
<FieldRef field="b"/>
</Apply>
</DerivedField>
</LocalTransformations></PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

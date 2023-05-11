context("Testing PMML functions")

test_that("Exists function is correctly converted", {
  code <- '
    test <- exists("testOne")
  '

  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="test" optype="continuous">
<Apply function="exists">
<FieldRef field="testOne"/>
</Apply></DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("c function is correctly converted", {
  code <- '
    a <- b %in% c(1, NA, "2")
  '

  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<Apply function="isIn">
<FieldRef field="b"/>
<Constant dataType="double">1</Constant>
<Constant dataType="NA">NA</Constant>
<Constant dataType="string">2</Constant>
</Apply></DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

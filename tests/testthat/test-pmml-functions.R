context("Testing PMML functions")

test_that("Exists function is correctly converted", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="test" optype="continuous">
<Apply function="exists">
<FieldRef field="testOne"/>
</Apply></DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-pmml-functions/code/test-exists-code.R", expected_pmml)
})

test_that("c function is correctly converted", {
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

  test_utils_test_code_file("test-pmml-functions/code/test-c-code.R", expected_pmml)
})

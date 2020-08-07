context("Testing converting colon operator")

test_that("Colon operator outside functions are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<Apply function="isIn">
<FieldRef field="b"/>
<Apply function="colonOperator">
<Constant dataType="double">6</Constant>
<Constant dataType="double">9</Constant>
</Apply>
</Apply>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-colon-operator/code/test-colon-operator-code-1.R", expected_pmml)
})

test_that("Colon operator inside functions are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<ParameterField name="b" dataType="double"/>
<Apply function="isIn">
<FieldRef field="b"/>
<Apply function="colonOperator">
<Constant dataType="double">6</Constant>
<Constant dataType="double">9</Constant>
</Apply>
</Apply>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-colon-operator/code/test-colon-operator-code-2.R", expected_pmml)
})

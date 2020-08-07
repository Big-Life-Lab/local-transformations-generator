context("Testing converting math expressions")

test_that("Math expressions outside functions are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="c" optype="continuous">
<Apply function="+">
<FieldRef field="a"/>
<FieldRef field="b"/>
</Apply>
</DerivedField>
<DerivedField name="d" optype="continuous">
<Apply function="-">
<FieldRef field="e"/>
<FieldRef field="f"/>
</Apply>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-math/code/test-math-code-1.R", expected_pmml)
})

test_that("Math expressions inside functions are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<ParameterField name="b" dataType="double"/>
<ParameterField name="c" dataType="double"/>
<Apply function="*">
<FieldRef field="b"/>
<FieldRef field="c"/>
</Apply>
</DefineFunction>
<DefineFunction name="d">
<ParameterField name="e" dataType="double"/>
<ParameterField name="f" dataType="double"/>
<Apply function="/">
<FieldRef field="e"/>
<FieldRef field="f"/>
</Apply>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-math/code/test-math-code-2.R", expected_pmml)
})

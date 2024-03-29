context("Data frame code")

test_that("Data frame code within single line functions using table within the function scope are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<ParameterField name="b" dataType="double"/>
<ParameterField name="table" dataType="double"/>
<MapValues outputColumn="c">
<FieldColumnPair column="b" field="b"/>
<TableLocator location="local" name="table"/>
</MapValues>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-df-code/function-scope.R", expected_pmml)
})

test_that("Data frame code within single line functions using tables outside the function scope are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<ParameterField name="b" dataType="double"/>
<MapValues outputColumn="d">
<FieldColumnPair column="c" field="b"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-df-code/outside-function-scope.R", expected_pmml)
})

test_that("Retreiving output column from function return", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<MapValues outputColumn="c">
<TableLocator>
<Apply function="b"></Apply>
</TableLocator>
</MapValues>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-df-code/output-column-function-return.R", expected_pmml)
})

test_that("Data frame query with no output column is correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<ParameterField name="table" dataType="double"/>
<MapValues>
<FieldColumnPair column="b" constant="1"/>
<TableLocator location="local" name="table"/>
</MapValues>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-df-code/no-output-column.R", expected_pmml)
})

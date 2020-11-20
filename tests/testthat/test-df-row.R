context("Testing accessing rows from data frames")

test_that("Accessing rows outside functions are correctly generated", {
  skip("")
  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="col" optype="continuous">
<MapValues outputColumn="col1">
<FieldColumnPair column="col1" constant="val"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-df-row/code/test-df-row-code-1.R", expected_pmml)
  expect_false(exists("row_vars"),
               info = "row_vars has not been cleared from global environment")
})

test_that("Accessing rows inside functions are correctly generated", {
  skip("")
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name=\"test_col_1\">
<ParameterField name=\"const\" dataType=\"double\"/>
<ParameterField name=\"rowAa\" dataType=\"double\"/>
<Apply function=\"+\">
<Apply function=\"+\">
<MapValues outputColumn=\"col3\">
<FieldColumnPair column=\"col1\" field=\"rowAa\"/>
<TableLocator location=\"taxonomy\" name=\"table\"/>
</MapValues>
<FieldRef field=\"const\"/>
</Apply>
<MapValues outputColumn=\"col4\">
<FieldColumnPair column=\"col2\" constant=\"val1\"/>
<TableLocator location=\"taxonomy\" name=\"table\"/>
</MapValues>
</Apply>
</DefineFunction>
<DefineFunction name=\"test_col_2\">
<ParameterField name=\"const\" dataType=\"double\"/>
<ParameterField name=\"rowAa\" dataType=\"double\"/>
<ParameterField name=\"row_threeAb\" dataType=\"double\"/>
<Apply function=\"+\">
<Apply function=\"+\">
<MapValues outputColumn=\"col3\">
<FieldColumnPair column=\"col1\" field=\"rowAa\"/>
<TableLocator location=\"taxonomy\" name=\"table\"/>
</MapValues>
<FieldRef field=\"const\"/>
</Apply>
<MapValues outputColumn=\"col4\">
<FieldColumnPair column=\"col3\" field=\"row_threeAb\"/>
<TableLocator location=\"taxonomy\" name=\"table\"/>
</MapValues>
</Apply>
</DefineFunction>
<DerivedField name=\"col\" optype=\"continuous\">
<Apply function=\"+\">
<Apply function=\"test_col_1\">
<Constant dataType=\"double\">1</Constant>
<FieldRef field=\"a\"/>
</Apply>
<Apply function=\"test_col_2\">
<Constant dataType=\"double\">1</Constant>
<FieldRef field=\"a\"/>
<FieldRef field=\"b\"/>
</Apply>
</Apply>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-df-row/code/test-df-row-code-2.R", expected_pmml)
  expect_false(exists("row_vars"),
               info = "row_vars has not been cleared from global environment")
})

test_that("Accessing rows inside functions that takes only one parameter which is a row are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="test_col_1">
<ParameterField name="colAa"/>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-df-row/code/test-df-row-code-3.R", expected_pmml)
  expect_false(exists("row_vars"),
               info = "row_vars has not been cleared from global environment")
})

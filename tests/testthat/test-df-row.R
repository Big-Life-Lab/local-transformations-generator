context("Testing accessing rows from data frames")

test_that("Accessing rows outside functions are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
<LocalTransformations>
<DerivedField name="col" optype="continuous">
<MapValues outputColumn="col1">
<FieldColumnPair column="col1" constant="val"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DerivedField>
</LocalTransformations>
</PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-df-row/code/test-df-row-code-1.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})

test_that("Accessing rows inside functions are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
<LocalTransformations>
<DefineFunction name="test_col_1">
<ParameterField name="const" dataType="double"/>
<Apply function="+">
<Apply function="+">
<MapValues outputColumn="col3">
<FieldColumnPair column="col1" constant="val"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
<FieldRef field="const"/>
</Apply>
<MapValues outputColumn="col4">
<FieldColumnPair column="col2" constant="val1"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues></Apply>
</DefineFunction>
<DefineFunction name="test_col_2">
<ParameterField name="const" dataType="double"/>
<Apply function="+">
<Apply function="+">
<MapValues outputColumn="col3">
<FieldColumnPair column="col1" constant="val"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues><FieldRef field="const"/>
</Apply><MapValues outputColumn="col4">
<FieldColumnPair column="col3" constant="val2"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</Apply>
</DefineFunction>
<DerivedField name="col" optype="continuous">
<Apply function="+">
<Apply function="test_col_1">
<Constant dataType="double">1</Constant>
</Apply>
<Apply function="test_col_2">
<Constant dataType="double">1</Constant>
</Apply>
</Apply>
</DerivedField>
</LocalTransformations>
</PMML>')
  
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-df-row/code/test-df-row-code-2.R"), srcFile = TRUE)
  expect_equal(actual_pmml, expected_pmml)
})
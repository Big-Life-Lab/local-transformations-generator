context("Testing accessing rows from data frames")

test_that("Accessing rows outside functions are correctly generated", {
  code <- '
 row <- table[table$col1 == "val", ]
col <- row$col1
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="row" optype="continuous">
<MapValues>
<FieldColumnPair column="col1" constant="val"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DerivedField>
<DerivedField name="col" optype="continuous">
<MapValues outputColumn="col1">
<TableLocator location="taxonomy" name="row" />
</MapValues>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
  expect_false(exists("row_vars"),
               info = "row_vars has not been cleared from global environment")
})

test_that("Accessing rows inside functions are correctly generated", {
  code <- '
test <- function(row, const, row_two) {
  return(row$col3 + const + row_two$col4)
}

row <- table[table$col1 == a, ]
row_two <- table[table$col2 == "val1", ]
row_three <- table[table$col3 == b, ]

col <- test(row, 1, row_two) + test(row, 1, row_three)
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="test">
<ParameterField name="row" dataType="double"/>
<ParameterField name="const" dataType="double"/>
<ParameterField name="row_two" dataType="double"/>
<Apply function="+">
<Apply function="+">
<MapValues outputColumn="col3">
<TableLocator location="local" name="row" />
</MapValues>
<FieldRef field="const"/>
</Apply>
<MapValues outputColumn="col4">
<TableLocator location="local" name="row_two" />
</MapValues>
</Apply>
</DefineFunction>
<DerivedField name="row" optype="continuous">
<MapValues>
<FieldColumnPair column="col1" field="a"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DerivedField>
<DerivedField name="row_two" optype="continuous">
<MapValues>
<FieldColumnPair column="col2" constant="val1"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DerivedField>
<DerivedField name="row_three" optype="continuous">
<MapValues>
<FieldColumnPair column="col3" field="b"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DerivedField>
<DerivedField name="col" optype="continuous">
<Apply function="+">
<Apply function="test">
<FieldRef field="row"/>
<Constant dataType="double">1</Constant>
<FieldRef field="row_two"/>
</Apply>
<Apply function="test">
<FieldRef field="row"/>
<Constant dataType="double">1</Constant>
<FieldRef field="row_three"/>
</Apply>
</Apply>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
  expect_false(exists("row_vars"),
               info = "row_vars has not been cleared from global environment")
})

test_that("Accessing rows inside functions that takes only one parameter which is a row are correctly generated", {
  code <- '
test <- function(row) {
  return(row$col3)
}

row <- table[table$col1 == a, ]

col <- test(row)
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="test">
<ParameterField name="row" dataType="double"/>
<MapValues outputColumn="col3">
<TableLocator location="local" name="row" />
</MapValues>
</DefineFunction>
<DerivedField name="row" optype="continuous">
<MapValues>
<FieldColumnPair column="col1" field="a"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DerivedField>
<DerivedField name="col" optype="continuous">
<Apply function="test">
<FieldRef field="row"/>
</Apply>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
  expect_false(exists("row_vars"),
               info = "row_vars has not been cleared from global environment")
})

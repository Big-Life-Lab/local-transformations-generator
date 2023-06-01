context("Testing accessing columns from data frames")

test_that("Wildcard column access expressions outside functions are correctly generated", {
 code <- '
# Test for when the value being compared to is a constant
a <- table[table$col1 == "val", ]$col2

# Test for when the value being compared to is in a variable
b <- table[table$col1 == val, ]$col2
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<MapValues outputColumn="col2">
<FieldColumnPair column="col1" constant="val"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DerivedField>
<DerivedField name="b" optype="continuous">
<MapValues outputColumn="col2">
<FieldColumnPair column="col1" field="val"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Non-wildcard column access expressions outside functions are correctly generated", {
 code <- '
a <- table["b", "c"]
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<MapValues outputColumn="c">
<FieldColumnPair column="index" constant="b"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})
 
test_that("Wildcard column access expressions inside functions throw an error", {
     code <- '
test <- function() {
  a <- table[table$col1 == "val", ]$col2
}
'
    
    expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="test">
<MapValues outputColumn="col2">
<FieldColumnPair column="col1" constant="val"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DefineFunction>
</LocalTransformations>
</PMML>'

    test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Non-wildcard column access expressions inside functions throw an error", {
    code <- '
    test <- function() {
  a <- table["b", "c"]
}
    '

    expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="test">
<MapValues outputColumn="c">
<FieldColumnPair column="index" constant="b"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DefineFunction>
</LocalTransformations>
</PMML>'

    test_utils_run_generate_pmml_test(code, expected_pmml)
})

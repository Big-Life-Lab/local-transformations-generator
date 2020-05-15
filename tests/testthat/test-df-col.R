source("../../R/strings.R")

context("Testing accessing columns from data frames")

test_that("Wildcard column access expressions outside functions are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
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
</PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-df-col/code/test-df-col-code-1.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})

test_that("Non-wildcard column access expressions outside functions are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<MapValues outputColumn="c">
<FieldColumnPair column="index" constant="b"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DerivedField>
</LocalTransformations>
</PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-df-col/code/test-df-col-code-2.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})

test_that("Wildcard column access expressions inside functions throw an error", {
  expect_error(getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-df-col/code/test-df-col-code-3.R"), srcFile = TRUE), strings.unsupported_df_col_access_expr_error)
})

test_that("Non-wildcard column access expressions inside functions throw an error", {
  expect_error(getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-df-col/code/test-df-col-code-4.R"), srcFile = TRUE), strings.unsupported_df_col_access_expr_error)
})
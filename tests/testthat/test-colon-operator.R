context("Testing converting colon operator")

test_that("Colon operator outside functions are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
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
</PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-colon-operator/code/test-colon-operator-code-1.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})

test_that("Colon operator inside functions are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
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
</PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-colon-operator/code/test-colon-operator-code-2.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})
context("Testing converting mutations")

test_that("Mutations outside functions are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
<LocalTransformations>
<DerivedField name="ALWDWKY_Mutated_1" optype="continuous">
<FieldRef field="ALWDWKY"/>
</DerivedField>
<DerivedField name="ALWDWKY_Mutated_2" optype="continuous">
<Apply function="+">
<FieldRef field="ALWDWKY_Mutated_1"/>
<Constant dataType="double">1</Constant>
</Apply>
</DerivedField>
<DerivedField name="SMKDSTY" optype="continuous">
<FieldRef field="ALWDWKY_Mutated_2"/>
</DerivedField></LocalTransformations></PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-mutation/code/test-mutation-code-1.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})
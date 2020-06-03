context("Testing converting constants")

test_that("Constant expressions outside functions are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<Constant dataType="string">string</Constant>
</DerivedField>
</LocalTransformations>
</PMML>'
  
  test_utils.test_code_file("test-constants/code/test-constants-code-1.R", expected_pmml)
})

test_that("Constant expressions inside functions are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="b">
<Constant dataType="double">1</Constant>
</DefineFunction>
</LocalTransformations>
</PMML>'
  
  test_utils.test_code_file("test-constants/code/test-constants-code-2.R", expected_pmml)
})
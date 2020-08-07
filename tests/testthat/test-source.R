context("Testing sourcing other files")

test_that("PMML is correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="b" optype="continuous">
<Constant dataType="double">3</Constant>
</DerivedField>
<DerivedField name="a" optype="continuous">
<Constant dataType="double">1</Constant>
</DerivedField>
</LocalTransformations></PMML>'

  test_utils_test_code_file("test-source/code/test-source-code-1.R", expected_pmml)
})

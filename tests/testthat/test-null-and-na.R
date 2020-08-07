context("Testing converting NULL and NA")

test_that("NULL outside functions are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<Constant dataType="NULL">NULL</Constant>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-null-and-na/code/test-null-code-1.R", expected_pmml)
})

test_that("NA outside functions are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<Constant dataType="NA">NA</Constant>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-null-and-na/code/test-na-code-1.R", expected_pmml)
})

test_that("NULL inside functions are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<Constant dataType="NULL">NULL</Constant>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-null-and-na/code/test-null-code-2.R", expected_pmml)
})

test_that("NA outside functions are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<Constant dataType="NA">NA</Constant>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-null-and-na/code/test-na-code-2.R", expected_pmml)
})

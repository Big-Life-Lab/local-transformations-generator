context("Testing converting constants")

test_that("Constant expressions outside functions are correctly generated", {
  code <- '
    a <- "string"
  '

  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<Constant dataType="string">string</Constant>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Constant expressions inside functions are correctly generated", {
  code <- '
    b <- function() {
        c <- 1
    }
  '
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="b">
<Constant dataType="double">1</Constant>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

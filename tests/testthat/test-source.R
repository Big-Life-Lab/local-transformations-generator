context("Testing sourcing other files")

test_that("PMML is correctly generated", {
code <- '
  source(file.path(getwd(), "source-code-2.R"))

a <- 1
'

source_code_2 <- '
  b <- 3
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="b" optype="continuous">
<Constant dataType="double">3</Constant>
</DerivedField>
<DerivedField name="a" optype="continuous">
<Constant dataType="double">1</Constant>
</DerivedField>
</LocalTransformations></PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml,
                                    files = list(
                                      'source-code-2' = list(
                                        type = 'R',
                                        contents = source_code_2
                                      )
                                    )
                                   )
})

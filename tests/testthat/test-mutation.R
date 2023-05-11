context("Testing converting mutations")

test_that("Mutations outside functions are correctly generated", {
  code <- '
ALWDWKY <- ALWDWKY

source(file.path(getwd(), "mutation-code-2.R"))
source(file.path(getwd(), "mutation-code-3.R"))
'

mutation_code_2 <- '
ALWDWKY <- ALWDWKY + 1
'

mutation_code_3 <- '
SMKDSTY <- ALWDWKY
'

  expected_pmml <- '<PMML>
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
</DerivedField></LocalTransformations></PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml,
    files = list(
         "mutation-code-2" = list(
                                  type = "R",
                                  contents = mutation_code_2),
         "mutation-code-3" = list(
                                  type = "R",
                                  contents = mutation_code_3)
         )
  )
})

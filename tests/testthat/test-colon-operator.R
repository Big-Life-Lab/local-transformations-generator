context("Testing converting colon operator")

test_that("Colon operator outside functions are correctly generated", {
  code <- '
  a <- b %in% 6:9
  '

  expected_pmml <- '<PMML>
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
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Colon operator inside functions are correctly generated", {
  code <- '
    a <- function(b) {
        return(b %in% 6:9)
    }
  '

  expected_pmml <- '<PMML>
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
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

context("Testing custom function call")

test_that("Calling custom functions with defaulted args are correctly generated", {
  code <- '
  func_one <- function(arg_one, default_arg = 0) {
    return(arg_one)
  }

  var_one <- func_one(1)
  '

  expected_pmml <- '
    <PMML>
        <LocalTransformations>
            <DefineFunction name="default(default_arg)">
                <ParameterField name="arg_one" dataType="double"/>
                <ParameterField name="default_arg" dataType="double"/>
                <Apply function="if">
                    <Apply function="equal">
                        <FieldRef field="default_arg"/>
                        <Constant dataType="NA">NA</Constant>
                    </Apply>
                    <Constant dataType="double">0</Constant>
                    <FieldRef field="default_arg"/>
                </Apply>
            </DefineFunction>
            <DefineFunction name="func_one">
                <ParameterField name="arg_one" dataType="double"/>
                <ParameterField name="default_arg" dataType="double"/>
                <FieldRef field="arg_one"/>
            </DefineFunction>
            <DerivedField name="var_one" optype="continuous">
                <Apply function="func_one">
                    <Constant dataType="double">1</Constant>
                    <Constant dataType="NA">NA</Constant>
                </Apply>
            </DerivedField>
        </LocalTransformations>
    </PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

context("Testing custom functions")

test_that("Simple custom function PMML is correctly generated", {
    expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="testFunction(testTwo)">
<ParameterField name="testOne" dataType="double"/>
<Apply function="+">
<FieldRef field="testOne"/>
<Constant dataType="double">1</Constant>
</Apply>
</DefineFunction>
<DefineFunction name="testFunction">
<ParameterField name="testOne" dataType="double"/>
<Apply function="testFunction(testTwo)">
<FieldRef field="testOne"/>
</Apply>
</DefineFunction>
</LocalTransformations>
</PMML>'

    test_utils_test_code_file("test-custom-functions/code/test-define-function-code.R", expected_pmml)
})

test_that("Default function arguments are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="default(argOne)">
<ParameterField name="argOne" dataType="double"/>
<ParameterField name="argTwo" dataType="double"/>
<Apply function="if">
<Apply function="equal">
<FieldRef field="argOne"/>
<Constant dataType="NA">NA</Constant>
</Apply>
<Apply function="+">
<Constant dataType="double">1</Constant>
<Constant dataType="double">1</Constant>
</Apply>
<FieldRef field="argOne"/>
</Apply>
</DefineFunction>
<DefineFunction name="default(argTwo)">
<ParameterField name="argOne" dataType="double"/>
<ParameterField name="argTwo" dataType="double"/>
<Apply function="if">
<Apply function="equal">
<FieldRef field="argTwo"/>
<Constant dataType="NA">NA</Constant>
</Apply>
<Constant dataType="string">a</Constant>
<FieldRef field="argTwo"/>
</Apply>
</DefineFunction>
<DefineFunction name="testFunc">
<ParameterField name="argOne" dataType="double"/>
<ParameterField name="argTwo" dataType="double"/>
<Apply function="+">
<Apply function="default(argOne)">
<FieldRef field="argOne"/>
<FieldRef field="argTwo"/>
</Apply>
<Apply function="default(argTwo)">
<FieldRef field="argOne"/>
<FieldRef field="argTwo"/>
</Apply>
</Apply>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-custom-functions/code/test-default-function-args-code.R", expected_pmml)
})

test_that("No return statement function are correctly generated", {
  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<Constant dataType="double">1</Constant>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_test_code_file("test-custom-functions/code/test-no-return-code.R", expected_pmml)
})

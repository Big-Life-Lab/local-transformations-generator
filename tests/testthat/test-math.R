context("Testing converting math expressions")

test_that("Math expressions outside functions are correctly generated", {
  code <- '
  c <- a + b

d <- e - f

g <- h^2
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="c" optype="continuous">
<Apply function="+">
<FieldRef field="a"/>
<FieldRef field="b"/>
</Apply>
</DerivedField>
<DerivedField name="d" optype="continuous">
<Apply function="-">
<FieldRef field="e"/>
<FieldRef field="f"/>
</Apply>
</DerivedField>
<DerivedField name="g" optype="continuous">
<Apply function="pow">
<FieldRef field="h"/>
<Constant dataType="double">2</Constant>
</Apply>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Math expressions inside functions are correctly generated", {
code <- '
a <- function(b, c) {
  return(b*c)
}

d <- function(e, f) {
  return(e/f)
}

g <- function(h) {
  return(h^2)
}
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<ParameterField name="b" dataType="double"/>
<ParameterField name="c" dataType="double"/>
<Apply function="*">
<FieldRef field="b"/>
<FieldRef field="c"/>
</Apply>
</DefineFunction>
<DefineFunction name="d">
<ParameterField name="e" dataType="double"/>
<ParameterField name="f" dataType="double"/>
<Apply function="/">
<FieldRef field="e"/>
<FieldRef field="f"/>
</Apply>
</DefineFunction>
<DefineFunction name="g">
<ParameterField name="h" dataType="double"/>
<Apply function="pow">
<FieldRef field="h"/>
<Constant dataType="double">2</Constant>
</Apply>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

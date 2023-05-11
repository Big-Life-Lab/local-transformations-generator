context("Testing if expressions")

test_that("If expressions outside functions are correctly generated", {
  code <- '
if (a == 0){
  b <- 0
  c <- 0
} else if(d == 1) {
  e <- 1
} else if(a == 3) {
  b <- 3
  c <- 3
} else {
  b <- 4
  c <- 4
}
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="b" optype="continuous">
<Apply function="if">
<Apply function="equal">
<FieldRef field="a"/>
<Constant dataType="double">0</Constant>
</Apply>
<Constant dataType="double">0</Constant>
<Apply function="if">
<Apply function="equal">
<FieldRef field="a"/>
<Constant dataType="double">3</Constant>
</Apply>
<Constant dataType="double">3</Constant>
<Apply function="if">
<Constant dataType="double">4</Constant>
</Apply>
</Apply>
</Apply>
</DerivedField>
<DerivedField name="c" optype="continuous">
<Apply function="if">
<Apply function="equal">
<FieldRef field="a"/>
<Constant dataType="double">0</Constant>
</Apply>
<Constant dataType="double">0</Constant>
<Apply function="if">
<Apply function="equal">
<FieldRef field="a"/>
<Constant dataType="double">3</Constant>
</Apply>
<Constant dataType="double">3</Constant>
<Apply function="if">
<Constant dataType="double">4</Constant>
</Apply>
</Apply>
</Apply>
</DerivedField>
<DerivedField name="e" optype="continuous">
<Apply function="if">
<Apply function="equal">
<FieldRef field="d"/>
<Constant dataType="double">1</Constant>
</Apply>
<Constant dataType="double">1</Constant>
<Constant dataType="NULL">NULL</Constant>
</Apply>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(
    code,
    expected_pmml
  )
})

test_that("If expressions inside functions that are not the last expression are correctly generated", {
  code <- '
test <- function(a, d) {
  if (a == 0){
    b <- 0
    c <- 0
  } else if(d == 1) {
    e <- 1
  } else if(a == 3) {
    b <- 3
    c <- 3
  } else {
    b <- 4
    c <- 4
  }
  
  return(b + c + e)
}
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="test(b)">
<ParameterField name="a" dataType="double"/>
<ParameterField name="d" dataType="double"/>
<Apply function="if">
<Apply function="equal">
<FieldRef field="a"/>
<Constant dataType="double">0</Constant>
</Apply>
<Constant dataType="double">0</Constant>
<Apply function="if">
<Apply function="equal">
<FieldRef field="a"/>
<Constant dataType="double">3</Constant>
</Apply>
<Constant dataType="double">3</Constant>
<Constant dataType="double">4</Constant>
</Apply>
</Apply>
</DefineFunction>
<DefineFunction name="test(c)">
<ParameterField name="a" dataType="double"/>
<ParameterField name="d" dataType="double"/>
<Apply function="if">
<Apply function="equal">
<FieldRef field="a"/>
<Constant dataType="double">0</Constant>
</Apply>
<Constant dataType="double">0</Constant>
<Apply function="if">
<Apply function="equal">
<FieldRef field="a"/>
<Constant dataType="double">3</Constant>
</Apply>
<Constant dataType="double">3</Constant>
<Constant dataType="double">4</Constant>
</Apply>
</Apply>
</DefineFunction>
<DefineFunction name="test(e)">
<ParameterField name="a" dataType="double"/>
<ParameterField name="d" dataType="double"/>
<Apply function="if">
<Apply function="equal">
<FieldRef field="d"/>
<Constant dataType="double">1</Constant>
</Apply>
<Constant dataType="double">1</Constant>
<Constant dataType="NULL">NULL</Constant>
</Apply>
</DefineFunction>
<DefineFunction name="test">
<ParameterField name="a" dataType="double"/>
<ParameterField name="d" dataType="double"/>
<Apply function="+">
<Apply function="+">
<Apply function="test(b)">
<FieldRef field="a"/>
<FieldRef field="d"/>
</Apply>
<Apply function="test(c)">
<FieldRef field="a"/>
<FieldRef field="d"/>
</Apply>
</Apply>
<Apply function="test(e)">
<FieldRef field="a"/>
<FieldRef field="d"/>
</Apply>
</Apply>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("If expressions inside functions as the last expressions are correctly generated",  {
  code <- '
test <- function(a, b) {
  if(a == 1) {
    c <- 1
    return(1)
  } else if(b == 2) {
    d <- 2
  } else if(a == 3) {
    c <- 3
    e <- 3
  } else {
    f <- 4
  }
}
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="test">
<ParameterField name="a" dataType="double"/>
<ParameterField name="b" dataType="double"/>
<Apply function="if">
<Apply function="equal">
<FieldRef field="a"/>
<Constant dataType="double">1</Constant>
</Apply>
<Constant dataType="double">1</Constant>
<Apply function="if">
<Apply function="equal">
<FieldRef field="b"/>
<Constant dataType="double">2</Constant>
</Apply>
<Constant dataType="double">2</Constant>
<Apply function="if">
<Apply function="equal">
<FieldRef field="a"/>
<Constant dataType="double">3</Constant>
</Apply>
<Constant dataType="double">3</Constant>
<Constant dataType="double">4</Constant>
</Apply>
</Apply>
</Apply>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

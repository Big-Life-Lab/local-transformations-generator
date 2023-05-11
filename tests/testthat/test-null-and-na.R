context("Testing converting NULL and NA")

test_that("NULL outside functions are correctly generated", {
  code <- '
    a <- NULL
  '

  expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<Constant dataType="NULL">NULL</Constant>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("NA outside functions are correctly generated", {
 code <- '
    a <- NA
 '

 expected_pmml <- '<PMML>
<LocalTransformations>
<DerivedField name="a" optype="continuous">
<Constant dataType="NA">NA</Constant>
</DerivedField>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("NULL inside functions are correctly generated", {
  code <- '
    a <- function() {
        return(NULL)
    }
 ' 

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<Constant dataType="NULL">NULL</Constant>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("NA outside functions are correctly generated", {
 code <- '
    a <- function() {
        return(NA)
    }
 '

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<Constant dataType="NA">NA</Constant>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

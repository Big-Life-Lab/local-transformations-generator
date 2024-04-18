context("Testing converting switch statement")

test_that("Switch statement with simple expressions", {
  code <- '
    test <- switch(
        a,
        "1" = "a",
        "2" = "b",
        "c"
    )
  '

  expected_pmml <- '
<PMML>
    <LocalTransformations>
        <DerivedField name="test" optype="continuous">
            <Apply function="if">
                <Apply function="equal">
                    <DataField name="a"/>
                    <Constant dataType="string">1</Constant>
                </Apply>
                <Constant dataType="string">a</Constant>
                <Apply function="if">
                    <Apply function="equal">
                        <DataField name="a"/>
                        <Constant dataType="string">2</Constant>
                    </Apply>
                    <Constant dataType="string">b</Constant>
                    <Constant dataType="string">c</Constant>
                </Apply>
            </Apply>
        </DerivedField>
    </LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Switch statement with complex expressions", {
  code <- '
    test <- switch(
        a,
        "1" = when_one(),
        "c"
    )'

  expected_pmml <- '
<PMML>
    <LocalTransformations>
        <DerivedField name="test" optype="continuous">
            <Apply function="if">
                <Apply function="equal">
                    <DataField name="a"/>
                    <Constant dataType="string">1</Constant>    
                </Apply>
                <Apply function="when_one"></Apply>    
                <Constant dataType="string">c</Constant>
            </Apply>
        </DerivedField>
    </LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Switch statement with no default expression", {
  code <- '
    test <- switch(
        a,
        "1" = "a",
        "2" = "b"
    )'

  expected_pmml <- '
<PMML>
    <LocalTransformations>
        <DerivedField name="test" optype="continuous">
            <Apply function="if">
                <Apply function="equal">
                    <DataField name="a"/>
                    <Constant dataType="string">1</Constant>    
                </Apply>
                <Constant dataType="string">a</Constant>
                <Apply function="if">
                    <Apply function="equal">
                        <DataField name="a"/>
                        <Constant dataType="string">2</Constant>    
                    </Apply>
                    <Constant dataType="string">b</Constant>
                    <Constant dataType="NA">NA</Constant>
                </Apply>
            </Apply>
        </DerivedField>
    </LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that(
    "Switch expressions with multiple expressions in branch should error",
    {
        code <- '
            test <- switch(
                a, 
                "1" = {
                    b <- 1
                    calc_result(b)
                },
                "c"
            )'

    test_utils_run_generate_pmml_test(
        code, expected_error = 'Multiple expressions in switch statement')
    }
)

context("Testing if expressions")

test_that("If expressions outside functions are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '<PMML>
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
</PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-if/code/test-if-code-1.R"), srcFile = TRUE)

  expect_equal(actual_pmml, expected_pmml)
})

test_that("If expressions inside functions that are not the last expression are correctly generated", {
  expected_pmml <- gsub("[\r\n]", "", '
<PMML>
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
</PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-if/code/test-if-code-2.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})

test_that("If expressions inside functions as the last expressions are correctly generated",  {
  expected_pmml <- gsub("[\r\n]", "", '
<PMML>
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
</PMML>')
  actual_pmml <- getPmmlStringFromRFile(file.path(getwd(), "../../assets/test/test-if/code/test-if-code-3.R"), srcFile = TRUE)
  
  expect_equal(actual_pmml, expected_pmml)
})
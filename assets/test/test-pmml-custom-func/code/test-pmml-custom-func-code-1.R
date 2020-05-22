reference_table <- read.csv(file.path(getwd(), "../../assets/test/test-pmml-custom-func/tables/reference-table.csv"))

#Test One
# @pmml_custom_func(z_score, Juice_cont, reference_table)
#Test One
zJuice <- zScore.fun(Juice_cont)
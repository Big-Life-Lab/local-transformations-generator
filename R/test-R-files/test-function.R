#PBL BMI Functions

BMI_cont <- function(weightlb, heightin_hft, heightin_hin) {

  weightkg <-
    ifelse(!is.na(weightlb), weightlb/2.2046226218, NA)
  heightm <-
    ifelse(!is.na(heightin_hft) & !is.na(heightin_hin), (((heightin_hft*12) + heightin_hin)/39.3701), NA)

  BMI_cont <-
    ifelse(!is.na(weightkg) & !is.na(heightm), weightkg/(heightm*heightm), NA)

  return(BMI_cont)
}

if (cond_SQ010=='Yes'){
  Diabetes_cat<-0
  HeartDis_cat<-0
} else if(cond_SQ010 == 'No') {
  testOne <- 1
} else {
  Diabetes_cat<-1
  HeartDis_cat<-1
}
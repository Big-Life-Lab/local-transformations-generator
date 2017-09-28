QuitTime.fun <- function(stpn) {
  QuitTime <-
    ifelse(stpn=='stpn1', 0,
    ifelse(stpn=='stpn2', 1,
    ifelse(stpn=='stpn3', 2,
    ifelse(stpn=='stpn4', stpny, NA))))
  return(QuitTime)
}

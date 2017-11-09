

mymean <- function(v) {
  m <- round(sum(v)/length(v), digits = 3)
  print("this is my mean")
  return(m)
}

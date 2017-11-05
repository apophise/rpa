

mean <- function(students.input, sex) {
  if ('M' == sex) {
    male.mean = students.input %>%
      filter(sex == "M") %>%
      summarise(mean = mean(height))
  } else {
    #mean height of female
    female.mean = students.input %>%
      filter(sex == "F") %>%
      summarise(mean = mean(height))
  }
}



library(zhangbo1234)
context("String length")

data("students")

students_same = students

students_same[,"height"]<-1.60

test_that("likunlantools_test", {
  expect_equal(nrow(checkHeight(students)), nrow(students))
  expect_equal(checkHeight(students_same,TRUE), checkHeight(students_same,FALSE))
})

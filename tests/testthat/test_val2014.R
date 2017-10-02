context("val2014")
output <- val2014()

test_that("Assignment: val2014()", {
  expect_true(exists("val2014"), info = "No object 'val2014' exists.")
  expect_is(val2014, class = "function", info = "Object 'val2014' is not a function.")
  expect_is(output, class = "list", info = "Object 'output' is not a list")
  expect_true(length(output)==3, info="There is not 3 data sets in the output")
  expect_is(output[[1]], class = "data.frame", info = "The first data set in output is not a data.frame")
  expect_is(output[[2]], class = "data.frame", info = "The first data set in output is not a data.frame")
  expect_is(output[[3]], class = "data.frame", info = "The first data set in output is not a data.frame")
  expect_true(any(colnames(output[[1]])=="KOMMUN"), info = "There is no column named 'KOMMUN' in the first data set")
  expect_true(any(colnames(output[[2]])=="KOMMUN"), info = "There is no column named 'KOMMUN' in the first data set")
  expect_true(any(colnames(output[[3]])=="KOMMUN"), info = "There is no column named 'KOMMUN' in the first data set")

})


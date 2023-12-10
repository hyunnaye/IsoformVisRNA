context("Test generateBarPlot")
library(IsoformVisRNA)

# Mock Data

samplenames <- c("A", "B", "C")
Ratio <- c("0.4", "2", "0.3")
group <- c("cat", "dog", "cat")
ratioDF <- data.frame(samplenames, Ratio, group)

test_that("test that plot generates correctly", {
  expect_type(generateBoxPlot(ratioDF), 'list')
  expect_type(generateBoxPlot(ratioDF, "test"), 'list')
  expect_type(generateBoxPlot(ratioDF, "test", "test"), 'list')
  expect_type(generateBoxPlot(ratioDF, "test", "test", "test"), 'list')
  expect_type(generateBoxPlot(ratioDF, "test", "test", "test", c("cat", "dog")), 'list')
})

test_that("test invalid input for order parameter", {
  expect_error(generateBoxPlot(generateBoxPlot(ratioDF, "test", "test", "test", c())))
  expect_error(generateBoxPlot(generateBoxPlot(ratioDF, "test", "test", "test", c("cat"))))
  expect_error(generateBoxPlot(generateBoxPlot(ratioDF, "test", "test", "test", c("dog"))))
})

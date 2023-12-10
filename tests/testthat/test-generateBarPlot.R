context("Test generateBarPlot")
library(IsoformVisRNA)

# Mock Data

samplenames <- c("A", "B", "C")
Ratio <- c("0.4", "2", "0.3")
group <- c("cat", "dog", "cat")
ratioDF <- data.frame(samplenames, Ratio, group)

test_that("test that plot generates correctly", {
  expect_type(generateBarPlot(ratioDF), 'list')
  expect_type(generateBarPlot(ratioDF, "test"), 'list')
  expect_type(generateBarPlot(ratioDF, "test", "test"), 'list')
  expect_type(generateBarPlot(ratioDF, "test", "test", "test"), 'list')
})

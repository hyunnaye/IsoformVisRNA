context("Test calculateRatios")
library(IsoformVisRNA)

test_that("first test for correct ratios", {
  TranscriptIDs <- c("A", "B")
  Sample1 <- c("1", "2")
  normCM <- data.frame(TranscriptIDs, Sample1)

  samplenames <- c("Sample1")
  Metadata_group <- c("C")
  metadata <- data.frame(samplenames, Metadata_group)

  Ratio <- c(2)
  ExpectedRatio <- data.frame(samplenames, Ratio, Metadata_group)

  expect_identical(ExpectedRatio, calculateRatios(normCM, metadata, "A", "B", "Metadata_group", NULL))
})

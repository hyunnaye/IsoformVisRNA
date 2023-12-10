#' Generate expression ratios for 2 given transcript ids for all samples.
#'
#' A function that returns a matrix that includes the expression ratio of given transcript ids for each sample in a given data.
#'
#' @param normCM matrix that includes all normalized count matrices where
#'    sample names are the columns and transcript ids are the rows. See data/Normalized_CM.rda for an example.
#'
#' @param metadata matrix that includes the relevant group information for samples. See data/Experiment_info.rda for an example.
#'
#' @param transcript_id1 the id of the first transcript ID for comparing expression.
#'
#' @param transcript_id2 the id of the second transcript ID for comparing expression.
#'
#' @param metadata_group the column name in metadata used to categorize samples into desired groups.
#'
#' @param output_path desired path to save the output in a csv.
#'
#' @return Returns a matrix that includes the expression ratio of given transcript ids for each sample.
#'
#' @examples
#' ratioDF <- calculateRatios(Normalized_CM, Experiment_info, "ENST00000316724.9", "ENST00000409400.1", "Biosample_summary")
#'
#' @references
#'   Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A Grammar of Data Manipulation_. R
#'   package version 1.1.3, <https://CRAN.R-project.org/package=dplyr>.
#' @export
#' @import dplyr
calculateRatios <- function(normCM, metadata, transcript_id1, transcript_id2, metadata_group, output_path) {
  samplenames <- colnames(normCM)[-1]
  desiredTranscripts <- normCM %>% filter(TranscriptIDs %in% c(transcript_id1, transcript_id2))
  ratioDF <- data.frame(samplenames)
  ratioList <- c()
  for (i in 1:length(samplenames)){
    ratio <- as.numeric(desiredTranscripts[2,i+1])/as.numeric(desiredTranscripts[1,i+1]) #ratio = transcript_id2:transcript_id1
    ratioList <- c(ratioList, ratio)
  }
  ratioDF$Ratio <- ratioList
  ratioDF[, metadata_group] <- metadata[, metadata_group]

  write.csv(ratioDF, output_path)

  return(ratioDF)
}

#' Generate a bar plot for the expression ratio created from calculateRatios.
#'
#' A function that generates a bar plot for the expression ratio created from calculateRatios.
#' It is sorted by ratio from lowest to highest and also sorted by metadata_group
#' from the matrix generated from calculateRatios.
#'
#' @param data matrix generated from calculateRatios.
#'
#' @param x_label desired label for the x-axis.
#'
#' @param y_label desired label for the y-axis.
#'
#' @param title desired title for plot.
#'
#' @return Returns the bar plot.
#'
#' @examples
#' ratioDF <- calculateRatios(Normalized_CM, Experiment_info, "ENST00000316724.9",
#'      "ENST00000409400.1", "Biosample_summary")
#' generateBarPlot(ratioDF, "Sample names", "Ratio", "Bar plot")
#'
#' @references
#'   H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
#'
#' @export
#' @import ggplot2
#'
generateBarPlot <- function(data, x_label, y_label, title) {
  barPlot<-ggplot(data=data, aes_string(x=paste("fct_reorder(reorder(",colnames(data)[1], ",", colnames(data)[2], "),", colnames(data)[3],")"), y = colnames(data)[2], fill=colnames(data)[3])) +
    geom_bar(stat = "identity", width=0.5)+
    labs(title = title) +
    theme(text = element_text(size=10), axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.5, size=8), plot.title = element_text(size=16, hjust=0.5))+
    xlab(x_label) +
    ylab(y_label)

  print(barPlot)
  return(barPlot)
}

#' Generate a box plot for the expression ratio created from calculateRatios.
#'
#' A function that generates a box plot for the expression ratio created from calculateRatios for each metadata_group.
#'
#' @param data matrix generated from calculateRatios.
#'
#' @param x_label desired label for the x-axis.
#'
#' @param y_label desired label for the y-axis.
#'
#' @param title desired title for plot.
#'
#' @param order desired ordered list of metadata_group names to sort the boxes in the plot. Default is NULL.
#'
#' @return Returns the box plot.
#'
#' @examples
#' ratioDF <- calculateRatios(Normalized_CM, Experiment_info, "ENST00000316724.9",
#'     "ENST00000409400.1", "Biosample_summary")
#' generateBoxPlot(ratioDF, "Disease Type", "Ratio", "Box plot",
#'     c("Control", "mild cognitive impairment", "Cognitive impairment",
#'     "Alzheimer's disease, Cognitive impairment", "Alzheimer's disease"))
#'
#' generateBoxPlot(ratioDF, "Disease Type", "Ratio", "Box plot")
#'
#' @references
#'   Kassambara A (2023). _ggpubr: 'ggplot2' Based Publication Ready Plots_. R package version 0.6.0,
#'   <https://CRAN.R-project.org/package=ggpubr>.
#'
#'   Wickham H (2022). _stringr: Simple, Consistent Wrappers for Common String Operations_. R package version
#'   1.5.0, <https://CRAN.R-project.org/package=stringr>.
#'
#' @export
#' @import ggpubr
#' @import stringr
generateBoxPlot <- function(data, x_label, y_label, title, order = NULL) {
  boxPlot <- ggboxplot(data=data, x = colnames(data)[3], y = colnames(data)[2],
                       fill = colnames(data)[3], order = order)+
    grids()+
    xlab(x_label) +
    labs(title = title) +
    scale_x_discrete(labels = function(x)
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x=element_text(size=6), text=element_text(size=8), plot.title = element_text(size=16, hjust=0.5), legend.text = element_text(size=8))+
    ylab(y_label)

  print(boxPlot)
}

#[END]


#' Combine all kallisto count matrices to a single data frame.
#'
#' A function that returns a matrix that includes all normalized count matrices where
#' sample names are the columns and transcript ids are the rows. See data/Normalized_CM.rda for an example.
#'
#' @param countdir path to the directory with all the count matrices.
#'
#' @return Returns a matrix that combined every count matrices into one matrix.
#'
#' @references
#'   R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical
#'   Computing, Vienna, Austria. <https://www.R-project.org/>.
#' @export
#' @import tools
combineCounts <- function(countdir) {
  filenames <- tools::file_path_sans_ext(list.files(countdir, pattern="*.tsv", full.names=FALSE))
  sample1 <- read_tsv(paste(countdir,'/', filenames[1], ".tsv", sep=""))
  sample1 <- sample1 %>%
    mutate(target_id = map(strsplit(.$target_id, split = '\\|'), 1) %>% unlist())

  countmatrix <- data.frame(sample1$target_id)
  colnames(countmatrix) <- "TranscriptIDs"
  countmatrix[,filenames[1]] <- as.numeric(sample1$tpm) #tpm is the normalized count (transcript per million)

  for (file in filenames[2:length(filenames)]){
    sampledata <- read_tsv(paste(countdir,'/', file, ".tsv", sep=""))
    countmatrix[,file] <- as.numeric(sampledata$tpm) # add tpm for transcript id for each sample.
  }
  return(countmatrix)
}
#[END]

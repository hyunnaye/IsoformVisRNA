#' Information of the samples provided for testing.
#'
#' This table is a subset of experiment_report.tsv downloaded from the ENCODE Project by clicking the 'Download TSV' button [here](https://www.encodeproject.org/report/?type=Experiment&status=released&internal_tags=RushAD&assay_title=total+RNA-seq&limit=200)
#'
#' @source Barbara Wold Lab at California Institute of Technology, USA
#'
#' @format A matrix with 4 columns
#' \describe{
#'  \item{Accession}{Accession id for that sample}
#'  \item{Biosample_age}{Age of the sample}
#'  \item{Biosample_summary}{Description of the sample's condition}
#'  \item{Quantification_file}{File id for that sample's transcript-level expression counts}
#' }
"Experiment_info"

#' Matrix of all the normalized count matrices combined into one.
#'
#' This table is combines all the normalized expressions from files listed in Quantification_file in Experiment_info.
#' The count matrices were retrieved from the ENCODE Project where they were generated using kallisto.
#' Each row are the normalized transcript-level expression counts where each column are the sample file names.
#'
#' @source Barbara Wold Lab at California Institute of Technology, USA
#'
#' @format A matrix with 24 columns
#' \describe{
#'  \item{TranscriptIDs}{Transcript ID that can be searched on Ensembl}
#'  \item{All other columns}{All other columns are the file names downloaded from the ENCODE Project}
#' }
"Normalized_CM"

#[END] written by Nayeon Hyun

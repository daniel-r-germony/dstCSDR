% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_CSDR_1921_XML.R
\name{read_xml_fchr}
\alias{read_xml_fchr}
\title{Read a DD Form 1921-1 XML File}
\usage{
read_xml_fchr(file_path)
}
\arguments{
\item{file_path}{The file to read.}
}
\value{
A list of two tables (tbl_df): \cr
\describe{
\item{metadata}{The document metadata (e.g., Program Name, Contract Number, Report As Of Date, etc.).}
\item{cost_report}{The WBS level cost report data.}
}
}
\description{
Parse the data for a given file name with a valid extension (i.e., '1921_1.xml').
}
\examples{
\dontrun{\donttest{
file_path <- paste0(system.file("extdata", package = "costTools"), "/CCDR_Data/")
get_files_ccdr(file_path)

cost_report_2 <- read_xml_fchr(paste0(file_path, ccdr_files$dir[2], ccdr_files$file[2]))
}}
}

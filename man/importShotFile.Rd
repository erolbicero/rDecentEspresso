% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importShotFile.R
\name{importShotFile}
\alias{importShotFile}
\title{Import Shot File}
\usage{
importShotFile(filePath, excludeSubset = c("clock"))
}
\arguments{
\item{filePath}{character string containing file path}

\item{excludeSubset}{character vector containing search strings to exclude from import; default='clock'}
}
\value{
a matrix with column headers containing .shot file data
}
\description{
Import data contained in .shot files generated from Decent Espresso profiles.  Imports data excluding
the 'settings' information.
}
\examples{
tmp1 <- importShotFile(paste0(path,file))
dim(head(tmp1))
[1] 6 9

tmp2 <- importShotFile(paste0(path,file),c("clock","goal"))
dim(head(tmp2))
[1]  6 12

}
\references{
https://stackoverflow.com/a/35761217
}
\author{
Erol Biceroglu
}

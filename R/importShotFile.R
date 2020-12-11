#' Import Shot File
#'
#' Import data contained in .shot files generated from Decent Espresso profiles.  Imports data excluding
#' the 'settings' information.
#'
#' @param filePath character string containing file path
#' @param excludeSubset character vector containing search strings to exclude from import; default='clock'
#'
#' @return a matrix with column headers containing .shot file data
#'
#' @author Erol Biceroglu
#'
#' @references
#' https://stackoverflow.com/a/35761217
#'
#' @examples
#' tmp1 <- importShotFile(paste0(path,file))
#' dim(head(tmp1))
#' [1] 6 9
#'
#' tmp2 <- importShotFile(paste0(path,file),c("clock","goal"))
#' dim(head(tmp2))
#' [1]  6 12
#'
#' @export
importShotFile <- function(filePath, excludeSubset = c("clock")) {
  result <- list()
  # https://stackoverflow.com/a/35761217
  con = file(filePath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 | line == "settings {" ) {
      break
    }

    lineSplit <- strsplit(line,"\\{")[[1]]
    headerInfo <- lineSplit[1]
    contentsInfo <- lineSplit[2]

    #clean up brackets
    headerInfo <- gsub(" ","",gsub("\\{","",headerInfo))
    contentsInfo <- gsub("\\}","",contentsInfo)

    #parse and convert to double
    info <- strsplit(contentsInfo," ")[[1]]
    info <- as.double(info)

    info <- list(info)
    names(info) <- headerInfo

    result <- c(result,info)
  }

  close(con)

  excludeIndices <- sapply(excludeSubset
  , function(excludeKey){
    excludeIndex <- sapply(gregexpr(excludeKey,names(result)),`[`,1) #exclude clock
    excludeIndex <- which(excludeIndex > 0)
  })
  if(class(excludeIndices)=="list"){excludeIndices <- do.call(c,excludeIndices)}

  result <- do.call(cbind,result[-c(excludeIndices)])
  return(result)

}

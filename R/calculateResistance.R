#' Calculate Resistance
#'
#' Calculate resistance given pressure and flow data, generated from .shot files.
#'
#' Resistance = Pressure/(Flow^2)
#'
#' @param shotData numeric matrix from .shot file, containing pressure and flow data
#' @param flowColumn character string of column pointing to flow data; default='espresso_flow'
#' @param pressureColumn character string of column pointing to pressure data; default='espresso_pressure'
#' @param resistanceColumnName character string of resulting column name that will be appended with resistance data; default='espresso_resistance'
#'
#' @return original shotData matrix with appended resistance values with name resistanceColumnName
#'
#' @author Erol Biceroglu
#'
#' @references
#' Collin Arneson
#'
#' @examples
#' tmp1 <- importShotFile(paste0(path,file))
#' dim(head(tmp1))
#'[1]  6 12
#' tmp1 <- calculateResistance(tmp1)
#' dim(head(tmp1))
#'[1]  6 13
#'
#' @export
calculateResistance <- function(shotData, flowColumn = "espresso_flow", pressureColumn = "espresso_pressure", resistanceColumnName = "espresso_resistance"){

if(  !(flowColumn %in% colnames(shotData) & pressureColumn %in% colnames(shotData))){
  stop("Error: must have flow and column data")
}

resistance <- resistanceVectorized(flow = shotData[,flowColumn]
                                   , pressure = shotData[,pressureColumn]
                                   )

shotData <- cbind(shotData,resistance)

colnames(shotData)[ncol(shotData)] <- resistanceColumnName

return(shotData)

}

resistanceVectorized <- function(flow,pressure){pressure/(flow^2)}

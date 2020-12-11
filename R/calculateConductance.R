#' Calculate conductance
#'
#' Calculate conductance given pressure and flow data, generated from .shot files.
#'
#' Conductance = (Flow^2)/Pressure
#'
#' @param shotData numeric matrix from .shot file, containing pressure and flow data
#' @param flowColumn character string of column pointing to flow data; default='espresso_flow'
#' @param pressureColumn character string of column pointing to pressure data; default='espresso_pressure'
#' @param conductanceColumnName character string of resulting column name that will be appended with conductance data; default='espresso_conductance'
#'
#' @return original shotData matrix with appended conductance values with name conductanceColumnName
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
#' tmp1 <- calculateConductance(tmp1)
#' dim(head(tmp1))
#'[1]  6 13
#'
#' @export
calculateConductance <- function(shotData, flowColumn = "espresso_flow", pressureColumn = "espresso_pressure", conductanceColumnName = "espresso_conductance"){

  if(  !(flowColumn %in% colnames(shotData) & pressureColumn %in% colnames(shotData))){
    stop("Error: must have flow and column data")
  }

  conductance <- conductanceVectorized(flow = shotData[,flowColumn]
                                     , pressure = shotData[,pressureColumn]
  )

  shotData <- cbind(shotData,conductance)

  colnames(shotData)[ncol(shotData)] <- conductanceColumnName

  return(shotData)

}

conductanceVectorized <- function(flow,pressure){(flow^2)/pressure}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param edgeData
make_igraph <- function(edgeData = CleanedData) {

  igraph_version <- graph_from_data_frame(edgeData)
  
  return(igraph_version)

}

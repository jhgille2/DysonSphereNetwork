#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param edgeData
make_igraph <- function(edgeData = CleanedData) {
  
  # A reduced data set that only has one recipe per
  # component. My (lazy) solution right now is to just
  # keep the first recipe available
  one_recipe_data <- edgeData %>% filter(str_detect(recipe_id, "_1"))

  igraph_full <- graph_from_data_frame(edgeData)
  reduced_graph  <- graph_from_data_frame(one_recipe_data)
  
  res <- list("full" = igraph_full, 
              "reduced" = reduced_graph)
  
  return(res)
}

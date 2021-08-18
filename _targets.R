## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  # The url for the recipe data
  tar_target(Recipe_url, 
             "https://raw.githubusercontent.com/ShanglinMo/DSP-Calculator/master/data/itemInfo_EN.json", 
             format = "url"), 
  
  # Download the recipe data from the URL
  tar_target(Get_Recipes, 
             jsonlite::fromJSON(Recipe_url)),
  
  # Clean up the recipe JSON
  tar_target(CleanedData, 
             clean_recipeData(recipes = Get_Recipes)), 
  
  # Create an igraph object from the cleaned data
  tar_target(Graph, 
             make_igraph(edgeData = CleanedData)), 
  
  # Use set operations to make various subgraphs of the full graph that correspond 
  # to different item production trees seperated by resource requirements
  tar_target(Resource_SubGraphs, 
             make_resource_subgraphs(fullgraph = Graph))

)

tar_load(CleanedData)


createNetworkFromDataFrames(edges = CleanedData)


igraph_version <- graph_from_data_frame(CleanedData)


small_graph <- make_ego_graph(igraph_version, 
                              order = 50, 
                              nodes = "super_magnetic_ring", 
                              mode = "in")[[1]]

# Coalesce the "made_in" attributes
fix_made_in <- function(graph = reduced_graph){
  
  # Indexes of made_in columns
  allattrs  <- get.edge.attribute(graph = graph)
  attrnames <- names(allattrs)
  
  made_in_attrs <- allattrs[which(str_detect(attrnames, "made_in"))]
  
  coalesced_attrs <- coalesce(!!!made_in_attrs)
  
  graph <- set_edge_attr(graph = graph, name = "made_in", value = coalesced_attrs)
  
  return(graph)
}

# A function to make a cytoscape gramp of all inputs to a set of components
cyto_from_nodes <- function(fullgraph = igraph_version, components = c("super_magnetic_ring", "plasma_exciter")){
  reduced_graph <- make_ego_graph(fullgraph, 
                                  order = 50, 
                                  nodes = components, 
                                  mode = "in")
  
  reduced_graph <- do.call(igraph::union, reduced_graph)
  
  createNetworkFromIgraph(fix_made_in(reduced_graph))
}


StarterComponents <- c("circuit_board", 
                       "iron_ingot", 
                       "copper_ingot", 
                       "magnetic_coil", 
                       "electric_motor", 
                       "steel", 
                       "gear", 
                       "plasma_exciter", 
                       "super_magnetic_ring", 
                       "processor", 
                       "microcrystalline_component")

cyto_from_nodes(components = StarterComponents)

InterstellarComps <- c("titanium_alloy", 
                       "particle_container", 
                       "processor")

InterstellarTransport <- cyto_from_nodes(components = c())

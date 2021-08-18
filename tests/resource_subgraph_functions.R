##################################################
## Project: DSP 
## Script purpose: Use set operations to find production
## subgraphs that correspond to various raw resource dependencies
## Date: 2021-07-31
## Author: Jay Gillenwater
##################################################

# Load in the recipe graph
tar_load(Graph)
tar_load(CleanedData)

# The starting planet has crude_oil_seep, iron_ore, copper_ore, stone_ore, and coal_ore as starting resources.
# 1. What components depend on only these resources.
#   - The components that can be made with the resources on the starting planet.
# 2. For each starting resource, find the components that are made with only one raw resource.
#   - Lay out factory by resource.
# 3. Find what components are made using raw resource combinations.
#   - Use to route components between resource factories.

# Starting planet resource veins
starting_veins <- c("iron_vein", 
                    "copper_vein", 
                    "coal_vein", 
                    "stone_vein", 
                    "crude_oil_seep")

# All the other raw resources
other_veins <- CleanedData %>%
  filter(str_detect(source, "_vein")) %>% 
  .$source %>% 
  setdiff(starting_veins)

# A function to get the component dependency graph from a starting resource
get_dependency_graph <- function(fullgraph = Graph, starting_resource = "iron_vein"){
  
  DependencyGraph <- make_ego_graph(graph = fullgraph, 
                                    order = diameter(fullgraph), 
                                    nodes = starting_resource, 
                                    mode = "out")[[1]]
  
  return(DependencyGraph)
}


## Section: Starting planet full graph
##################################################

# Get the union of the graphs for the starting and other veins, then get the difference
# between the two graphs
starting_graphs <- map(starting_veins, function(x) get_dependency_graph(fullgraph = Graph, 
                                                                       starting_resource = x)) %>% 
  set_names(starting_veins)

starting_union <- do.call(igraph::union, starting_graphs)


other_graphs <- map(other_veins, function(x) get_dependency_graph(fullgraph = Graph, 
                                                                 starting_resource = x))
other_union <- do.call(igraph::union, other_graphs)


starting_dependencies <- igraph::difference(starting_union, other_union)

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

# Remove isolated vertices, and fix the "made_in" edge attribute in the union graph (used in cytoscape style)
# This graph is 
starting_dependencies <- delete.vertices(starting_dependencies, degree(starting_dependencies)==0) %>% 
  fix_made_in()


# A function that repeats these steps for arbitrary vectors of available and non-available resources
ResourceSubset <- function(starting_graph = Graph, starting_resources = starting_veins, exclude_resources = other_veins){
  
  # Get the union of the graphs for the starting and other veins, then get the difference
  # between the two graphs
  starting_graphs <- map(starting_resources, function(x) get_dependency_graph(fullgraph = starting_graph, 
                                                                              starting_resource = x)) %>% 
    set_names(starting_resources)
  
  # The union of all the graphs
  starting_union <- do.call(igraph::union, starting_graphs)
  
  # Graphs for the items to exclude
  other_graphs <- map(exclude_resources, function(x) get_dependency_graph(fullgraph = starting_graph, 
                                                                          starting_resource = x))
  other_union <- do.call(igraph::union, other_graphs)
  
  # The difference between these two graphs
  starting_dependencies <- igraph::difference(starting_union, other_union)
  
  removeVertices <- which(names(V(starting_dependencies)) %in% names(V(other_union)))
  
  starting_dependencies <- delete.vertices(starting_dependencies, removeVertices)
  
  # Coalesce the "made_in" attributes
  fix_made_in <- function(graph = reduced_graph){
    
    # Indexes of made_in columns
    allattrs  <- get.edge.attribute(graph = graph)
    attrnames <- names(allattrs)
    
    made_in_attrs          <- allattrs[which(str_detect(attrnames, "made_in"))]
    amount_attrs           <- allattrs[which(str_detect(attrnames, "amount"))]
    production_speed_attrs <- allattrs[which(str_detect(attrnames, "production_speed"))]
    item_qty_attrs         <- allattrs[which(str_detect(attrnames, "item_qty"))]
    
    coalesced_attrs_made_in          <- coalesce(!!!made_in_attrs)
    coalesced_attrs_amount           <- coalesce(!!!amount_attrs)
    coalesced_attrs_production_speed <- coalesce(!!!production_speed_attrs)
    coalesced_attrs_item_qty         <- coalesce(!!!item_qty_attrs)
    
    graph <- set_edge_attr(graph = graph, name = "made_in", value = coalesced_attrs_made_in)
    graph <- set_edge_attr(graph = graph, name = "amount", value = coalesced_attrs_amount)
    graph <- set_edge_attr(graph = graph, name = "production_speed", value = coalesced_attrs_production_speed)
    graph <- set_edge_attr(graph = graph, name = "item_qty", value = coalesced_attrs_item_qty)
    
    return(graph)
  }
  
  # Remove isolated vertices, and fix the "made_in" edge attribute in the union graph (used in cytoscape style)
  # This graph is 
  starting_dependencies <- delete.vertices(starting_dependencies, degree(starting_dependencies)==0) %>% 
    fix_made_in()
  
  return(starting_dependencies)
}

## Section: Resource graphs
##################################################

# The dependency graphs for each of the starting resources, only with the components that can be made on the 
# first planet
starting_resource_dependencies<- map(starting_veins, function(x) get_dependency_graph(fullgraph = starting_dependencies, 
                                                                           starting_resource = x)) %>% 
  set_names(starting_veins)

# A function that returns the difference between a resource specific tree, and the union of the graphs of
# other resource specific trees. 
# The "resource_graphs" argument is a named list of neighborhood graphs
# The "resourceName" argument has to be the name of one of the elements of resource_graphs
get_resource_graph <- function(resource_graphs = starting_resource_dependencies, resourceNames = "iron_vein"){
  
  # The graph of the desired resource
  desired_resource <- resource_graphs[resourceNames]
  desired_resource <- do.call(igraph::union, desired_resource)
  
  otherNames <- names(resource_graphs)[which(!(names(resource_graphs) %in% resourceNames))]
  
  # All the other graphs
  other_resources <- resource_graphs[otherNames]
  
  # Union of all the other graphs
  other_union <- do.call(igraph::union, other_resources)
  
  # Get the vertex names for eaach graph and get the names of the vertexes that are only in 
  # the first graph
  desired_vertices <- V(desired_resource) %>% names()
  other_vertices   <- V(other_union) %>% names()
  
  # The difference between the two
  vertex_diff <- setdiff(desired_vertices, other_vertices)

  # The difference between the single resource graph and the union of the other resource graphs 
  difference_graph <- induced_subgraph(desired_resource, vertex_diff) %>% 
    fix_made_in()
  
  return(difference_graph)
}

# The components that are made with only one of the starting resources
single_resource_dependencies <- map(names(starting_resource_dependencies), function(x) get_resource_graph(starting_resource_dependencies, x)) %>% 
  set_names(names(starting_resource_dependencies))

## Section: Resource combination components
##################################################

# The components that are made with each combination of the starting resources
resource_combinations <- expand_grid(x = names(starting_resource_dependencies), 
                                     y = names(starting_resource_dependencies))
resource_combinations <- resource_combinations[which(!resource_combinations$x == resource_combinations$y),]

Combinations_list <- resource_combinations %>% 
  t() %>%
  as.data.frame() %>% 
  as.list()

names(Combinations_list) <- paste(resource_combinations$x, resource_combinations$y, sep = "__")

# A function to get the components that are created from a combination of raw resources
get_resource_combinations <- function(resource_graphs = starting_resource_dependencies, resourceNames = c("iron_vein", "copper_vein")){
  
  # Get the components that are only made of these two resources
  initial_subgraph <- get_resource_graph(resource_graphs = resource_graphs, 
                                         resourceNames = resourceNames)
  
  # Now use this graph to get the dependency graph for each starting resource
  restricted_dependencies <- map(resourceNames, function(x) get_dependency_graph(fullgraph = initial_subgraph, 
                                                                                  starting_resource = x))
  
  restricted_vertices <- map(restricted_dependencies, function(x) names(V(x))) %>%
    reduce(dplyr::intersect)
  
  graph_intersection <- induced_subgraph(initial_subgraph, restricted_vertices)
  
  return(graph_intersection)
}


resource_combination_dependencies <- map(Combinations_list, function(x) get_resource_combinations(starting_resource_dependencies, x)) %>% 
  set_names(names(Combinations_list))

resource_combination_dependencies <- resource_combination_dependencies[which(map(resource_combination_dependencies, gsize) > 0)]


## Section: Cytoscape vis
##################################################

# Example in cytoscape...
createNetworkFromIgraph(resource_combination_dependencies$iron_vein__copper_vein)

# A function to make a cytoscape graph from these lists
export_to_cytoscape <- function(graphList = single_resource_dependencies){
  
  for(i in 1:length(graphList)){
    createNetworkFromIgraph(graphList[[i]], 
                            title = names(graphList)[[i]])
  }
  
}

export_to_cytoscape(single_resource_dependencies)
export_to_cytoscape(resource_combination_dependencies)
createNetworkFromIgraph(starting_dependencies, title = "Full Starting")


Starting_new <- c("iron_vein", 
                    "copper_vein", 
                    "coal_vein", 
                    "stone_vein", 
                  "titanium_vein", 
                  "fire_ice_vein", 
                  "silicon_vein")

Other_new <- c("crude_oil_seep", 
               "kimberlite_vein", 
               "fractal_silicon_vein", 
               "organic_crystal_vein", 
               "optical_grating_crystal_vein", 
               "spiniform_stalagmite_crystal_vein", 
               "unipolar_magnet_vein")


createNetworkFromIgraph(ResourceSubset(starting_resources = Starting_new, exclude_resources = Other_new), title = "starting")

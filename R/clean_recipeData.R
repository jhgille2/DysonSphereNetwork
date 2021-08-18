#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param recipes
clean_recipeData <- function(recipes = Get_Recipes) {
  
  # A function to return a tidy tibble from a recipe json
  tidy_recipe <- function(recipe, recipeName){
    
    # Pluck out the recipes table from the list
    flatTable <- pluck(recipe, "recipes")
    
    # Add the name of the item to this tibble
    flatTable %<>%
      mutate(itemName = recipeName, 
             recipe_id = paste(recipeName, 1:n(), sep = "_"))
    
    return(flatTable)
  }
  
  Cleaned_Recipes <- map2(recipes$recipe, names(recipes$recipe), tidy_recipe) %>% 
    reduce(rbind) %>% 
    unnest(ingredients) %>% 
    rename(source = name) %>% 
    unnest(byproduct, names_sep = "_") %>% 
    mutate(byproduct_name = ifelse(itemName == "refined_oil", "hydrogen", byproduct_name), 
           byproduct_amount = ifelse(itemName == "refined_oil", 15, byproduct_amount), 
           item_qty = round(amount/production_speed, 4)) %>% 
    select(source, itemName, recipe_id, amount, made_in, production_speed, item_qty) %>% 
    rename(target = itemName)

  return(Cleaned_Recipes)
}

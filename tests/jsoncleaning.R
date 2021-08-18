tar_load(Recipe_url)


recipes <- fromJSON(Recipe_url)

# A function to return a tidy tibble from a recipe json
tidy_recipe <- function(recipe, recipeName){
  
  # Plusk out the recipes table from the list
  flatTable <- pluck(recipe, "recipes")
  
    # Add the name of the item to this tibble
    flatTable %<>%
      mutate(itemName = recipeName, 
             recipe_id = paste(recipeName, 1:n(), sep = "_"))
  
  return(flatTable)
}


tidied_recipes <- map2(recipes$recipe, names(recipes$recipe), tidy_recipe) %>% 
  reduce(rbind)

test <- map2(recipes$recipe, names(recipes$recipe), tidy_recipe) %>% 
  reduce(rbind) %>% 
  unnest(ingredients) %>% 
  rename(source = name) %>% 
  unnest(byproduct, names_sep = "_") %>% 
  mutate(byproduct_name = ifelse(itemName == "refined_oil", "hydrogen", byproduct_name), 
         byproduct_amount = ifelse(itemName == "refined_oil", 15, byproduct_amount)) %>% 
  select(itemName, recipe_id, source, amount, made_in, production_speed, byproduct_name, byproduct_amount)

byproduct_recs <- test %>% 
  filter(!is.na(byproduct_name))

newjson <- fromJSON("https://raw.githubusercontent.com/factoriolab/factorio-lab/main/src/data/dsp/data.json")

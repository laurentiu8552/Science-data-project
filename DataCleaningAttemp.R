library(readr)
library(dplyr)
library(tidyr)
library(readxl)
# Read the CSV file with semicolon delimiter and comma decimal separator
df <- read_excel("jysk_case_competition_final.xlsx")
# Create a mapping of product titles to their known product groups and categories
product_mapping <- df %>%
  filter(!is.na(product_group_level_1) & !is.na(product_category_level_2)) %>%
  select(product_title, product_group_level_1, product_category_level_2) %>%
  distinct()

# Function to impute product group and category based on product title
impute_product_info <- function(df, mapping) {
  # Join the original dataframe with the mapping
  imputed_df <- df %>%
    left_join(mapping, by = "product_title") %>%
    # Use the original values if they exist, otherwise use the mapped values
    mutate(
      product_group_level_1 = coalesce(product_group_level_1.x, product_group_level_1.y),
      product_category_level_2 = coalesce(product_category_level_2.x, product_category_level_2.y)
    ) %>%
    # Remove duplicate columns
    select(-ends_with(".x"), -ends_with(".y"))
  
  return(imputed_df)
}

# Impute missing product groups and categories
imputed_data <- impute_product_info(df, product_mapping)

# Check the results of imputation
missing_before <- df %>%
  filter(is.na(product_group_level_1) | is.na(product_category_level_2))

missing_after <- imputed_data %>%
  filter(is.na(product_group_level_1) | is.na(product_category_level_2))

# Print summary of imputation
cat("Missing entries before imputation:", nrow(missing_before), "\n")
cat("Missing entries after imputation:", nrow(missing_after), "\n")

# Optional: If some entries are still missing, we can do a fuzzy matching
# This requires additional libraries like 'fuzzyjoin'
library(fuzzyjoin)

# Fuzzy matching for any remaining missing entries
fuzzy_impute <- function(df, mapping) {
  # Create a fuzzy join based on string similarity
  fuzzy_mapped <- stringdist_left_join(
    df %>% filter(is.na(product_group_level_1) | is.na(product_category_level_2)),
    mapping,
    by = "product_title",
    max_dist = 3,  # Adjust this threshold as needed
    method = "dl"  # Damerau-Levenshtein distance
  )
  
  # Print any matches found through fuzzy matching
  print("Fuzzy Matches:")
  print(fuzzy_mapped %>% 
          select(product_title.x, product_title.y, 
                 product_group_level_1.y, 
                 product_category_level_2.y))
}

# Uncomment to run fuzzy matching if needed
# fuzzy_impute(df, product_mapping)

# Demonstrate the mapping
print("Product Title to Category Mapping:")
print(product_mapping)
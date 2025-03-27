library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Read the Excel file
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

# Manual categorization for remaining missing entries
final_imputed_data <- imputed_data %>%
  mutate(
    # Product Group Level 1 categorization
    product_group_level_1 = case_when(
      # Furniture items
      is.na(product_group_level_1) & str_detect(product_title, regex("chair|table|sofa|desk|shelf|cabinet|wardrobe|bed|bench|stool", ignore_case = TRUE)) ~ "Furniture",
      # Window items
      is.na(product_group_level_1) & str_detect(product_title, regex("blind|curtain|shade|window", ignore_case = TRUE)) ~ "Windows",
      # Mattresses and bedding
      is.na(product_group_level_1) & str_detect(product_title, regex("mattress|pillow|duvet|bedding|sheet|blanket", ignore_case = TRUE)) ~ "Mattresses",
      # Homeware and decoration
      is.na(product_group_level_1) & str_detect(product_title, regex("frame|decoration|mirror|cushion|rug|carpet|lamp|light", ignore_case = TRUE)) ~ "Homeware",
      # Kitchen items
      is.na(product_group_level_1) & str_detect(product_title, regex("kitchen|plate|cup|bowl|pot|pan|utensil", ignore_case = TRUE)) ~ "Kitchen",
      # Bathroom items
      is.na(product_group_level_1) & str_detect(product_title, regex("bathroom|towel|bath|shower|toilet", ignore_case = TRUE)) ~ "Bathroom",
      # Mark uncategorized items
      is.na(product_group_level_1) ~ "Uncategorized",
      TRUE ~ product_group_level_1
    ),
    # Product Category Level 2 categorization
    product_category_level_2 = case_when(
      # Chairs
      is.na(product_category_level_2) & str_detect(product_title, regex("chair|stool|bench", ignore_case = TRUE)) ~ "Chair",
      # Tables
      is.na(product_category_level_2) & str_detect(product_title, regex("table|desk", ignore_case = TRUE)) ~ "Table",
      # Sofas
      is.na(product_category_level_2) & str_detect(product_title, regex("sofa|couch|settee", ignore_case = TRUE)) ~ "Sofa",
      # Storage
      is.na(product_category_level_2) & str_detect(product_title, regex("shelf|cabinet|wardrobe|drawer|storage", ignore_case = TRUE)) ~ "Storage",
      # Beds
      is.na(product_category_level_2) & str_detect(product_title, regex("bed|mattress|headboard", ignore_case = TRUE)) ~ "Bed",
      # Window coverings
      is.na(product_category_level_2) & str_detect(product_title, regex("blind|curtain|shade", ignore_case = TRUE)) ~ "Window Covering",
      # Decoration
      is.na(product_category_level_2) & str_detect(product_title, regex("frame|mirror|decoration|cushion|rug|carpet", ignore_case = TRUE)) ~ "Decoration",
      # Lighting
      is.na(product_category_level_2) & str_detect(product_title, regex("lamp|light|chandelier", ignore_case = TRUE)) ~ "Lighting",
      # Kitchen items
      is.na(product_category_level_2) & str_detect(product_title, regex("plate|cup|bowl|pot|pan|utensil", ignore_case = TRUE)) ~ "Kitchenware",
      # Bathroom items
      is.na(product_category_level_2) & str_detect(product_title, regex("towel|bath|shower|toilet", ignore_case = TRUE)) ~ "Bathroom Accessories",
      # Mark uncategorized items
      is.na(product_category_level_2) ~ "Uncategorized",
      TRUE ~ product_category_level_2
    )
  )

# Create two versions of the final dataset:
# 1. With uncategorized items marked as "Uncategorized"
final_data_with_uncategorized <- final_imputed_data

# 2. With uncategorized items removed
final_data_clean <- final_imputed_data %>%
  filter(product_group_level_1 != "Uncategorized" & 
         product_category_level_2 != "Uncategorized")

# Print summary of imputation
cat("Original number of rows:", nrow(df), "\n")
cat("Rows with uncategorized items:", nrow(final_data_with_uncategorized %>% 
    filter(product_group_level_1 == "Uncategorized" | 
           product_category_level_2 == "Uncategorized")), "\n")
cat("Rows after removing uncategorized items:", nrow(final_data_clean), "\n")

# Show distribution of product groups and categories
cat("\nDistribution of Product Groups (including uncategorized):\n")
print(table(final_data_with_uncategorized$product_group_level_1))

cat("\nDistribution of Product Categories (including uncategorized):\n")
print(table(final_data_with_uncategorized$product_category_level_2))

# Show examples of uncategorized items
uncategorized_items <- final_data_with_uncategorized %>%
  filter(product_group_level_1 == "Uncategorized" | 
         product_category_level_2 == "Uncategorized")

if (nrow(uncategorized_items) > 0) {
  cat("\nExamples of uncategorized items:\n")
  print(uncategorized_items %>% 
          select(product_title, product_group_level_1, product_category_level_2) %>% 
          head())
}

# You can choose which version of the data to use:
# final_data_with_uncategorized - keeps all rows but marks uncategorized items
# final_data_clean - removes rows that couldn't be categorized
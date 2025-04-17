library(tidyverse)
library(stringr)

# List all CSV files in your D:/wh/ folder
files <- list.files(path = "D:/wh/", pattern = "*.csv", full.names = TRUE)

# Function to read and standardize each file
read_and_standardize <- function(file) {
  year <- str_extract(file, "\\d{4}") %>% as.integer()
  
  df <- read_csv(file, show_col_types = FALSE) %>%
    mutate(year = year) %>%
    rename_with(~str_to_lower(str_replace_all(.x, "[ .]", "_")))
  
  # Standardize column names across years
  df <- df %>%
    rename(
      country = matches("country|country_or_region"),
      happiness_score = matches("happiness_score|score|life_ladder"),
      gdp_per_capita = matches("economy|gdp_per_capita"),
      social_support = matches("social_support|family"),
      life_expectancy = matches("healthy_life_expectancy|life_expectancy"),
      freedom = matches("freedom_to_make_life_choices|freedom"),
      generosity = matches("generosity"),
      corruption = matches("perceptions_of_corruption|corruption"),
      happiness_rank = matches("happiness_rank|overall_rank")
    )
  
  # Convert corruption to numeric if it's character
  if (is.character(df$corruption)) {
    df$corruption <- as.numeric(df$corruption)
  }
  
  return(df)
}

# Combine all files with consistent column types
happiness_combined <- map_df(files, read_and_standardize)

# Save the merged data
write_csv(happiness_combined, "D:/wh/world_happiness_merged.csv")
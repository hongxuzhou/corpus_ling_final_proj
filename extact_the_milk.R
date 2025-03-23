# Load the necessary libraries
library(epubr)
library(here)
library(stringr)
library(xml2)
library(rvest)
library(purrr)
library(dplyr)
library(tidyr)

# 1. Define file path
file_path <- here("raw_data", "Fortunately_the Milk.epub")
book_id <- "FTM"

# 2. Use `epubR` to read epub content and metadata
file_data <- epub(file_path)

# 3. Extract metadata
book_metadata <- file_data |> 
  select(-data) |> # Exclude the nested data column
  as.list()

#  4. Extract data nested in the `data` column
text_data <- file_data$data[[1]]

# 5. Identify the structure of the data
# find the model of the sections and the chapter markers
print(text_data$section)

# The results show that the chapters start from "Chapter_"
# So we can use regex to extract them

chapters <- text_data |> 
  # First, filter the chapters 
  filter(str_detect(section, "Chapter_")) |> 
  # Then, extract the chapter number
  mutate(
    chapter_num = as.numeric(str_extract(section, "\\d+")),
    # create chapter titles
    chapter_title = paste("Chapter", chapter_num)
  ) |> 
  # Rank based on chapter number
  arrange(chapter_num)

# 6. Build discourse-level corpus by combining all the chapters
ftm_corpus <- chapters |> 
  mutate(
    # Add metadata
    book_id = book_id,
    title = book_metadata$title, 
  ) |> 
  # rank the columns 
  select(book_id, 
         title, 
         chapter_num, 
         chapter_title,
         nword,
         nchar,
         text,
         section)

print(ftm_corpus)
  
  

# Load the necessary libraries
library(epubr)
library(here)
library(stringr)
library(dplyr)
library(tidyr)

# 1. Define file path and book identifier
file_path <- here("raw_data", "The Graveyard Book.epub")
book_id <- "TGB"  # Short identifier for The Graveyard Book

# 2. Use `epubR` to read epub content and metadata
file_data <- epub(file_path)

# 3. Extract metadata
book_metadata <- file_data |> 
  select(-data) |> # Exclude the nested data column
  as.list()

# 4. Extract data nested in the `data` column
text_data <- file_data$data[[1]]

# 5. Based on the screenshot, we know which section IDs correspond to chapters
# Create a mapping of section IDs to chapter numbers
chapter_mapping <- tibble(
  section = c("id123", "id121", "id119", "id117", "id115", "id112", "id110", "id18"),
  chapter_num = 1:8,
  chapter_title = c("Chapter 1", "Chapter 2", "Chapter 3", "Chapter 4", 
                    "Chapter 5", "Chapter 6", "Chapter 7", "Chapter 8")
)

# 6. Filter and process chapters using the mapping
chapters <- text_data |> 
  # Filter to include only the sections that are actual chapters
  filter(section %in% chapter_mapping$section) |> 
  # Join with the chapter mapping to get chapter numbers and titles
  left_join(chapter_mapping, by = "section") |> 
  # Sort by chapter number
  arrange(chapter_num)

# 7. Build discourse-level corpus
tgb_corpus <- chapters |> 
  mutate(
    # Add metadata
    book_id = book_id,
    title = book_metadata$title
  ) |> 
  # Organize columns
  select(book_id, 
         title, 
         chapter_num, 
         chapter_title,
         nword,
         nchar,
         text,
         section)

# Print the resulting corpus to check
print(tgb_corpus)

# 8. Save the corpus
# Save as RDS (R's native binary format for preserving objects)
saveRDS(tgb_corpus, here("corpora", paste0(book_id, "_corpus.rds")))

# Save a TSV file as a backup
write.table(tgb_corpus, 
            here("corpora_backup", 
                 paste0(book_id, "_corpus.tsv")),
            sep = "\t",
            row.names = FALSE,
            quote = TRUE)


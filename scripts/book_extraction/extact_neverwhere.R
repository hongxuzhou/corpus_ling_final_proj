# Load necessary libraries
library(epubr)
library(here)
library(stringr)
library(dplyr)
library(tidyr)

# 1. Define file path and book ID
file_path <- here("raw_data", "Neverwhere.epub")
book_id <- "NW"  # Short identifier for Neverwhere

# 2. Read the epub content
file_data <- epub(file_path)

# 3. Extract metadata
book_metadata <- file_data |> 
  select(-data) |> 
  as.list()

# 4. Extract text data
text_data <- file_data$data[[1]]

# 5. Create a manual mapping from section IDs to chapter numbers
# This ensures we correctly handle the non-consecutive IDs
section_to_chapter <- tibble(
  section = c("id122", "id121", "id120", "id119", "id118", "id117", 
              "id116", "id115", "id114", "id113", "id112", "id111", 
              "id110", "id19", "id18", "id17", "id16", "id15", 
              "id14", "id13", "id12"),
  chapter_num = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 
                  14, 15, 16, 17, 18, 19, 20),
  chapter_title = c("Prologue", 
                    paste("Chapter", 1:20))
)

# 6. Filter relevant sections and apply the mapping
neverwhere_corpus <- text_data |> 
  # Only include sections in our mapping
  filter(section %in% section_to_chapter$section) |>
  # Join with our mapping table
  left_join(section_to_chapter, by = "section") |>
  # Create consistent chapter_id format
  mutate(
    chapter_id = paste0("Chapter_", chapter_num),
    book_id = book_id,
    title = "Neverwhere"
  ) |>
  # Select and order the final columns
  select(book_id, title, chapter_id, chapter_num, chapter_title, nword, nchar, text, section) |>
  # Sort by chapter number
  arrange(chapter_num)

# 7. Print preview to verify results
print("Final corpus preview:")
print(head(neverwhere_corpus, 3))
print(tail(neverwhere_corpus, 3))

# 8. Save the corpus
saveRDS(neverwhere_corpus, here("corpora", paste0(book_id, "_corpus.rds")))

write.table(neverwhere_corpus, 
            here("corpora_backup", paste0(book_id, "_corpus.tsv")),
            sep = "\t",
            row.names = FALSE,
            quote = TRUE)


# Load the necessary libraries
library(epubr)
library(here)
library(stringr)
library(dplyr)
library(tidyr)

# 1. Define file path and book ID
file_path <- here("raw_data", "The Ocean at the End of the Lane.epub")
book_id <- "OCEAN"

# 2. Use `epubr` to read epub content and metadata
file_data <- epub(file_path)

# 3. Extract metadata
book_metadata <- file_data %>% 
  select(-data) %>% # Exclude the nested data column
  as.list()

# 4. Extract data nested in the `data` column
text_data <- file_data$data[[1]]

# 5. Print the structure to verify
print(head(text_data))

# 6. Manual mapping of section IDs to chapters based on your observations
# Create a lookup table for mapping section IDs to chapter numbers
chapter_mapping <- tribble(
  ~section, ~chapter_num, ~chapter_title,
  "id34",   0,            "Prologue",
  "id35",   1,            "Chapter 1",
  "id36",   2,            "Chapter 2",
  "id37",   3,            "Chapter 3",
  "id38",   4,            "Chapter 4",
  "id39",   5,            "Chapter 5",
  "id40",   6,            "Chapter 6",
  "id41",   7,            "Chapter 7",
  "id42",   8,            "Chapter 8",
  "id43",   9,            "Chapter 9",
  "id44",   10,           "Chapter 10",
  "id45",   11,           "Chapter 11",
  "id46",   12,           "Chapter 12",
  "id47",   13,           "Chapter 13",
  "id48",   14,           "Chapter 14",
  "id49",   15,           "Chapter 15",
  "id50",   16,           "Epilogue"
)

# 7. Filter text_data to include only the main body (prologue to epilogue)
# and join with chapter mapping
ocean_corpus <- text_data %>%
  # Join with the chapter mapping
  inner_join(chapter_mapping, by = "section") %>%
  # Add metadata
  mutate(
    book_id = book_id,
    title = "The Ocean at the End of the Lane"
  ) %>%
  # Arrange by chapter number
  arrange(chapter_num) %>%
  # Select and reorder columns
  select(
    book_id,
    title,
    chapter_num,
    chapter_title,
    nword,
    nchar,
    text,
    section
  )

# 8. Print sample of the corpus to verify
print(head(ocean_corpus))

# 9. Save the corpus
# Will save as RDS as the main corpus
saveRDS(ocean_corpus, here("corpora", paste0(book_id, "_corpus.rds")))


# 10. Save a TSV file as a backup
# Don't use CSV because of punctuation in the text
write.table(
  ocean_corpus, 
  here("corpora_backup", paste0(book_id, "_corpus.tsv")),
  sep = "\t",
  row.names = FALSE,
  quote = TRUE  # Important to use quote=TRUE for text with tab characters
)

# 11. Optional: Generate summary statistics
cat("\nCorpus Summary:\n")
cat("Number of chapters:", nrow(ocean_corpus), "\n")
cat("Total word count:", sum(ocean_corpus$nword), "\n")
cat("Total character count:", sum(ocean_corpus$nchar), "\n")


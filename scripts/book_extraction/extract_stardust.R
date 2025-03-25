# Load the necessary libraries
library(epubr)
library(here)
library(stringr)
library(dplyr)
library(tidyr)

# 1. Define file path and book ID
file_path <- here("raw_data", "Stardust.epub")
book_id <- "ST"  # Short ID for Stardust

# 2. Use epubr to read epub content and metadata
file_data <- epub(file_path)

# 3. Extract metadata
book_metadata <- file_data |> 
  select(-data) |> # Exclude the nested data column
  as.list()

# 4. Extract data nested in the `data` column
text_data <- file_data$data[[1]]

# 5. Create a manual mapping of section IDs to chapter numbers
# Based on the screenshot, id113 is chapter 1, id112 is chapter 2, etc.
chapter_mapping <- tribble(
  ~section, ~chapter_num, ~chapter_title,
  "id113",   1,           "Chapter 1",
  "id112",   2,           "Chapter 2",
  "id111",   3,           "Chapter 3",
  "id110",   4,           "Chapter 4",
  "id19",    5,           "Chapter 5",
  "id18",    6,           "Chapter 6",
  "id17",    7,           "Chapter 7",
  "id16",    8,           "Chapter 8",
  "id15",    9,           "Chapter 9",
  "id14",    10,          "Chapter 10"
)

# 6. Filter and join with the mapping to create the corpus
st_corpus <- text_data %>%
  # Join with our manual mapping
  inner_join(chapter_mapping, by = "section") %>%
  # Extract chapter subtitle (first line after chapter heading)
  mutate(
    # Clean the chapter heading and extract subtitle
    subtitle = str_remove(text, "^CHAPTER\\s+[A-Z]+") %>%
      str_trim() %>%
      str_extract("^[^\n]+")
  ) %>%
  # Add book metadata
  mutate(
    book_id = book_id,
    title = "Stardust"
  ) %>%
  # Reorder columns for clarity
  select(
    book_id,
    title,
    chapter_num,
    chapter_title,
    #subtitle,      # All wrong and useless
    section,       # Original section ID
    nword,
    nchar,
    text
  ) %>%
  # Make sure chapters are in correct order
  arrange(chapter_num)

# 7. Save the corpus
 saveRDS(st_corpus, here("corpora", paste0(book_id, "_corpus.rds")))
# 
 # Save a TSV backup
 write.table(
   st_corpus,
   here("corpora_backup", paste0(book_id, "_corpus.tsv")),
   sep = "\t",
   row.names = FALSE,
   quote = TRUE  # Using quote=TRUE for story text with special characters
 )

# Preview the result
print(st_corpus)

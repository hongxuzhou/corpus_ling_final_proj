# Load the necessary libraries
library(epubr)
library(here)
library(stringr)
library(dplyr)
library(tidyr)

# 1. Define file path and book ID
file_path <- here("raw_data", "Smoke and Mirrors.epub")
book_id <- "SM"  # Short ID for Smoke and Mirrors

# 2. Use epubr to read epub content and metadata
file_data <- epub(file_path)

# 3. Extract metadata
book_metadata <- file_data |> 
  select(-data) |> # Exclude the nested data column
  as.list()

# 4. Extract data nested in the `data` column
text_data <- file_data$data[[1]]

# 5. Create a mapping of the story sections to chapter numbers
# The stories start at id145 and end at id111 (non-consecutive)

# Get only the story sections (filtering out front/back matter)
story_sections <- text_data %>%
  # Filter sections that contain actual stories
  # This is based on your comment that stories start at id145 and end at id111
  filter(
    # Extract the numeric part from section ID
    as.numeric(str_extract(section, "\\d+")) <= 145,
    as.numeric(str_extract(section, "\\d+")) >= 111
  ) %>%
  # We need to sort by the order they appear in the book, not by ID number
  # Since the IDs are in descending order based on your screenshot
  arrange(desc(as.numeric(str_extract(section, "\\d+"))))

# 6. Create a new corpus with chapter numbering
sm_corpus <- story_sections %>%
  # Add chapter numbers sequentially
  mutate(
    # Extract the story ID number for reference
    story_id = as.numeric(str_extract(section, "\\d+")),
    # Create sequential chapter numbers
    chapter_num = row_number(),
    # Get the title of each story from the first line of text
    #story_title = str_trim(str_extract(text, "^[^\\.\\n]+")),
    # Forget about it, wont use it anyway
    # Create standardized chapter titles
    chapter_title = paste("Chapter", chapter_num)
  ) %>%
  # Add book metadata
  mutate(
    book_id = book_id,
    title = "Smoke and Mirrors"
  ) %>%
  # Reorder columns for clarity
  select(
    book_id,
    title,
    chapter_num,
    chapter_title,
    #story_title,   # Keep the original story title
    section,       # Original section ID
    nword,
    nchar,
    text
  )

# 7. Save the corpus
 saveRDS(sm_corpus, here("corpora", paste0(book_id, "_corpus.rds"))) 

# # Save a TSV backup
 write.table(
   sm_corpus,
   here("corpora_backup", paste0(book_id, "_corpus.tsv")),
   sep = "\t",
   row.names = FALSE,
   quote = TRUE  # Using quote=TRUE for story text with special characters
 )

# Preview the result
print(sm_corpus)
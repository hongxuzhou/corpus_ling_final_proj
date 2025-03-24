# Load necessary libraries
library(epubr)
library(here)
library(stringr)
library(dplyr)
library(tidyr)

# 1. Define file path and book ID
file_path <- here("raw_data", "Odd and the Frost Giants.epub")
book_id <- "OFG"  # Short ID for Odd and the Frost Giants

# 2. Use epubr to read epub content and metadata
file_data <- epub(file_path)

# 3. Extract metadata
book_metadata <- file_data |> 
  select(-data) |> # Exclude the nested data column
  as.list()

# 4. Extract data nested in the `data` column
text_data <- file_data$data[[1]]

# 5. Examine the structure of the data
print("Section IDs available:")
print(text_data$section)

# 6. Filter to include only the main content (id115 to id123)
# These IDs correspond to the chapters and content based on your screenshot
main_content <- text_data |> 
  filter(section %in% paste0("id", 115:123))

# 7. Identify and extract chapter information using regex
# The pattern looks like "CHAPTER 1ODD", "CHAPTER 2THE FOX...", etc.
chapters <- main_content |> 
  # Filter for sections that contain chapter headings
  filter(str_detect(text, "^CHAPTER \\d")) |> 
  # Extract chapter information
  mutate(
    # First extract the full chapter header (e.g., "CHAPTER 1ODD")
    chapter_header = str_extract(text, "^CHAPTER \\d+[A-Z]+[A-Z\\s,']*"),
    
    # Extract chapter number
    chapter_num = as.numeric(str_extract(chapter_header, "\\d+")),
    
    # Extract chapter title by removing the "CHAPTER N" part
    chapter_title = str_replace(chapter_header, "^CHAPTER \\d+", ""),
    
    # Clean up chapter title
    chapter_title = str_trim(chapter_title)
  ) |> 
  # Arrange by chapter number to ensure correct order
  arrange(chapter_num)

# 8. Build discourse-level corpus by combining all the chapters
ofg_corpus <- chapters |> 
  mutate(
    # Add metadata
    book_id = book_id,
    title = "Odd and the Frost Giants",
    # Extract full text content for analysis
    full_text = text,
    # Create a standardized chapter identifier
    chapter_id = paste0(book_id, "_CH", sprintf("%02d", chapter_num))
  ) |> 
  # Select and arrange columns for the final corpus
  select(
    book_id,
    title,
    chapter_id,
    chapter_num,
    chapter_title,
    nword,
    nchar,
    full_text,
    section
  )

# Print summary of the created corpus
print("Corpus summary:")
print(summary(ofg_corpus))
print(paste("Total chapters extracted:", nrow(ofg_corpus)))
print(paste("Total words in corpus:", sum(ofg_corpus$nword)))

# 9. Save the corpus
# First create directories if they don't exist
dir.create(here("corpora"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("corpora_backup"), showWarnings = FALSE, recursive = TRUE)

# Save as RDS (preferred format for R objects)
saveRDS(ofg_corpus, here("corpora", paste0(book_id, "_corpus.rds")))

# Save as TSV for backup/compatibility
write.table(
  ofg_corpus,
  file = here("corpora_backup", paste0(book_id, "_corpus.tsv")),
  sep = "\t",
  row.names = FALSE,
  quote = TRUE
)

print("Corpus saved successfully!")

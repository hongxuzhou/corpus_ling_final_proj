# Load necessary libraries
library(epubr)
library(here)
library(stringr)
library(dplyr)
library(tidyr)

# 1. Define file path and book ID
file_path <- here("raw_data", "Coraline.epub")
book_id <- "CL"  # Short identifier for Coraline

# 2. Read the epub content
file_data <- epub(file_path)

# 3. Extract metadata
book_metadata <- file_data |> 
  select(-data) |> 
  as.list()

# 4. Extract text data
text_data <- file_data$data[[1]]

# 5. Filter only the chapter sections (id139 to id127)
# These are the actual chapters based on your screenshot
chapters <- text_data |> 
  filter(as.numeric(str_extract(section, "\\d+")) >= 127,
         as.numeric(str_extract(section, "\\d+")) <= 139) |>
  # Sort in descending order (id139 to id127)
  arrange(desc(as.numeric(str_extract(section, "\\d+"))))

# 6. Extract chapter information
# For Coraline, chapter titles use Roman numerals
chapters <- chapters |> 
  mutate(
    # Extract Roman numeral from the start of the text
    roman_numeral = str_extract(text, "^[IVX]+\\."),
    # Remove the period from the Roman numeral
    roman_numeral_clean = str_replace(roman_numeral, "\\.", ""),
    # Convert Roman numeral to chapter number
    chapter_num = match(roman_numeral_clean, c("I", "II", "III", "IV", "V", 
                                               "VI", "VII", "VIII", "IX", 
                                               "X", "XI", "XII", "XIII")),
    # Extract chapter title (everything after the Roman numeral and period)
    chapter_title = str_trim(str_replace(str_extract(text, "^[IVX]+\\.[A-Z ]+"), 
                                         roman_numeral, "")),
    # Combine for a clean chapter identifier
    chapter_id = paste0("Chapter_", chapter_num)
  ) |>
  # Re-sort by chapter number
  arrange(chapter_num)

# 7. Build the corpus structure
coraline_corpus <- chapters |> 
  mutate(
    # Add metadata
    book_id = book_id,
    title = book_metadata$title,
    # Format chapter info consistently
    chapter_title = paste(roman_numeral_clean, chapter_title)
  ) |> 
  # Select and organize columns
  select(book_id, 
         title, 
         chapter_id,
         chapter_num,
         chapter_title,
         nword,
         nchar,
         text,
         section)

# Print a preview of the corpus
print(head(coraline_corpus))

# 8. Save the corpus
# Save as RDS (R's native data format)
saveRDS(coraline_corpus, here("corpora", paste0(book_id, "_corpus.rds")))

# Save as TSV for backup (more reliable than CSV for text with punctuation)
write.table(coraline_corpus, 
            here("corpora_backup", paste0(book_id, "_corpus.tsv")),
            sep = "\t",
            row.names = FALSE,
            quote = TRUE)  # Set quote=TRUE for text containing delimiters

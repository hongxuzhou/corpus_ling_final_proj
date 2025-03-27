# Corpus Extraction Script for American Gods
# This script extracts the 20 main chapters and handles special sections

# 1. Load required libraries
library(epubr)
library(here)
library(stringr)
library(dplyr)

# 2. Define file path and book ID
file_path <- here("raw_data", "American Gods.epub")
book_id <- "AG"

# 3. Read the EPUB file
file_data <- epub(file_path)
text_data <- file_data$data[[1]]

# 4. Create direct mapping for chapters with word boundaries 
# to avoid partial matches (FOUR vs FOURTEEN)
chapter_patterns <- c(
  "ONE" = 1, "TWO" = 2, "THREE" = 3, "FOUR" = 4, "FIVE" = 5,
  "SIX" = 6, "SEVEN" = 7, "EIGHT" = 8, "NINE" = 9, "TEN" = 10,
  "ELEVEN" = 11, "TWELVE" = 12, "THIRTEEN" = 13, "FOURTEEN" = 14, "FIFTEEN" = 15,
  "SIXTEEN" = 16, "SEVENTEEN" = 17, "EIGHTEEN" = 18, "NINETEEN" = 19, "TWENTY" = 20
)

# 5. Identify chapters and special sections
processed_data <- text_data |>
  mutate(
    # Start with no chapter assignment
    chapter_num = NA_integer_,
    # Mark if this is a main chapter start
    is_chapter_start = FALSE
  )

# Apply main chapter identification
for (i in 1:nrow(processed_data)) {
  # Check for main chapter patterns
  for (j in 1:length(chapter_patterns)) {
    pattern_name <- names(chapter_patterns)[j]
    chapter_num <- chapter_patterns[j]
    
    # Use word boundary to avoid partial matches
    if (str_detect(processed_data$text[i], paste0("^CHAPTER ", pattern_name, "\\b"))) {
      processed_data$chapter_num[i] <- chapter_num
      processed_data$is_chapter_start[i] <- TRUE
      break
    }
  }
}

# 6. Handle special sections
# Assign interludes to Chapter 12
interlude_rows <- which(str_detect(processed_data$text, "^INTERLUDE"))
processed_data$chapter_num[interlude_rows] <- 12

# Assign postscript to Chapter 20
postscript_rows <- which(str_detect(processed_data$text, "^Postscript"))
processed_data$chapter_num[postscript_rows] <- 20

# Handle "Coming to America" and other special narrative sections
special_rows <- which(str_detect(processed_data$text, 
                                 "^Coming to America|^Somewhere in America|^Meanwhile") & 
                        is.na(processed_data$chapter_num))

for (idx in special_rows) {
  # Find preceding chapters
  prev_chapters <- which(processed_data$is_chapter_start & (1:nrow(processed_data) < idx))
  if (length(prev_chapters) > 0) {
    closest_chapter <- max(prev_chapters)
    processed_data$chapter_num[idx] <- processed_data$chapter_num[closest_chapter]
  }
}

# 7. Filter and combine text by chapter
final_corpus <- processed_data |>
  # Keep only rows with assigned chapters and exclude PART dividers
  filter(!is.na(chapter_num), !str_detect(text, "^PART ")) |>
  # Group by chapter number only
  group_by(chapter_num) |>
  # Combine content within each chapter (using .groups='drop' to avoid issues)
  summarize(
    # Keep the section ID from the main chapter (first occurrence)
    section = first(section[is_chapter_start]),
    # Create proper chapter title
    chapter_title = paste("CHAPTER", names(chapter_patterns)[match(first(chapter_num), chapter_patterns)]),
    # Combine all text for this chapter
    text = paste(text, collapse = "\n\n"),
    # Recalculate counts
    nword = sum(nword),
    nchar = sum(nchar),
    .groups = 'drop'
  ) |>
  # Remove any potential duplicates
  distinct() |>
  # Ensure chapters are in order
  arrange(chapter_num) |>
  # Add remaining metadata
  mutate(
    book_id = book_id,
    title = "American Gods",
    chapter_id = sprintf("%s_CH%02d", book_id, chapter_num)
  ) |>
  # Organize columns
  select(
    book_id,
    title,
    chapter_id,
    chapter_num,
    chapter_title,
    nword,
    nchar,
    text,
    section
  )

# 8. Save the corpus
saveRDS(final_corpus, here("corpora", paste0(book_id, "_corpus.rds")))
write.table(final_corpus, 
            here("corpora_backup", paste0(book_id, "_corpus.tsv")),
            sep = "\t",
            row.names = FALSE,
            quote = TRUE)

# 9. Output summary
cat(sprintf("Processing complete: Extracted %d chapters from 'American Gods'.\n", 
            nrow(final_corpus)))
cat(sprintf("Total word count: %d\n", sum(final_corpus$nword)))
print(final_corpus |> select(chapter_num, chapter_title))

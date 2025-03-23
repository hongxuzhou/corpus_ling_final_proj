library(epubr)
library(here)

# Trial process the first book 
file <- "/Users/hongxuzhou/corpus_linguistics/final_project/raw_data/Coraline.epub"
coraline <- epub(file)
coraline
coraline_meta <- epub_meta(file)
file2 <- here("raw_data", "Fortunately_the Milk.epub")
milk <- epub(file2)

# Try to read the content of the book 
milk_text <- epub_unzip(file2)
milk_text
coraline_text <- epub_unzip(file)
coraline_text

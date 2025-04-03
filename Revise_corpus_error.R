# the book AB has two chapter 4 -- AB_ch4. Meanwhile, chapter 14 (AB_ch14) is 2k word less than the bigger chapter 4. That means the bigger chapter 4 may have interlogue combined. 
# I need to first remove the current chapter 14, then renanme the bigger chapter 4 to chapte 14
# next, since the mistake is in the document level, I need to re-run the whole process to get the correct sub-corpora.

# Import the corpus AB
AB_corpus <- readRDS(here("corpora", "AB_corpus.rds"))

# 1. Remove the current chapter 14 entry
AB_corpus <- AB_corpus %>%
  filter(!(chapter_num == 14))

# 2. Update the mislabelled chapter 4 to chapter 14
AB_corpus <- AB_corpus %>%
  mutate(
    chapter_num = ifelse(chapter_id == "AB_CH04" & section == "id14", 14, chapter_num),
    chapter_id = ifelse(chapter_id == "AB_CH04" & section == "id14", "AB_CH14", chapter_id),
    chapter_title = ifelse(section == "id14", "CHAPTER FOURTEEN: TEENWHICH COMES TO SEVERAL CONCLUSIONS", chapter_title)
  )

# 3. 保存语料库
# 主语料库保存为RDS
saveRDS(AB_corpus, here("corpora", "AB_corpus.rds"))

# 保存TSV备份
write.table(AB_corpus, 
            here("corpora_backup",  "AB_corpus.tsv"),
            sep = "\t",
            row.names = FALSE,
            quote = TRUE)

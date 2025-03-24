# 处理Anansi Boys的修改代码 - 修复第14章捕获问题
library(epubr)
library(here)
library(stringr)
library(dplyr)
library(tidyr)

# 1. 定义文件路径
file_path <- here("raw_data", "Anansi Boys.epub")
book_id <- "AB"

# 2. 使用epubR读取epub内容和元数据
file_data <- epub(file_path)

# 3. 提取元数据
book_metadata <- file_data |> 
  select(-data) |> 
  as.list()

# 4. 提取数据
text_data <- file_data$data[[1]]

# 5. 首先检查标记问题 - 输出所有可能的章节开始
chapter_patterns <- text_data |>
  filter(str_detect(text, "^CHAPTER")) |>
  select(section, text)

print("可能的章节标记:")
print(chapter_patterns)

# 6. 处理章节与非章节内容 - 使用更强健的模式匹配
processed_chapters <- text_data |>
  # 识别章节开始 - 使用更宽松的匹配模式
  mutate(
    # 检测章节开始 - 简化模式，只检查以CHAPTER开头
    is_chapter = str_detect(text, "^CHAPTER (ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN|ELEVEN|TWELVE|THIRTEEN|FOURTEEN)"),
    
    # 提取章节标记部分
    chapter_label = str_extract(text, "^CHAPTER (ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN|ELEVEN|TWELVE|THIRTEEN|FOURTEEN)"),
    
    # 将章节标记转换为数字 - 使用更简单的匹配
    chapter_num = case_when(
      str_detect(chapter_label, "ONE") ~ 1,
      str_detect(chapter_label, "TWO") ~ 2,
      str_detect(chapter_label, "THREE") ~ 3,
      str_detect(chapter_label, "FOUR") ~ 4,
      str_detect(chapter_label, "FIVE") ~ 5,
      str_detect(chapter_label, "SIX") ~ 6,
      str_detect(chapter_label, "SEVEN") ~ 7,
      str_detect(chapter_label, "EIGHT") ~ 8,
      str_detect(chapter_label, "NINE") ~ 9,
      str_detect(chapter_label, "TEN") ~ 10,
      str_detect(chapter_label, "ELEVEN") ~ 11,
      str_detect(chapter_label, "TWELVE") ~ 12,
      str_detect(chapter_label, "THIRTEEN") ~ 13,
      str_detect(chapter_label, "FOURTEEN") ~ 14,
      TRUE ~ NA_real_
    )
  ) |>
  # 在创建章节组前输出调试信息
  mutate(
    debug_info = paste(row_number(), section, is_chapter, chapter_num, str_sub(text, 1, 30), sep=" | ")
  )

# 输出调试信息 - 特别检查id14和相关章节
print("调试信息 - 章节检测:")
print(processed_chapters |> 
        filter(str_detect(section, "id14|id13|id15") | 
                 chapter_num == 14 | 
                 str_detect(text, "FOURTEEN")) |> 
        select(section, is_chapter, chapter_num, debug_info))

# 继续正常处理
processed_chapters <- processed_chapters |>
  # 为每个内容分配章节组
  mutate(
    # 每当遇到新章节时组号增加
    chapter_group = cumsum(is_chapter)
  ) |>
  # 按章节分组
  group_by(chapter_group) |>
  # 合并每个章节组内的文本
  summarize(
    section = first(section[is_chapter]),
    chapter_num = first(chapter_num),
    chapter_label = first(chapter_label),
    
    # 合并文本
    text = paste(text, collapse = "\n\n"),
    
    # 计算总字数和字符数
    nword = sum(nword),
    nchar = sum(nchar)
  ) |>
  # 确保有章节号
  filter(!is.na(chapter_num)) |>
  # 按章节号排序
  arrange(chapter_num)

# 7. 检查是否找到了第14章
if (!any(processed_chapters$chapter_num == 14)) {
  print("警告：未找到第14章！进行手动查找...")
  
  # 手动查找第14章
  ch14_row <- which(str_detect(text_data$text, "CHAPTER FOURTEEN"))
  
  if (length(ch14_row) > 0) {
    print(paste("在行", ch14_row, "找到第14章标记"))
    print(paste("文本开头:", str_sub(text_data$text[ch14_row], 1, 50)))
    
    # 手动添加第14章
    manual_ch14 <- tibble(
      section = text_data$section[ch14_row],
      chapter_num = 14,
      chapter_label = "CHAPTER FOURTEEN",
      text = text_data$text[ch14_row],
      nword = text_data$nword[ch14_row],
      nchar = text_data$nchar[ch14_row]
    )
    
    processed_chapters <- bind_rows(processed_chapters, manual_ch14) |>
      arrange(chapter_num)
  }
}

# 8. 构建最终语料库
final_corpus <- processed_chapters |>
  mutate(
    # 添加元数据
    book_id = book_id,
    title = book_metadata$title,
    
    # 创建章节标题 - 提取描述部分
    chapter_subtitle = str_replace(text, "^CHAPTER (ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN|ELEVEN|TWELVE|THIRTEEN|FOURTEEN).*?(\\w+.*?)(?=\\s|$)", "\\2"),
    chapter_title = paste0(chapter_label, ": ", str_trim(chapter_subtitle)),
    
    # 创建规范化的章节ID
    chapter_id = sprintf("%s_CH%02d", book_id, chapter_num)
  ) |>
  # 整理列顺序
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

# 9. 特殊处理 ANANSI AND BIRD
anansi_bird_row <- which(str_detect(text_data$text, "^ANANSI AND BIRD"))
if (length(anansi_bird_row) > 0) {
  # 找到需要合并的章节 - 应该是第7章
  ch7_idx <- which(final_corpus$chapter_num == 7)
  
  if (length(ch7_idx) > 0) {
    # 获取要合并的文本
    anansi_bird_text <- text_data$text[anansi_bird_row]
    
    # 合并到第7章
    final_corpus$text[ch7_idx] <- paste(final_corpus$text[ch7_idx], 
                                        "\n\n", 
                                        anansi_bird_text)
    
    # 更新字数和字符数
    final_corpus$nword[ch7_idx] <- final_corpus$nword[ch7_idx] + 
      text_data$nword[anansi_bird_row]
    final_corpus$nchar[ch7_idx] <- final_corpus$nchar[ch7_idx] + 
      text_data$nchar[anansi_bird_row]
  }
}

# 10. 保存语料库
# 主语料库保存为RDS
saveRDS(final_corpus, here("corpora", paste0(book_id, "_corpus.rds")))

# 保存TSV备份
write.table(final_corpus, 
            here("corpora_backup", paste0(book_id, "_corpus.tsv")),
            sep = "\t",
            row.names = FALSE,
            quote = TRUE)

# 11. 输出处理摘要和检查
cat(sprintf("处理完成: 从《%s》中提取了%d个章节。\n", 
            book_metadata$title, nrow(final_corpus)))
cat(sprintf("总字数: %d\n", sum(final_corpus$nword)))

# 12. 检查是否所有章节都被捕获
expected_chapters <- 1:14
missing_chapters <- setdiff(expected_chapters, final_corpus$chapter_num)

if (length(missing_chapters) > 0) {
  cat("警告：以下章节未被捕获:", paste(missing_chapters, collapse=", "), "\n")
} else {
  cat("所有预期章节均已成功捕获。\n")
}

# 13. 显示捕获的章节
print(final_corpus |> select(chapter_num, chapter_title))
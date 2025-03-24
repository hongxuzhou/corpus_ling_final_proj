# 修改后的语料库构建代码 - 改进章节识别
library(epubr)
library(here)
library(stringr)
library(dplyr)
library(tidyr)

# 1. 定义文件路径
file_path <- here("raw_data", "American Gods.epub")
book_id <- "AG"

# 2. 使用epubR读取epub内容和元数据
file_data <- epub(file_path)

# 3. 提取元数据
book_metadata <- file_data |> 
  select(-data) |> 
  as.list()

# 4. 提取数据
text_data <- file_data$data[[1]]

# 5. 过滤掉前面的内容，只保留从CHAPTER ONE开始的部分
# 首先找到CHAPTER ONE对应的行号
chapter_one_index <- which(str_detect(text_data$text, "^CHAPTER (ONE|1)"))[1]

# 过滤保留从CHAPTER ONE开始的内容
filtered_data <- text_data |>
  filter(row_number() >= chapter_one_index)

# 6. 处理章节与非章节内容
processed_chapters <- filtered_data |>
  # 识别正式章节 - 同时处理英文和数字形式的章节标识
  mutate(
    # 判断是否为章节开始 - 增强匹配模式
    is_chapter = str_detect(text, "^CHAPTER (ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN|ELEVEN|TWELVE|THIRTEEN|FOURTEEN|FIFTEEN|SIXTEEN|SEVENTEEN|EIGHTEEN|NINETEEN|TWENTY|[0-9]+)"),
    # 去除PART标识
    is_part = str_detect(text, "^PART [A-Z]+"),
    
    # 提取章节标识
    chapter_label = str_extract(text, "^CHAPTER (ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN|ELEVEN|TWELVE|THIRTEEN|FOURTEEN|FIFTEEN|SIXTEEN|SEVENTEEN|EIGHTEEN|NINETEEN|TWENTY|[0-9]+)"),
    
    # 将章节标识转换为数字
    chapter_num = case_when(
      # 处理拼写的数字
      str_detect(chapter_label, "ONE$") ~ 1,
      str_detect(chapter_label, "TWO$") ~ 2,
      str_detect(chapter_label, "THREE$") ~ 3,
      str_detect(chapter_label, "FOUR$") ~ 4,
      str_detect(chapter_label, "FIVE$") ~ 5,
      str_detect(chapter_label, "SIX$") ~ 6,
      str_detect(chapter_label, "SEVEN$") ~ 7,
      str_detect(chapter_label, "EIGHT$") ~ 8,
      str_detect(chapter_label, "NINE$") ~ 9,
      str_detect(chapter_label, "TEN$") ~ 10,
      str_detect(chapter_label, "ELEVEN$") ~ 11,
      str_detect(chapter_label, "TWELVE$") ~ 12,
      str_detect(chapter_label, "THIRTEEN$") ~ 13,
      str_detect(chapter_label, "FOURTEEN$") ~ 14,
      str_detect(chapter_label, "FIFTEEN$") ~ 15,
      str_detect(chapter_label, "SIXTEEN$") ~ 16,
      str_detect(chapter_label, "SEVENTEEN$") ~ 17,
      str_detect(chapter_label, "EIGHTEEN$") ~ 18,
      str_detect(chapter_label, "NINETEEN$") ~ 19,
      str_detect(chapter_label, "TWENTY$") ~ 20,
      # 处理数字形式的章节号
      str_detect(chapter_label, "[0-9]+$") ~ as.numeric(str_extract(chapter_label, "[0-9]+")),
      TRUE ~ NA_real_
    )
  ) |>
  # 过滤掉PART标记行
  filter(!is_part) |>
  # 为每个内容分配其所属的章节
  mutate(
    # 使用cumsum创建章节组，每当遇到新章节时增加
    chapter_group = cumsum(is_chapter),
    # 提取章节标题(只保留第一行的文本)
    chapter_title = if_else(is_chapter, chapter_label, NA_character_)
  ) |>
  # 按章节分组
  group_by(chapter_group) |>
  # 合并每个章节组内的所有文本
  summarize(
    section = first(section[is_chapter]),
    chapter_num = first(chapter_num),
    chapter_title = first(chapter_title),
    # 合并文本，保留章节标题
    text = paste(text, collapse = "\n\n"),
    # 计算总字数和字符数
    nword = sum(nword),
    nchar = sum(nchar)
  ) |>
  # 确保存在章节号
  filter(!is.na(chapter_num)) |>
  # 按章节号排序
  arrange(chapter_num)

# 7. 构建最终语料库
final_corpus <- processed_chapters |>
  mutate(
    # 添加元数据
    book_id = book_id,
    title = "American Gods",
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

# 8. 保存语料库
# 主语料库保存为RDS
saveRDS(final_corpus, here("corpora", paste0(book_id, "_corpus.rds")))

# 保存TSV备份
write.table(final_corpus, 
            here("corpora_backup", paste0(book_id, "_corpus.tsv")),
            sep = "\t",
            row.names = FALSE,
            quote = TRUE)  # 使用quote=TRUE防止文本中的制表符问题

# 9. 输出处理摘要
cat(sprintf("处理完成: 从《%s》中提取了%d个章节。\n", 
            book_metadata$title, nrow(final_corpus)))
cat(sprintf("总字数: %d\n", sum(final_corpus$nword)))

# 10. 调试输出 - 查看找到的章节
print(final_corpus |> select(chapter_num, chapter_title))


# 01_data_preprocessing.R
# 读取原始数据、清洗数据、特征工程和数据合并

# 加载配置
source("code/00_config.R")

# 函数：读取企业信息数据
read_company_info <- function(file_path) {
  # 读取企业基本信息表
  # 参数：file_path - 数据文件路径
  # 返回：包含企业基本信息的数据框
  
  cat("正在读取企业信息数据...\n")
  
  # 检查文件是否存在
  if (!file.exists(file_path)) {
    stop("文件不存在: ", file_path)
  }
  
  # 读取Excel文件中的"企业信息"工作表
  # 使用readxl包读取Excel文件
  company_info <- readxl::read_excel(file_path, sheet = "企业信息")
  
  # 检查数据是否读取成功
  if (nrow(company_info) == 0) {
    stop("企业信息工作表为空")
  }
  
  # 显示数据基本信息
  cat("成功读取企业信息数据\n")
  return(company_info)
}

# 函数：读取并处理发票数据
process_invoice_data <- function(file_path, invoice_type = "进项") {
  # 读取并预处理发票数据
  # 参数：file_path - 数据文件路径, invoice_type - 发票类型（"进项"或"销项"）
  # 返回：处理后的发票数据
  
  cat("正在处理", invoice_type, "发票数据\n")
  
  # 确定要读取的工作表名称
  sheet_name <- ifelse(invoice_type == "进项", "进项发票信息", "销项发票信息")
  
  # 读取Excel文件中的发票数据
  invoice_data <- readxl::read_excel(file_path, sheet = sheet_name)
  
  # 检查数据是否读取成功
  if (nrow(invoice_data) == 0) {
    stop(sheet_name, "工作表为空或不存在")
  }
  
  cat("原始", invoice_type, "发票数量:", nrow(invoice_data), "\n")
  
  # 步骤1：剔除作废发票
  valid_invoices <- invoice_data %>% 
    filter(发票状态 == "有效发票")
  
  cat("剔除作废发票后，剩余有效", invoice_type, "发票数量:", nrow(valid_invoices), "\n")
  return(valid_invoices)
}

# 函数：计算企业财务指标
calculate_financial_metrics <- function(valid_invoices, company_info, invoice_type) {
  # 计算每个企业的财务指标
  # 参数：valid_invoices - 有效发票数据, company_info - 企业信息, invoice_type - 发票类型
  # 返回：包含财务指标的数据框
  
  cat("正在计算", invoice_type, "财务指标\n")
  
  # 按企业代号分组汇总
  financial_data <- valid_invoices %>%
    group_by(企业代号) %>%
    summarise(
      # 计算总金额（取绝对值，因为发票可能是负值）
      总金额 = sum(abs(价税合计)),
      # 计算有效发票数量
      发票数量 = n(),
      # 计算负值销项发票比例（现在改为仅对销项发票有意义）
      负值销项发票比例 = ifelse(invoice_type == "销项", 
                        sum(价税合计 < 0) / n(), 
                        NA),
      # 新增：计算发票金额的变异系数（标准差/均值）
      发票金额变异系数 = ifelse(mean(abs(价税合计)) > 0, 
                        sd(abs(价税合计)) / mean(abs(价税合计)), 
                        NA)
    ) %>%
    ungroup()
  
  # 重命名列以区分进项和销项
  if (invoice_type == "进项") {
    colnames(financial_data) <- c("企业代号", 
                                  "总支出", 
                                  "进项发票数量", 
                                  "负值销项发票比例",
                                  "进项发票金额变异系数")
    # 进项发票不需要负值发票比例
    financial_data <- financial_data %>% select(-负值销项发票比例)
  } else {
    colnames(financial_data) <- c("企业代号", 
                                  "总营收", 
                                  "销项发票数量", 
                                  "负值销项发票比例",
                                  "销项发票金额变异系数")
  }
  
  return(financial_data)
}

# 函数：计算作废发票比例
calculate_void_invoice_ratio <- function(file_path, invoice_type = "进项") {
  # 计算作废发票比例
  # 参数：file_path - 数据文件路径, invoice_type - 发票类型
  # 返回：包含作废发票比例的数据框
  
  cat("正在计算", invoice_type, "作废发票比例...\n")
  
  # 确定要读取的工作表名称
  sheet_name <- ifelse(invoice_type == "进项", "进项发票信息", "销项发票信息")
  
  # 读取原始发票数据
  invoice_data <- readxl::read_excel(file_path, sheet = sheet_name)
  
  # 按企业代号分组计算作废比例
  void_ratio_data <- invoice_data %>%
    group_by(企业代号) %>%
    summarise(
      总发票数量 = n(),
      作废发票数量 = sum(发票状态 == "作废发票"),
      作废发票比例 = ifelse(总发票数量 > 0, 作废发票数量 / 总发票数量, 0)
    ) %>%
    select(企业代号, 作废发票比例) %>%
    ungroup()
  
  # 重命名列以区分进项和销项
  if (invoice_type == "进项") {
    colnames(void_ratio_data) <- c("企业代号", "进项作废发票比例")
  } else {
    colnames(void_ratio_data) <- c("企业代号", "销项作废发票比例")
  }
  
  cat(invoice_type, "作废发票比例计算完成\n")
  return(void_ratio_data)
}

# 函数：编码分类变量
encode_categorical_variables <- function(data) {
  # 对分类变量进行数值编码
  # 参数：原始数据
  # 返回：编码后的数据
  
  cat("正在编码分类变量...\n")
  
  encoded_data <- data %>%
    mutate(
      # 将是否违约转换为数值变量：是->1, 否->0
      是否违约数值 = ifelse(是否违约 == "是", 1, 0),
      
      # 将信誉评级转换为有序数值变量：A->3, B->2, C->1, D->0
      信誉评级数值 = case_when(
        信誉评级 == "A" ~ 3,
        信誉评级 == "B" ~ 2,
        信誉评级 == "C" ~ 1,
        信誉评级 == "D" ~ 0,
        TRUE ~ NA_real_
      )
      # 删除信誉评级A/B/C/D的独热编码，保留有序数值编码即可
    )
  
  cat("分类变量编码完成\n")
  return(encoded_data)
}

# 函数：合并所有数据
merge_all_data <- function(company_info, purchase_data, sales_data, 
                           purchase_void_ratio = NULL, sales_void_ratio = NULL) {
  # 合并企业信息、进项数据和销项数据
  # 参数：所有要合并的数据框
  # 返回：完整的数据集
  
  cat("正在合并所有数据...\n")
  
  # 首先合并进项和销项数据
  financial_combined <- full_join(purchase_data, sales_data, by = "企业代号")
  
  # 然后合并企业信息
  final_data <- company_info %>%
    left_join(financial_combined, by = "企业代号")
  
  # 如果有作废发票比例数据，合并进来
  if (!is.null(purchase_void_ratio)) {
    final_data <- final_data %>%
      left_join(purchase_void_ratio, by = "企业代号")
  }
  
  if (!is.null(sales_void_ratio)) {
    final_data <- final_data %>%
      left_join(sales_void_ratio, by = "企业代号")
  }
  
  # 计算衍生特征
  final_data <- final_data %>%
    mutate(
      # 毛利润 = 总营收 - 总支出
      毛利润 = ifelse(!is.na(总营收) & !is.na(总支出), 总营收 - 总支出, NA),
      # 运营规模 = 总营收 + 总支出
      运营规模 = ifelse(!is.na(总营收) & !is.na(总支出), 总营收 + 总支出, NA),
      # 利润率 = 毛利润 / 总营收
      利润率 = ifelse(!is.na(毛利润) & !is.na(总营收) & 总营收 > 0, 毛利润 / 总营收, NA),
      # 资金周转率 = 总营收 / 总支出（衡量资金使用效率）
      资金周转率 = ifelse(!is.na(总营收) & !is.na(总支出) & 总支出 > 0, 
                     总营收 / 总支出, NA),
    )
  
  cat("数据合并完成，最终数据集包含", nrow(final_data), "家企业\n")
  return(final_data)
}

# 主执行流程
cat("开始数据预处理流程\n")

# 1. 读取附件1：123家有信贷记录企业的数据
cat("=== 处理附件1：123家有信贷记录企业 ===\n")
company_info_1 <- read_company_info(paste0("data/raw/", "附件1：123家有信贷记录企业的相关数据.xlsx"))

# 2. 处理附件1的进项发票数据
purchase_invoices_1 <- process_invoice_data(paste0("data/raw/", "附件1：123家有信贷记录企业的相关数据.xlsx"), "进项")
purchase_metrics_1 <- calculate_financial_metrics(purchase_invoices_1, company_info_1, "进项")

# 3. 处理附件1的销项发票数据
sales_invoices_1 <- process_invoice_data(paste0("data/raw/", "附件1：123家有信贷记录企业的相关数据.xlsx"), "销项")
sales_metrics_1 <- calculate_financial_metrics(sales_invoices_1, company_info_1, "销项")

# 4. 计算作废发票比例
purchase_void_ratio_1 <- calculate_void_invoice_ratio(paste0("data/raw/", "附件1：123家有信贷记录企业的相关数据.xlsx"), "进项")
sales_void_ratio_1 <- calculate_void_invoice_ratio(paste0("data/raw/", "附件1：123家有信贷记录企业的相关数据.xlsx"), "销项")

# 5. 合并附件1的所有数据（包含新变量）
final_dataset_1 <- merge_all_data(company_info_1, purchase_metrics_1, sales_metrics_1, 
                                  purchase_void_ratio_1, sales_void_ratio_1)

# 6. 编码分类变量
final_dataset_encoded_1 <- encode_categorical_variables(final_dataset_1)

# 7. 处理缺失值
cat("正在处理缺失值\n")

# 对附件1的数据处理缺失值
final_dataset_clean_1 <- final_dataset_encoded_1 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

# 8. 保存处理后的数据为CSV格式
cat("正在保存处理后的数据为CSV格式\n")

# 保存附件1处理后的数据
write.csv(final_dataset_clean_1, 
          file = paste0("data/processed/", "processed_company_data_with_credit.csv"),
          row.names = FALSE, 
          fileEncoding = "GBK")
cat("附件1数据保存至:", paste0("data/processed/", "processed_company_data_with_credit.csv"), "\n")
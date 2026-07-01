# 02_correlation_analysis.R
# 对附件1数据进行系统的相关性分析，重点识别与违约相关的特征

# 加载配置
source("code/00_config.R")

# 函数：加载预处理数据
load_processed_data <- function() {
  # 加载附件1处理后的数据
  # 返回：包含信贷记录的企业数据
  
  cat("正在加载预处理数据\n")
  
  file_path <- paste0("data/processed/", "processed_company_data_with_credit.csv")
  
  if (!file.exists(file_path)) {
    stop("预处理数据文件不存在: ", file_path, "\n请先运行01_data_preprocessing.R")
  }
  
  data <- read.csv(file_path, fileEncoding = "GBK")
  
  cat("企业数量:", nrow(data), "\n")
  cat("特征数量:", ncol(data), "\n")
  cat("列名:", paste(colnames(data), collapse = ", "), "\n")
  
  return(data)
}

# 函数：数据质量检查
check_data_quality <- function(data) {
  # 检查数据质量
  # 参数：data - 数据集
  
  cat("\n=== 数据质量检查 ===\n")
  
  # 目标变量分布
  if ("是否违约" %in% colnames(data)) {
    cat("是否违约分布:\n")
    default_table <- table(data$是否违约)
    print(default_table)
    cat("违约率:", round(mean(data$是否违约数值, na.rm = TRUE) * 100, 2), "%\n")
  }
  
  if ("信誉评级" %in% colnames(data)) {
    cat("\n信誉评级分布:\n")
    print(table(data$信誉评级))
  }
  
  # 缺失值检查
  cat("\n缺失值统计:\n")
  missing_count <- colSums(is.na(data))
  missing_vars <- missing_count[missing_count > 0]
  
  if (length(missing_vars) > 0) {
    print(missing_vars)
  } else {
    cat("无缺失值\n")
  }
  
  # 零值检查
  cat("\n零值比例较高的变量:\n")
  zero_ratio <- colSums(data == 0, na.rm = TRUE) / nrow(data)
  high_zero_vars <- zero_ratio[zero_ratio > 0.8]
  
  if (length(high_zero_vars) > 0) {
    print(round(high_zero_vars, 3))
  } else {
    cat("无高零值比例变量\n")
  }
}

# 函数：选择分析变量
select_analysis_variables <- function(data) {
  # 选择用于相关性分析的变量
  # 参数：data - 原始数据
  # 返回：分析用的数据子集
  
  cat("\n=== 选择分析变量 ===\n")
  
  # 排除的变量（标识符、重复变量等）
  exclude_vars <- c("企业代号", "企业名称", "是否违约", "信誉评级")
  
  # 选择数值型变量
  analysis_vars <- setdiff(colnames(data), exclude_vars)
  
  # 进一步筛选，只保留有意义的数值变量
  numeric_vars <- analysis_vars[sapply(data[analysis_vars], is.numeric)]
  
  cat("最终选择的分析变量 (", length(numeric_vars), "个):\n", sep = "")
  cat(paste(numeric_vars, collapse = ", "), "\n")
  
  return(data[, numeric_vars, drop = FALSE])
}

# 函数：计算与违约的相关性
calculate_default_correlations <- function(analysis_data) {
  # 计算各变量与是否违约的相关性
  # 参数：analysis_data - 分析数据
  # 返回：相关性结果
  
  cat("\n=== 计算与违约的相关性 ===\n")
  
  if (!"是否违约数值" %in% colnames(analysis_data)) {
    stop("分析数据中缺少'是否违约数值'变量")
  }
  
  # 计算相关系数
  cor_results <- data.frame(
    变量 = character(),
    相关系数 = numeric(),
    绝对值 = numeric(),
    相关性强度 = character(),
    stringsAsFactors = FALSE
  )
  
  for (var in colnames(analysis_data)) {
    if (var != "是否违约数值") {
      # 计算相关系数
      cor_value <- cor(analysis_data[[var]], 
                       analysis_data[["是否违约数值"]], 
                       use = "complete.obs",
                       method = "spearman")
      
      # 判断相关性强度
      strength <- ifelse(abs(cor_value) >= 0.7, "强",
                         ifelse(abs(cor_value) >= 0.5, "中等",
                                ifelse(abs(cor_value) >= 0.3, "弱", "很弱")))
      
      cor_results <- rbind(cor_results, 
                           data.frame(
                             变量 = var,
                             相关系数 = round(cor_value, 4),
                             绝对值 = round(abs(cor_value), 4),
                             相关性强度 = strength
                           ))
    }
  }
  
  # 按绝对值排序
  cor_results <- cor_results[order(cor_results$绝对值, decreasing = TRUE), ]
  
  cat("与违约相关性最高的10个变量:\n")
  print(head(cor_results, 10))
  
  return(cor_results)
}

# 函数：绘制相关性热力图
plot_comprehensive_correlation <- function(analysis_data, results_dir) {
  # 绘制全面的相关性热力图
  # 参数：analysis_data - 分析数据, results_dir - 结果目录
  
  cat("\n=== 绘制相关性热力图 ===\n")
  
  # 计算相关系数矩阵
  cor_matrix <- cor(analysis_data, use = "complete.obs", method = "spearman")
  
  # 创建热力图
  p <- ggcorrplot(cor_matrix,
                  method = "circle",
                  type = "lower",
                  lab = TRUE,
                  lab_size = 2.5,
                  colors = c(COLOR_PALETTE[4], "white", COLOR_PALETTE[1]),
                  title = "企业特征相关性热力图",
                  ggtheme = theme_minimal()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 保存图形
  ggsave(paste0(results_dir, "comprehensive_correlation_heatmap.png"),
         p, width = 12, height = 10, dpi = 300)
  cat("相关性热力图已保存\n")
  
  return(cor_matrix)
}

# 函数：绘制与违约相关性条形图
plot_default_correlation_bars <- function(cor_results, results_dir) {
  # 绘制与违约相关性的条形图
  # 参数：cor_results - 相关性结果, results_dir - 结果目录
  
  cat("\n=== 绘制与违约相关性条形图 ===\n")
  
  # 选择前15个变量
  top_vars <- head(cor_results, 15)
  
  p <- ggplot(top_vars, aes(x = reorder(变量, 相关系数), 
                            y = 相关系数, 
                            fill = 相关系数)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(low = COLOR_PALETTE[1], 
                         mid = "white", 
                         high = COLOR_PALETTE[4],
                         midpoint = 0,
                         name = "相关系数") +
    coord_flip() +
    labs(title = "与是否违约相关性最高的变量",
         x = "变量",
         y = "相关系数") +
    theme_minimal() +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5))
  
  # 添加数值标签
  p <- p + geom_text(aes(label = round(相关系数, 3)), 
                     hjust = ifelse(top_vars$相关系数 >= 0, -0.2, 1.2),
                     size = 3)
  
  ggsave(paste0(results_dir, "default_correlation_bars.png"),
         p, width = 14, height = 10, dpi = 300)
  cat("相关性条形图已保存\n")
}

# 函数：重要变量的分组比较
plot_important_variables_comparison <- function(data, cor_results, results_dir) {
  # 对重要变量进行分组可视化比较
  # 参数：data - 原始数据, cor_results - 相关性结果, results_dir - 结果目录
  
  cat("\n=== 重要变量分组比较 ===\n")
  
  # 选择相关性最高的8个变量
  top_vars <- head(cor_results$变量, 8)
  
  plots <- list()
  
  for (i in seq_along(top_vars)) {
    var <- top_vars[i]
    
    # 对金额类变量进行对数变换
    if (grepl("总营收|总支出|运营规模|毛利润|销项发票数量|进项发票数量", var)) {
      # 创建新变量：log(1 + x) 避免对0取对数
      temp_data <- data
      temp_data[[paste0("log_", var)]] <- log1p(temp_data[[var]])
      
      p <- ggplot(temp_data, aes(x = factor(是否违约数值, labels = c("未违约", "违约")), 
                                 y = .data[[paste0("log_", var)]], 
                                 fill = 是否违约)) +
        geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
        scale_fill_manual(values = c("否" = COLOR_PALETTE[1], "是" = COLOR_PALETTE[4])) +
        labs(title = paste(var, "\n相关系数:", round(cor_results$相关系数[cor_results$变量 == var], 3)),
             x = "是否违约",
             y = paste("log(1 +", var, ")")) +
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(size = 10))
    } else {
      # 非金额变量保持原样
      p <- ggplot(data, aes(x = factor(是否违约数值, labels = c("未违约", "违约")), 
                            y = .data[[var]], 
                            fill = 是否违约)) +
        geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
        scale_fill_manual(values = c("否" = COLOR_PALETTE[1], "是" = COLOR_PALETTE[4])) +
        labs(title = paste(var, "\n相关系数:", round(cor_results$相关系数[cor_results$变量 == var], 3)),
             x = "是否违约",
             y = var) +
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(size = 10))
    }
    
    plots[[i]] <- p
  }
  
  # 组合图形
  combined_plot <- wrap_plots(plots, ncol = 2) +
    plot_annotation(title = "重要变量在违约组和未违约组的分布比较",
                    theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
  
  ggsave(paste0(results_dir, "important_variables_comparison.png"),
         combined_plot, width = 12, height = 10, dpi = 300)
  cat("重要变量比较图已保存\n")
}

# 函数：执行统计检验
perform_comprehensive_tests <- function(data, cor_results) {
  # 执行全面的统计检验
  # 参数：data - 原始数据, cor_results - 相关性结果
  # 返回：统计检验结果
  
  cat("\n=== 执行统计检验 ===\n")
  
  # 选择变量进行检验
  test_vars <- head(cor_results$变量, 14)
  
  test_results <- data.frame(
    变量 = character(),
    检验方法 = character(),
    p值 = numeric(),
    显著性 = character(),
    stringsAsFactors = FALSE
  )
  
  for (var in test_vars) {
    # 分组数据
    group0 <- data[[var]][data$是否违约数值 == 0 & !is.na(data[[var]])]
    group1 <- data[[var]][data$是否违约数值 == 1 & !is.na(data[[var]])]
    
    # 检查样本量
    if (length(group0) >= 3 & length(group1) >= 3) {
      # Mann-Whitney U检验（非参数）
      mw_test <- wilcox.test(group0, group1, exact = FALSE)
      
      # t检验（参数）
      t_test <- t.test(group0, group1)
      
      # 添加结果
      test_results <- rbind(test_results,
                            data.frame(
                              变量 = var,
                              检验方法 = "t检验",
                              p值 = t_test$p.value,
                              显著性 = ifelse(t_test$p.value < 0.05, "显著", "不显著")
                            ))
    }
  }
  
  # 格式化p值
  test_results$p值 <- round(test_results$p值, 4)
  
  cat("统计检验结果:\n")
  print(test_results)
  
  return(test_results)
}

# 函数：信誉评级分析
analyze_credit_rating <- function(data, results_dir) {
  # 分析信誉评级与违约的关系
  # 参数：data - 原始数据, results_dir - 结果目录
  
  cat("\n=== 信誉评级分析 ===\n")
  
  if (!"信誉评级" %in% colnames(data)) {
    cat("数据中缺少信誉评级信息\n")
    return(NULL)
  }
  
  # 创建交叉表
  rating_table <- table(data$信誉评级, data$是否违约)
  cat("信誉评级 vs 是否违约:\n")
  print(rating_table)
  
  # 计算违约率
  default_rates <- prop.table(rating_table, margin = 1)[, "是"]
  cat("\n各评级违约率:\n")
  print(round(default_rates, 4))
  
  # 卡方检验
  chi_test <- chisq.test(rating_table)
  cat("\n卡方检验结果:\n")
  print(chi_test)
  
  # 绘制堆积柱状图
  p <- ggplot(data, aes(x = 信誉评级, fill = 是否违约)) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("否" = COLOR_PALETTE[1], "是" = COLOR_PALETTE[4])) +
    labs(title = "信誉评级与违约关系",
         x = "信誉评级",
         y = "比例",
         fill = "是否违约") +
    theme_minimal()
  
  ggsave(paste0(results_dir, "credit_rating_analysis.png"),
         p, width = 8, height = 6, dpi = 300)
  cat("信誉评级分析图已保存\n")
  
  return(list(table = rating_table, default_rates = default_rates, chi_test = chi_test))
}

# 函数：生成综合分析报告
generate_comprehensive_report <- function(data, cor_results, test_results, rating_analysis, results_dir) {
  # 生成综合分析报告
  # 参数：各种分析结果
  
  cat("\n=== 生成分析报告 ===\n")
  
  # 创建报告文件
  report_file <- paste0(results_dir, "correlation_analysis_report.txt")
  sink(report_file)
  
  cat("=== 企业信贷风险相关性分析报告 ===\n\n")
  cat("分析时间:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("数据概况:\n")
  cat("- 企业数量:", nrow(data), "\n")
  cat("- 特征数量:", ncol(data), "\n")
  cat("- 违约率:", round(mean(data$是否违约数值) * 100, 2), "%\n\n")
  
  cat("1. 与违约最相关的变量:\n")
  strong_cor <- cor_results[cor_results$绝对值 >= 0.3, ]
  if (nrow(strong_cor) > 0) {
    for (i in 1:min(5, nrow(strong_cor))) {
      cat("   ", i, ". ", strong_cor$变量[i], " (r = ", strong_cor$相关系数[i], ")\n", sep = "")
    }
  } else {
    cat("   未发现强相关变量 (|r| >= 0.3)\n")
  }
  
  cat("\n2. 统计显著性总结:\n")
  sig_vars <- unique(test_results$变量[test_results$p值 < 0.05])
  if (length(sig_vars) > 0) {
    cat("   显著变量 (p < 0.05):", paste(sig_vars, collapse = ", "), "\n")
  } else {
    cat("   无显著变量\n")
  }
  
  cat("\n3. 信誉评级分析:\n")
  if (!is.null(rating_analysis)) {
    cat("   各评级违约率:\n")
    for (rating in names(rating_analysis$default_rates)) {
      cat("     ", rating, ": ", round(rating_analysis$default_rates[rating] * 100, 2), "%\n", sep = "")
    }
    p_value <- rating_analysis$chi_test$p.value
    if (p_value < 0.0001) {
      cat("   卡方检验 p值: < 0.0001 (极其显著)\n")
    } else {
      cat("   卡方检验 p值:", round(p_value, 4), "\n")
    }
  }
  
  
  sink()
  
  # 保存详细结果
  write.csv(cor_results, 
            paste0(results_dir, "detailed_correlation_results.csv"),
            row.names = FALSE, fileEncoding = "GBK")
  
  write.csv(test_results,
            paste0(results_dir, "statistical_test_results.csv"),
            row.names = FALSE, fileEncoding = "GBK")
  
  cat("分析报告已保存至:", report_file, "\n")
  cat("详细结果已保存至输出目录\n")
}

# 主执行流程
cat("开始企业信贷风险相关性分析...\n\n")

# 创建输出目录
results_dir <- "results/correlation_analysis/"

# 1. 加载数据
company_data <- load_processed_data()

# 2. 数据质量检查
check_data_quality(company_data)

# 3. 选择分析变量
analysis_data <- select_analysis_variables(company_data)

# 4. 计算相关性
cor_results <- calculate_default_correlations(analysis_data)

# 5. 绘制图形
cor_matrix <- plot_comprehensive_correlation(analysis_data, results_dir)
plot_default_correlation_bars(cor_results, results_dir)
plot_important_variables_comparison(company_data, cor_results, results_dir)

# 6. 统计检验
test_results <- perform_comprehensive_tests(company_data, cor_results)

# 7. 信誉评级分析
rating_analysis <- analyze_credit_rating(company_data, results_dir)

# 8. 生成报告
generate_comprehensive_report(company_data, cor_results, test_results, rating_analysis, results_dir)

cat("\n=== 相关性分析完成 ===\n")
cat("所有结果已保存至:", results_dir, "\n")
cat("主要输出文件:\n")
cat("- comprehensive_correlation_heatmap.png: 相关性热力图\n")
cat("- default_correlation_bars.png: 相关性条形图\n")
cat("- important_variables_comparison.png: 重要变量比较图\n")
cat("- credit_rating_analysis.png: 信誉评级分析图\n")
cat("- correlation_analysis_report.txt: 分析报告\n")
cat("- detailed_correlation_results.csv: 详细相关性结果\n")
cat("- statistical_test_results.csv: 统计检验结果\n")
# 03_prediction_model.R
# 构建预测模型：使用逻辑回归预测企业违约概率

# 加载配置
source("code/00_config.R")

# 函数：数据归一化/标准化
normalize_features <- function(train_data, test_data, variables) {
  # 对特征进行标准化处理 (Z-score standardization)
  # 参数：train_data - 训练集, test_data - 测试集, variables - 需要归一化的特征名
  # 返回：归一化后的训练集、测试集和预处理对象
  
  cat("\n=== 数据归一化 (Standardization) ===\n")
  
  # 确保依赖包已加载
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("请安装 'caret' 包以进行数据预处理")
  }
  
  # 1. 基于训练集计算均值和标准差 (建立规则)
  # method = c("center", "scale") 意味着 (x - mean) / sd
  pre_proc_values <- caret::preProcess(train_data[, variables], method = c("center", "scale"))
  
  cat("归一化规则已基于训练集建立 (Center & Scale)\n")
  
  # 2. 将规则应用到训练集
  train_norm <- train_data
  train_norm[, variables] <- predict(pre_proc_values, train_data[, variables])
  
  # 3. 将规则应用到测试集 (注意：这里必须使用训练集的规则，不能重新计算)
  test_norm <- test_data
  test_norm[, variables] <- predict(pre_proc_values, test_data[, variables])
  
  cat("数据转换完成\n")
  cat("- 训练集特征均值 (预览): ", round(mean(train_norm[, variables[1]]), 4), 
      " (应接近0)\n")
  cat("- 训练集特征标准差 (预览): ", round(sd(train_norm[, variables[1]]), 4), 
      " (应接近1)\n")
  
  return(list(
    train = train_norm, 
    test = test_norm, 
    scaler = pre_proc_values
  ))
}

# 函数：加载预处理数据和相关性结果
load_data_and_correlations <- function() {
  # 加载附件1处理后的数据和相关性分析结果
  # 返回：数据和相关性结果
  
  cat("正在加载数据和相关性结果...\n")
  
  # 加载附件1数据
  data_file <- paste0("data/processed/", "processed_company_data_with_credit.csv")
  if (!file.exists(data_file)) {
    stop("预处理数据文件不存在: ", data_file, "\n请先运行01_data_preprocessing.R")
  }
  company_data <- read.csv(data_file, fileEncoding = "GBK")
  
  # 加载相关性结果
  cor_file <- paste0("results/correlation_analysis/", "detailed_correlation_results.csv")
  if (!file.exists(cor_file)) {
    stop("相关性结果文件不存在: ", cor_file, "\n请先运行02_correlation_analysis.R")
  }
  cor_results <- read.csv(cor_file, fileEncoding = "GBK")
  
  cat("数据加载完成\n")
  cat("- 企业数量:", nrow(company_data), "\n")
  cat("- 相关性变量数量:", nrow(cor_results), "\n")
  
  return(list(data = company_data, cor_results = cor_results))
}

# 函数：选择建模变量
select_modeling_variables <- function(data, cor_results, test_results, top_n = 6) {
  # 根据相关性和统计显著性选择建模变量
  # 参数：data - 数据, cor_results - 相关性结果, test_results - 统计检验结果, top_n - 选择前n个变量
  # 返回：选择的变量名和数据子集
  
  cat("\n=== 选择建模变量 ===\n")
  
  # 从统计检验结果中获取显著变量
  significant_vars <- test_results %>%
    filter(显著性 == "显著" & p值 < 0.05) %>%
    pull(变量)
  
  cat("统计显著的变量 (p < 0.05):", length(significant_vars), "个\n")
  if (length(significant_vars) > 0) {
    cat(paste(significant_vars, collapse = ", "), "\n")
  }
  
  # 从显著变量中选择相关性最高的top_n个
  top_vars <- cor_results %>%
    filter(变量 %in% significant_vars & 变量 != "是否违约数值") %>%
    arrange(desc(绝对值)) %>%
    head(top_n) %>%
    pull(变量)
  
  # 如果显著变量不足top_n个，用相关性最高的变量补充
  if (length(top_vars) < top_n) {
    cat("显著变量不足", top_n, "个，用相关性最高的变量补充\n")
    
    additional_vars <- cor_results %>%
      filter(!变量 %in% top_vars & 变量 != "是否违约数值") %>%
      arrange(desc(绝对值)) %>%
      head(top_n - length(top_vars)) %>%
      pull(变量)
    
    top_vars <- c(top_vars, additional_vars)
  }
  
  cat("\n最终选择的前", length(top_vars), "个建模变量:\n")
  for (i in seq_along(top_vars)) {
    cor_value <- cor_results$相关系数[cor_results$变量 == top_vars[i]]
    p_value <- ifelse(top_vars[i] %in% test_results$变量, 
                      test_results$p值[test_results$变量 == top_vars[i]], 
                      NA)
    
    significance_note <- ifelse(!is.na(p_value) & p_value < 0.05, " (显著)", " (不显著)")
    cat(i, ". ", top_vars[i], " (r = ", round(cor_value, 4), significance_note, ")\n", sep = "")
  }
  
  # 检查这些变量是否都在数据中
  missing_vars <- setdiff(top_vars, colnames(data))
  if (length(missing_vars) > 0) {
    warning("以下变量在数据中缺失: ", paste(missing_vars, collapse = ", "))
    top_vars <- setdiff(top_vars, missing_vars)
  }
  
  # 创建建模数据集（包含目标变量和选择的特征）
  modeling_vars <- c("是否违约数值", top_vars)
  modeling_data <- data[, modeling_vars, drop = FALSE]
  
  # 移除有缺失值的行
  complete_cases <- complete.cases(modeling_data)
  modeling_data <- modeling_data[complete_cases, ]
  
  cat("\n最终建模数据集:\n")
  cat("- 样本数量:", nrow(modeling_data), "\n")
  cat("- 特征数量:", ncol(modeling_data) - 1, "\n")
  cat("- 违约率:", round(mean(modeling_data$是否违约数值) * 100, 2), "%\n")
  
  return(list(variables = top_vars, data = modeling_data))
}

# 函数：分割训练集和测试集
split_train_test <- function(data, test_size = 0.3) {
  # 分割数据为训练集和测试集
  # 参数：data - 数据, test_size - 测试集比例
  # 返回：训练集和测试集
  
  cat("\n=== 分割训练集和测试集 ===\n")
  
  set.seed(100)  # 确保可重复性
  
  # 创建分割索引
  train_index <- createDataPartition(data$是否违约数值, 
                                     p = 1 - test_size, 
                                     list = FALSE)
  
  # 分割数据
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  cat("训练集: ", nrow(train_data), "个样本 (", 
      round(nrow(train_data)/nrow(data)*100, 1), "%)\n", sep = "")
  cat("测试集: ", nrow(test_data), "个样本 (", 
      round(nrow(test_data)/nrow(data)*100, 1), "%)\n", sep = "")
  cat("训练集违约率:", round(mean(train_data$是否违约数值) * 100, 2), "%\n")
  cat("测试集违约率:", round(mean(test_data$是否违约数值) * 100, 2), "%\n")
  
  return(list(train = train_data, test = test_data))
}

# 函数：训练LASSO逻辑回归模型
train_lasso_model <- function(train_data, variables) {
  # 训练LASSO逻辑回归模型
  # 参数：train_data - 训练数据, variables - 特征变量
  # 返回：训练好的模型和最优lambda
  
  cat("\n=== 训练LASSO逻辑回归模型 ===\n")
  
  # 确保glmnet包已安装
  if (!require(glmnet, quietly = TRUE)) {
    install.packages("glmnet")
    library(glmnet)
  }
  
  # 准备数据
  x_train <- as.matrix(train_data[, variables])
  y_train <- train_data$是否违约数值
  
  cat("训练数据维度:", dim(x_train), "\n")
  cat("目标变量分布:\n")
  print(table(y_train))
  
  # 设置lambda序列
  lambda_seq <- 10^seq(2, -4, length = 100)
  
  # 使用交叉验证选择最优lambda
  set.seed(123)
  cv_lasso <- cv.glmnet(x_train, y_train, 
                        alpha = 1,           # LASSO回归
                        family = "binomial", # 二分类
                        lambda = lambda_seq,
                        nfolds = 5,          # 5折交叉验证
                        type.measure = "deviance") # 使用偏差
  
  # 获取最优lambda
  best_lambda <- cv_lasso$lambda.min
  cat("最优lambda值:", round(best_lambda, 6), "\n")
  
  # 使用最优lambda训练最终模型
  lasso_model <- glmnet(x_train, y_train, 
                        alpha = 1, 
                        family = "binomial",
                        lambda = best_lambda)
  
  # 显示模型系数
  cat("\nLASSO模型系数:\n")
  coef_matrix <- as.matrix(coef(lasso_model))
  coef_df <- data.frame(
    变量 = rownames(coef_matrix),
    系数 = round(coef_matrix[, 1], 4)
  )
  
  # 过滤非零系数
  non_zero_coef <- coef_df[abs(coef_df$系数) > 0.001, ]
  print(non_zero_coef)
  
  # 显示被筛选掉的变量
  zero_vars <- coef_df[abs(coef_df$系数) <= 0.001 & coef_df$变量 != "(Intercept)", "变量"]
  if (length(zero_vars) > 0) {
    cat("\n被LASSO筛选掉的变量:\n")
    cat(paste(zero_vars, collapse = ", "), "\n")
  }
  
  return(list(model = lasso_model, 
              cv_model = cv_lasso, 
              lambda = best_lambda,
              variables = variables))
}

# 函数：模型预测和评估
evaluate_model <- function(model_obj, test_data, results_dir) {
  # 在测试集上评估LASSO模型性能
  # 参数：model_obj - 模型对象, test_data - 测试数据, results_dir - 结果目录
  # 返回：评估结果
  
  cat("\n=== 模型评估 ===\n")
  
  # 提取模型和变量
  lasso_model <- model_obj$model
  variables <- model_obj$variables
  
  # 准备测试数据
  x_test <- as.matrix(test_data[, variables])
  y_test <- test_data$是否违约数值
  
  # 预测概率
  predictions_prob <- predict(lasso_model, newx = x_test, type = "response")[, 1]
  
  # 使用0.5作为分类阈值
  predictions_class <- ifelse(predictions_prob > 0.5, 1, 0)
  
  # 计算评估指标
  actual <- y_test
  conf_matrix <- table(预测 = predictions_class, 实际 = actual)
  
  # 计算各种指标
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  precision <- ifelse(sum(conf_matrix[2, ]) > 0, 
                      conf_matrix[2, 2] / sum(conf_matrix[2, ]), 0)
  recall <- ifelse(sum(conf_matrix[, 2]) > 0, 
                   conf_matrix[2, 2] / sum(conf_matrix[, 2]), 0)
  f1_score <- ifelse((precision + recall) > 0, 
                     2 * (precision * recall) / (precision + recall), 0)
  
  # 计算AUC-ROC
  roc_obj <- roc(actual, predictions_prob)
  auc_value <- auc(roc_obj)
  
  # 打印结果
  cat("混淆矩阵:\n")
  print(conf_matrix)
  
  cat("\n性能指标:\n")
  cat("- 准确率:", round(accuracy, 4), "\n")
  cat("- 精确率:", round(precision, 4), "\n")
  cat("- 召回率:", round(recall, 4), "\n")
  cat("- F1分数:", round(f1_score, 4), "\n")
  cat("- AUC-ROC:", round(auc_value, 4), "\n")
  
  # 绘制ROC曲线
  p_roc <- ggroc(roc_obj, color = COLOR_PALETTE[1], size = 1) +
    geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "gray") +
    labs(title = paste0("LASSO模型ROC曲线 (AUC = ", round(auc_value, 3), ")"),
         x = "1 - 特异度",
         y = "敏感度") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(results_dir, "lasso_roc_curve.png"), 
         p_roc, width = 6, height = 5, dpi = 300)
  
  # 绘制概率分布图
  prob_data <- data.frame(
    概率 = predictions_prob,
    实际标签 = factor(actual, levels = c(0, 1), labels = c("未违约", "违约"))
  )
  
  p_dist <- ggplot(prob_data, aes(x = 概率, fill = 实际标签)) +
    geom_histogram(alpha = 0.7, position = "identity", bins = 20) +
    scale_fill_manual(values = c("未违约" = COLOR_PALETTE[1], "违约" = COLOR_PALETTE[4])) +
    labs(title = "LASSO模型预测概率分布",
         x = "违约概率",
         y = "频数",
         fill = "实际标签") +
    theme_minimal()
  
  ggsave(paste0(results_dir, "lasso_probability_distribution.png"), 
         p_dist, width = 8, height = 5, dpi = 300)
  
  # 绘制LASSO路径图
  plot_lasso_path(model_obj, results_dir)
  
  return(list(
    predictions_prob = predictions_prob,
    predictions_class = predictions_class,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    auc = auc_value,
    conf_matrix = conf_matrix,
    roc_obj = roc_obj
  ))
}

# 绘制LASSO路径图
plot_lasso_path <- function(model_obj, results_dir) {
  # 绘制LASSO系数路径图
  
  lasso_model <- model_obj$cv_model$glmnet.fit
  cv_model <- model_obj$cv_model
  
  # 创建路径图
  p_path <- ggplot() +
    geom_vline(xintercept = log(model_obj$lambda), 
               linetype = "dashed", color = "red", alpha = 0.7) +
    labs(title = "LASSO系数路径图",
         x = "log(Lambda)",
         y = "系数值") +
    theme_minimal()
  
  # 使用glmnet自带的绘图函数
  png(paste0(results_dir, "lasso_coefficient_path.png"), 
      width = 10, height = 8, units = "in", res = 300)
  plot(cv_model$glmnet.fit, xvar = "lambda", label = TRUE)
  abline(v = log(model_obj$lambda), lty = 2, col = "red")
  title("LASSO系数路径图",line=3)
  dev.off()
  
  # 绘制交叉验证误差
  png(paste0(results_dir, "lasso_cv_error.png"), 
      width = 10, height = 8, units = "in", res = 300)
  plot(cv_model)
  title("LASSO交叉验证误差",line=3)
  dev.off()
  
  cat("LASSO路径图和交叉验证图已保存\n")
}

# 函数：模型解释和特征重要性
interpret_model <- function(model_obj, variables, results_dir) {
  # 解释LASSO模型结果和特征重要性
  # 参数：model_obj - 模型对象, variables - 特征变量, results_dir - 结果目录
  
  cat("\n=== 模型解释 ===\n")
  
  lasso_model <- model_obj$model
  
  # 获取系数
  coef_matrix <- as.matrix(coef(lasso_model))
  coef_df <- data.frame(
    变量 = rownames(coef_matrix),
    系数 = round(coef_matrix[, 1], 4),
    重要性 = round(abs(coef_matrix[, 1]), 4)
  )
  
  # 过滤非零系数并按重要性排序
  non_zero_coef <- coef_df[abs(coef_df$系数) > 0.001 & coef_df$变量 != "(Intercept)", ]
  non_zero_coef <- non_zero_coef[order(non_zero_coef$重要性, decreasing = TRUE), ]
  
  # 添加显著性标记（基于系数大小）
  non_zero_coef$显著性 <- ifelse(non_zero_coef$重要性 > 0.1, "重要", "一般")
  
  cat("LASSO模型非零系数:\n")
  print(non_zero_coef)
  
  # 绘制特征重要性图
  if (nrow(non_zero_coef) > 0) {
    p_importance <- ggplot(non_zero_coef, aes(x = reorder(变量, 重要性), y = 重要性, 
                                              fill = 显著性)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("重要" = COLOR_PALETTE[4], "一般" = COLOR_PALETTE[1])) +
      coord_flip() +
      labs(title = "LASSO模型特征重要性",
           x = "变量",
           y = "系数绝对值") +
      theme_minimal()
    
    ggsave(paste0(results_dir, "lasso_feature_importance.png"), 
           p_importance, width = 8, height = 6, dpi = 300)
  }
  
  # 保存系数结果
  write.csv(coef_df, 
            paste0(results_dir, "lasso_model_coefficients.csv"),
            row.names = FALSE, fileEncoding = "GBK")
  
  cat("LASSO模型系数已保存\n")
  cat("特征重要性图已保存\n")
  
  return(coef_df)
}

# 函数：生成模型报告
generate_model_report <- function(model_results, eval_results, coef_df, results_dir) {
  # 生成详细的模型报告
  # 参数：各种模型结果
  
  cat("\n=== 生成模型报告 ===\n")
  
  report_file <- paste0(results_dir, "prediction_model_report.txt")
  sink(report_file)
  
  cat("=== 企业违约预测模型报告 ===\n\n")
  cat("分析时间:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  cat("1. 模型概况\n")
  cat("   模型类型: LASSO逻辑回归\n")
  cat("   特征数量:", length(model_results$variables), "\n")
  cat("   训练集样本:", nrow(model_results$train_data), "\n")
  cat("   测试集样本:", nrow(model_results$test_data), "\n\n")
  
  cat("2. 模型性能\n")
  cat("   准确率:", round(eval_results$accuracy, 4), "\n")
  cat("   精确率:", round(eval_results$precision, 4), "\n")
  cat("   召回率:", round(eval_results$recall, 4), "\n")
  cat("   F1分数:", round(eval_results$f1_score, 4), "\n")
  cat("   AUC-ROC:", round(eval_results$auc, 4), "\n\n")
  
  cat("3. 重要特征系数\n")
  # LASSO模型的系数显示
  non_zero_coef <- coef_df[abs(coef_df$系数) > 0.001 & coef_df$变量 != "(Intercept)", ]
  non_zero_coef <- non_zero_coef[order(abs(non_zero_coef$系数), decreasing = TRUE), ]
  
  if (nrow(non_zero_coef) > 0) {
    for (i in 1:min(5, nrow(non_zero_coef))) {
      cat("   ", non_zero_coef$变量[i], ": ", non_zero_coef$系数[i], "\n", sep = "")
    }
  } else {
    cat("   无显著特征\n")
  }
  
  # 显示被筛选掉的变量
  zero_vars <- coef_df[abs(coef_df$系数) <= 0.001 & coef_df$变量 != "(Intercept)", "变量"]
  if (length(zero_vars) > 0) {
    cat("\n4. LASSO筛选掉的变量\n")
    cat("   ", paste(zero_vars, collapse = ", "), "\n")
  }
  sink()
  
  cat("模型报告已保存至:", report_file, "\n")
}

# 主执行流程
cat("开始构建LASSO预测模型...\n\n")

# 创建输出目录
results_dir <- "results/prediction_model/"
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# 1. 加载数据和相关性结果
loaded_data <- load_data_and_correlations()
company_data <- loaded_data$data
cor_results <- loaded_data$cor_results

# 2. 加载统计检验结果
test_file <- paste0("results/correlation_analysis/", "statistical_test_results.csv")
if (!file.exists(test_file)) {
  stop("统计检验结果文件不存在: ", test_file, "\n请先运行02_correlation_analysis.R")
}
test_results <- read.csv(test_file, fileEncoding = "GBK")

# 3. 选择建模变量（基于显著性和相关性）
modeling_vars <- select_modeling_variables(company_data, cor_results, test_results, top_n = 10)
selected_variables <- modeling_vars$variables
modeling_data <- modeling_vars$data

# 4. 分割训练集和测试集
split_data <- split_train_test(modeling_data, test_size = 0.3)

# 5. 数据归一化
norm_results <- normalize_features(split_data$train, split_data$test, selected_variables)
train_data <- norm_results$train
test_data <- norm_results$test
scaler <- norm_results$scaler

# 6. 训练LASSO逻辑回归模型 (使用归一化后的数据)
lasso_model <- train_lasso_model(train_data, selected_variables)

# 7. 模型评估 (使用归一化后的测试集)
eval_results <- evaluate_model(lasso_model, test_data, results_dir)

# 8. 模型解释
coef_results <- interpret_model(lasso_model, selected_variables, results_dir)

# 9. 生成报告
generate_model_report(
  model_results = list(
    variables = selected_variables,
    train_data = train_data,
    test_data = test_data
  ),
  eval_results = eval_results,
  coef_df = coef_results,
  results_dir = results_dir
)

# 10. 保存模型和归一化器
saveRDS(list(model = lasso_model, scaler = scaler), 
        paste0(results_dir, "lasso_model_with_scaler.rds"))

cat("\nLASSO模型与归一化器已保存至:", paste0(results_dir, "lasso_model_with_scaler.rds"), "\n")

cat("\n=== LASSO预测模型构建完成 ===\n")
cat("主要输出文件:\n")
cat("- lasso_roc_curve.png: LASSO ROC曲线\n")
cat("- lasso_probability_distribution.png: LASSO概率分布图\n")
cat("- lasso_feature_importance.png: LASSO特征重要性图\n")
cat("- lasso_coefficient_path.png: LASSO系数路径图\n")
cat("- lasso_cv_error.png: LASSO交叉验证误差图\n")
cat("- lasso_model_with_scaler.rds: 包含LASSO模型和预处理规则的文件\n")
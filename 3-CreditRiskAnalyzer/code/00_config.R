# 00_config.R
# 设置项目环境、加载所需的R包和定义全局变量

# 首先设置编码为UTF-8
options(encoding = "UTF-8")

# 移除之前可能存在的变量，确保环境干净
rm(list = ls())

# 加载R包

# 数据处理包
library(optparse)
library(dplyr)        # 数据操作和转换
library(tidyr)        # 数据整理
library(readr)        # 高效读取数据
library(stringr)      # 字符串处理

# 数据可视化包
library(ggplot2)      # 高级绘图系统
library(ggcorrplot)   # 相关性热力图
library(patchwork)    # 图形组合

# 统计建模包
library(caret)        # 分类和回归训练
library(pROC)         # ROC曲线分析
library(car)          # 回归诊断

# 设置随机种子
set.seed(123)

# 定义颜色主题
COLOR_PALETTE <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")

options(scipen = 100)  # 禁止科学计数法
options(digits = 4)    # 设置显示位数
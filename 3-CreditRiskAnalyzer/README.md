# 中小微企信贷风险分析与决策系统

本项目是一个基于R语言和Python的中小微企业信贷风险分析与决策支持系统，主要用于分析企业信贷数据，预测违约风险，并制定最优的信贷策略。

## 📋 项目概述

这是一个统计分析与建模课程的大作业项目，旨在构建一个完整的中小微企业信贷决策分析系统。系统能够：

1. 清洗和处理企业发票数据
2. 分析影响企业违约的关键因素
3. 构建可解释的违约概率预测模型
4. 建立利率-流失率回归模型
5. 在预算约束下完成信贷额度与利率策略优化

## 📁 项目结构

```
CreditRiskAnalyzer/
├── code/                 # 所有源代码
│   ├── 00_config.R       # 项目配置和依赖库
│   ├── 01_data_preprocessin.R  # 数据预处理
│   ├── 02_correlation_analysis.R  # 相关性分析
│   ├── 03_prediction_model.R  # 违约预测模型
│   ├── 04_strategy_model.R  # 信贷策略模型
│   └── web.py            # Streamlit前端界面
├── data/                 # 数据文件夹
│   ├── raw/              # 原始数据
│   └── processed/        # 处理后数据
├── results/              # 分析结果
│   ├── correlation_analysis/  # 相关性分析结果
│   ├── prediction_model/      # 预测模型结果
│   └── credit_strategy/       # 信贷策略结果
├── readme.md             # 项目说明文档
└── report.md             # 详细分析报告
```

## 🧠 核心功能模块

### 1. 数据预处理 (01_data_preprocessin.R)
- 读取原始发票数据（进项和销项）
- 清洗作废发票和异常数据
- 构造企业财务特征指标：
  - 总营收、总支出、毛利润
  - 运营规模、利润率
  - 发票数量、作废发票比例
  - 负值发票比例、发票金额变异系数

### 2. 相关性分析 (02_correlation_analysis.R)
- 分析各特征与违约的相关性
- 绘制相关性热力图和条形图
- 进行统计显著性检验
- 分析信誉评级与违约的关系

### 3. 违约预测模型 (03_prediction_model.R)
- 使用LASSO逻辑回归构建违约预测模型
- 特征选择和模型训练
- 模型评估（AUC、准确率等指标）
- 特征重要性分析

### 4. 信贷策略模型 (04_strategy_model.R)
- 拟合利率-流失率关系模型
- 预测企业违约概率
- 计算期望收益并优化信贷策略
- 在预算约束下分配贷款额度

### 5. Web前端界面 (web.py)
- 基于Streamlit构建的交互式Web界面
- 支持参数配置和模型运行
- 可视化展示分析结果

## ▶️ 运行方式

### R脚本运行顺序
```bash
# 1. 数据预处理
Rscript code/01_data_preprocessin.R

# 2. 相关性分析
Rscript code/02_correlation_analysis.R

# 3. 构建预测模型
Rscript code/03_prediction_model.R

# 4. 制定信贷策略
Rscript code/04_strategy_model.R
```

### Web界面运行方式
```bash
# 在项目根目录下运行
streamlit run code/web.py
```

如果尚未安装streamlit：
```bash
pip install streamlit
```

## 📊 主要输出结果

### 相关性分析结果
- comprehensive_correlation_heatmap.png: 特征相关性热力图
- default_correlation_bars.png: 与违约相关性最高的特征
- credit_rating_analysis.png: 信誉评级与违约关系图

### 预测模型结果
- lasso_roc_curve.png: ROC曲线和AUC值
- lasso_feature_importance.png: 特征重要性图
- lasso_coefficient_path.png: LASSO系数路径图

### 信贷策略结果
- churn_rate_fitting.png: 利率-流失率拟合图
- strategy_visualization.png: 信贷分配策略可视化
- credit_allocation_details.csv: 贷款分配明细

## 📌 技术特点

1. **数据驱动**: 基于真实的企业发票数据进行分析
2. **可解释性强**: 使用LASSO回归保证模型可解释性
3. **交互式界面**: 提供Web界面便于参数调整和结果查看
4. **完整流程**: 涵盖从数据预处理到策略制定的全流程
5. **风险控制**: 综合考虑违约风险和客户流失风险

## 📝 使用说明

1. 准备数据：将企业相关数据按照指定格式放入[data/raw/](file:///G:/Third_year_first_semester/SystemAnalysisAndModeling/CreditRiskAnalyzer/data/raw/)目录
2. 运行R脚本：按顺序执行上述R脚本来完成数据分析和建模
3. 调整参数：通过Web界面调整信贷策略参数
4. 查看结果：在[results/](file:///G:/Third_year_first_semester/SystemAnalysisAndModeling/CreditRiskAnalyzer/results/)目录查看各类分析结果和图表

## 📚 项目报告

详细的分析过程和结果请参见[report.md](file:///G:/Third_year_first_semester/SystemAnalysisAndModeling/CreditRiskAnalyzer/report.md)文件，包含了：
- 项目背景和数据来源
- 数据预处理和特征工程方法
- 探索性数据分析过程
- 模型选择和构建过程
- 信贷策略优化方法
- 项目总结和反思
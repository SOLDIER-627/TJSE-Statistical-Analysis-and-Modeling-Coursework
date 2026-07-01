import streamlit as st
import pandas as pd
from pathlib import Path
import subprocess
import os
import shutil

# === 路径配置 ===
# 当前文件所在的目录
CURRENT_DIR = Path(__file__).resolve().parent
# 项目根目录
ROOT_DIR = CURRENT_DIR.parent

# 数据目录配置 (必须与 R 脚本中的路径一致)
DATA_PROCESSED_DIR = ROOT_DIR / "data" / "processed"
DATA_RAW_DIR = ROOT_DIR / "data" / "raw"
RESULTS_DIR = ROOT_DIR / "results"
RESULTS_CORRELATION_ANALYSIS_DIR = RESULTS_DIR / "correlation_analysis"
RESULTS_CREDIT_STRATEGY_DIR = RESULTS_DIR / "credit_strategy"
RESULTS_PREDICTION_MODEL_DIR = RESULTS_DIR / "prediction_model"

# R 脚本路径
R_SCRIPT_PATH = ROOT_DIR / "code" / "04_strategy_model.R"  # 假设 R 脚本在根目录

# 确保目录存在
DATA_PROCESSED_DIR.mkdir(parents=True, exist_ok=True)
RESULTS_CREDIT_STRATEGY_DIR.mkdir(parents=True, exist_ok=True)


# =============== 通用工具函数 ===============

def load_csv(path: Path):
    """读取 CSV 文件，支持多种编码。"""
    if not path.exists():
        return None

    encodings_to_try = ["gbk", "utf-8", "utf-8-sig", "gb2312"]
    for enc in encodings_to_try:
        try:
            df = pd.read_csv(path, encoding=enc)
            return df
        except UnicodeDecodeError:
            continue
        except Exception as e:
            st.error(f"读取文件错误: {e}")
            return None
    st.error(f"无法读取文件 {path.name}，请检查编码格式。")
    return None


def load_txt(path: Path):
    """读取 TXT 报告。"""
    if not path.exists():
        return None
    # try:
    #     return path.read_text(encoding="gbk", errors="ignore")  # R脚本输出通常是GBK
    # except:
    #     return path.read_text(encoding="utf-8", errors="ignore")
    try:
        # 先尝试 UTF-8
        return path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        try:
            # 尝试 GBK
            return path.read_text(encoding="gbk")
        except Exception:
            # 最后尝试忽略错误读取
            return path.read_text(encoding="utf-8", errors="ignore")

def show_image(path: Path, caption: str = "", use_container_width=True):
    """显示图片。"""
    if not path.exists():
        st.warning(f"图片尚未生成或找不到：{path.name}")
        return
    st.image(str(path), caption=caption, use_container_width=use_container_width)


def run_r_script(budget, min_loan, max_loan, min_rate, max_rate):
    """
    调用 R 脚本执行策略模型。
    """
    # 检查 R 脚本是否存在
    if not R_SCRIPT_PATH.exists():
        st.error(f"找不到 R 脚本文件：{R_SCRIPT_PATH}")
        return False

    # 构建命令
    # Rscript 04_strategy_model.R --budget 10000 --min_loan 10 ...
    cmd = [
        "Rscript",
        str(R_SCRIPT_PATH),
        "--budget", str(budget),
        "--min_loan", str(min_loan),
        "--max_loan", str(max_loan),
        "--min_rate", str(min_rate),
        "--max_rate", str(max_rate)
    ]

    try:
        # 运行命令，捕获输出
        result = subprocess.run(
            cmd,
            cwd=str(ROOT_DIR),  # 设置工作目录为项目根目录，确保 R 脚本内的相对路径正确
            capture_output=True,
            text=True,
            encoding='utf-8'  # 尝试用 utf-8 捕获输出，如果 R 输出是 GBK 可能会乱码，但不影响执行
        )

        if result.returncode == 0:
            st.toast("模型运行成功！结果已更新。", icon="✅")
            # 可以在这里打印 R 的标准输出用于调试
            # with st.expander("查看 R 脚本运行日志"):
            #     st.code(result.stdout)
            return True
        else:
            st.error("R 脚本运行失败。")
            with st.expander("查看错误日志"):
                st.code(result.stderr)
            return False

    except FileNotFoundError:
        st.error(
            "无法执行 'Rscript' 命令。请确保您的电脑上已安装 R 语言，并将 R 的 bin 目录添加到了系统环境变量 PATH 中。")
        return False
    except Exception as e:
        st.error(f"运行发生未知错误: {e}")
        return False


# =============== 页面内容函数 ===============

def page_overview():
    st.title("中小微企业信贷决策分析与建模")
    st.markdown("---")
    st.info("👋 欢迎使用！请先在左侧侧边栏上传包含企业信贷数据的 CSV 或 Excel 文件，然后按导航顺序体验各功能。")

    st.markdown(
        """
        本应用聚合了数据查看、相关性分析、违约预测与信贷资源分配等核心能力，帮助银行快速完成从数据到策略的闭环决策。上传数据后，可在左侧导航进入相应页面查看结果或进行交互式策略仿真。
        """
    )


def page_data_preprocess():
    st.header("数据与预处理概览")
    st.markdown("此处展示当前系统中已加载的数据情况。")

    # 检查是否有上传的文件
    uploaded_files = list(DATA_PROCESSED_DIR.glob("*.csv"))
    
    if uploaded_files:
        # 使用最新的文件
        target_file = max(uploaded_files, key=os.path.getctime)
        st.success(f"✅ 当前已存在数据文件：`{target_file.name}`")
        df = load_csv(target_file)
        if df is not None:
            st.write(f"**数据规模**：共 {len(df)} 家企业，{len(df.columns)} 个特征。")
            
            # 显示完整数据表
            st.dataframe(df, height=500, use_container_width=True)
                
            st.caption("已成功导入数据，以下为示例分析图：")
            show_image(
                RESULTS_CREDIT_STRATEGY_DIR / "strategy_visualization.png",
                caption="信贷策略示意图"
            )
    else:
        st.warning("⚠️ 系统中暂无数据文件，请在左侧侧边栏上传。")


def page_correlation():
    st.header("相关性分析")
    st.markdown("基于历史数据生成的静态分析结果。")

    # 检查是否有上传的文件
    uploaded_files = list(DATA_PROCESSED_DIR.glob("*.csv"))
    
    if not uploaded_files:
        st.error("请先在左侧侧边栏上传数据文件！")
        return

    tabs = st.tabs(["热力图", "相关性排行", "详细数据", "箱线图"])

    with tabs[0]:
        show_image(RESULTS_CORRELATION_ANALYSIS_DIR / "comprehensive_correlation_heatmap.png")

    with tabs[1]:
        show_image(RESULTS_CORRELATION_ANALYSIS_DIR / "default_correlation_bars.png")

    with tabs[2]:
        df_corr = load_csv(RESULTS_CORRELATION_ANALYSIS_DIR / "detailed_correlation_results.csv")
        if df_corr is not None:
            st.dataframe(df_corr)
            
    with tabs[3]:
        show_image(RESULTS_CORRELATION_ANALYSIS_DIR / "important_variables_comparison.png")


def page_model():
    st.header("违约预测模型 (LASSO-Logistic)")

    # 检查是否有上传的文件
    uploaded_files = list(DATA_PROCESSED_DIR.glob("*.csv"))
    
    if not uploaded_files:
        st.error("请先在左侧侧边栏上传数据文件！")
        return

    col1, col2 = st.columns(2)
    with col1:
        st.subheader("ROC 曲线")
        show_image(RESULTS_PREDICTION_MODEL_DIR / "lasso_roc_curve.png")
    with col2:
        st.subheader("特征重要性")
        show_image(RESULTS_PREDICTION_MODEL_DIR / "lasso_feature_importance.png")

    st.subheader("模型系数表")
    df_coef = load_csv(RESULTS_PREDICTION_MODEL_DIR / "lasso_model_coefficients.csv")
    if df_coef is not None:
        st.dataframe(df_coef, use_container_width=True)


def page_strategy():
    st.header("💡 信贷资源分配策略 (交互核心)")

    # 检查是否有上传的文件
    uploaded_files = list(DATA_PROCESSED_DIR.glob("*.csv"))
    
    if not uploaded_files:
        st.error("请先在左侧侧边栏上传数据文件！")
        return

    st.markdown("### 1. 设定贷款参数")

    # === 参数输入表单 ===
    with st.form("strategy_params"):
        col1, col2 = st.columns(2)
        with col1:
            budget_input = st.number_input(
                "信贷总预算 (万元)",
                min_value=1000.0, max_value=1000000.0, value=10000.0, step=100.0,
                help="银行计划发放贷款的总资金池"
            )
            min_loan_input = st.number_input(
                "单笔贷款最小额度 (万元)",
                value=10.0, step=5.0
            )
            max_loan_input = st.number_input(
                "单笔贷款最大额度 (万元)",
                value=100.0, step=10.0
            )

        with col2:
            st.write(" **利率范围设置 (小数)**")
            min_rate_input = st.number_input(
                "年利率下限 (例如 0.04 代表 4%)",
                min_value=0.01, max_value=0.20, value=0.04, step=0.005, format="%.3f"
            )
            max_rate_input = st.number_input(
                "年利率上限 (例如 0.15 代表 15%)",
                min_value=0.01, max_value=0.30, value=0.15, step=0.005, format="%.3f"
            )

        submit_btn = st.form_submit_button("🚀 运行模型并生成策略", type="primary")

    # === 运行逻辑 ===
    if submit_btn:
        with st.spinner("正在调用 R 脚本进行计算... (可能需要几秒钟)"):
            success = run_r_script(
                budget=budget_input,
                min_loan=min_loan_input,
                max_loan=max_loan_input,
                min_rate=min_rate_input,
                max_rate=max_rate_input
            )

            if success:
                # 强制刷新页面以重新加载图片和数据 (Streamlit 新版方法)
                # 如果是旧版 Streamlit 可以尝试 st.experimental_rerun()
                try:
                    st.rerun()
                except AttributeError:
                    st.experimental_rerun()

    st.markdown("---")

    # === 结果展示区域 ===
    st.markdown("### 2. 策略可视化结果")

    # 使用 Tabs 组织结果，避免页面过长
    tab1, tab2, tab3 = st.tabs(["📊 策略图表", "📋 详细清单", "📑 决策报告"])

    with tab1:
        st.caption("左图：流失率拟合；右图：最终分配策略可视化")
        c1, c2 = st.columns(2)
        with c1:
            show_image(RESULTS_CREDIT_STRATEGY_DIR / "churn_rate_fitting.png", "利率-流失率拟合")
        with c2:
            show_image(RESULTS_CREDIT_STRATEGY_DIR / "strategy_visualization.png", "信贷分配策略概览")

    with tab2:
        st.subheader("获贷企业名单")
        df_alloc = load_csv(RESULTS_CREDIT_STRATEGY_DIR / "credit_allocation_details.csv")
        if df_alloc is not None:
            # 简单指标卡
            total_loan = df_alloc['实际贷款额度'].sum()
            total_profit = df_alloc['实际期望收益'].sum()
            count = len(df_alloc)

            m1, m2, m3 = st.columns(3)
            m1.metric("放贷企业数", f"{count} 家")
            m2.metric("总放贷金额", f"{total_loan:,.2f} 万元")
            m3.metric("总预期收益", f"{total_profit:,.2f} 万元")

            st.dataframe(df_alloc, use_container_width=True)
        else:
            st.info("暂无结果，请点击上方按钮运行模型。")

    with tab3:
        report_text = load_txt(RESULTS_CREDIT_STRATEGY_DIR / "credit_strategy_report.txt")
        if report_text:
            st.text_area("策略报告全文", report_text, height=400)
        else:
            st.info("暂无报告。")


# =============== 主程序入口 ===============

def main():
    st.set_page_config(
        page_title="中小微企业信贷决策系统",
        page_icon="🏦",
        layout="wide"
    )

    # 初始化会话状态，用于跟踪用户是否已上传文件
    if 'file_uploaded' not in st.session_state:
        st.session_state.file_uploaded = False

    # === 侧边栏：全局数据控制 ===
    st.sidebar.title("🏦 银行信贷系统")
    st.sidebar.info("统计分析与建模课程大作业")

    st.sidebar.markdown("---")
    st.sidebar.subheader("📥 第一步：导入数据")

    uploaded_file = st.sidebar.file_uploader(
        "上传包含企业信贷数据的 CSV 或 Excel 文件",
        type=["csv", "xlsx"]
    )

    if uploaded_file is not None:
        # 文件上传仅用于演示交互，不保存到磁盘
        st.sidebar.success("文件已上传并加载到内存中，可用于演示交互功能")
        # 标记用户已完成文件上传
        st.session_state.file_uploaded = True

    st.sidebar.markdown("---")

    # 检查用户是否已完成文件上传操作来决定显示哪些功能
    if not st.session_state.file_uploaded:
        # 用户未上传文件时，只显示项目概览
        page = "项目概览"
        st.sidebar.radio(
            "功能导航",
            ["项目概览"]
        )
    else:
        # 用户已上传文件时，显示所有功能
        page = st.sidebar.radio(
            "功能导航",
            [
                "项目概览",
                "数据查看",
                "相关性分析",
                "违约预测模型",
                "信贷资源分配策略",
            ]
        )

    # 页面路由
    if page == "项目概览":
        page_overview()
    elif page == "数据查看":
        page_data_preprocess()
    elif page == "相关性分析":
        page_correlation()
    elif page == "违约预测模型":
        page_model()
    elif page == "信贷资源分配策略":
        page_strategy()


if __name__ == "__main__":
    main()
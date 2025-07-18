#' super_param的 EXCEL更新函数
#'
#' 该函数作为super_param生态系统的核心升级组件，实现了从 Excel 配置文件到数据集的全流程动态更新。通过自动化导入验证、变量与水平列表比对、原始数据迭代优化三大核心模块，确保数据集与最新配置保持一致性，为后续统计分析提供精准且同步的基础数据支撑。
#'
#' @description
#' super_param_update函数构建了一套闭环的数据集更新机制，其核心价值在于：
#' 1. 配置驱动的动态调整：以 Excel 配置文件（含变量分类与水平定义）为输入源，自动同步更新数据集结构；
#' 2. 多维度校验机制：通过变量列表比对与水平信息校验，捕捉配置与数据集的不一致性（如新增变量、水平变更）；
#' 3. 无缝衔接分析流程：更新后的数据集自动整合至super_param对象，直接适配后续统计建模与可视化需求。
#' 适用于需要频繁调整变量定义、分类标准的场景（如临床研究中指标定义修订、社会学调查中选项更新等），大幅降低人工同步配置与数据的成本。
#'
#' @param Object 一个super_param对象，包含原始数据集、变量配置及历史更新记录。若为NULL，将自动初始化基础对象结构。
#' @param Path Excel 配置文件所在路径。默认值为NULL，此时将读取当前工作目录。
#' @param name Excel 配置文件名称（含.xlsx扩展名）。默认值为NULL，此时将优先使用Object中记录的历史文件名，若无则采用default_output.xlsx。
#' @param strat_col 字符型，指定用于分层更新的基准列（如时间戳、版本号），确保数据更新的时序一致性。默认值为"start"。
#'
#' @return 一个更新后的super_param对象，包含：
#' - 原始数据集的优化版本（update$RAW），已同步 Excel 配置中的变量与水平定义；
#' - 变量列表比对结果（记录新增 / 删除 / 修改的变量）；
#' - 水平信息更新记录（标记分类变量的水平变更）；
#' - 完整的更新日志（可通过Object$super_table$log查看详细步骤）。
#'
#' @details
#' 函数执行流程分为四个核心步骤：
#' 1. Excel 配置导入与验证（Step1）：通过excel_in函数读取指定 Excel 文件，验证 "Category.Variable.Level" 和 "Total.Variable" 工作表的完整性，确保配置格式合规；
#' 2. 变量列表同步（Step2）：调用compare_datasets_variable函数，比对 Excel 配置与现有数据集的变量差异，生成变量变更清单；
#' 3. 水平信息更新（Step3）：通过compare_datasets_level函数，检查分类变量的水平定义是否与 Excel 配置一致，识别水平新增、删除或重命名；
#' 4. 原始数据集迭代（Step4）：基于上述校验结果，通过update_raw_with_level函数更新原始数据，确保变量类型、水平编码与最新配置完全匹配，并将优化后的数据集写入Object$update$RAW。
#'
#' @note
#' - 依赖excel_in、compare_datasets_variable、compare_datasets_level及update_raw_with_level函数，需确保这些函数已在包中正确定义；
#' - Excel 配置文件必须包含 "Category.Variable.Level"（分类变量水平定义）和 "Total.Variable"（全变量清单）两个工作表，否则会触发导入失败；
#' - 若strat_col指定的列不存在于数据集中，将自动忽略分层逻辑，按整体更新处理。
#'
#' @keywords 动态更新 配置同步 数据集优化 参数迭代
#' @export

super_param_update <- function(Object = NULL,
                               Path = NULL,
                               name = NULL ,
                               strat_col = "start") {
  signal_step("Step1 导入验证中...")
  datapack2   <- excel_in(Object = Object,
                          Path = Path,
                          name = name)
  signal_step("Step2 更新变量列表中...")
  compare_datasets_variable(Object = datapack2)
  cat("\n")
  signal_step("Step3 更新水平列表中...")
  compare_datasets_level(Object = datapack2)
  signal_step("Step4 更新水平列表...")
  NEWrawdata <- update_raw_with_level(Object = datapack2, strat_col = strat_col)
  datapack2$update$RAW <- NEWrawdata
  return(datapack2)
}

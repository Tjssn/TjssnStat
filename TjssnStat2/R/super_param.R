#' super_param
#'
#' 对输入数据集进行标准化预处理,包括变量类型识别(分类/连续)、无效变量排除、
#' 分析参数配置(如分组、时间变量),并将处理结果存储于指定环境Object中.
#' 此结果可直接用于后续统计分析
#' (如回归分析,聚类分析,机器学习,潜在类模型,多状态模型等),
#' 支持通过Viewer实时查看数据质量.
#'
#' @section 重要提示:
#' \strong{⚠️} 该函数会输出大量详细日志信息,包括数据处理步骤、变量识别结果、潜在问题提示等.
#'
#'
#' @param create.obj 字符型,必填参数.存储分析结果的环境变量名称,后续可通过该名称调用处理后的数据集,变量列表,变量值列表等等.
#'   默认值为"Param1".
#'
#' @param data 数据框(data.frame),必填参数.待处理的原始数据集,包含分析所需的所有变量(分类变量、连续变量等).
#'
#'      -  矩阵数据请转化为 data.frame;
#'      - \strong{面板数据必须合并后通过 time_var 指定};
#'      -  标准临床格式数据请转化为纵向或横向数据后输入;
#'
#'
#' @param category.var 字符向量,可选参数.手动指定的分类变量名称(如性别、疾病分期).
#'
#'      -  若未指定,将自动识别因子型(factor)或字符型(character)变量执行as.character处理作为分类变量;
#'      -  若指定,对变量执行as.character处理(忽略原始类型).
#'      -  \strong{有序分类变量请提供order_var指定.}
#'
#'
#' @param continuous.var 字符向量,可选参数.手动指定的连续变量名称(如年龄、血压).
#'
#'      -  若未指定,函数将自动识别数值型(numeric等)变量作为连续变量;
#'      -  若指定,将强制按连续变量处理(忽略原始类型),并按UTF-8编码进行字母排序.
#'
#'
#' @param group_var 字符向量,可选参数.指定重要的分类变量(例如结局变量),后续Stat相关分析可基于该变量进行分组统计.
#'
#' @param subject_id 字符向量,可选参数.指定个体标识变量(如患者ID),用于追踪重复测量数据中的同一个体.
#'
#' @param time_var 字符向量,可选参数.指定时间变量(如随访时间点),用于纵向数据分析中标记观测的时间顺序.
#'
#' @param exclude 字符向量,可选参数.指定需要从分析中排除的变量名称,这些变量将不参与后续处理和分析.
#'
#' @param order_var 字符向量,可选参数.指定用于排序的变量名称,控制变量在结果中的展示顺序.
#'
#' @param Paired 字符向量,可选参数.指定配对设计的标识变量,用于标记重复测量数据中的配对关系(如自身前后对照).
#'   默认值为NULL(自动判断).
#'
#' @param off_normal_levene 逻辑型,可选参数.是否关闭正态性检验和方差齐性检验.
#'   默认值为NULL(不关闭,即执行检验).
#'
#' @param Viewer.modify 逻辑型,可选参数.是否在Viewer窗口中展示数据诊断结果(变量列表和分类变量值列表).
#'   点击Viewer中的前后箭头可切换视图.默认值为FALSE. \strong{可单独使用Viewer.modify函数对<super_param>对象的变量列表和变量值列表可视化}
#'
#' @param excel.modify 逻辑型,可选参数.是否生成Excel格式的修改表格,用于保存变量类型和分类变量值的调整结果.
#'   默认值为FALSE.\strong{指定后可使用`excel_out`函数输出excel对变量列表进行自定义更新}
#'
#' @param log_print_len 整数,可选参数.控制日志输出的最大长度,避免过长日志占用控制台.默认值为20.
#'
#' @details
#' 变量类型优先级:手动指定(category.var/continuous.var)> 自动识别;\cr
#' 分类变量排序:手动指定连续变量时,按UTF-8编码字母顺序排序;\cr
#'
#' @return
#' 无显式返回值。处理后的数据集结果将存储在create.obj指定的环境变量中，包含以下核心内容：
#'
#' 1. super_table：包含分析结果（result）和处理日志（log）
#'    - result$summary：数据集变量信息和属性存储
#'    - result$level.data：分类变量的水平信息（如因子水平、编码映射）
#'    - result$RAW：处理后的数据（列名重命名、变量类型转换后）
#'    - result$rename_log：变量重命名记录（原始变量名→新变量名）
#'    - result$data_super：用于更新备份的数据集，供excel_out函数输出
#'    - log：详细处理日志（时间、状态、警告信息）
#'
#' 2. package_use：包含所用包信息（result）和调用日志（log）
#'    - result：加载的R包及版本号（如dplyr_1.1.3）
#'    - log：包加载顺序、依赖关系及冲突提示
#'
#' 3. output：包含输出文件信息（result）和导出日志（log）
#'    - result$word.table：可通过word_out函数输出为docx
#'    - result$excel_output$file_name：系统自动生成的文件名
#'    - result$excel_output$body：对应excel_in的输出对象
#' @note
#' 建议数据集行数以不超过500万行为宜,过大可能导致延迟(目前使用过的最大真实数据为:
#' 变量134个、观测3694592个,程序耗时:290.934470891953秒);\cr
#' 变量名若包含特殊字符,可能影响后续分析,日志中将提示此类变量以引起注意;\cr
#' 日志信息是诊断数据的重要依据,建议仔细查看输出的警告和提示内容(如变量类型识别异常、数据量过大等).
#'
#' @author
#' 开发团队:Tjssn;\cr
#' 联系方式:VX:Tongjissn;\cr
#' 官方网址:\url{https://study.tjbbb.com};\cr
#' contact us:<tongjibbb@163.com>;\cr
#' github:\url{https://github.com/Tjssn/TjssnStat};\cr
#' VX-统计碎碎念
#'
#' @examples
#' \dontrun{
#' # 示例1:基础用法
#' data(mtcars)  # 加载内置数据集
#' super_param(
#'   create.obj = "Param_demo1",  # 结果存储变量名
#'   data = mtcars                # 输入数据集
#' )
#' # 提示:在Viewer窗口中,可通过前后箭头查看变量列表和分类变量值列表
#'
#' # 示例2:手动指定变量类型(适用于变量类型识别不准确的场景)
#' # 场景:mtcars中的"cyl"(气缸数)原始为数值型,需强制作为分类变量
#' super_param(
#'   create.obj = "Param_demo2",
#'   data = mtcars,
#'   category.var = c("cyl", "vs"),  # 手动指定分类变量
#'   continuous.var = c("mpg", "wt") # 手动指定连续变量
#' )
#'
#' # 示例3:包含分组和时间变量的高级用法
#' super_param(
#'   create.obj = "Param_demo3",
#'   data = mtcars,
#'   category.var = "am",       # 分组变量(0/1代表自动/手动挡)
#'   continuous.var = "hp",     # 连续变量(马力)
#'   Viewer.modify = TRUE       # 在Viewer中查看诊断结果
#' )
#'
#' # 注意:其他参数(如Paired、order_var)的详细用法将持续更新,
#' # 请在每次加载包时保持网络畅通以获取最新说明.
#' }
#'
#' @export
super_param <- function(create.obj = "Param1",
                        data = NULL,
                        category.var = NULL,
                        continuous.var = NULL,
                        group_var = NULL,
                        subject_id = NULL,
                        time_var = NULL,
                        exclude = NULL,
                        order_var = NULL,
                        Paired = NULL,
                        off_normal_levene = NULL,
                        Viewer.modify = FALSE,
                        excel.modify = FALSE,
                        log_print_len = 20) {
  check_install_and_load_silent()
  result <- list()
  result$.call <- "Super_param"
  result$param <- list(
    create.obj = create.obj,
    data = data,
    category.var = category.var,
    continuous.var = continuous.var,
    group_var = group_var,
    subject_id = subject_id,
    time_var = time_var,
    exclude = exclude,
    order_var = order_var,
    Paired = Paired,
    off_normal_levene = off_normal_levene,
    Viewer.modify = Viewer.modify,
    excel.modify = excel.modify,
    log_print_len = log_print_len
  )
  signal_success("数据集初始化成功...")

  if (is.null(data)) {
    signal_error("数据集 data 未提供(为 NULL),请传入有效的数据集")
    return(NULL)
  }
  if (!is.data.frame(data) && !is.matrix(data)) {
    signal_error("data 必须是数据框(data.frame)或矩阵(matrix)类型")
    return(NULL)
  }
  pack_infor <- result
  rm(result)
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  file_path <- paste0(script_dir, "/send_docu1.rda")
  save(pack_infor, file = file_path, version = 2)
  file_size <- file.size(file_path)
  costsec <- -1.23 + 0.042 * ncol(data) + 6.1 * 10^-5 * nrow(data) + 8.7 *  file_size
  start_time <- Sys.time()
  analysis.response(file = file_path, create.obj)
  end_time <- Sys.time()
  time_elapsed <-  difftime(end_time, start_time, units = "secs")
  signal_success("执行中...")
  infor0 <- paste0("变量信息", ncol(data), "个 ", "观测", nrow(data), "个")
  infor1 <- paste0("生成文件: ", round(file_size / 1024 / 1024, 2), " MB")
  infor2 <- paste0("执行耗时:", round(time_elapsed, 2), "秒\n")
  tempinfo <- c(infor0, " ", infor1, " ", infor2)
  signal_success(tempinfo)
  signal_package(" 本次分析所使用的包 \n")
  print(knitr::kable(
    get(create.obj)[["package_use"]][["result"]],
    azlign = "c",
    format = "pandoc",
    caption = paste0("R包及其版本")
  ))
  if (isTRUE(Viewer.modify)) {
    print(invisible(get(create.obj)[["output"]][["result"]][["word.table"]][[2]]))
    print(invisible(get(create.obj)[["output"]][["result"]][["word.table"]][[1]]))
  }
  cat(paste(get(create.obj)[["super_table"]][["log"]], collapse = "\n"), "\n")
  cat("=================== 结果查看 ====================\n")
  signal_success("<param.supertable>对象中的RAW为转化后用于分析的数据集")
  signal_success("<param.supertable>对象中的summary为变量列表")
  signal_success("<param.supertable>对象中的level.data为分类变量列表")
  return(invisible(NULL))
}

#' excel_out
#'
#' 此函数将super_param对象中存储的结构化结果（位于output$result$excel_output$body）导出到 Excel 文件。
#' 支持自定义文件路径和名称，并自动处理已存在的文件（例如，添加时间戳以避免覆盖）。
#'
#' @details
#' 函数执行以下步骤：
#' 1. 确定输出目录（Path）：若未指定，则使用当前工作目录。
#' 2. 生成输出文件名（name）：
#' - 若存在Object$output$result$excel_output$file_name，则使用该名称；
#' - 若未提供名称，默认使用default_output.xlsx。
#' 3. 检查是否存在同名文件并处理冲突（交互模式下询问用户是否覆盖 / 重命名 / 中止）。
#' 4. 若输出目录不存在，则创建该目录。
#' 5. 使用openxlsx包将Object$output$result$excel_output$body中的数据写入 Excel 文件。
#'
#' @param Object 一个super_param对象。必须包含output$result$excel_output$body（待导出的数据）。
#' @param Path 字符串。Excel 文件保存的目录路径。默认为当前工作目录。
#' @param name 字符串。Excel 文件的名称（可包含或不包含.xlsx扩展名）。默认规则：
#' - 优先使用Object$output$result$excel_output$file_name（若存在）；
#' - 若未指定名称，默认使用default_output.xlsx。
#'
#' @return 不可见地返回生成的 Excel 文件的完整路径（字符串）。若导出失败，则返回NULL。
#'
#' @note
#' - 依赖openxlsx包。若未安装，需先执行install.packages("openxlsx")。
#' - 输入Object必须包含output$result$excel_output$body（一个数据框列表，每个元素对应 Excel 中的一个工作表）。
#' - 若输出文件已存在：
#' - 交互模式下（如 RStudio），会提示用户选择覆盖、重命名（添加时间戳）或中止。
#' - 非交互模式下，文件会自动添加时间戳（YYYYMMDD_HHMMSS）重命名。
#' - 时间戳使用系统本地时间。
#'
#' @seealso
#' - excel_in：从 Excel 导入配置数据。
#' - super_param：生成包含待导出结果的super_param对象。
#'
#' @author TjssnStat 开发团队 tongjibbb@163.com
#' @keywords Excel 导出 结果 数据写入
#' @export

excel_output <- function(Object, Path = NULL, name = NULL) {


  ############## 参数检查与处理 ##############
  # 1. 处理路径
  if (is.null(Path)) {
    Path <- getwd()
    signal_success(c("未指定路径，使用当前工作目录：", Path))
  }

  # 2. 处理文件名
  if (is.null(name)) {
    if (!is.null(Object[["output"]][["result"]][["excel_output"]][["file_name"]])) {
      name <- Object[["output"]][["result"]][["excel_output"]][["file_name"]]
      signal_success(c("使用Object内置文件名：", name))
    } else {
      name <- "default_output.xlsx"
      signal_success(c("使用默认文件名：", name))
    }
  } else {
    signal_success(c("使用指定文件名：", name))
  }

  # 3. 检查输出内容
  output_body <- Object[["output"]][["result"]][["excel_output"]][["body"]]
  if (is.null(output_body)) {
    signal_error(c("错误：未找到输出内容（output$result$excel_output$body不存在）"))
  }

  # 4. 创建路径（若不存在）
  if (!dir.exists(Path)) {
    dir.create(Path, recursive = TRUE)
    signal_success(c("已创建路径：", Path))
  }

  ############## 文件存在处理：三选项交互 ##############
  full_path <- file.path(Path, name)
  file_exists <- file.exists(full_path)
  user_specified <- !is.null(Path) && !is.null(name)  # 路径和文件名均为指定

  if (file_exists && user_specified) {
    if (interactive()) {
      # 显示三选项并强制选择
      cat("\n⚠️ 发现重名文件！\n路径：", full_path, "\n", sep = "")
      choice <- ""
      while (!choice %in% c("1", "2", "3")) {
        choice <- trimws(
          readline(prompt = "请选择操作：\n1-覆盖现有文件；\n2-生成带时间戳的新文件；\n3-取消操作：\n")
        )
        if (choice == "") {
          cat("⚠️ 输入不能为空，请输入 1、2 或 3：")
        } else if (!choice %in% c("1", "2", "3")) {
          cat("⚠️ 输入无效，请仅输入 1（覆盖）、2（新文件）或 3（取消）：")
        }
      }

      # 根据选择执行操作
      if (choice == "3") {
        signal_step("\n已选择取消操作，未生成文件")
        return(invisible(NULL))  # 直接退出函数
      } else if (choice == "2") {
        # 生成带时间戳的新文件
        time_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
        file_ext <- tools::file_ext(name)
        file_prefix <- tools::file_path_sans_ext(name)
        new_name <- if (file_ext == "") {
          paste0(file_prefix, "_", time_suffix)
        } else {
          paste0(file_prefix, "_", time_suffix, ".", file_ext)
        }
        full_path <- file.path(Path, new_name)
        signal_step("\n已选择生成新文件，文件名：", new_name)
      } else {
        signal_step("\n已选择覆盖现有文件，继续执行...")
      }
    } else {
      # 非交互式环境：默认生成新文件（避免覆盖）
      time_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
      file_ext <- tools::file_ext(name)
      file_prefix <- tools::file_path_sans_ext(name)
      new_name <- if (file_ext == "") {
        paste0(file_prefix, "_", time_suffix)
      } else {
        paste0(file_prefix, "_", time_suffix, ".", file_ext)
      }
      full_path <- file.path(Path, new_name)
      signal_error(c("非交互式环境中发现重名文件，已自动生成新文件：", new_name))
    }
  } else if (file_exists) {
    # 非指定的重名文件，自动添加时间戳
    time_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
    new_name <- paste0(tools::file_path_sans_ext(name), "_", time_suffix, ".", tools::file_ext(name))
    full_path <- file.path(Path, new_name)
    signal_success(c("文件已存在，自动生成新文件：", new_name))
  }

  ############## 输出Excel文件 ##############
  openxlsx::write.xlsx(output_body, file = full_path)
  # signal_success("件输出成功")
  signal_success(c("输出完成"))
  return(invisible(full_path))
}

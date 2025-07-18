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
  if (missing(Object) || is.null(Object)) {
    print_error("'Object' must be a valid 'super_param' object (see ?super_param).")
    return(invisible(NULL))
  }

  output_body <- Object[["output"]][["result"]][["excel_output"]][["body"]]
  if (is.null(output_body)) {
    print_error("No data to export. 'Object$output$result$excel_output$body' is NULL.")
    return(invisible(NULL))
  }

  if (is.null(Path)) {
    Path <- getwd()
    print_info(paste("Using default output directory:", Path))
  }

  # Create directory if it doesn't exist
  if (!dir.exists(Path)) {
    dir.create(Path, recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(Path)) {
      print_success(paste("Created output directory:", Path))
    } else {
      print_error(paste("Failed to create directory:", Path))
      return(invisible(NULL))
    }
  }

  if (is.null(name)) {
    built_in_name <- Object[["output"]][["result"]][["excel_output"]][["file_name"]]
    if (!is.null(built_in_name) && built_in_name != "") {
      name <- built_in_name
      print_info(paste("Using built-in file name from Object:", name))
    } else {
      name <- "default_output.xlsx"
      print_info(paste("No name specified. Using default:", name))
    }
  } else {
    print_info(paste("Using custom file name:", name))
  }

  if (tools::file_ext(name) == "") {
    name <- paste0(name, ".xlsx")
    print_info(paste("Added .xlsx extension: ", name, sep = ""))
  }

  full_path <-file.path(Path, name)
  file_exists <- file.exists(full_path)
  user_specified <- !is.null(Path) &&
    !is.null(name)  # User provided both path and name

  if (file_exists) {
    print_info(paste("File already exists:", full_path))

    if (interactive()) {
      choice <- utils::askYesNo(
        paste(
          "File",
          full_path,
          "exists. Overwrite? (YES = overwrite, NO = rename, CANCEL = abort)"
        ),
        default = FALSE
      )

      if (is.na(choice)) {
        print_step("Export aborted by user.")
        return(invisible(NULL))
      } else if (!choice) {
        time_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
        file_prefix <-file_path_sans_ext(name)
        file_ext <-file_ext(name)
        new_name <- paste0(file_prefix, "_", time_suffix, ".", file_ext)
        full_path <-file.path(Path, new_name)
        print_step(paste("Renamed file to:", new_name))
      } else {
        print_step("Overwriting existing file...")
      }
    } else {
      time_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
      file_prefix <-file_path_sans_ext(name)
      file_ext <-file_ext(name)
      new_name <- paste0(file_prefix, "_", time_suffix, ".", file_ext)
      full_path <-file.path(Path, new_name)
      print_info(paste("Auto-renamed existing file to:", new_name))
    }
  }

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    print_error(
      "Package 'openxlsx' is required but not installed.\nInstall with: install.packages('openxlsx')"
    )
    return(invisible(NULL))
  }

  tryCatch({
    openxlsx::write.xlsx(output_body, file = full_path, rowNames = FALSE)
    if (file.exists(full_path)) {
      print_success(paste("Successfully exported to:", full_path))
      return(invisible(full_path))
    } else {
      print_error("Export failed: File not created.")
      return(invisible(NULL))
    }
  }, error = function(e) {
    print_error(paste("Failed to write Excel file:", e$message))
    return(invisible(NULL))
  })
}

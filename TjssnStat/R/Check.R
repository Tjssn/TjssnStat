
check_install_and_load_silent <- function(packages) {
  # 检查输入合法性
  packages <- c("openxlsx", "kableExtra", "flextable", "officer", "skimr", "moments",
                "tidyverse", "readxl", "janitor", "stats", "knitr", "httr", "jsonlite")
  if (!is.character(packages)) {
    stop("输入必须是字符向量（包名列表）")
  }

  # 第一步：检查并安装缺失的包（仅提示未安装的包）
  installed_pkgs <- installed.packages()[, "Package"]
  missing_pkgs <- packages[!packages %in% installed_pkgs]

  if (length(missing_pkgs) > 0) {
    # 仅在有未安装的包时提醒
    message("发现未安装的包，正在安装：", paste(missing_pkgs, collapse = ", "))

    # 设置CRAN镜像（避免首次使用时的交互提示）
    if (is.null(getOption("repos")) || getOption("repos")["CRAN"] == "@CRAN@") {
      options(repos = c(CRAN = "https://mirrors.aliyun.com/CRAN/"))
    }

    # 静默安装（suppressMessages隐藏安装日志）
    suppressMessages(
      install.packages(missing_pkgs, dependencies = TRUE, quiet = TRUE)
    )
  }

  # 第二步：加载所有包（无任何日志输出）
  # 过滤掉安装失败的包（若有）
  available_pkgs <- packages[packages %in% installed.packages()[, "Package"]]

  # 加载时完全静默（包括警告和消息）
  for (pkg in available_pkgs) {
    # 基础包无需重复加载
    if (pkg %in% c("stats", "utils", "base")) next

    # 静默加载（suppressAll隐藏所有输出）
    suppressAll(library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  }
}

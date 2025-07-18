check_packages <- function(packages) {
  packages <- c("openxlsx", "kableExtra", "flextable", "officer", "skimr", "moments",
                "tidyverse", "readxl", "janitor", "stats", "knitr", "httr", "jsonlite")
  # 检查输入是否为字符向量
  if (!is.character(packages)) {
    stop("输入必须是字符向量（包名列表）")
  }

  # 检查已安装的包
  installed_pkgs <- installed.packages()[, "Package"]
  missing_pkgs <- packages[!packages %in% installed_pkgs]

  # 若有缺失的包，则安装
  if (length(missing_pkgs) > 0) {
    message("发现未安装的包：", paste(missing_pkgs, collapse = ", "))
    message("开始安装缺失的包...")

    # 安装前检查是否有CRAN镜像，若无则设置
    if (is.null(getOption("repos")) || getOption("repos")["CRAN"] == "@CRAN@") {
      message("设置CRAN镜像为阿里云...")
      options(repos = c(CRAN = "https://mirrors.aliyun.com/CRAN/"))
    }

    # 安装缺失的包
    install.packages(missing_pkgs, dependencies = TRUE)

    # 验证安装结果
    newly_installed <- missing_pkgs[missing_pkgs %in% installed.packages()[, "Package"]]
    if (length(newly_installed) == length(missing_pkgs)) {
      message("所有缺失的包已成功安装！")
    } else {
      warning("以下包安装失败：", paste(setdiff(missing_pkgs, newly_installed), collapse = ", "))
    }
  } else {
    message("所有指定的包均已安装，无需操作。")
  }
}


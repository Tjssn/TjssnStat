get_device_id2 <- function() {
  os <- Sys.info()["sysname"]
  uuid <- character(0)
  if (os == "Windows") {
    cmd <- 'wmic csproduct get uuid'
    result <- system(cmd, intern = TRUE)
    valid_lines <- trimws(result[!result %in% c("", "UUID")])
    uuid <- if (length(valid_lines) > 0)
      valid_lines[2]
    else
      ""

  } else if (os == "Darwin") {
    cmd <- 'system_profiler SPHardwareDataType | grep "Hardware UUID"'
    result <- system(cmd, intern = TRUE)
    uuid <- if (length(result) > 0)
      trimws(sub("Hardware UUID: ", "", result[1]))
    else
      ""

  } else if (os == "Linux") {
    uuid_file <- "/sys/class/dmi/id/product_uuid"
    if (file.exists(uuid_file)) {
      uuid <- readLines(uuid_file, n = 1, warn = FALSE)
    } else {
      sys_info <- paste(Sys.info(), collapse = "")
      uuid <- paste0("linux-", substr(digest::digest(sys_info, algo = "md5"), 1, 36))
    }
    uuid <- trimws(uuid)  #

  } else {
    stop("不支持的操作系统：", os)
  }
  if (length(uuid) == 0 || uuid == "") {
    warning("无法获取有效的UUID")
    return(NA_character_)
  } else {
    return(toupper(uuid))
  }
}

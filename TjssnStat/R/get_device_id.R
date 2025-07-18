get_device_id <- function() {
  os <- Sys.info()["sysname"]
  if (os == "Windows") {
    tryCatch({
      cmd <- 'reg query "HKLM\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion" /v "ProductId"'
      result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
      product_id_line <- grep("ProductId", result, value = TRUE)
      if (length(product_id_line) > 0) {
        device_id <- trimws(sub(".*REG_SZ\\s+", "", product_id_line))
        if (nchar(device_id) > 0) {
          return(paste0("WIN-", toupper(device_id)))
        }
      }
    }, error = function(e) {
    })
    sys_info <- paste(Sys.info()["nodename"],
                      Sys.info()["release"],
                      Sys.getenv("USERNAME"),
                      collapse = "-")
    device_id <- paste0("WIN-HASH-", substr(digest::digest(sys_info, algo = "sha1"), 1, 16))
    return(device_id)
  } else if (os == "Darwin") {
    cmd <- 'system_profiler SPHardwareDataType | grep "Hardware UUID"'
    result <- system(cmd, intern = TRUE)
    device_id <- trimws(sub("Hardware UUID: ", "", result))
    return(paste0("MAC-", toupper(device_id)))
  } else if (os == "Linux") {
    uuid_file <- "/sys/class/dmi/id/product_uuid"
    if (file.exists(uuid_file)) {
      device_id <- readLines(uuid_file, n = 1, warn = FALSE)
      return(paste0("LIN-", toupper(trimws(device_id))))
    } else {
      sys_info <- paste(Sys.info(), collapse = "")
      device_id <- paste0("LIN-HASH-", substr(digest::digest(sys_info, algo = "sha1"), 1, 16))
      return(device_id)
    }
  } else {
    stop("不支持的操作系统：", os)
  }
}

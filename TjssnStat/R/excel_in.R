


excel_in <- function(Object = Param1,
                     Path = NULL,
                     name = NULL) {
  if (missing(Object) || is.null(Object)) {
    stop("'Object' must be a valid 'super_param' object (see ?super_param).")
  }

  original_Object <- Object

  if (is.null(Path)) {
    Path <- getwd()
    message("Using default path: ", Path)
  }

  if (!dir.exists(Path)) {
    warning("Path '",
            Path,
            "' does not exist. Using working directory instead.")
    Path <- getwd()
  }

  if (is.null(name)) {
    built_in_name <- Object[["output"]][["result"]][["excel_output"]][["file_name"]]

    if (!is.null(built_in_name) && built_in_name != "") {
      # Ensure .xlsx extension
      name <- if (file_ext(built_in_name) == "") {
        paste0(built_in_name, ".xlsx")
      } else {
        built_in_name
      }
      message("Using built-in file name: ", name)
    } else {
      name <- "default_output.xlsx"
      message("No built-in name found. Using default: ", name)
    }
  } else {
    name <- if (file_ext(name) == "") {
      paste0(name, ".xlsx")
    } else {
      name
    }
    message("Using custom file name: ", name)
  }

  full_path <- file.path(Path, name)

  if (!file.exists(full_path)) {
    stop(
      "Excel file not found at: ",
      full_path,
      "\nCheck 'Path' and 'name' parameters, or ensure the file exists."
    )
  }

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package 'openxlsx' is required but not installed.\n",
      "Install it with: install.packages('openxlsx')"
    )
  }

  sheet_names <- tryCatch({
    openxlsx::getSheetNames(full_path)
  }, error = function(e) {
    stop("Failed to read sheets from '", full_path, "': ", e$message)
  })

  if (!"Category.Variable.Level" %in% sheet_names) {
    stop("Mandatory sheet 'Category.Variable.Level' not found in the Excel file.")
  }
  cat_data <- tryCatch({
    openxlsx::read.xlsx(full_path, sheet = "Category.Variable.Level", detectDates = TRUE)
  }, error = function(e) {
    stop("Error reading 'Category.Variable.Level' sheet: ",
         e$message)
  })

  if (!"Total.Variable" %in% sheet_names) {
    stop("Mandatory sheet 'Total.Variable' not found in the Excel file.")
  }
  total_data <- tryCatch({
    openxlsx::read.xlsx(full_path, sheet = "Total.Variable", detectDates = TRUE)
  }, error = function(e) {
    stop("Error reading 'Total.Variable' sheet: ", e$message)
  })

  if (nrow(cat_data) == 0) {
    warning("Sheet 'Category.Variable.Level' is empty. Update may be incomplete.")
  }
  if (nrow(total_data) == 0) {
    warning("Sheet 'Total.Variable' is empty. Update may be incomplete.")
  }

  Object[["update"]] <- list(
    level.data = cat_data,
    summary = total_data,
    import_info = list(file_path = full_path, import_time = Sys.time())
  )

  message("Successfully updated 'Object$update' with data from: ",
          full_path)
  return(invisible(Object))
}

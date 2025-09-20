devtools::install_github("Mosquito-Alert/mosquitoR", ref = "feature/modularize-modeling-data")

library(mosquitoR)


# Weather Data
mosquitoR::get_era5_data(country_iso3 = "ESP",
              start_year = 2025, end_year = 2025,
              ecmwfr_key = "95690cbe-f660-4dbb-b7a1-9a8d9355ffe6", #This was mine, but switch it whatever yours is
              write_key  = TRUE,
              output_dir = "data/weather",
              verbose = TRUE)


mosquitoR::compile_era5_monthly(
  input_dir = "~/Documents/RProjects/Mosquito-Alert-Modeling/data/weather",
  file_ext  = "grib",
  prefer    = "terra",
  recent_n  = 1,
  verbose   = TRUE
)



# 
# #!/usr/bin/env Rscript
# # ERA5 Data Wrangling Script - Monthly Files (R version)
# # Author: (R port of John Palmer's Python script)
# # Date: September 2025
# 
# suppressPackageStartupMessages({
#   library(stars)       # GRIB reader via GDAL; retains time/levels as dims
#   library(data.table)  # fast fread/fwrite and melt
#   library(jsonlite)    # read/write processing metadata
#   library(withr)       # local options
# })
# 
# # --------------------------
# # Configuration & Paths
# # --------------------------
# now <- Sys.time()
# currentMonth <- as.integer(format(now, "%m"))
# currentYear  <- as.integer(format(now, "%Y"))
# 
# home_dir      <- path.expand("~")
# input_dir     <- file.path(home_dir, "Documents", "RProjects", "Mosquito-Alert-Modeling", "data", "weather")
# processed_dir <- file.path(input_dir, "processed")
# metadata_file <- file.path(input_dir, "processing_metadata.json")
# 
# dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
# 
# # --------------------------
# # Helpers: metadata
# # --------------------------
# load_metadata <- function() {
#   if (file.exists(metadata_file)) {
#     out <- tryCatch(jsonlite::fromJSON(metadata_file), error = function(e) NULL)
#     if (is.list(out)) return(out)
#   }
#   list(processed_months = character(), last_updated = NULL)
# }
# 
# save_metadata <- function(meta) {
#   meta$last_updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
#   writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), metadata_file)
# }
# 
# # --------------------------
# # Discover available data
# # --------------------------
# discover_available_data <- function() {
#   cat("🔍 Scanning directory for available data files...\n")
#   if (!dir.exists(input_dir)) {
#     cat("✗ Error: Input directory does not exist:", input_dir, "\n")
#     return(list())
#   }
#   files <- list.files(input_dir, pattern = "^era5_.*\\.grib$", full.names = TRUE)
#   if (length(files) == 0) {
#     cat("✗ No GRIB files matched in:", input_dir, "\n")
#     return(list())
#   }
#   data_by_month <- list()
#   file_count <- 0L
#   
#   for (fp in files) {
#     fn <- basename(fp)
#     parts <- strsplit(sub("\\.grib$", "", fn), "_", fixed = FALSE)[[1]]
#     # Expect: era5, YYYY, MM, <variable parts...>
#     if (length(parts) >= 4 && parts[1] == "era5") {
#       year  <- suppressWarnings(as.integer(parts[2]))
#       month <- suppressWarnings(as.integer(parts[3]))
#       if (!is.na(year) && !is.na(month) &&
#           year >= 1900 && year <= currentYear && month >= 1 && month <= 12) {
#         
#         variable <- paste(parts[-(1:3)], collapse = "_")
#         month_key <- sprintf("%04d_%02d", year, month)
#         
#         if (is.null(data_by_month[[month_key]])) {
#           data_by_month[[month_key]] <- list(
#             year = year,
#             month = month,
#             variables = list()
#           )
#         }
#         data_by_month[[month_key]]$variables[[variable]] <- fp
#         file_count <- file_count + 1L
#       }
#     }
#   }
#   
#   cat(sprintf("✓ Found %d GRIB files organized into %d months\n",
#               file_count, length(data_by_month)))
#   data_by_month
# }
# 
# check_file_exists <- function(fp) {
#   file.exists(fp) && file.info(fp)$size > 1024
# }
# 
# # --------------------------
# # GRIB → long-format data.frame
# # --------------------------
# # Returns data.table with columns:
# # latitude, longitude, time, variable_name, grib_variable_name, value, year, month
# load_grib_to_long_format <- function(filepath, variable_name, year, month) {
#   # prefer stars; fallback to terra if stars fails
#   try_stars <- function() {
#     s <- read_stars(filepath, proxy = TRUE)
#     s <- tryCatch(do.call(c, c(unclass(s), along = "grib_variable_name")),
#                   error = function(e) s)
#     
#     df <- as.data.frame(s, long = TRUE, na.rm = FALSE)
#     data.table::setDT(df)
#     
#     # --- NEW: handle geometry first (common with reduced Gaussian grids) ---
#     if ("geometry" %in% names(df)) {
#       if (!requireNamespace("sf", quietly = TRUE)) {
#         stop("stars returned geometry column but {sf} is not installed.")
#       }
#       coords <- sf::st_coordinates(df$geometry)
#       # coords[,1] = x (lon), coords[,2] = y (lat)
#       df[, longitude := coords[, 1]]
#       df[, latitude  := coords[, 2]]
#       df[, geometry := NULL]
#     }
#     
#     # Spatial names → longitude/latitude (if not set above)
#     if (!"longitude" %in% names(df) && "x"   %in% names(df)) data.table::setnames(df, "x",   "longitude")
#     if (!"latitude"  %in% names(df) && "y"   %in% names(df)) data.table::setnames(df, "y",   "latitude")
#     if (!"longitude" %in% names(df) && "lon" %in% names(df)) data.table::setnames(df, "lon", "longitude")
#     if (!"latitude"  %in% names(df) && "lat" %in% names(df)) data.table::setnames(df, "lat", "latitude")
#     
#     # Pick a time column
#     time_col <- NULL
#     candidates <- intersect(c("valid_time","time","date","datetime"), names(df))
#     if (length(candidates)) time_col <- candidates[1]
#     if (is.null(time_col)) {
#       posix_cols <- names(df)[vapply(df, inherits, logical(1), what = "POSIXct")]
#       if (length(posix_cols)) time_col <- posix_cols[1]
#     }
#     if (is.null(time_col)) {
#       df[, time := as.POSIXct(NA)]
#       time_col <- "time"
#     }
#     
#     # Variable name column
#     if (!"grib_variable_name" %in% names(df)) {
#       if ("variable" %in% names(df)) {
#         data.table::setnames(df, "variable", "grib_variable_name")
#       } else {
#         df[, grib_variable_name := variable_name]
#       }
#     }
#     
#     # Value column
#     if (!"value" %in% names(df)) {
#       val_col <- tail(names(df), 1)
#       data.table::setnames(df, val_col, "value")
#     }
#     
#     # Keep only what we need
#     keep <- intersect(c("latitude","longitude", time_col, "grib_variable_name", "value"), names(df))
#     if (!length(keep)) return(data.table::data.table())  # nothing to keep → empty
#     df <- df[, ..keep]
#     
#     # Types & coercions
#     if (!all(c("latitude","longitude") %in% names(df))) {
#       stop("Latitude/longitude columns not found after reading GRIB.")
#     }
#     df[, latitude  := as.numeric(latitude)]
#     df[, longitude := as.numeric(longitude)]
#     
#     if (!inherits(df[[time_col]], "POSIXct")) {
#       df[, (time_col) := as.POSIXct(get(time_col), tz = "UTC")]
#     }
#     
#     if (inherits(df$value, "difftime")) {
#       df[, value := as.numeric(value, units = "secs")]
#     } else if (is.list(df$value)) {
#       df[, value := suppressWarnings(as.numeric(unlist(value)))]
#     } else {
#       df[, value := suppressWarnings(as.numeric(value))]
#     }
#     
#     data.table::setnames(df, time_col, "time")
#     df[, `:=`(variable_name = variable_name,
#               year = as.integer(year),
#               month = as.integer(month))]
#     data.table::setcolorder(df,
#                             c("latitude","longitude","time","variable_name","grib_variable_name","value","year","month"))
#     
#     n0 <- nrow(df)
#     df <- df[!is.na(value)]
#     if (nrow(df) < n0) {
#       cat(sprintf("  ⚠ Removed %d rows with invalid values from %s\n",
#                   n0 - nrow(df), basename(filepath)))
#     }
#     df
#   }
#   
#   try_terra <- function() {
#     # Fallback using terra; may be heavier on memory for large stacks
#     if (!requireNamespace("terra", quietly = TRUE)) {
#       stop("terra not installed and stars failed.")
#     }
#     r <- terra::rast(filepath)
#     # Try to get time dimension (if available)
#     tvals <- tryCatch(terra::time(r), error = function(e) NULL)
#     # Convert to data.frame (this can be large!)
#     # We’ll do a safe approach: write to table of points by chunk
#     # For simplicity here, we’ll use terra::as.data.frame (may be memory heavy).
#     df <- as.data.frame(r, xy = TRUE, cells = FALSE, na.rm = FALSE)
#     setDT(df)
#     setnames(df, c("x","y"), c("longitude","latitude"))
#     
#     # If multiple layers (time or levels), gather them
#     data_cols <- setdiff(names(df), c("longitude","latitude"))
#     df_long <- melt(df, id.vars = c("latitude","longitude"),
#                     measure.vars = data_cols,
#                     variable.name = "grib_variable_name",
#                     value.name = "value")
#     
#     # Infer time from layer names if terra named them like "lyr.1", else NA
#     df_long[, time := as.POSIXct(NA)]
#     if (!is.null(tvals) && length(tvals) == length(data_cols)) {
#       # map each layer to corresponding time
#       layermap <- data.table(grib_variable_name = data_cols, tval = tvals)
#       df_long <- merge(df_long, layermap, by = "grib_variable_name", all.x = TRUE)
#       setnames(df_long, "tval", "time")
#     }
#     
#     df_long[, `:=`(variable_name = variable_name,
#                    year = as.integer(year),
#                    month = as.integer(month))]
#     setcolorder(df_long, c("latitude","longitude","time","variable_name","grib_variable_name","value","year","month"))
#     df_long[!is.na(value)]
#   }
#   
#   out <- tryCatch(try_stars(), error = function(e) {
#     cat(sprintf("  ⚠ stars read failed for %s (%s). Trying terra...\n", basename(filepath), e$message))
#     tryCatch(try_terra(), error = function(e2) {
#       cat(sprintf("✗ Error loading %s: %s\n", basename(filepath), e2$message))
#       data.table()
#     })
#   })
#   
#   out
# }
# 
# # --------------------------
# # Process a month
# # --------------------------
# process_month <- function(year, month, variables_list) {
#   month_key <- sprintf("%04d_%02d", year, month)
#   cat(sprintf("\n📅 Processing %d-%02d\n", year, month))
#   cat(strrep("-", 40), "\n")
#   
#   year_dir <- file.path(processed_dir, sprintf("%04d", year))
#   dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
#   output_file <- file.path(year_dir, sprintf("era5_%04d_%02d_all_variables.csv.gz", year, month))
#   
#   if (file.exists(output_file) && file.info(output_file)$size > 1024) {
#     cat(sprintf("✓ Already processed: %s (%.1f MB)\n",
#                 basename(output_file), file.info(output_file)$size / 1024^2))
#     return(list(success = TRUE, key = month_key, files_processed = 0L))
#   }
#   
#   monthly_list <- list()
#   files_processed <- 0L
#   
#   for (variable_name in names(variables_list)) {
#     fp <- variables_list[[variable_name]]
#     if (!check_file_exists(fp)) {
#       cat("⚠ Missing file:", basename(fp), "\n")
#       next
#     }
#     cat(sprintf("  ✓ Processing: %s\n", variable_name))
#     dt <- load_grib_to_long_format(fp, variable_name, year, month)
#     if (nrow(dt)) {
#       monthly_list[[length(monthly_list) + 1L]] <- dt
#       files_processed <- files_processed + 1L
#     } else {
#       cat(sprintf("  ⚠ No data from: %s\n", variable_name))
#     }
#   }
#   
#   if (length(monthly_list)) {
#     cat(sprintf("  📊 Combining %d variables...\n", length(monthly_list)))
#     df_month <- rbindlist(monthly_list, use.names = TRUE, fill = TRUE)
#     
#     cat(sprintf("  💾 Saving to: %s\n", basename(output_file)))
#     # Ensure reproducible float formatting & fast gzip
#     withr::local_options(list(
#       datatable.fread.datatable = TRUE
#     ))
#     # fwrite will auto-compress based on .gz extension
#     data.table::fwrite(df_month, file = output_file)
#     
#     file_size_mb <- file.info(output_file)$size / 1024^2
#     cat(sprintf("  ✓ Saved: %s rows, %.1f MB\n", format(nrow(df_month), big.mark=","), file_size_mb))
#     
#     rm(df_month, monthly_list); gc()
#     list(success = TRUE, key = month_key, files_processed = files_processed)
#   } else {
#     cat(sprintf("  ✗ No data processed for %d-%02d\n", year, month))
#     list(success = FALSE, key = month_key, files_processed = 0L)
#   }
# }
# 
# # --------------------------
# # Create recent (3 months) combined file
# # --------------------------
# create_recent_combined_file <- function() {
#   cat("\n📋 Creating recent data summary...\n")
#   # Gather monthly files newest-first
#   year_dirs <- list.dirs(processed_dir, full.names = TRUE, recursive = FALSE)
#   year_dirs <- rev(sort(year_dirs))  # newest first
#   recent_files <- character()
#   
#   for (yd in year_dirs) {
#     mfiles <- list.files(yd, pattern = "^era5_.*_all_variables\\.csv\\.gz$", full.names = TRUE)
#     mfiles <- rev(sort(mfiles)) # newest first
#     if (length(mfiles)) {
#       recent_files <- c(recent_files, mfiles)
#       if (length(recent_files) >= 3) break
#     }
#   }
#   recent_files <- head(recent_files, 3)
#   
#   if (length(recent_files)) {
#     combined_file <- file.path(input_dir, "era5_recent_3months.csv.gz")
#     cat(sprintf("  📊 Combining %d recent files...\n", length(recent_files)))
#     
#     lst <- vector("list", length(recent_files))
#     for (i in seq_along(recent_files)) {
#       cat(sprintf("  ✓ Loaded: %s\n", basename(recent_files[i])))
#       lst[[i]] <- data.table::fread(recent_files[i], showProgress = FALSE)
#     }
#     df_combined <- rbindlist(lst, use.names = TRUE, fill = TRUE)
#     data.table::fwrite(df_combined, combined_file)
#     
#     file_size_mb <- file.info(combined_file)$size / 1024^2
#     cat(sprintf("  💾 Saved recent summary: %s rows, %.1f MB\n",
#                 format(nrow(df_combined), big.mark=","), file_size_mb))
#   } else {
#     cat("  ⚠ No recent monthly files found to combine.\n")
#   }
# }
# 
# # --------------------------
# # Main
# # --------------------------
# main <- function() {
#   cat("ERA5 Data Wrangling - Monthly File Organization (R)\n")
#   cat(strrep("=", 60), "\n")
#   cat(sprintf("Run time: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
#   
#   metadata <- load_metadata()
#   processed_months <- unique(as.character(metadata$processed_months %||% character()))
#   
#   data_by_month <- discover_available_data()
#   if (length(data_by_month) == 0) {
#     cat("✗ No valid ERA5 GRIB files found.\n")
#     quit(status = 1)
#   }
#   
#   cat(sprintf("📊 Found data for %d months\n", length(data_by_month)))
#   cat(sprintf("📋 Previously processed: %d months\n", length(processed_months)))
#   
#   total_processed <- 0L
#   total_files <- 0L
#   
#   for (month_key in sort(names(data_by_month))) {
#     if (month_key %in% processed_months) {
#       cat(sprintf("⏭ Skipping %s (already processed)\n", month_key))
#       next
#     }
#     mi <- data_by_month[[month_key]]
#     res <- process_month(mi$year, mi$month, mi$variables)
#     if (isTRUE(res$success)) {
#       processed_months <- unique(c(processed_months, res$key))
#       total_processed <- total_processed + 1L
#       total_files <- total_files + res$files_processed
#     }
#   }
#   
#   metadata$processed_months <- processed_months
#   save_metadata(metadata)
#   
#   create_recent_combined_file()
#   
#   cat("\n", strrep("=", 60), "\n", sep = "")
#   cat("FINAL SUMMARY\n")
#   cat(strrep("=", 60), "\n")
#   cat(sprintf("Months processed this run: %d\n", total_processed))
#   cat(sprintf("Files processed this run: %d\n", total_files))
#   cat(sprintf("Total months available: %d\n", length(data_by_month)))
#   cat(sprintf("Output directory: %s\n", processed_dir), "\n")
#   cat(strrep("=", 60), "\n")
#   cat("✓ ERA5 monthly processing completed!\n")
# }
# 
# # Null-coalescing helper
# `%||%` <- function(a, b) if (is.null(a)) b else a
# 
# if (identical(environment(), globalenv())) {
#   main()
# }

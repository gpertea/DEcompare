# R/utils_helpers.R

library(data.table)
library(qs)
library(tools) # For file_ext

# --- File Reading ---

readFile <- function(filepath, original_name = NULL) {
  if (!file.exists(filepath)) {
    # Use basename(filepath) for error message as original_name might be NULL
    stop("File not found: ", basename(filepath))
  }

  # Use original_name for extension checks if provided, otherwise fallback to filepath
  filename_for_check <- if (!is.null(original_name)) original_name else filepath
  lc_filename_for_check <- tolower(filename_for_check)

  # --- Debugging ---
  #message("Checking file: ", filename_for_check)
  #message("Lowercase check name: ", lc_filename_for_check)
  # ---------------

  # Check based on the original name (or filepath if original name is null)
  if (endsWith(lc_filename_for_check, ".gz")) {
    # message("Detected .gz suffix") # Debugging
    # It's a gzipped file. Check if it's a supported text type based on original name.
    if (endsWith(lc_filename_for_check, ".tab.gz") ||
        endsWith(lc_filename_for_check, ".tsv.gz") ||
        endsWith(lc_filename_for_check, ".csv.gz")) {

      # message("Detected supported compressed text type") # Debugging
      # fread handles these directly - read using the actual datapath
      dt <- tryCatch({
        data.table::fread(filepath) # Read the temporary datapath
      }, error = function(e) {
        stop("Error reading compressed text file (", basename(filename_for_check), "): ", e$message)
      })
    } else {
      # message("Unsupported .gz type detected") # Debugging
      stop("Unsupported compressed file type. Only .tab.gz, .tsv.gz, .csv.gz are supported via fread.")
    }
  } else {
    # Not a .gz file, check other extensions based on original name
    ext_outer <- tolower(tools::file_ext(filename_for_check))
    # message("Detected non-gz extension: ", ext_outer) # Debugging

    if (ext_outer %in% c("tsv", "tab", "csv")) {
      dt <- tryCatch({
        data.table::fread(filepath) # Read the temporary datapath
      }, error = function(e) {
        stop("Error reading text file (", basename(filename_for_check), "): ", e$message)
      })
    } else if (ext_outer == "qs") {
      obj <- tryCatch({
        qs::qread(filepath) # Read the temporary datapath
      }, error = function(e) {
        stop("Error reading qs file (", basename(filename_for_check), "): ", e$message)
      })
      if (!is.data.frame(obj)) stop("qs file did not contain a data.frame")
      dt <- data.table::as.data.table(obj, keep.rownames = "fid_from_rownames")
    } else if (ext_outer == "rds") {
      obj <- tryCatch({
        readRDS(filepath) # Read the temporary datapath
      }, error = function(e) {
        stop("Error reading rds file (", basename(filename_for_check), "): ", e$message)
      })
      if (!is.data.frame(obj)) stop("rds file did not contain a data.frame")
      dt <- data.table::as.data.table(obj, keep.rownames = "fid_from_rownames")
    } else if (ext_outer %in% c("rdata", "rda")) {
      env <- new.env()
      tryCatch({
        load(filepath, envir = env) # Read the temporary datapath
      }, error = function(e) {
        stop("Error loading RData file (", basename(filename_for_check), "): ", e$message)
      })
      dfs <- ls(env)[sapply(ls(env), function(x) is.data.frame(get(x, envir = env)))]
      if (length(dfs) == 0) stop("No data.frame found in RData file")
      if (length(dfs) > 1) warning("Multiple data.frames found in RData, using the first one: ", dfs[1])
      dt <- data.table::as.data.table(get(dfs[1], envir = env), keep.rownames = "fid_from_rownames")
    } else {
      stop("Unsupported file extension: ", ext_outer, " (from ", basename(filename_for_check), ")")
    }
  }

  # --- Post-reading checks ---
  # (Keep the post-reading checks for 'fid_from_rownames' as before)
  if ("fid_from_rownames" %in% names(dt)) {
      if (!"fid" %in% names(dt)) {
          setnames(dt, "fid_from_rownames", "fid")
          if (!is.character(dt$fid)) dt[, fid := as.character(fid)]
      } else {
          dt[, fid_from_rownames := NULL]
          warning("Data had rownames but also an existing 'fid' column. Using the 'fid' column.")
      }
  }

  return(dt)
}
# --- Data Validation and Initial Processing ---

validateData <- function(dt) {
  req_cols <- c("t", "logFC", "P.Value", "adj.P.Val")
  missing_cols <- setdiff(req_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  # Could add type checks here if needed
  return(TRUE)
}

ensureFid <- function(dt) {
  if ("fid" %in% names(dt)) {
    # Ensure fid is character and unique
    if (!is.character(dt$fid)) dt[, fid := as.character(fid)]
    if (anyDuplicated(dt$fid)) stop("Column 'fid' contains duplicate values.")
  } else if ("fid_from_rownames" %in% names(dt)) {
    # Use rownames if fid doesn't exist
    data.table::setnames(dt, "fid_from_rownames", "fid")
    if (!is.character(dt$fid)) dt[, fid := as.character(fid)] # Should be character already
    if (anyDuplicated(dt$fid)) stop("Rownames (used as 'fid') contain duplicate values.")
  } else {
    stop("Data must have a 'fid' column or rownames to be used as feature IDs.")
  }
  # Remove the temporary rownames column if it exists and wasn't used
  if ("fid_from_rownames" %in% names(dt) && !"fid" %in% names(dt)) {
      dt[, fid_from_rownames := NULL]
  }
  return(dt)
}

initialProcessDE <- function(dt) {
  validateData(dt)
  dt <- ensureFid(dt)
  # Sort by t-statistic descending
  data.table::setorder(dt, -t)
  # Ensure key columns have correct types (example)
  num_cols <- c("t", "logFC", "P.Value", "adj.P.Val")
  for(col in num_cols) {
      if(!is.numeric(dt[[col]])) {
          warning("Column '", col, "' is not numeric. Attempting conversion.")
          dt[, (col) := as.numeric(get(col))]
      }
  }
  # Check for NAs introduced by conversion
  if (any(sapply(dt[, ..num_cols], function(x) any(is.na(x))))){
      warning("NAs introduced during numeric conversion in required columns. Check input data.")
  }

  return(dt)
}

calculateInitialCounts <- function(dt) {
  if (is.null(dt) || nrow(dt) == 0) {
    return(list(total = 0, fdr05 = 0))
  }
  list(
    total = nrow(dt),
    fdr05 = sum(dt$adj.P.Val < 0.05, na.rm = TRUE)
  )
}

# --- FID Harmonization ---

handleFidPrefix <- function(dt1, dt2) {
  # Ensure both are valid data.tables with 'fid'
  if (is.null(dt1) || !is.data.table(dt1) || !"fid" %in% names(dt1) ||
      is.null(dt2) || !is.data.table(dt2) || !"fid" %in% names(dt2)) {
    return(list(dt1 = dt1, dt2 = dt2)) # Return unchanged if invalid input
  }

  # Make copies to avoid modifying originals by reference
  dt1_mod <- copy(dt1)
  dt2_mod <- copy(dt2)

  # Determine which symbol column to use (prefer gene_name)
  sym_col1 <- if ("gene_name" %in% names(dt1_mod)) "gene_name" else if ("Symbol" %in% names(dt1_mod)) "Symbol" else NULL
  sym_col2 <- if ("gene_name" %in% names(dt2_mod)) "gene_name" else if ("Symbol" %in% names(dt2_mod)) "Symbol" else NULL

  # Check if fids already contain a pipe character, suggesting a prefix exists
  prefix_exists1 <- any(grepl("\\|", dt1_mod$fid))
  prefix_exists2 <- any(grepl("\\|", dt2_mod$fid))

  # Scenario 1: Both have symbol columns
  if (!is.null(sym_col1) && !is.null(sym_col2)) {
    # Add prefix only if it doesn't already exist
    if (!prefix_exists1) {
      # Ensure symbol column doesn't contain NA or empty strings before pasting
      dt1_mod[!is.na(get(sym_col1)) & get(sym_col1) != "", fid := paste0(get(sym_col1), "|", fid)]
    }
    if (!prefix_exists2) {
      dt2_mod[!is.na(get(sym_col2)) & get(sym_col2) != "", fid := paste0(get(sym_col2), "|", fid)]
    }
  }
  # Scenario 2 & 3: Only one has symbol column, or neither has
  else {
    # Remove prefix if present in either (if one lacks symbols, both should lack prefix)
    if (prefix_exists1) {
      dt1_mod[, fid := sub("^.*?\\|", "", fid)] # Remove prefix up to the first pipe
    }
    if (prefix_exists2) {
      dt2_mod[, fid := sub("^.*?\\|", "", fid)] # Remove prefix up to the first pipe
    }
  }

  # Ensure uniqueness again after potential modification
  if (anyDuplicated(dt1_mod$fid)) warning("Duplicate fids generated in dataset 1 after prefix handling.")
  if (anyDuplicated(dt2_mod$fid)) warning("Duplicate fids generated in dataset 2 after prefix handling.")

  return(list(dt1 = dt1_mod, dt2 = dt2_mod))
}


# --- Overlap Metrics ---

calculateOverlapMetrics <- function(dt1_filt, dt2_filt) {
  if (is.null(dt1_filt) || nrow(dt1_filt) == 0 || is.null(dt2_filt) || nrow(dt2_filt) == 0) {
    return(list(overlap_n = 0, overlap_coef = 0, spearman_cor = NA, weighted_cor = NA, merged_data = data.table()))
  }

  # Ensure 'fid' exists
  if (!"fid" %in% names(dt1_filt) || !"fid" %in% names(dt2_filt)) {
      stop("Filtered data tables must contain 'fid' column for overlap calculation.")
  }

  # Find overlapping features based on 'fid'
  common_fids <- intersect(dt1_filt$fid, dt2_filt$fid)
  overlap_n <- length(common_fids)

  # Calculate overlap coefficient (Sorensen-Dice)
  overlap_coef <- if ((nrow(dt1_filt) + nrow(dt2_filt)) > 0) {
    round(2 * overlap_n / (nrow(dt1_filt) + nrow(dt2_filt)), 3)
  } else {
    0
  }

  spearman_cor <- NA
  weighted_cor <- NA # Placeholder
  merged_data <- data.table()

  if (overlap_n > 1) { # Need at least 2 points for correlation
    # Merge data for overlapping features
    # Select only necessary columns to avoid conflicts (fid, t)
    dt1_sub <- dt1_filt[fid %in% common_fids, .(fid, t1 = t)]
    dt2_sub <- dt2_filt[fid %in% common_fids, .(fid, t2 = t)]
    merged_data <- merge(dt1_sub, dt2_sub, by = "fid")

    # Calculate Spearman correlation on 't' statistics
    spearman_cor <- tryCatch({
        round(cor(merged_data$t1, merged_data$t2, method = "spearman", use="pairwise.complete.obs"), 3)
    }, warning = function(w){ NA }, error = function(e){ NA }) # Handle cases with insufficient data/variance

    # Placeholder for weighted correlation
    weighted_cor <- getWeightedRankCor(merged_data)
  } else {
      # Create empty merged_data with correct columns if no/little overlap
      merged_data <- data.table(fid=character(), t1=numeric(), t2=numeric())
  }

  return(list(
    overlap_n = overlap_n,
    overlap_coef = overlap_coef,
    spearman_cor = spearman_cor,
    weighted_cor = weighted_cor,
    merged_data = merged_data # Return merged data for potential plotting use
  ))
}

# Placeholder for weighted rank correlation
getWeightedRankCor <- function(dt_merged) {
  # For now, just return Spearman if available
  if (!is.null(dt_merged) && nrow(dt_merged) > 1 && "t1" %in% names(dt_merged) && "t2" %in% names(dt_merged)) {
      cor_val <- tryCatch({
          round(cor(dt_merged$t1, dt_merged$t2, method = "spearman", use="pairwise.complete.obs"), 3)
      }, warning = function(w){ NA }, error = function(e){ NA })
      return(cor_val)
  } else {
    return(NA)
  }
}

# --- Filtering Logic ---
applyFilters <- function(dt, max_pval, top_n) {
    if (is.null(dt) || nrow(dt) == 0) return(dt)

    dt_filt <- dt[P.Value <= max_pval]

    if (top_n > 0 && nrow(dt_filt) > top_n) {
        # Sort by P.Value ascending to get top N
        setorder(dt_filt, P.Value)
        dt_filt <- head(dt_filt, top_n)
        # Re-sort by t-statistic descending for consistency if needed elsewhere
        # setorder(dt_filt, -t)
    }

    return(dt_filt)
}

# --- UI Helpers ---
formatCount <- function(n) {
    format(n, big.mark = ",", scientific = FALSE)
}

# R/utils.R

# --- Libraries ---
# Ensure these are installed: install.packages(c("data.table", "qs"))
library(data.table) # Still used for fread, but converting output
library(qs)

# --- Constants ---
REQUIRED_COLS <- c("t", "logFC", "P.Value", "adj.P.Val")
SYMBOL_COLS <- c("gene_name", "Symbol") # Preferred order

# --- Utility Functions ---

#' Find Symbol Column
#' Helper function to find the first matching symbol column name.
#' @param df Data frame to check.
#' @return The name of the symbol column found, or NULL.
find_sym_col <- function(df) {
    if (!is.data.frame(df)) return(NULL)
    for (col in SYMBOL_COLS) {
        if (col %in% names(df)) return(col)
    }
    return(NULL)
}

#' Load DE Results File (Simplified)
#'
#' Loads differential expression results, relying on fread for tabular formats
#' (including compressed) and specific functions for binary formats.
#' Ensures required columns and 'fid'/rownames, sorts by t-statistic,
#' and returns a data.frame.
#'
#' @param filepath Path to the input file (Shiny's datapath).
#' @param original_filename The original name of the uploaded file, used for hint.
#' @return A data.frame containing the DE results.
#' @export
load_deres_file <- function(filepath, original_filename = "Unknown") {
  # --- Debugging ---
  message("--- Loading File ---")
  message("Received datapath: ", filepath)
  message("Original filename hint: ", original_filename)
  # -----------------

  if (is.null(filepath) || !file.exists(filepath)) {
    stop("File path is invalid or file does not exist: ", filepath)
  }

  # Determine load method based on original filename extension hint primarily
  ext_lower <- ""
  if (original_filename != "Unknown") {
      ext_lower <- tolower(tools::file_ext(original_filename))
      if (ext_lower == "gz") {
          # Handle double extensions like .tab.gz
          fn_no_gz <- tools::file_path_sans_ext(original_filename)
          ext_lower <- paste0(tolower(tools::file_ext(fn_no_gz)), ".gz") # e.g., "tab.gz"
      }
      message("Detected extension from original filename: ", ext_lower)
  } else {
      # Fallback if original name isn't available (shouldn't happen with fileInput)
      ext_lower <- tolower(tools::file_ext(filepath))
      message("Using extension from datapath as fallback: ", ext_lower)
  }


  df <- NULL
  tryCatch({
    # Prioritize specific binary formats first
    if (endsWith(ext_lower, "qs")) {
        message("Attempting to load as QS file...")
        obj <- qs::qread(filepath)
        if (!is.data.frame(obj)) stop("QS file did not contain a data.frame.")
        df <- obj
    } else if (endsWith(ext_lower, "rds")) {
        message("Attempting to load as RDS file...")
        obj <- readRDS(filepath)
        if (!is.data.frame(obj)) stop("RDS file did not contain a data.frame.")
        df <- obj
    } else if (endsWith(ext_lower, "rdata") || endsWith(ext_lower, "rda")) {
        message("Attempting to load as RData file...")
        env <- new.env()
        loaded_names <- load(filepath, envir = env)
        df_obj_name <- NULL
        for (name in loaded_names) {
            if (is.data.frame(env[[name]])) {
                df_obj_name <- name
                break
            }
        }
        if (is.null(df_obj_name)) stop(".RData file does not contain a data.frame object.")
        df <- env[[df_obj_name]]
    } else {
        # --- Assume Tabular (let fread figure it out) ---
        # Includes .tsv, .tab, .txt, .csv and their .gz versions
        message("Assuming tabular format, attempting load with fread...")
        # Pass the datapath directly to fread. data.table=FALSE returns data.frame.
        # fread handles decompression automatically based on file magic numbers/path.
        df <- data.table::fread(cmd = NULL, file = filepath, data.table = FALSE, stringsAsFactors = FALSE)
        message("fread completed.")
        # Check for 'fid' column AFTER attempting load
        if (!"fid" %in% names(df)) {
           stop("Loaded tabular file must contain an 'fid' column for feature identifiers.")
        }
    }
  }, error = function(e) {
    stop("Error loading file '", original_filename, "' (path: ", basename(filepath), "): ", e$message)
  })

  # --- Data Validation and Preparation (using 'df') ---
  if (!is.data.frame(df)) stop("Loaded object is not a data.frame.")

  # Check/Create 'fid' column and rownames (same logic as before)
  has_rownames <- !is.null(rownames(df)) && !all(rownames(df) == as.character(1:nrow(df)))
  has_fid_col <- "fid" %in% names(df)

  if (has_rownames && !has_fid_col) {
    df$fid <- rownames(df)
    message("Created 'fid' column from existing rownames.")
    has_fid_col <- TRUE
  } else if (!has_rownames && has_fid_col) {
    if (anyDuplicated(df$fid)) warning("Duplicate values found in 'fid' column. Using these duplicates as rownames.")
    rownames(df) <- df$fid
    has_rownames <- TRUE
    message("Created rownames from existing 'fid' column.")
  } else if (!has_rownames && !has_fid_col) {
    stop("Data must have rownames or an 'fid' column for feature identifiers.")
  } else if (has_rownames && has_fid_col) {
    if (!identical(as.character(rownames(df)), as.character(df$fid))) {
       warning("Rownames and 'fid' column differ. Using rownames as the primary ID and updating 'fid' column.")
       df$fid <- rownames(df)
    }
  }
  df$fid <- as.character(df$fid)
  rownames(df) <- as.character(rownames(df))

  # Check for required columns (t, logFC, P.Value, adj.P.Val)
  missing_cols <- setdiff(REQUIRED_COLS, names(df))
  if (length(missing_cols) > 0) stop("Required columns missing: ", paste(missing_cols, collapse = ", "))

  # Ensure numeric types for key columns
  for (col in REQUIRED_COLS) {
      if (!is.numeric(df[[col]])) {
          tryCatch({ df[[col]] <- as.numeric(df[[col]]) },
                   warning = function(w){ stop("Column '", col, "' could not be coerced to numeric: ", w$message) },
                   error = function(e){ stop("Column '", col, "' could not be coerced to numeric: ", e$message) })
      }
  }

  # Remove rows with NA in critical columns
  df <- df[!is.na(df$t) & !is.na(df$P.Value), , drop = FALSE]

  # Sort by t-statistic descending
  if (nrow(df) > 0) {
      original_rownames <- rownames(df)
      sort_order <- order(df$t, decreasing = TRUE, na.last = TRUE)
      df <- df[sort_order, , drop = FALSE]
      rownames(df) <- original_rownames[sort_order]
  }

  # Final check: ensure rownames match fid
  if (!identical(rownames(df), df$fid)) {
      warning("Mismatch between rownames and fid column after processing. Resetting rownames from fid.")
      rownames(df) <- df$fid
  }

  message("File loading and initial processing complete for: ", original_filename)
  message("--- End Loading File ---")
  return(df)
}

#' Add Gene Symbol Prefix to Rownames
#'
#' Checks if both data.frames have gene symbol columns and adds a 'SYMBOL|'
#' prefix to the rownames, using the 'fid' column as the base ID.
#' Operates on copies of the input data.frames.
#'
#' @param df1 First data.frame (a copy will be modified).
#' @param df2 Second data.frame (a copy will be modified).
#' @param sym_col1 Name of the symbol column in df1 (e.g., from find_sym_col).
#' @param sym_col2 Name of the symbol column in df2 (e.g., from find_sym_col).
#' @return A list containing:
#'         - df1: The potentially modified first data.frame.
#'         - df2: The potentially modified second data.frame.
#'         - applied: Logical indicating if prefixing was attempted and potentially changed rownames.
#'         - error: Logical indicating if an error (like duplicates) occurred.
#' @export
add_gene_symbol_prefix <- function(df1, df2, sym_col1, sym_col2) {
    if (!is.data.frame(df1) || !is.data.frame(df2) || is.null(sym_col1) || is.null(sym_col2)) {
        # Should not happen if called correctly, but return originals if inputs invalid
        return(list(df1 = df1, df2 = df2, applied = FALSE, error = TRUE))
    }

    # Work on copies
    df1_mod <- df1
    df2_mod <- df2
    applied_flag <- FALSE
    error_flag <- FALSE

    prefix_rownames <- function(df, sym_col) {
        if (nrow(df) == 0 || !("fid" %in% names(df))) return(list(df=df, changed=FALSE, error=FALSE)) # Skip if empty or no fid

        original_rownames <- rownames(df)
        symbols <- df[[sym_col]]
        fids <- df$fid # Use fid as the base ID

        # Ensure symbols are character, replace NA/NULL with empty string
        symbols <- as.character(symbols)
        symbols[is.na(symbols) | is.null(symbols)] <- ""

        # Create prefixed names only where symbol is not empty
        # Format: SYMBOL|FID
        needs_prefix <- nzchar(symbols)
        new_rownames <- ifelse(needs_prefix, paste0(symbols, "|", fids), fids) # Default to fid if no symbol

        # Check for duplicates introduced by prefixing
        if (anyDuplicated(new_rownames[needs_prefix])) {
            warning("Adding gene symbols would introduce duplicate rownames. Reverting to original 'fid' based rownames for this dataset.")
            # Ensure rownames are just the fids
            rownames(df) <- df$fid
            return(list(df=df, changed=FALSE, error=TRUE)) # Error occurred
        } else {
            rownames(df) <- new_rownames
            # Check if any rownames actually changed
            changed <- !identical(original_rownames, new_rownames)
            return(list(df=df, changed=changed, error=FALSE))
        }
    }

    res1 <- prefix_rownames(df1_mod, sym_col1)
    res2 <- prefix_rownames(df2_mod, sym_col2)

    df1_mod <- res1$df
    df2_mod <- res2$df
    # Set applied flag if either dataset had changes
    applied_flag <- res1$changed || res2$changed
    # Set error flag if either dataset had an error
    error_flag <- res1$error || res2$error

    return(list(df1 = df1_mod, df2 = df2_mod, applied = applied_flag, error = error_flag))
}


#' Check Initial Overlap Between Datasets
#'
#' Checks the percentage of overlapping features (rownames) between two datasets
#' before any filtering is applied. Issues a warning if overlap is low.
#'
#' @param df1 First data.frame (after loading/prefixing).
#' @param df2 Second data.frame (after loading/prefixing).
#' @param threshold Minimum required overlap percentage (0 to 1). Default 0.5 (50%).
#' @return A list containing:
#'         - overlap_count: Number of overlapping features.
#'         - overlap_percent: Percentage overlap relative to the smaller dataset.
#'         - warning_message: A string message if overlap is below threshold, NULL otherwise.
#' @export
check_initial_overlap <- function(df1, df2, threshold = 0.5) {
    if (!is.data.frame(df1) || !is.data.frame(df2)) {
        return(list(overlap_count = 0, overlap_percent = 0, warning_message = "One or both datasets are not valid data frames."))
    }

    ids1 <- rownames(df1)
    ids2 <- rownames(df2)
    n1 <- length(ids1)
    n2 <- length(ids2)
    min_n <- min(n1, n2)

    if (min_n == 0) {
        return(list(overlap_count = 0, overlap_percent = 0, warning_message = "One or both datasets have zero features."))
    }

    overlap_count <- length(intersect(ids1, ids2))
    overlap_percent <- overlap_count / min_n
    warning_message <- NULL

    if (overlap_percent < threshold) {
        warning_message <- sprintf(
            "Warning: Low initial overlap between datasets (%d features, %.1f%%). Expected at least %.1f%% of the smaller dataset (%d features). Check feature identifiers.",
            overlap_count, overlap_percent * 100, threshold * 100, min_n
        )
    }

    return(list(
        overlap_count = overlap_count,
        overlap_percent = overlap_percent,
        warning_message = warning_message
    ))
}


#' Filter DE Results
#'
#' Filters a DE results data.frame based on maximum P-value and Top N selection.
#'
#' @param deres_df The input data.frame (output from `load_deres_file`).
#' @param max_pval Maximum P.Value threshold (inclusive).
#' @param top_n Select top N features by ascending P.Value (after P.Value thresholding).
#'          If 0 or less, this filter is ignored.
#' @return A filtered data.frame.
#' @export
filter_deres <- function(deres_df, max_pval = 1.0, top_n = 0) {
  if (!is.data.frame(deres_df) || nrow(deres_df) == 0) {
    # Return empty df with same columns if input is invalid or empty
    return(deres_df[0, , drop = FALSE])
  }
  if (is.null(max_pval) || !is.numeric(max_pval) || max_pval < 0 || max_pval > 1) max_pval <- 1.0
  if (is.null(top_n) || !is.numeric(top_n)) top_n <- 0

  # Apply P.Value filter using base R subsetting
  filtered_df <- deres_df[deres_df$P.Value <= max_pval, , drop = FALSE]

  # Apply Top N filter (based on ascending P.Value)
  if (top_n > 0 && nrow(filtered_df) > 0) {
    # Sort by P.Value ascending using base R order
    sort_order <- order(filtered_df$P.Value, decreasing = FALSE, na.last = TRUE)
    filtered_df <- filtered_df[sort_order, , drop = FALSE]
    # Take head
    filtered_df <- head(filtered_df, top_n)
    # Optional: Restore original sorting by t-stat? Maybe not needed.
    # sort_order_t <- order(filtered_df$t, decreasing = TRUE, na.last = TRUE)
    # filtered_df <- filtered_df[sort_order_t, , drop = FALSE]
  }

  return(filtered_df)
}

#' Calculate Overlap Metrics
#'
#' Calculates the number and coefficient of overlapping features (rownames).
#'
#' @param filtered_df1 First filtered data.frame.
#' @param filtered_df2 Second filtered data.frame.
#' @return A list with `count` (number of overlapping features) and
#'         `coeff` (overlap coefficient).
#' @export
calculate_overlap_metrics <- function(filtered_df1, filtered_df2) {
  if (!is.data.frame(filtered_df1) || !is.data.frame(filtered_df2)) {
    return(list(count = 0, coeff = 0))
  }
  ids1 <- rownames(filtered_df1)
  ids2 <- rownames(filtered_df2)
  n1 <- length(ids1)
  n2 <- length(ids2)
  min_n <- min(n1, n2)

  if (min_n == 0) {
    return(list(count = 0, coeff = 0))
  }

  overlap_count <- length(intersect(ids1, ids2))
  overlap_coeff <- overlap_count / min_n

  return(list(count = overlap_count, coeff = overlap_coeff))
}


#' Calculate Correlation Metrics for Overlapping Features
#'
#' Calculates Spearman correlation on the 't' statistic for overlapping features.
#' Includes a placeholder for weighted correlation.
#'
#' @param overlap_df A data.frame containing the merged data for overlapping
#'                   features. Expected columns: `t.x`, `t.y`.
#' @return A list with `spearman_cor` and `weighted_cor`. Returns NA if
#'         correlation cannot be calculated (e.g., < 2 overlapping points).
#' @export
calculate_correlation_metrics <- function(overlap_df) {
  spearman_cor <- NA_real_
  weighted_cor <- NA_real_

  if (is.data.frame(overlap_df) && nrow(overlap_df) >= 2 &&
      "t.x" %in% names(overlap_df) && "t.y" %in% names(overlap_df)) {

    # Calculate Spearman correlation using base R cor
    spearman_cor <- tryCatch({
        cor(overlap_df$t.x, overlap_df$t.y, method = "spearman", use = "pairwise.complete.obs")
    }, warning = function(w) NA_real_, error = function(e) NA_real_)

    # Calculate weighted correlation (placeholder)
    weighted_cor <- getWeightedRankCor(overlap_df)
  }

  return(list(spearman_cor = spearman_cor, weighted_cor = weighted_cor))
}


#' Get Weighted Rank Correlation (Placeholder)
#'
#' Placeholder function for weighted rank correlation. Currently returns
#' the standard Spearman correlation.
#'
#' @param overlap_df A data.frame containing the merged data for overlapping
#'                   features. Expected columns: `t.x`, `t.y`.
#' @return The calculated correlation value (currently Spearman).
#' @export
getWeightedRankCor <- function(overlap_df) {
  # Placeholder implementation: returns standard Spearman correlation for now
  if (is.data.frame(overlap_df) && nrow(overlap_df) >= 2 &&
      "t.x" %in% names(overlap_df) && "t.y" %in% names(overlap_df)) {

      cor_val <- tryCatch({
          cor(overlap_df$t.x, overlap_df$t.y, method = "spearman", use = "pairwise.complete.obs")
      }, warning = function(w) NA_real_, error = function(e) NA_real_)
      return(cor_val)
  } else {
      return(NA_real_)
  }
}


#' Calculate Rank Difference Color
#'
#' Calculates colors based on the rank difference of two numeric vectors.
#' Red indicates concordance (small rank difference), Blue indicates discordance.
#' Uses base R functions.
#'
#' @param vals1 Numeric vector 1 (e.g., t-statistics from dataset 1).
#' @param vals2 Numeric vector 2 (e.g., t-statistics from dataset 2).
#' @return A character vector of hex color codes.
#' @export
calculate_rank_diff_color <- function(vals1, vals2) {
    if (length(vals1) != length(vals2) || length(vals1) == 0) {
        return(character(0))
    }

    # Handle potential NAs before ranking
    complete_cases <- !is.na(vals1) & !is.na(vals2)
    if (sum(complete_cases) < 2) return(rep("#808080", length(vals1))) # Not enough data, return grey

    ranks1 <- rank(vals1[complete_cases])
    ranks2 <- rank(vals2[complete_cases])

    # Calculate absolute rank difference, scaled by the maximum possible difference (N-1)
    n_complete <- length(ranks1)
    max_rank_diff <- if(n_complete > 1) n_complete - 1 else 1 # Avoid division by zero if n=1
    rank_diff_scaled <- abs(ranks1 - ranks2) / max_rank_diff
    rank_diff_scaled[is.nan(rank_diff_scaled)] <- 0 # Handle case N=1

    # Create color palette (Red=concordant, Blue=discordant) using base R colorRampPalette
    col_fun <- colorRampPalette(c("red", "grey", "blue"))(100)

    # Assign colors based on scaled difference
    # Map 0-1 to 1-100 indices, ensuring indices are within bounds
    color_indices <- pmin(pmax(1, floor(rank_diff_scaled * 99.99) + 1), 100)

    # Initialize color vector with grey for NA cases
    colors_out <- rep("#808080", length(vals1))
    colors_out[complete_cases] <- col_fun[color_indices]

    return(colors_out)
}

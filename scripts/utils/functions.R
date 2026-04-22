# General Utility Functions
# Simplified and modernized for general data analysis

#' Load and install packages automatically
#' Uses pacman for efficient dependency management
forceLibrary <- function(list.of.packages) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(char = list.of.packages)
  invisible()
}

naToZero = function(x) {
  x[is.na(x)] = 0
  return(x)
}

forceSetWd = function(x) {
  if (!dir.exists(x)) {
    dir.create(x, recursive = TRUE)
  }
  if (dir.exists(x)) {
    setwd(x)
  } else {
    warning('Could not create or set directory: ', x)
  }
}

mergeFiles = function(files_patt = 'quant.sf', by_col = 'Name', row_names = F, ...) {
  forceLibrary(c('pbmcapply', 'dplyr', 'tibble'))
  
  files = list.files(pattern = files_patt, recursive = T)
  files = files[!grepl('total', files)]
  
  if (length(files) == 0) stop("No files found matching pattern.")
  
  message('Merging ', length(files), ' files...')
  
  read_and_tag <- function(f) {
    df <- read.table(f, header = T, stringsAsFactors = F)
    if (row_names) {
      df <- df %>% rownames_to_column(var = "rowname")
    }
    col_idx <- if(row_names) "rowname" else by_col
    colnames(df)[colnames(df) != col_idx] <- paste(colnames(df)[colnames(df) != col_idx], f, sep = '_')
    return(df)
  }
  
  # Iterative merge (could be optimized with reduce for very large sets)
  res <- read_and_tag(files[1])
  if(length(files) > 1) {
    for(i in 2:length(files)) {
      next_df <- read_and_tag(files[i])
      res <- merge(res, next_df, by = if(row_names) "rowname" else by_col, ...)
    }
  }
  
  return(res)
}

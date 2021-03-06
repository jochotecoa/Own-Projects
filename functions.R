forceLibrary <- function(list.of.packages) {
  checkNewPackages <- function(list.of.packages) {
    new.packages.log = !(list.of.packages %in% installed.packages()[,"Package"])
    new.packages <- list.of.packages[new.packages.log]
    return(new.packages)
  }
  new.packages = checkNewPackages(list.of.packages)
  if (length(new.packages)) {
    print(paste('Trying to install the following packages:', paste(new.packages)))
    install.packages(new.packages)
    new.packages = checkNewPackages(list.of.packages)
    if (length(new.packages)) {
      print(paste(paste(new.packages), 'were not installed through the easy way'))
      print("Let's try the hard way then")
      setRepositories(graphics = F, ind = 1:8)
      install.packages(new.packages)
      new.packages = checkNewPackages(list.of.packages)
      if (length(new.packages)) {
        stop('forceLibrary was not able to install the following packages: ', 
             paste(new.packages))
      }
    }
  } 
  
  lapply(list.of.packages, library, character.only = T)
  
  invisible()
}

cleanProtIds = function(protein_table) {
  protein_table = protein_table[!grepl(protein_table[, 1], pattern = ':'), ]
  if (class(protein_table) != "data.frame") {
    protein_table = as.data.frame(protein_table)
  }
  names = strsplit(as.character(protein_table[, 1]), '\\|')
  names = as.character(lapply(names, '[', 2))
  protein_table$uniprot_gn = names
  return(protein_table)
}

naToZero = function(x) {
  x[is.na(x)] = 0
  return(x)
}

transcrToGene = function(table, aggregate = F, prot_cod = F) {
  forceLibrary(c('biomaRt', 'dplyr'))
  
  # sampl = table[nrow(table), ]
  enst_col = apply(table, 2, grepl, pattern = 'ENST') %>% 
    apply(2, any) %>%
    as.logical()
  if (sum(enst_col) == 0) {
    table[, 'rownames'] = rownames(table)
    enst.rown = grepl(pattern = 'ENST', x = table[, 'rownames'])
    if (!sum(enst.rown)) {stop(print(table[1, ]))}
    enst_col = grepl(pattern = 'rownames', x = colnames(table))
  }
  # If integer or else, it might try to aggregate it
  table[, enst_col] = as.character(table[, enst_col])
  version = grepl('\\.', table[, enst_col]) %>% sum()
  if (version) {version = T} else {version = F}
  if (version) {
    transcript_id = 'ensembl_transcript_id_version'
  } else {
    transcript_id = 'ensembl_transcript_id'
  }
  
  values = table[, enst_col]
  mart.human = useMart(biomart = 'ENSEMBL_MART_ENSEMBL', 
                       dataset = 'hsapiens_gene_ensembl',
                       host = 'http://apr2018.archive.ensembl.org') 
  if (prot_cod) {
    transcr_biotypes = getBM(attributes = c(transcript_id, 'transcript_biotype'), 
                             filters = transcript_id, values = values, 
                             mart = mart.human)
    isProtCod = transcr_biotypes$transcript_biotype == 'protein_coding'
    values = values[isProtCod]
    print(paste0(sum(!isProtCod), ' transcripts were not protein_coding'))
  }
  
  new_cols = getBM(attributes = c(transcript_id, 'ensembl_gene_id'), 
                   filters = transcript_id, values = values, mart = mart.human)
  
  table = merge.data.frame(x = table, y = new_cols, 
                           by.x = colnames(table)[enst_col], 
                           by.y = transcript_id)
  if (nrow(table) < length(values)) {
    print(paste0(length(values) - nrow(table), 
                 ' transcripts had no gene matched'))
  }
  if (prot_cod & !aggregate) {
    table = tibble::rownames_to_column(table)
  }
  if (aggregate) {
    int_cols = grepl('integer', sapply(X = table[1, ], FUN = typeof))
    int_cols = int_cols + grepl('double', sapply(X = table[1, ], 
                                                 FUN = typeof))
    int_cols = as.logical(int_cols)
    table = aggregate(x = table[, int_cols], by = list(table$ensembl_gene_id), 
                      FUN = sum, na.rm = T)
    colnames(table)[1] = 'ensembl_gene_id'
  }
  return(table)
}

rmMirnas = function(x) {
  mirna.cols = grep(pattern = 'hsa', x = colnames(x))
  y = x[, -mirna.cols]
  return(y)
}

forceSetWd = function(x) {
  if (dir.exists(x)) {
    setwd(x)
  } else {
    dir.create(x)
    if (dir.exists(x)) {
      setwd(x)
    } else {
      warning(c('Warning: ', x, 
                ' could not be created as a dir due to permission issues'))
    }
  }
}

mergeFiles = function(files_patt =  'quant.sf', by_col = 'Name', row_names = F, ...) {
  
  forceLibrary(c('pbmcapply', 'dplyr'))
  files = list.files(pattern = files_patt, recursive = T)
  files = files[!grepl('total', files)]
  # files = files[-1]
  print(paste('Number of files found:', length(files)))
  file = files[1]
  stopifnot(file.exists(file))
  voom_file = read.table(file, header = T, stringsAsFactors = F)
  if (row_names) {
    voom_file = voom_file %>% tibble::rownames_to_column() %>% 
      dplyr::select(rowname, everything())
    by_col = 'rowname'
  }
  colnames(voom_file)[-1] = paste(colnames(voom_file)[-1], file, sep = '_')
  big_quant_voom = voom_file
  pb = progressBar(max = length(files[-1]))
  for (file in files[-1]) {
    voom_file = read.table(file, header = T, stringsAsFactors = F)
    if (row_names) {
      voom_file = voom_file %>% tibble::rownames_to_column() %>% 
        dplyr::select(rowname, everything())
    }
    colnames(voom_file)[-1] = paste(colnames(voom_file)[-1], file, sep = '_')
    big_quant_voom = merge.data.frame(big_quant_voom, voom_file, by = by_col, ...)
    setTxtProgressBar(pb, grep(file, files[-1]))
  }
  close(pb)
  return(big_quant_voom)
} 

openMart2018 <- function(...) {
  forceLibrary('biomaRt')
  mart.human = useMart(biomart = 'ENSEMBL_MART_ENSEMBL', 
                       dataset = 'hsapiens_gene_ensembl',
                       host = 'http://apr2018.archive.ensembl.org', ...) 
}

filterProtCod = function(table) {
  forceLibrary('biomaRt')
  
  sampl = table[nrow(table), ]
  enst_col = grep(pattern = 'ENST', x = sampl)
  if (length(enst_col) > 1) {enst_col = enst_col[1]}
  if (length(enst_col) == 0) {
    table[, 'rownames'] = rownames(table)
    sampl = table[nrow(table), ]
    enst_col = grep(pattern = 'ENST', x = sampl)[1]
    if (is.na(enst_col)) {print(sampl)}
  }
  version = grepl('\\.', sampl[, enst_col])
  if (length(version) == 0) {version = F}
  if (version) {
    transcript_id = 'ensembl_transcript_id_version'
  } else {
    transcript_id = 'ensembl_transcript_id'
  }
  
  values = table[, enst_col]
  mart.human = useMart(biomart = 'ENSEMBL_MART_ENSEMBL', 
                       dataset = 'hsapiens_gene_ensembl',
                       host = 'http://apr2018.archive.ensembl.org') 
  
  transcr_biotypes = getBM(attributes = c(transcript_id, 'transcript_biotype'), 
                           filters = transcript_id, values = values, 
                           mart = mart.human)
  isProtCod = transcr_biotypes$transcript_biotype == 'protein_coding'
  values = values[isProtCod]
  print(paste0(sum(!isProtCod), ' transcripts were not protein_coding'))
  new_table = table[table[, enst_col] %in% values, ]
  return(new_table)
}
filterSamplesBySeqDepth = function(df) {
  seq_depth_ratio <- df %>% 
    colSums(na.rm = T) %>% 
    `/` (mean(.)) %>% 
    log2() %>% 
    abs() 
  if (!all(seq_depth_ratio < 2)) {
    warning(sum(!(seq_depth_ratio < 2)), 
            ' sample(s) filtered out due to sequencing depth', immediate. = T)
  }
  
  df = df[, seq_depth_ratio < 2]
}

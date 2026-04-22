# Bio-informatics Utility Functions
# Extracted from original functions.R

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

transcrToGene = function(table, aggregate = F, prot_cod = F) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(biomaRt, dplyr)
  
  enst_col = apply(table, 2, grepl, pattern = 'ENST') %>% 
    apply(2, any) %>%
    as.logical()
  if (sum(enst_col) == 0) {
    table[, 'rownames'] = rownames(table)
    enst.rown = grepl(pattern = 'ENST', x = table[, 'rownames'])
    if (!sum(enst.rown)) {stop(print(table[1, ]))}
    enst_col = grepl(pattern = 'rownames', x = colnames(table))
  }
  
  table[, enst_col] = as.character(table[, enst_col])
  version = any(grepl('\\.', table[, enst_col]))
  transcript_id = if(version) 'ensembl_transcript_id_version' else 'ensembl_transcript_id'
  
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
  
  if (aggregate) {
    int_cols = sapply(table, is.numeric)
    table = aggregate(x = table[, int_cols], by = list(table$ensembl_gene_id), 
                      FUN = sum, na.rm = T)
    colnames(table)[1] = 'ensembl_gene_id'
  }
  return(table)
}

rmMirnas = function(x) {
  mirna.cols = grep(pattern = 'hsa', x = colnames(x))
  if(length(mirna.cols) > 0) return(x[, -mirna.cols])
  return(x)
}

openMart2018 <- function(...) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(biomaRt)
  useMart(biomart = 'ENSEMBL_MART_ENSEMBL', 
          dataset = 'hsapiens_gene_ensembl',
          host = 'http://apr2018.archive.ensembl.org', ...) 
}

filterProtCod = function(table) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(biomaRt)
  
  sampl = table[nrow(table), ]
  enst_col = grep(pattern = 'ENST', x = sampl)[1]
  
  version = any(grepl('\\.', table[, enst_col]))
  transcript_id = if(version) 'ensembl_transcript_id_version' else 'ensembl_transcript_id'
  
  values = table[, enst_col]
  mart.human = openMart2018()
  
  transcr_biotypes = getBM(attributes = c(transcript_id, 'transcript_biotype'), 
                             filters = transcript_id, values = values, 
                             mart = mart.human)
  isProtCod = transcr_biotypes$transcript_biotype == 'protein_coding'
  values = values[isProtCod]
  return(table[table[, enst_col] %in% values, ])
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
  return(df[, seq_depth_ratio < 2])
}

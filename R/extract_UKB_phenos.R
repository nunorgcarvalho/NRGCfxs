extract_UKB_phenos <- function(FieldIDs, phenos.csv = c()) {
  
  cols2keep <- c("eid",paste0(FieldIDs,"-0.0"))
  current_cols <- c("eid")
  for (i in 1:length(phenos.csv)) {
    print(paste0("Reading phenotype .csv # ", i))
    cols2keep <- c("eid",cols2keep[!cols2keep %in% current_cols])
    
    pheno_i <- as_tibble(fread(phenos.csv[i], select = cols2keep))
    current_cols <- c(current_cols, colnames(pheno_i)[2:ncol(pheno_i)])
    
    if (i==1) {pheno <- pheno_i
    } else {pheno <- pheno %>% left_join(pheno_i, by="eid")}
    
    # ends prematurely if all columns already acquired
    if (length(current_cols) == (length(FieldIDs)+1)) {break}
  }
  
  return(pheno)
}

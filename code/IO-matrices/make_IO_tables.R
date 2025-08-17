setwd(file.path(
  "~", "Dropbox-HarvardUniversity", "Martin Bernstein",
  "Fall 2025", "Other", "LCC"
))
source(file.path("code", "IO-matrices", "NIPA-utility-functions.R"))

#Load code crosswalks.
cw <- fread(file.path("data", "codes and crosswalks",
                      "NNA_codes_crosswalks.csv"))

#Check comprehensive crosswalk
verify_crosswalk(cw) #Prints if error, otherwise does nothing.



years <- 1963:2023
for (i in seq_along(years)) {

  year <- years[i]
  print(paste0("Building matrix for ", year))

  # Read IO data
  res <- load_io_table(year)
  m <- res[[1]]
  colcodes <- res[[2]]

  # Extract codes for industries (columns) and code with NNA codes
  colcodes <- pivot_longer(colcodes,
                           cols = seq_len(ncol(colcodes)),
                           names_to = "industry", values_to = "ind_code")
  setDT(colcodes)
  colcodes <- colcodes[!is.na(ind_code)]

  #Apply NNA codes
  if(year <= 1996){
    usecw <- unique(cw[, .(IO_pre1997_code, NNA_code)])
    colcodes[, NNA_code := 
               usecw[.SD, on = .(IO_pre1997_code = ind_code), x.NNA_code]]
  }else{
    usecw <- unique(cw[, .(IO_post1997_code, NNA_code)])
    colcodes[, NNA_code :=
               usecw[.SD, on = .(IO_post1997_code = ind_code), x.NNA_code]]
  }

  # Clean
  m <- m[7:nrow(m), ]
  setDT(m)

  # Use this to build matrix
  res <- build_matrix(m, colcodes, cw, year)
  dt <- res[[1]]
  aggs <- res[[2]]

  # Combine into one dataset
  if(i == 1){
    all_data <- dt
    all_aggs <- aggs
  }else{
    all_data <- rbind(all_data, dt)
    all_aggs <- rbind(all_aggs, aggs)
  }
}


write.csv(all_data, file = file.path("data", "constructed data", "IO-matrices",
                                     "IO_matrices_long.csv"),
          row.names = FALSE)

write.csv(all_aggs, file = file.path("data", "constructed data", "IO-matrices",
                                     "industry_GO=VA+intermediates.csv"),
          row.names = FALSE)
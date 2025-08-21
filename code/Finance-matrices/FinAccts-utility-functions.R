date_conv2 <- function(qstr) {
  if(nchar(qstr[1])==4){
    return(as.numeric(qstr))
  }else{
    # Map quarters to starting month
    quarter_months <- c(Q1 = "01", Q2 = "04", Q3 = "07", Q4 = "10")
    
    # Extract year and quarter
    year <- substr(qstr,1,4)
    quarter <- substr(qstr,5,6)
    
    # Get corresponding month for each quarter
    month <- quarter_months[quarter]
    
    # Construct date string
    date_str <- paste0(year, "-", month, "-01")
    
    # Convert to Date
    as.Date(date_str) 
  }
}

get_leontief <- function(mat1, date, sectors, alpha = 0.9, row = "holder"){
  L <- setDT(as.data.frame(solve(diag(nrow(mat1)) - alpha * mat1)))
  colnames(L) <- colnames(mat1)
  if(row == "holder"){
    rowname <- "holder_sector"
    colname <- "issuer_sector"
    valuename <- "share_of_holder"
  }else{
    rowname <- "issuer_sector"
    colname <- "holder_sector"
    valuename <- "share_of_issuer"
  }
  
  L[, (rowname) := colnames(mat1)]
  L <- pivot_longer(L, cols = 1:(nrow(sectors)), names_to = all_of(colname),
                    values_to = all_of(valuename)) 
  setDT(L)
  L[, Date := date]
  return(L)
}

format_matrix <- function(mat, centrality_for){
  # Use correct direction of flow depending on centrality measure
  if(centrality_for == "borrower"){
    values_measure = "share_of_holder"
    sector_names = "issuer_sector"
  }else if(centrality_for == "lender"){
    values_measure = "share_of_issuer"
    sector_names = "holder_sector"
  }else{
    print(paste0("Error: centrality_for must be 'borrower' or 'lender'"))
  }
  
  # Format to matrix
  mat1 <- setDT(pivot_wider(mat,
                            names_from = all_of(sector_names),
                            values_from = all_of(values_measure)))
  sectors <- mat1[, 1]
  mat1[, c("holder_sector", "issuer_sector") := NULL]
  mat1 <- as.matrix(mat1)
  
  return(list(mat1, sectors))
}

get_eigenvalues <- function(mat1, date, sectors, centrality_for = "borrower"){
  
  # Get eigenvector centrality (eigenvec of largest eigenval)
  eigs <- eigen(t(mat1), symmetric = FALSE)
  v <- as.numeric(eigs$vectors[, 1])
  if(sum(v <= 5e-4) == length(v)){
    v <- -v
  }
  if(sum(v < -5e-4) > 0){
    print(paste0("Warning: negative centrality at date ", date))
  }
  
  # Format
  eigv <- setDT(data.frame(ev = v, sector = sectors))
  names(eigv)[2] <- "sector"
  eigv[, Date := date]
  eigv[, centrality_for := centrality_for]
  
  return(eigv)
}



measure_centrality <- function(d){
  setorder(d, Date, holder_sector, issuer_sector)
  
  # Holder is lender, issuer is borrower.
  # Want: lending as a fraction of total amount lent.
  d[, share_of_holder := Level / sum(Level), by = .(Date, holder_sector)]
  d[, share_of_issuer := Level / sum(Level), by = .(Date, issuer_sector)]
  d[is.nan(share_of_holder), share_of_holder := 0]
  d[is.nan(share_of_issuer), share_of_issuer := 0]
  
  # ggplot(d, aes(Date, share_of_holder, fill = issuer_sector))+
  #   geom_bar(position = "stack", stat = "identity")+
  #   facet_wrap(~holder_sector)
  
  all_dates <- sort(unique(d$Date))
  
  for(i in seq_along(all_dates)){
    date <- all_dates[i]
    
    # Get centrality measures and Leontief focusing on share lent to each borrower
    mat_borrowercentrality <- d[Date == date & holder_sector != "Discrepancy",
                                .(holder_sector, issuer_sector, share_of_holder)]
    res <- format_matrix(mat_borrowercentrality, centrality_for = "borrower")
    mat1 <- res[[1]]
    sectors <- res[[2]]
    mat1[is.na(mat1)] <- 0
    eigv1 <- get_eigenvalues(mat1, date, sectors, centrality_for = "borrower")
    L1 <- get_leontief(mat1, date, sectors, alpha = decay_parameter, row = "holder")
    
    
    # Focusing on share borrowed from each lender
    mat_lendercentrality <- d[Date == date & holder_sector != "Discrepancy",
                              .(holder_sector, issuer_sector, share_of_issuer)]
    res <- format_matrix(mat_lendercentrality, centrality_for = "lender")
    mat2 <- res[[1]]
    sectors <- res[[2]]
    mat2[is.na(mat2)] <- 0
    eigv2 <- get_eigenvalues(mat2, date, sectors, centrality_for = "lender")
    L2 <- get_leontief(mat2, date, sectors, alpha = decay_parameter, row = "issuer")
    
    # Join and format
    eigv <- rbind(eigv1, eigv2)
    L <- merge(L1, L2)
    if(i == 1){
      all_eigvs <- eigv
      all_leontief <- L
    }else{
      all_eigvs <- rbind(all_eigvs, eigv)
      all_leontief <- rbind(all_leontief, L)
    }
    
  }
  
  return(list(all_eigvs, all_leontief))
}

get_fwtw <- function(){
  #FWTW data
  fw<-fread(file.path("data", "Financial Accounts", "fwtw_data.csv"))
  fw[, Date := date_conv2(Date)]
  fw[, year := year(Date)]
  #Put into billions
  fw[, Level := Level / 1000]
  
  #add gdp:
  gd <- fread(file.path("data", "fred", "GDP.csv"))
  names(gd) <- c("Date", "gdp")
  fw[, gdp := gd[.SD, on = .(Date), x.gdp]]
  
  #Add in other quarters for before 1952:
  tofix <- fw[year < 1952]
  tofix[, Date := NULL]
  dates <- setDT(data.frame(Date=c(paste0(1945:1951, "-01-01"), 
                                   paste0(1945:1951, "-04-01"), 
                                   paste0(1945:1951, "-07-01"))))
  dates[, Date := as.Date(Date)]
  dates[, year := year(Date)]
  fixed <- merge(dates, tofix, by = 'year', allow.cartesian=T)
  fw <- rbind(fw, fixed)
  setorder(fw, Date)
  
  
  # unique(fw$`Instrument Name`)
  # unique(fw$`Holder Name`)
  # unique(fw$`Issuer Name`)
  
  totals <- fw[`Holder Name` == "Total" | `Issuer Name` == "Total"]
  fw <- fw[`Holder Name` != "Total" & `Issuer Name` != "Total"]
  
  #Get coarse categories
  fw <- get_coarse_categories(fw)
  
  return(fw)
}

get_coarse_categories <- function(fw){
  coarse_categories <- data.frame(name =
                                    c("Nonfin Corp Bus",
                                      "Nonfin Noncorp Bus",
                                      "Households",
                                      "Federal Govt.",
                                      "State/Local Govt.",
                                      "Rest of World"),
                                  sector = c(rep("Non-financial business",
                                                 times = 2),
                                             "Households",
                                             rep("State/Local Govt.",
                                                 times = 2),
                                             "Rest of World")) %>%
    setDT()
  
  fw[, holder_sector := coarse_categories[.SD, on = .(name = `Holder Name`),
                                          x.sector]]
  fw[is.na(holder_sector), holder_sector := "Financial sector"]
  
  fw[, issuer_sector := coarse_categories[.SD, on = .(name = `Issuer Name`),
                                          x.sector]]
  fw[is.na(issuer_sector), issuer_sector := "Financial sector"] 
}

get_dfa <- function(){
  dfa <-fread(file.path("data", "Financial Accounts", "dfa",
                        "dfa-income-levels-detail.csv"))
  dfa[, Date := date_conv(Date)]
  
  cutoffs <- dfa[, c("Category", "Minimum Income Cutoff", "Maximum Income Cutoff")] %>%
    pivot_longer(cols = 2:3, names_to = "threshold", values_to = "value") %>%
    setDT()
  cutoffs <- cutoffs[!is.na(value)]
  
  dfa[, c("Minimum Income Cutoff", "Maximum Income Cutoff") := NULL]
  dont_pivot <- c("Date", "Category", "Assets","Liabilities",
                  "Nonfinancial assets", "Financial assets",
                  "Net worth", "Household count")
  dont_track <- c("Loans (Liabilities)", "Loans (Assets)", "Debt securities")
  
  assets <- c("Real estate", "Consumer durables", "Deposits",
              "Money market fund shares",
              "U.S. government and municipal securities",
              "Corporate and foreign bonds",
              "Other loans and advances (Assets)",
              "Mortgages", "Corporate equities and mutual fund shares",
              "Life insurance reserves", "Annuities", "DC pension entitlements",
              "DB pension entitlements", "Miscellaneous other equity",
              "Miscellaneous assets")
  liabilities <- c("Home mortgages", "Consumer credit",
                   "Depository institutions loans n.e.c.",
                   "Other loans and advances (Liabilities)",
                   "Deferred and unpaid life insurance premiums")
  
  do_pivot <- names(dfa)[!names(dfa) %in% c(dont_pivot, dont_track)]
  dfa <- pivot_longer(dfa, cols = all_of(do_pivot), names_to = "Instrument",
                      values_to = "Level") %>%
    setDT()
  dfa[, (dont_track) := NULL]
  dfa[Instrument %in% assets, type := "asset"]
  dfa[Instrument %in% liabilities, type := "liability"]
  
  dfa[Instrument %in% c("Real estate", "Consumer durables"),
      category := "Nonfinancial"]
  dfa[is.na(category), category := "Financial"]
  
  dfa[type == "asset", sumassets := sum(Level), by = .(Date, Category)]
  dfa[type == "liability", sumliabs := sum(Level), by = .(Date, Category)]
  max1 <- max(abs(dfa$sumliabs - dfa$Liabilities), na.rm = TRUE)
  max2 <- max(abs(dfa$sumassets - dfa$Assets), na.rm = TRUE)
  if(max(max1, max2) > 100){
    print("Warning: aggregation error")
  }
  dfa[, c("sumassets", "sumliabs") := NULL]
  
  
  # This sum agrees with "B.101.H households total assets"
  unique(dfa[, .(Date, Category, Assets)])[, .(Assets = sum(Assets)),
                                           by = .(Date)]
  # So, the "Level" in dfa is the total assets held in the category.
  return(dfa)
}
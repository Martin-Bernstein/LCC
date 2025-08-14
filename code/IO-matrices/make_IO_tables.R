setwd(file.path(
  "~", "Dropbox-HarvardUniversity", "Martin Bernstein",
  "Fall 2025", "Other", "LCC"
))

source(file.path("code", "IO-matrices", "NIPA-utility-functions.R"))

years <- 1963:2023
years <- 1997:2023
#i<-38
for (i in seq_along(years)) {
  # Read data
  year <- years[i]
  print(paste0("Building matrix for ", year))
  if (year <= 1996) {
    m <- read_excel(
      file.path(
        "data", "NIPA",
        "IO tables", "IO_1963-1996",
        "IOUse_Before_Redefinitions_PRO_1963-1996_Summary.xlsx"
      ),
      sheet = as.character(year)
    )
    colnames(m) <- m[5, ]
    colnames(m)[1:2] <- c("IOCode", "Name")
    colcodes <- m[6, 3:ncol(m)]
  } else {
    m <- read_excel(
      file.path(
        "data", "NIPA", "IO tables", "IO_1997-2023",
        "IOUse_Before_Redefinitions_PRO_1997-2023_Summary.xlsx"
      ),
      sheet = as.character(year)
    )
    colnames(m) <- m[6, ]
    colcodes <- m[5, ]
  }
  
  # Extract codes for industries (columns)
  colcodes <- pivot_longer(colcodes,
                           cols = seq_len(ncol(colcodes)),
                           names_to = "industry", values_to = "ind_code")
  setDT(colcodes)
  colcodes <- colcodes[!is.na(ind_code)]
  
  # Clean
  m <- m[7:nrow(m), ]
  setDT(m)
  # Use this to build matrix
  res <- build_matrix(m, colcodes, year)
  dt <- res[[1]]
  agg <- res[[2]]
  if(i == 1){
    all_data <- dt
    all_agg <- agg
  }else{
    all_data <- rbind(all_data, dt)
    all_agg <- rbind(all_agg, agg)
  }
}

#Function to build clean IO matrix and aggregates
#CURRENTLY ONLY WORKS post 1997: need to fix this for pre 1996.
build_matrix <- function(m, colcodes, year){
  #Format long
  key <- m[!is.na(IOCode) & !is.na(Name), .(IOCode, Name)]
  
  setnames(m, old = c("Name", "IOCode"), new = c("commodity", "code_commodity"))
  
  dt <- setDT(pivot_longer(m, cols = 3:(ncol(m)-2),
                           names_to = "industry", values_to = "value"))
  dt[, year := year]
  dt[,c('Total Final Uses (GDP)','Total Commodity Output') := NULL]
  
  #Clean, get key
  dt[,code_industry := colcodes[.SD,on = .(industry), x.ind_code]]
  
  #Remove final uses
  dt <- dt[!substr(code_industry,1,1)=="F"]
  convert <- c("value")
  dt[,(convert) := lapply(.SD, as.numeric),.SDcols = convert]
  
  #Industry aggregates:
  totals <- dt[commodity == "Total Industry Output", .(industry, value)]
  dt[, total_industry_output := totals[.SD, on = .(industry), x.value]]
  
  #For post 1997
  aggs <- dt[commodity == 'Total Intermediate' | 
               code_commodity %in% c("V001", "V002", "V003")]
  
  ##NEED TO MAKE ANOTHER for pre 1997
  ##HERE## different codes
  
  aggs[,code_commodity := NULL]
  setnames(aggs, old = "commodity", new = "item")
  aggs <- aggs[, .(code_industry, industry, item, value, total_industry_output)]
  
  #Format - remove aggregates
  dt <- dt[!is.na(code_commodity) & !is.na(commodity)] #No code in post-1996
  dt <- dt[!code_industry %in% c("T001", "T004", "T007")] #These are agg codes in pre-1996
  dt <- dt[!code_commodity %in% c("T005", "T006", "T008")]
  
  
  dt <- dt[, .(year, code_industry, industry, code_commodity, commodity, 
               value, total_industry_output)]
  
  
  #Shares
  setorder(dt,code_industry,code_commodity)
  dt[is.na(value), value := 0]
  dt[,share := value / total_industry_output]
  
  #Test that sums are correct:
  dt[, test := sum(value) / total_industry_output, by=.(code_industry)]
  resid <- max(abs(dt$test - 1))
  if(resid > 1e-3){
    print(paste0("Warning: error of ", round(resid, digits = 5),
                 " in year ",year))
  }
  
  return(list(dt, aggs))
}
library(paletteer)
library(patchwork)
library(readxl)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(plotly)
library(zoo)
library(docstring)

backg<-theme(
  plot.background = element_rect(fill = "transparent", colour = NA),
  panel.background = element_rect(fill = "transparent", colour = NA), 
  legend.background = element_rect(fill = "transparent",colour=NA),
  legend.box.background = element_rect(fill='transparent',colour = NA),
  legend.key = element_rect(fill = "transparent", colour = NA)
)


load_io_table <- function(year){
  #' Load IO table for a given year
  if (year <= 1996) {
    m <- suppressMessages(read_excel(
      file.path(
        "data", "NIPA",
        "IO tables", "IO_1963-1996",
        "IOUse_Before_Redefinitions_PRO_1963-1996_Summary.xlsx"
      ),
      sheet = as.character(year)
    ))
    colnames(m) <- m[5, ]
    colnames(m)[1:2] <- c("IOCode", "Name")
    colcodes <- m[6, 3:ncol(m)]
  } else {
    m <- suppressMessages(read_excel(
      file.path(
        "data", "NIPA", "IO tables", "IO_1997-2023",
        "IOUse_Before_Redefinitions_PRO_1997-2023_Summary.xlsx"
      ),
      sheet = as.character(year)
    ))
    colnames(m) <- m[6, ]
    colcodes <- m[5, ]
  }
  return(list(m, colcodes))
}



build_matrix <- function(m, colcodes, cw, year){
  #' Function to build clean IO matrix
  #' 
  #' @param m data table. The raw IO matrix
  #' @param colcodes data table. Key for column (industry) IO codes
  #' @param cw data table. Crosswalk for our NNA codes
  #' @param year int. The year being assembled
  #' 
  #' @returns data table. The IO matrix in clean long format
  
  #Codes for commodities (rows)
  key <- m[, .(IOCode, Name)]
  if(year <= 1996){
    usecw <- unique(cw[, .(IO_pre1997_code, NNA_code)])
    setnames(usecw, old = "IO_pre1997_code", new = "IOCode")
  }else{
    usecw <- unique(cw[, .(IO_post1997_code, NNA_code)])
    setnames(usecw, old = "IO_post1997_code", new = "IOCode")
  }
  key[, NNA_code := usecw[.SD, on = .(IOCode), x.NNA_code]]
  
  
  #Pivot and add commmodity NNA codes
  dt <- setDT(pivot_longer(m, cols = 3:ncol(m),
                           names_to = "industry", values_to = "value"))
  dt[, code_commodity := unique(key[, .(Name, NNA_code)])
     [.SD, on = .(Name), x.NNA_code]]
  #Add industry codes
  dt[, code_industry := colcodes[.SD, on = .(industry), x.NNA_code]]
  
  suppressWarnings(dt[, value := as.numeric(value)]) # Converts ".." to NA.
  dt[is.na(value), value := 0]
  
  setnames(dt, old = "Name", new = "commodity")
  #Total industry output and value added (per unique NNA_codes):
  totals <- dt[commodity %in% c("Total Industry Output", "Total Value Added",
                                "Total Intermediate"),
               .(code_industry, value, commodity)]
  totals <- totals[, .(value = sum(value)), 
                   by = .(code_industry, commodity)]
  
  #Aggregate by NNA code, since some are duplicated:
  dt <- dt[, .(value = sum(value)), by = .(code_industry, code_commodity)]
  dt[, year := year]
  dt <- dt[!is.na(code_commodity)] #Removes all aggregate commodities
  dt <- dt[!is.na(code_industry)] #Removes all final use industries
  
  #Add names and totals
  NNAcodes <- unique(cw[, .(NNA_code, NNA_industry)])
  dt[, industry := NNAcodes[.SD, on = .(NNA_code = code_industry), x.NNA_industry]]
  dt[, commodity := NNAcodes[.SD, on = .(NNA_code = code_commodity), x.NNA_industry]]
  
  dt[, industry_total_output := totals[commodity == "Total Industry Output"]
     [.SD,on = .(code_industry), x.value]]
  dt[, industry_value_added := totals[commodity == "Total Value Added"]
     [.SD,on = .(code_industry), x.value]]
  dt[, industry_intermediates := totals[commodity == "Total Intermediate"]
     [.SD,on = .(code_industry), x.value]]
  
  #Format
  dt <- dt[, .(year, code_industry, industry, code_commodity, commodity, 
               value, industry_value_added, industry_total_output, industry_intermediates)]
  setorder(dt,code_industry,code_commodity)
  
  
  #Shares
  dt[,share_in_output := value / industry_total_output]
  
  #Test that sums are correct:
  dt[, test := sum(value) / industry_intermediates, by=.(code_industry)]
  resid <- max(abs(dt$test - 1))
  if(resid > 0.02){
    print(paste0("Warning: error of ", round(resid, digits = 5),
                 " in year ",year))
  }
  dt[, c("test", "industry_intermediates") := NULL]
  
  return(dt)
}
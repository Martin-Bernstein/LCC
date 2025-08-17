setwd(file.path(
  "~", "Dropbox-HarvardUniversity", "Martin Bernstein",
  "Fall 2025", "Other", "LCC"
))

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

backg <- theme(
  plot.background = element_rect(fill = "transparent", colour = NA),
  panel.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.box.background = element_rect(fill = "transparent", colour = NA),
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
  #' @returns list. The first item is the IO matrix in clean long format,
  #' the second is a data table of industry aggregates (GO, intermediates, VA)
  
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
  totals[, year := year]
  
  #Aggregate by NNA code, since some are duplicated:
  dt <- dt[, .(value = sum(value)), by = .(code_industry, code_commodity)]
  dt[, year := year]
  dt <- dt[!is.na(code_commodity)] #Removes all aggregate commodities
  dt <- dt[!is.na(code_industry)] #Removes all final use industries
  
  #Add names and totals
  NNAcodes <- unique(cw[, .(NNA_code, NNA_industry)])
  dt[, industry := NNAcodes[.SD, on = .(NNA_code = code_industry),
                            x.NNA_industry]]
  dt[, commodity := NNAcodes[.SD, on = .(NNA_code = code_commodity),
                             x.NNA_industry]]
  totals[, industry := NNAcodes[.SD, on = .(NNA_code = code_industry),
                                x.NNA_industry]]
  
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
  dt[,share_in_industry_output := value / industry_total_output]
  
  #Test that sums are correct:
  dt[, test := sum(value) / industry_intermediates, by=.(code_industry)]
  resid <- max(abs(dt$test - 1))
  if(resid > 0.02){
    print(paste0("Warning: error of ", round(resid, digits = 5),
                 " in year ",year))
  }
  dt[, c("test", "industry_intermediates", "value",
         "industry_value_added", "industry_total_output") := NULL]
  
  totals <- totals[!is.na(code_industry)]
  setnames(totals, old = "commodity", new = "aggregate")
  totals <- totals[, .(year, code_industry, industry, aggregate, value)]
  return(list(dt, totals))
}

verify_crosswalk <- function(cw){
  ind <- fread(file.path("data", "codes and crosswalks",
                         "IND1990_codes.csv"))
  ind <- ind[Code != 0]
  ind[, in_cw := Code %in% cw$IND1990_code]
  check_cw_problems(cw, ind)
  
  io <- fread(file.path("data", "codes and crosswalks",
                        "IO_pre1997_codes.csv"))
  io[, in_cw := IO_pre1997_code %in% cw$IO_pre1997_code]
  check_cw_problems(cw, io)
  
  io <- fread(file.path("data", "codes and crosswalks",
                        "IO_post1997_codes.csv"))
  io[, in_cw := IO_post1997_code %in% cw$IO_post1997_code]
  check_cw_problems(cw, io)
  
  
}

check_cw_problems <- function(cw, ind, codename){
  problems <- nrow(ind[in_cw == F])
  if(problems > 0){
    print(paste0("Warning: Unassigned ", codename, " codes"))
  }
  rm(ind, problems) 
}

decompose_grossoutput <- function(g, years, userows, usecw){
  #' Function to format a KLEMS gross output by industry table
  #' 
  #' @param g data table. The raw KLEMS table.
  #' @param years integer. Vector of years contained in the KLEMS table
  #' @param userows integer. Vector of rows to use from the raw KLEMS table.
  #' @param usecw data table. The crosswalk table to convert from industry names
  #' to our NNA code. Should have column names "industry" and "NNA_code"
  #' 
  #' @returns data table. The cleaned, long-formatted KLEMS table.

  # Format
  names(g)<-as.character(g[1,])
  g <- g[2:nrow(g),]
  tonum <- names(g)[3:ncol(g)]
  g[,(tonum) := lapply(.SD,as.numeric),.SDcols=tonum]
  g[,(tonum):=lapply(.SD,nafill,fill=0),.SDcols=tonum]
  #Only include the correct rows (exclude addenda and empty space)
  g <- g[userows, ]
  #Need to name industries
  g[, industry := trimws(industry)]
  #These rows are item names, not industry names
  items <- g[2:9, industry]
  
  #NNA codes: only at same level of disaggregation as IO tables
  g[, nna_code := usecw[.SD, on = .(industry), x.NNA_code]]
  #Naming conventions all line up, except for these:
  if(min(years) < 1997){
    g[industry %in% c("General government", "Government enterprises"),
      nna_code := "G"] 
  }else{
    g[industry %in% c("National defense", "Nondefense"), nna_code := "G"]
  }
  
  #Check that everything was coded
  diff1 <- setdiff(unique(g[!is.na(nna_code), nna_code]), unique(usecw$NNA_code))
  diff2 <- setdiff(unique(usecw$NNA_code), unique(g[!is.na(nna_code), nna_code]))
  if(length(unique(c(diff1, diff2))) > 1){
    if(length(unique(c(diff1, diff2))) > 1){
      print("Warning: missing industries")
    }
  }
  rm(diff1, diff2)
  
  
  # Name what each item is
  g[is.na(nna_code), item := industry]
  g[is.na(item), item := 'Gross output']
  # Identify aggregated industries
  g[!industry %in% items & is.na(nna_code), nna_code := "don't include"]
  # Now fix the industry names
  g[is.na(nna_code), industry:= NA]
  # Forward fill the industry and code columns
  g[, industry := na.locf(industry, na.rm = FALSE)]
  g[, nna_code := na.locf(nna_code, na.rm = FALSE)]
  
  g <- g[nna_code != "don't include"]
  
  # Now pivot longer:
  g <- pivot_longer(g,as.character(years),names_to='year',values_to='value')%>%
    setDT()
  g[,year := as.numeric(year)]
  
  # Aggregate appropriately:
  g <- g[, .(value = sum(value)), by = .(nna_code, year, item)]
  g[, industry := nna[.SD, on = .(NNA_code = nna_code), x.NNA_industry]]
  
  g[, gross_output := g[item == "Gross output"]
    [.SD, on = .(industry, nna_code, year), x.value]]
  g[, value_added := g[item == "Value added"]
    [.SD, on = .(industry, nna_code, year), x.value]]
  # Gross output = Value added + Intermediate inputs
  # Value added = Compensation of employees + taxes-dubsidies + Gross op. surplus
  g <- g[!item %in% c("Gross output", "Value added", "Energy inputs",
                      "Materials inputs", "Purchased-services inputs")]
  g[, share_in_output := value / gross_output]
  
  g[, c("value_added", "value") := NULL]
  g <- g[, .(year, nna_code, industry, item, share_in_output, gross_output)]
  
  g[, industry := factor(industry, levels = nna$NNA_industry)] 
  itemlevels <- c("Compensation of employees",
                  "Gross operating surplus",
                  "Taxes on production and imports less subsidies",
                  "Intermediate inputs")
  g[, item := factor(item, levels = itemlevels)]
  
  #These years are just missing
  g[year < 1987 & share_in_output == 0 & item !="Intermediate inputs",
    share_in_output := NA]
  
  return(g)
}

format_total_intermediates <- function(g){
  #' Extract just gross output and total intermediates for all sectors.
  #' @param g data table. The raw KLEMS data.
  #' @returns data table. Long-formatted aggregate data.
  names(g) <- as.character(g[1, ])
  # Take overlapping year out from historical data
  if(max(suppressWarnings(as.numeric(names(g))), na.rm = TRUE) == 1997){
    g[, `1997` := NULL]
  }
  g <- g[c(2, 7), ]
  g[, Line := NULL]
  g <- pivot_longer(g, cols = 2:ncol(g),
                    names_to = "year",
                    values_to = "value")%>%
    setDT()
  g[, year := as.numeric(year)]
  g[, value := as.numeric(value)]
  return(g)
}
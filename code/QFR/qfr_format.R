library(readxl)
library(data.table)
library(tidyr)
library(dplyr)

source(file.path("code", "utility-functions.R"))

# Assemble QFR from the excel files
# In decade folders, with quarters on separate sheets.
for(decade in c((194:202) * 10)){
  
  extension <- ".xls"
  if(decade == 2020) extension <- ".xlsx"
  
  folder <- paste0(decade, "s")
  
  yr_range <- decade:(decade + 9)
  if(decade == 1940) yr_range <- 1947:1949 
  if(decade == 2020) yr_range <- 2020:2025
  
  for(y in yr_range){
    
    print(paste0("Building QFR. Year = ", y))
    
    yname <- y
    
    q_range <- 1:4
    if(y == 2025) q_range <- 1:2
    
    for(q in q_range){
      
      if(y == 2000 & q < 4) yname <- paste0(y, "SIC")
      if(y == 2000 & q == 4) yname <- paste0(y, "NAICS")
      
      thissheet <- paste0(y, "Q", q)
      if(decade > 2000) thissheet <- paste0(q, "Q", y)
      if(y %in% 2010:2011) thissheet <- paste0(q, "Q ", y)
      
      d <- suppressMessages(read_excel(file.path("data",
                                                 "QFR",
                                                 "qfrhistoricaltables",
                                                 folder,
                                                 paste0("QFRHistorical",
                                                        yname,
                                                        extension)),
                                       sheet = thissheet)) %>%
        setDT()
      
      
      names(d) <- paste0(d[1, ], "_", d[2, ])
      names(d)[1] <- c("bsheet_item")
      d <- d[3:nrow(d), ]
      
      # Convert columns 2:ncol(d) to numeric in-place (handles duplicate names)
      for (j in 2:ncol(d)) {
        set(d, j = j, value = suppressWarnings(as.numeric(d[[j]])))
      }
      
      # Drop any columns that are all zeros (ignoring NA)
      keep <- sapply(d[, 2:ncol(d)], function(x) sum(x, na.rm = TRUE) != 0)
      d <- d[, c(TRUE, keep), with = FALSE]
      
      d <- pivot_longer(d, cols = 2:ncol(d), names_sep = "_",
                        names_to = c("industry_code", "asset_size_code"),
                        values_to = "value") %>%
        setDT()
      
      
      d[, quarter := q]
      if(q == 1){
        df <- d
      }else{
        df <- rbind(df, d)
      }
    }
    df[, year := y]
    
    # build key of industry codes
    keysheet <- "KEY"
    if(y == 2023) keysheet <- "2022 NAICS KEY"
    if(y == 2009) keysheet <- "KEY SERVICES"
    
    key <- suppressMessages(read_excel(file.path("data",
                                                 "QFR",
                                                 "qfrhistoricaltables",
                                                 folder,
                                                 paste0("QFRHistorical",
                                                        yname,
                                                        extension)
    ),
    sheet = keysheet)) %>%
      setDT()
    
    # Find indices of industry codes
    endidx <- which(key[, 1] == "Asset Size Code Definitions") - 2
    startidx <- which(key[, 1] == "Industry Code Definitions") + 5
    
    industry_key <- key[startidx:endidx, 1:4]
    names(industry_key) <- c("columns", "QFR_industry_code",
                             "QFR_industry_title", "standard_industry_code")
    industry_key[, year := y]
    if(y < 2000){
      industry_key[, standard_code_type := "SIC_1987"]
    }else if(y < 2023){
      industry_key[, standard_code_type := "NAICS_2007"]
    }else{
      industry_key[, standard_code_type := "NAICS_2022"]
    }
    
    # Find indices of bsheet item codes
    startidx <- which(key[, 2] == "Financial Data Item Code") + 2
    lastrow <- max(as.numeric(as.vector(key[, 1])[[1]]), na.rm = TRUE)
    endidx <- which(key[, 1] == as.character(lastrow))
    
    asset_key <- key[startidx:endidx, 2:3]
    names(asset_key) <- c("bsheet_item", "description")
    # Collapse descriptions and clean:
    asset_key[, group_id := cumsum(!is.na(bsheet_item))]
    asset_key <- asset_key[
      , .(
        bsheet_item = first(na.omit(bsheet_item)),
        description = paste(na.omit(description), collapse = " ")
      ),
      by = group_id
    ][, group_id := NULL]
    # remove any empty rows
    asset_key <- asset_key[!is.na(bsheet_item) & bsheet_item != ""]
    asset_key[, year := y]
    
    # Key of asset sizes:
    startidx <- which(key[, 1] == "Asset Size Code") + 2
    endidx <- which(key[, 2] == "Financial Data Item Code") - 4
    size_key <- key[startidx:endidx, c(1, 3)]
    names(size_key) <- c("asset_size_code", "description")
    # Join description of asset sizes
    size_key[, asset_size_code := as.numeric(asset_size_code)] 
    size_key[, year := y]
    d[, asset_size_code := as.numeric(asset_size_code)]
    d[, asset_size_description :=
        size_key[.SD, on = .(asset_size_code), x.description]]
    # Join description of industry names.
    # Trim whitespace of industry names:
    d[, industry_code := gsub("\\s+", "", industry_code)]
    industry_key[, QFR_industry_code := gsub("\\s+", "", QFR_industry_code)]
    # Now perform the join
    # d[, industry_name :=
    #     industry_key[.SD,
    #                  on = .(QFR_industry_code = industry_code),
    #                  x.QFR_industry_title]]
    
    
    if(y==decade | y == 1947){
      decade_df <- df
      decade_key <- industry_key
      decade_asset <- asset_key
      decade_size <- size_key
    }else{
      decade_df <- rbind(decade_df, df)
      decade_key <- rbind(decade_key, industry_key)
      decade_asset <- rbind(decade_asset, asset_key)
      decade_size <- rbind(decade_size, size_key)
    }
  }
  if(decade == 1940){
    all_df <- decade_df
    all_key <- decade_key
    all_assetkey <- decade_asset
    all_sizekey <- decade_size
  }else{
    all_df <- rbind(all_df, decade_df)
    all_key <- rbind(all_key, decade_key)
    all_assetkey <- rbind(all_assetkey, decade_asset)
    all_sizekey <- rbind(all_sizekey, decade_size)
  }
  
}
# Set neat date:
all_df[, date := paste0(year, "Q", quarter)]
all_df[, date := date_conv2(date)]
# And size codes
all_df[, asset_size_code := as.numeric(asset_size_code)]
all_df[is.na(asset_size_code)]



# Build wide format to help make industry crosswalks
all_key <- all_key[!is.na(QFR_industry_code)]
maxn <- max(all_key[, .(Ns = .N), by = .(year)]$Ns)
wide_key <- data.table(codes = rep(NA, times = maxn))
for(y in 1947:2025){
  yr_codes <- all_key[year == y, QFR_industry_code]
  yr_names <- all_key[year == y, QFR_industry_title]
  wide_key[1:length(yr_codes), c(paste0("codes_", y)) := yr_codes]
  wide_key[1:length(yr_codes), c(paste0("names_", y)) := yr_names]
}
wide_key[, codes := NULL]
write.csv(wide_key, file = file.path("data", "QFR", "crosswalks", "wide",
                                     "wide_industry_coverage.csv"),
          na = "",
          row.names = FALSE)
write.csv(all_key, file = file.path("data", "QFR", "crosswalks",
                                    "industry_codes.csv"),
          row.names = FALSE)


# Same wide format for bsheet item crosswalks.
maxn <- max(all_assetkey[, .(Ns = .N), by = .(year)]$Ns)
wide_key <- data.table(codes = rep(NA, times = maxn))
for(y in 1947:2025){
  yr_codes <- all_assetkey[year == y, bsheet_item]
  yr_names <- all_assetkey[year == y, description]
  wide_key[1:length(yr_codes), c(paste0("codes_", y)) := yr_codes]
  wide_key[1:length(yr_codes), c(paste0("names_", y)) := yr_names]
}
wide_key[, codes := NULL]
write.csv(wide_key, file = file.path("data", "QFR", "crosswalks", "wide",
                                     "wide_bsheet_items.csv"),
          na = "",
          row.names = FALSE)
write.csv(all_assetkey, file = file.path("data", "QFR", "crosswalks",
                                         "bsheet_item_codes.csv"),
          row.names = FALSE)


# And for asset sizes - need to make consistent size codes
maxn <- max(all_sizekey[, .(Ns = .N), by = .(year)]$Ns)
wide_key <- data.table(codes = rep(NA, times = maxn))
for(y in 1947:2025){
  yr_codes <- all_sizekey[year == y, asset_size_code]
  yr_names <- all_sizekey[year == y, description]
  wide_key[1:length(yr_codes), c(paste0("codes_", y)) := yr_codes]
  wide_key[1:length(yr_codes), c(paste0("names_", y)) := yr_names]
}
wide_key[, codes := NULL]
write.csv(wide_key, file = file.path("data", "QFR", "crosswalks", "wide",
                                     "wide_asset_size_codes.csv"),
          na = "",
          row.names = FALSE)
write.csv(all_sizekey, file = file.path("data", "QFR", "crosswalks",
                                         "asset_size_codes.csv"),
          row.names = FALSE)








write.csv(all_df, file = file.path("data", "constructed data", "firm-assets",
                                   "qfr_long.csv"),
          row.names = FALSE)
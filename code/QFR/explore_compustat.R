v <- fread(file.path("data", "compustat", "compustat_variables.csv"))

assetvars <- v[tolower(description) %like% "asset", variable]
dataids <- c("costat", "curcd", "datafmt", "indfmt", "consol", "tic", "datadate",
             "gvkey", "conm", "cusip", "cik", "exchg", "fyr", "fic")
idvars <- c(dataids, v[c(1:6, 13, 15, 31), variable])

d <- fread(file.path("data", "compustat", "compustat_annual.csv"),
           select = unique(c(idvars, assetvars)))


dt <- d[!is.na(naics)]
dt <- pivot_longer(dt, cols = all_of(assetvars),
                   names_to = "asset_item",
                   values_to = "value") %>%
  setDT()

av <- v[tolower(description) %like% "asset"]
write.csv(av, file = file.path("data", "compustat", "compustat_assets.csv"),
          row.names = FALSE)

## HERE: looking at compustat_asset_hierarchy.csv.
# Can I create the proper aggregation/subcategories of assets
# from that? And then, what does compustat say about asset bsheet composition
# in each NAICS code?
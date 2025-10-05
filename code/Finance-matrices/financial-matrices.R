setwd(file.path(
  "~", "Dropbox-HarvardUniversity", "Martin Bernstein",
  "Fall 2025", "Other", "LCC"
))

source(file.path("code", "utility-functions.R"))

decay_parameter <- 0.9

fw <- get_fwtw()

#### Aggregated Five sectors #####
dt <- fw[, .(Level = sum(Level)),
         by = .(Date, holder_sector, issuer_sector, gdp)]

# double check that the matrix is correct.
# The concept to agree with the IO one is,
# Build a matrix where a_{ij} is j lends to i.
# out of i's total borrowing
# all_dates <- sort(unique(dt$Date))
# for(i in seq_along(all_dates)){
#   d <- dt[Date == all_dates[i]]
#   d <- d[holder_sector != "Discrepancy"]
#   # We're asking: what's i's borrowing from j out of i's total borrowing.
#   # Issuer is borrower
#   d[, share_of_issuers_borrowing := Level / sum(Level),
#     by = .(Date, issuer_sector)]
#   setorder(d, holder_sector, issuer_sector)
#   d <- d[, .(issuer_sector, holder_sector, share_of_issuers_borrowing)]
#   d <- pivot_wider(d, names_from = holder_sector,
#                    values_from = share_of_issuers_borrowing) %>%
#     setDT()
#   
#   issuer_sectors <- d$issuer_sector
#   d[, issuer_sector := NULL]
#   holder_sectors <- names(d)
#   
#   if(sum(holder_sectors != issuer_sectors) > 0){
#     print("Warning: matrix is unbalanced (issuers noteq holders")
#   }
#   
#   d <- as.matrix(d)
#   # Then, because we've built matrix B in notes,
#   # eigenvector centrality relies on B'.
#   # (this is different from the IO version, where
#   # the data are already formatted as B'.)
#   v <- eigen(t(d))
#   # Centrality is first eigenvec (which is on largest eigenval,
#   # guaranteed all positive and real).
#   eigv <- as.numeric(v$vectors[, 1])
#   # Unique only up to scale; sometimes comes out all negative.
#   if(sum(eigv <= 0) == length(eigv)){
#     eigv <- -eigv
#   }
#   df <- setDT(data.frame(holder = holder_sectors,
#                          centrality = eigv,
#                          Date = all_dates[i])) 
#   
#   if(i == 1){
#     alldf <- df
#   }else{
#     alldf <- rbind(alldf, df)
#   }
# }
# 
# ggplot(alldf, aes(Date, centrality)) +
#   geom_line() +
#   facet_wrap(~holder) + 
#   theme_bw()

# INDEED: when I did this, I got "centrality_for == 'lender'" below.)
# So original thinking was correct.
# What this clarifies is that *ldner centrality* is indeed the concept
# I'm intuitively most interested in; it's analogous to "supplier centrality"
# In IO tables because it's like 'supplier of capital.'

res <- measure_centrality(copy(dt))
all_eigvs <- res[[1]]
all_leontief <- res[[2]]

ggplot(all_eigvs[centrality_for == "borrower"], aes(Date, ev)) +
  geom_line() +
  facet_wrap(~sector) +
  theme_bw() +
  labs(x = NULL, y = "Centrality as borrower")

ggplot(all_eigvs[centrality_for == "lender"], aes(Date, ev)) +
  geom_line() +
  facet_wrap(~sector) +
  theme_bw() +
  labs(x = NULL, y = "Centrality as lender")


##### Disaggregated all financial instits #####
d <- fw[,.(Level = sum(Level)), by = .(Date,`Holder Name`, `Issuer Name`, gdp)]
setnames(d, old = c("Holder Name", "Issuer Name"),
         new = c("holder_sector", "issuer_sector"))

res <- measure_centrality(d)

all_eigvs <- res[[1]]
all_leontief <- res[[2]]

ggplot(all_eigvs[centrality_for == "borrower"], aes(Date, abs(ev))) +
  geom_line() +
  facet_wrap(~sector) +
  theme_bw() +
  labs(x = NULL, y = "Centrality as borrower")

ggplot(all_eigvs[centrality_for == "lender"], aes(Date, abs(ev))) +
  geom_line() +
  facet_wrap(~sector) +
  theme_bw() +
  labs(x = NULL, y = "Centrality as lender")
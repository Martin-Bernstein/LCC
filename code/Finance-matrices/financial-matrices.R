setwd(file.path(
  "~", "Dropbox-HarvardUniversity", "Martin Bernstein",
  "Fall 2025", "Other", "LCC"
))

source(file.path("code", "utility-functions.R"))

decay_parameter <- 0.9

fw <- get_fwtw()

#### Aggregated Five sectors #####
d <- fw[, .(Level = sum(Level)),
        by = .(Date, holder_sector, issuer_sector, gdp)]

res <- measure_centrality(d)
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
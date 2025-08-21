##### Distributional FWTW #####
# Format FWTW
fw <- get_fwtw()
# Format DFAs
dfa <- get_dfa()
dfa[, c("Assets", "Nonfinancial assets", "Net worth", "Household count") := NULL]


f <- fw
df <- dfa[category == "Financial"]

# Split into holders and issuers
dfa_holders <- df[type == "asset"]
dfa_issuers <- df[type == "liability"]
fwtw_holders <- f[`Holder Name` == "Households"]
fwtw_issuers <- f[`Issuer Name` == "Households"]

# Consistent instruments across FWTW and DFA, for holders
cw <- fread(file.path("data", "codes and crosswalks", "fwtw_dfa_instrument_crosswalk.csv"))
usecw <- unique(cw[, .(my_instrument_holder, dfa_instrument_holder)])
dfa_holders[, my_instrument := usecw[.SD, on =
                                       .(dfa_instrument_holder = Instrument),
                                     x.my_instrument_holder]]

usecw <- unique(cw[, .(my_instrument_holder, fwtw_code)])
fwtw_holders[, my_instrument :=
               usecw[.SD, on = .(fwtw_code = `Instrument Code`),
                     x.my_instrument_holder]]


# Consistent instruments for issuers
dfa_issuers[, my_instrument := Instrument]
usecw <- unique(cw[, .(dfa_instrument_issuer, fwtw_code)])
fwtw_issuers[, my_instrument :=
               usecw[.SD, on = .(fwtw_code = `Instrument Code`),
                     x.dfa_instrument_issuer]]

fwtw_holders[, type := "Holder"]
fwtw_issuers[, type := "Issuer"]
allfwtw <- rbind(fwtw_holders, fwtw_issuers)

dfa_holders[, type := "Holder"]
dfa_issuers[, type := "Issuer"]
alldfa <- rbind(dfa_holders, dfa_issuers)
# Aggregate across consistent instruments
alldfa <- alldfa[, .(Level = sum(Level)), by = .(my_instrument,
                                                 type,
                                                 Date,
                                                 Category,
                                                 `Financial assets`)]

# Amount of this instrument held by this percentile group:
alldfa[, pctile_share := Level / sum(Level),
       by = .(Date, my_instrument, type)]

# Average shares:
avgs <- unique(alldfa[, .(Date, `Financial assets`, Category, type)])
avgs[, share := `Financial assets` / sum(`Financial assets`),
     by = .(Date, type)]

alldfa[is.nan(pctile_share), pctile_share :=
         avgs[.SD, on = .(Category, Date, type), x.share]]

allfwtw <- allfwtw[, .(Level = sum(Level)),
                   by = .(my_instrument, `Holder Name`, `Issuer Name`,
                          `Holder Code`, `Issuer Code`, type, Date)]

# These are the shares of each instrument issued (held) by each issuer (holder)
allfwtw[type == "Holder", share := Level / sum(Level),
        by = .(my_instrument, `Holder Name`, `Holder Code`, Date)]
allfwtw[type == "Issuer", share := Level / sum(Level),
        by = .(my_instrument, `Issuer Name`, `Issuer Code`, Date)]


# Need to combine:
# one entry per instrument per issuer (holder) per percentile group:
all <- merge(
  unique(allfwtw[, .(my_instrument, `Holder Name`, `Issuer Name`, type,
                     `Holder Code`, `Issuer Code`, Date)]),
  unique(alldfa[, .(my_instrument, Date, Category, type)]),
  by = c("my_instrument", "Date", "type"),
  allow.cartesian = TRUE
)

# Join issuer (holder) shares of instruments,
# and fwtw levels (across all pctile groups)
all[, c("share_instrument_from_issuer/holder",
        "Level_allHH_by_instrument_issuer/holder") :=
      allfwtw[.SD, on = .(`Holder Code`,
                          `Issuer Code`,
                          my_instrument,
                          type,
                          Date),
              .(x.share, x.Level)]]

# Household total holdings of each instrument, fwtw
allfwtw[type == "Holder", Level_all := sum(Level),
             by = .(Date, `Holder Code`, my_instrument)]
# Total issuing of each instrument
allfwtw[type == "Issuer", Level_all := sum(Level),
        by = .(Date, `Issuer Code`, my_instrument)]

alldfa[, Level_all_pctiles := sum(Level),
       by = .(Date, my_instrument, type)]

#Proof of concept:
allfwtw[, Level_all_dfa :=
          unique(alldfa[, .(my_instrument, Date, type, Level_all_pctiles)])
        [.SD, on = .(Date, my_instrument, type), x.Level_all_pctiles / 1000]]
allfwtw[, gdp := gd[.SD, on = .(Date), x.gdp]]

# The instrument mapping is correct.
p <- ggplot(allfwtw[type == "Holder"],
            aes(Date, Level_all / gdp, color = "FWTW")) +
  geom_line() +
  geom_line(mapping = aes(Date, Level_all_dfa / gdp, color = "DFA")) +
  facet_wrap(~my_instrument) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "HH holdings per GDP", color = NULL)
ggsave(p, file = file.path("figures", "exploration", "Finance-matrices",
                           "DFA_FWTW_instrument-reconciliation.png"),
       width = 8, height = 6, units = "in")


p <- ggplot(allfwtw[type == "Issuer"],
            aes(Date, Level_all / gdp, color = "FWTW")) +
  geom_line() +
  geom_line(mapping = aes(Date, Level_all_dfa / gdp, color = "DFA")) +
  facet_wrap(~my_instrument) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "HH issuances per GDP", color = NULL)

# How much each pctile group owns/issues of each instrument
all[, pctile_share_of_instrument :=
      alldfa[.SD, on = .(Category, Date, my_instrument, type), x.pctile_share]]

# So now can get total fwtw level by pctile group
all[, Level := `Level_allHH_by_instrument_issuer/holder` *
      pctile_share_of_instrument]

# Check:
all[, sum_pctile_categories :=
      sum(Level), by = .(my_instrument, `Holder Code`,
                         `Issuer Code`, type, Date)]

error <- max(abs(all$sum_pctile_categories -
                   all$`Level_allHH_by_instrument_issuer/holder`))
if(error > 1e-10){
  print("Warning: mismatch!")
}

all[, c("share_instrument_from_issuer/holder",
        "Level_allHH_by_instrument_issuer/holder",
        "pctile_share_of_instrument",
        "sum_pctile_categories") := NULL]

#### Join back to rest of fwtw ####
fw_nonhh <- fw[`Holder Name` != "Households" & `Issuer Name` != "Households"]
# Aggregate across instruments.
fw_nonhh <- fw_nonhh[, .(Level = sum(Level)),
                     by = .(Date, `Issuer Name`, `Issuer Code`,
                            `Holder Name`, `Holder Code`)]

# Household issuer and holder now unique by percentile category
all[, category_code := categories[.SD, on = .(Category), x.Category_code]]
all[type == "Issuer", `Issuer Name` := paste0(`Issuer Name`, "_", Category)]
all[type == "Holder", `Holder Name` := paste0(`Holder Name`, "_", Category)]

all[, `Issuer Code` := as.character(`Issuer Code`)]
all[, `Holder Code` := as.character(`Holder Code`)]
all[type == "Issuer", `Issuer Code` := paste0(`Issuer Code`, "_", Category)]
all[type == "Holder", `Holder Code` := paste0(`Holder Code`, "_", Category)]

# Aggregate across instruments
all <- all[, .(Level = sum(Level)),
           by = .(Date, `Issuer Name`, `Holder Name`,
                  `Issuer Code`, `Holder Code`)]

#Join the two datasets:
fw_dfa <- rbind(all, fw_nonhh)
fw_dfa <- fw_dfa[Date >= "1989-07-01"]
setnames(fw_dfa, old = c("Holder Name", "Issuer Name"),
         new = c("holder_sector", "issuer_sector"))
res <- measure_centrality(fw_dfa)

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

### NEXT: should just do all HH groups, then Fin, Bus, Gov, ROW.
### too crowded currently.
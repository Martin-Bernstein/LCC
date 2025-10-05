setwd(file.path(
  "~", "Dropbox-HarvardUniversity", "Martin Bernstein",
  "Fall 2025", "Other", "LCC"
))

source(file.path("code", "utility-functions.R"))

decay_parameter <- .9

##### Distributional FWTW #####
# Format FWTW
fw <- get_fwtw()
# Format DFAs
dfa <- get_dfa()
dfa[, c("Assets", "Nonfinancial assets", "Net worth", "Household count") := NULL]


f <- fw
df <- dfa[category == "Financial"]

# GDP data
gd <- fread(file.path("data", "fred", "GDP.csv"))
names(gd) <- c("Date", "gdp")

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

# Have to handle HH to HH flows.
hh_to_hh <- allfwtw[`Holder Name` == "Households" &
                      `Issuer Name` == "Households"]
# Later, will need to allocate HH to HH flows.
# What do I need to do here?
# For each instrument, allocate proportionally.
# So if $10, 40, 50 goes into one instrument across three groups,
# and $80, 10, 10 comes out across the three groups,
# you would allocate proportionally: 8,1,1; 32,4,4; 40,5,5.
# But we don't have distribution for both sides of the same instrument.
# So, we should: for each held instrument, draw proportional arrows into agg HH.
# Allocate to each group based on the proportional arrows going out
# for the corresponding issued instrument.
usecw <- unique(cw[, .(fwtw_code, dfa_instrument_issuer)])
hh_to_hh[type == "Holder",
         my_issued_instrument :=
           usecw[.SD, on = .(fwtw_code = `Instrument Code`),
                 x.dfa_instrument_issuer]]

# Continue with aggregation to get shares, then will return to this.
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

setorder(all, Date, type, my_instrument, Category, `Issuer Name`, `Holder Name`)
# Allocate the HH <> HH flows!
hh_to_hh
allfwtw

# For each instrument, allocate proportionally.
# So if $10, 40, 50 goes into one instrument across three groups,
# and $80, 10, 10 comes out of that instrument across the three groups,
# you would allocate proportionally: 8,1,1; 32,4,4; 40,5,5.
# But we don't have distribution for both sides of the same instrument.
# So, we should: for each held instrument, draw proportional arrows into agg HH.
# Allocate to each group based on the proportional arrows going out
# for the corresponding issued instrument.

# allfwtw shares are:
# shares of each instrument issued (held) by each issuer (holder)
# So when type == "Holder", this is saying
# how much of the total held by households is issued by each issuer.
# And when type == "Isuser", this is saying
# how much of the total issued by households is held by each holder.
# So in HH to HH flows (tshares): when Holder, you're seeing how much
# of the total holdings of that instrument is issued by HHs.

# In alldfa, share is "share of this instrument held/issued by this pctile group."

# We see $100 municipal secturities held by HH issued by HH.
# First q: how much of that is owned by each percentile group?
# Second q: how much of that is issued by each percentile group?
# Need a dataset that's instrument by pctile group:
hh_matrix <- merge(
  unique(hh_to_hh[, .(`Instrument Name`, `Instrument Code`, my_instrument,
                      my_issued_instrument, Date, type)]),
  unique(alldfa[, .(my_instrument, type, Date, Category)]),
  by = c("Date", "my_instrument", "type"),
  allow.cartesian = TRUE
)

# Now since type is issuer or holder, we can have amt lent to all HH
# per category(pctile) per instrument,
# and amt borrowed from all HH per category per instrument.

# Total level of instrument held/issued
hh_matrix[, Level_all_hh := hh_to_hh[.SD, on = .(Date, `Instrument Code`, type),
                                     x.Level]]
# Share of this instrument held/issued by each pctile group:
hh_matrix[, pctile_share :=
            alldfa[.SD, on = .(type, my_instrument, Date, Category),
                   x.pctile_share]]
hh_matrix[, Level := Level_all_hh * pctile_share]
hh_matrix[, c("Level_all_hh") := NULL]

# Now, have amt of this instrument held and issued by each group.
# Now, need to draw each instrument going out.
holders <- hh_matrix[type == "Holder"]
issuers <- hh_matrix[type == "Issuer"]

setnames(holders, old = c("Category", "pctile_share", "Level"),
         new = c("holder_pctile", "holder_share", "holder_level"))
setnames(issuers, old = c("Category", "my_instrument", "pctile_share"),
         new = c("issuer_pctile", "my_issued_instrument", "issuer_share"))

# each held instrument maps to a separate category of issued instrument:
to_crossjoin <- unique(issuers[, .(Date, my_issued_instrument, issuer_pctile,
                                   issuer_share)])
# Now can build a matrix of from/to, crosswalking the instruments.
hh_matrix <- merge(
  holders, to_crossjoin, by = c("Date", "my_issued_instrument"),
  allow.cartesian = TRUE
)

# Now have share of instrument in and share of instrument out.
hh_matrix[, Level := holder_level * issuer_share]
# So can aggregate:
hh_matrix <- hh_matrix[, .(Level = sum(Level)),
                       by = .(Date, holder_pctile, issuer_pctile)]

#SO: I need to think about how to use this to implement the strategy in the comment above.
#And diagrammed in my notebook.
for(i in seq_along(all_dates)){
  test <- (all[`Issuer Name` == "Households" & `Holder Name` == "Households" &
                 Date == all_dates[i]])
  testhh <- hh_matrix[Date == all_dates[i]]
  a <- sum(test[type == "Holder"]$Level)
  b <- sum(test[type == "Issuer"]$Level)
  c <- sum(testhh$Level)
  if(abs(a - b) > 1e-8){
    print(paste0("Warning: HH to HH aggregate flows don't match in ",
                 all_dates[i]))
  }else if(abs(a - c) > 1e-8){
    print(paste0("Warning: distributional HH flows don't match in ",
                 all_dates[i]))
  } 
}
# Now, combine:
hh_matrix[, `Holder Name` := paste0("Households_", holder_pctile)]
hh_matrix[, `Issuer Name` := paste0("Households_", issuer_pctile)]
hh_matrix[, c("Holder Code", "Issuer Code") := 15]

all <- all[!(`Holder Name` == "Households" & `Issuer Name` == "Households")]
all <- all[, .(Level = sum(Level)),
           by = .(Date, `Holder Name`, `Issuer Name`,
                  Category, `Holder Code`, `Issuer Code`)]
all[`Holder Name` == "Households",
    `Holder Name` := paste0("Households_", Category)]
all[`Issuer Name` == "Households",
    `Issuer Name` := paste0("Households_", Category)]

all[, Category := NULL]
hh_matrix[, c("holder_pctile", "issuer_pctile") := NULL]

all <- rbind(all, hh_matrix)


#### Join back to rest of fwtw ####
fw_nonhh <- fw[`Holder Name` != "Households" & `Issuer Name` != "Households"]
# Aggregate across instruments.
fw_nonhh <- fw_nonhh[, .(Level = sum(Level)),
                     by = .(Date, `Issuer Name`, `Issuer Code`,
                            `Holder Name`, `Holder Code`)]


#Join the two datasets:
fw_dfa <- rbind(all, fw_nonhh)
fw_dfa <- fw_dfa[Date >= "1989-07-01"]
setnames(fw_dfa, old = c("Holder Name", "Issuer Name"),
         new = c("holder_sector", "issuer_sector"))


res <- measure_centrality(copy(fw_dfa))

all_eigvs <- res[[1]]
all_leontief <- res[[2]]

ggplot(all_eigvs[centrality_for == "lender"], aes(Date, abs(ev))) +
  geom_line() +
  facet_wrap(~sector) +
  theme_bw() +
  labs(x = NULL, y = "Centrality as lender")

ggplot(all_eigvs[centrality_for == "borrower"], aes(Date, abs(ev))) +
  geom_line() +
  facet_wrap(~sector) +
  theme_bw() +
  labs(x = NULL, y = "Centrality as borrower")



### NEXT: should just do all HH groups, then Fin, Bus, Gov, ROW.
### too crowded currently.
categories <- unique(fw_dfa[, .(`Holder Code`, holder_sector)])
categories <- setDT(data.frame(code = c(15, 31, 10, 11, 21, 26),
                               category = c("Households", "Government",
                                            "Non-financial business",
                                            "Non-financial business",
                                            "Government", "Rest of World")))
fw_dfa[, holder_category := categories[.SD, on = .(code = `Holder Code`),
                                       x.category]]
fw_dfa[, issuer_category := categories[.SD, on = .(code = `Issuer Code`),
                                       x.category]]
fw_dfa[`Holder Code` == 15, holder_category := holder_sector]
fw_dfa[`Issuer Code` == 15, issuer_category := issuer_sector]

fw_dfa[is.na(issuer_category), issuer_category := "Financial sector"]
fw_dfa[is.na(holder_category), holder_category := "Financial sector"]

agg <- fw_dfa[, .(Level = sum(Level)),
              by = .(Date, holder_category, issuer_category)]
setnames(agg, old = c("holder_category", "issuer_category"),
         new = c("holder_sector", "issuer_sector"))
res <- measure_centrality(copy(agg))


all_eigvs <- res[[1]]
all_leontief <- res[[2]]

ggplot(all_eigvs[centrality_for == "lender"], aes(Date, abs(ev))) +
  geom_line() +
  facet_wrap(~sector) +
  theme_bw() +
  labs(x = NULL, y = "Centrality as lender")

ggplot(all_eigvs[centrality_for == "borrower"], aes(Date, abs(ev))) +
  geom_line() +
  facet_wrap(~sector) +
  theme_bw() +
  labs(x = NULL, y = "Centrality as borrower")

all_leontief[Date == "2025-01-01"]

tt_load("2020-09-22")
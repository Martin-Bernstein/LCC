setwd(file.path(
  "~", "Dropbox-HarvardUniversity", "Martin Bernstein",
  "Fall 2025", "Other", "LCC"
))
source(file.path("code", "IO-matrices", "NIPA-utility-functions.R"))

nna <- fread(file.path("data", "codes and crosswalks",
                       "NNA_codes.csv"))
cw <- fread(file.path("data", "codes and crosswalks",
                      "NNA_codes_crosswalks.csv"))

#### GO decomp from IO tables ####
# To check that this lines up with KLEMS - below, saw that it does.
# aggs <- fread(file.path("data", "constructed data", "IO-matrices",
#                         "industry_GO=VA+intermediates_fromIOtables.csv"))
# 
# aggs[, industry := factor(industry, levels = nna$NNA_industry)]
# aggs[, total_output := 
#        aggs[aggregate == "Total Industry Output"][.SD, on = 
#                                                     .(year, code_industry),
#                                                   x.value]]
# aggs <- aggs[aggregate != "Total Industry Output"]
# aggs[, share_in_output := value / total_output]

# p <- ggplot(aggs, aes(year, value / total_output, fill = aggregate))+
#   geom_bar(position = "stack", stat = "identity")+
#   facet_wrap(~industry)+
#   theme_bw()+
#   labs(x = NULL, y = "Share in gross output", fill = NULL)+
#   theme(legend.position = "bottom")
# p

#### GO decomp from KLEMS ####

# 1997 - 2023
g <- fread(file.path("data", "NIPA", "GDP-by-industry",
                     "grossoutput_decomp_ind.csv"))
years <- 1997:2023
userows <- 1:873
usecw <- unique(cw[, .(IO_post1997_industry, NNA_code)])
setnames(usecw, old = "IO_post1997_industry", "industry")

g <- decompose_grossoutput(g, years, userows, usecw)

# 1963 - 1996
g_historical <- fread(file.path("data", "NIPA", "GDP-by-industry",
                                "grossoutput_decomp_ind_historical.csv"))
years <- 1963:1996
userows <- 1:882
usecw <- unique(cw[, .(IO_pre1997_industry, NNA_code)])
setnames(usecw, old = "IO_pre1997_industry", "industry")
g_historical <- decompose_grossoutput(g_historical, years, userows, usecw)


# Combine and plot
g <- rbind(g, g_historical)
setorder(g, year, industry, item)
p <- ggplot(g, aes(year, share_in_output, fill = item))+
  geom_bar(position = "stack", stat = "identity")+
  facet_wrap(~industry)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x = NULL, y = "Share in gross output", fill = NULL)
p
ggsave(p, file = file.path("figures", "exploration", "IO-matrices",
                           "GO = wL + GOS + T + M.png"),
       width = 15, height = 15, units = "in")


# Compare KLEMS with IO aggregates:
# Good, everything lines up!
# ggplot(g[item == "Intermediate inputs"], aes(year, share_in_output,
#                                              color = "KLEMS"))+
#   geom_line(data = aggs[aggregate == "Total Intermediate"],
#             mapping = aes(color = "IO Tables"))+
#   geom_line()+
#   facet_wrap(~industry)+
#   theme_bw()+
#   theme(legend.position = "bottom")+
#   labs(x = NULL, y = "Share in total output", color = NULL)
# rm(aggs)

#### Impute pre-1987 ####
# Strategy: just apply 1987 VA shares from 1963 to 1986.
# Then, adjust everyone's labor share in VA up or down
# so that the imputation matches the aggregate labor share

#First, look at how stable these shares are:
va <- g[item == "Intermediate inputs"]
va[, value_added := gross_output * (1 - share_in_output)]
g[, value_added := va[.SD, on = .(nna_code, year), x.value_added]]
g[, share_in_va := share_in_output * gross_output / value_added]

toplot <- g[item != "Intermediate inputs"]

# Force full height visually using coord_cartesian (preserves bar structure)
p <- ggplot(toplot, aes(year, share_in_va, fill = item)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_cartesian(ylim = c(-0.15, 1.3), clip = "on") +  # Don't lose bars entirely
  facet_wrap(~industry) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "Share in value added", fill = NULL)

ggsave(p, file = file.path("figures", "exploration", "IO-matrices",
                           "VA = wL + GOD + T.png"),
       width = 15, height = 15, units = "in")

# not too bad. So, take shares of value added in 1987
shares <- g[year == 1987 & item != "Intermediate inputs"]
g[, va_shares_1987 := shares[.SD, on = .(nna_code, item), x.share_in_va]]
# and use those to imput pre-1987 shares in gross output
g[is.na(share_in_output),
  share_in_output := va_shares_1987 * value_added / gross_output]

p <- ggplot(g, aes(year, share_in_output, fill = item))+
  geom_bar(position = "stack", stat = "identity")+
  facet_wrap(~industry)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x = NULL, y = "Share in gross output", fill = NULL)
ggsave(p, file = file.path("figures", "exploration", "IO-matrices",
                           "GO = wL + GOS + T + M_with_imputation.png"),
       width = 15, height = 15, units = "in")

g[, c("value_added", "share_in_va", "va_shares_1987") := NULL]
g[item != "Intermediate inputs" & year < 1987, imputed_share := TRUE]
g[is.na(imputed_share), imputed_share := FALSE]

write.csv(g, file = file.path("data", "constructed data", "IO-matrices", "industry_output_decomposed_KLEMS.csv"),
          row.names = FALSE)

#### Align imputations with aggregate labor share ####

##### Aggregate labor share of gross output #####
###### GDI (without intermediates) ######
gdi <- fread(file.path('data','NIPA','table 1','GDI_decomp_1-10.csv'))
names(gdi)<-as.character(gdi[1,])
gdi <- gdi[Line %in% 1:24]
cols <- names(gdi)[3:ncol(gdi)]
gdi[,c(cols) := lapply(.SD, as.numeric),.SDcols = cols]
gdi <- pivot_longer(gdi,3:ncol(gdi),names_to='year')%>%
  setDT()
#Only keep these for now:
gdi <- gdi[Line %in% c(2,7,8,10,20,21)]
gdi[is.na(value),value:=0]
gdi[,year:=as.numeric(year)]
gdi[,Line:=as.numeric(Line)]
#Subsidies should be negative (subtracted from total)
gdi[Line==8,value:=-value]
gdi[,gdi := sum(value),by=.(year)]

newnames <- setDT(data.frame(Line = c(2,7,8,10,20,21),
                             Name = c('Compensation of employees',
                                      'Taxes','Subsidies','Net operating surplus (private)',
                                      'Net operating surplus (government)',
                                      'Spending on fixed capital')))
gdi[, Name := newnames[.SD,on=.(Line),x.Name]]
gdi[, c("item", "Line") := NULL]

#Aggregate GDI
p<-ggplot(gdi,aes(year,value/gdi,fill=Name))+
  geom_bar(position='stack',stat='identity')+
  theme_bw()+
  theme(legend.position='bottom')+
  labs(fill=NULL,x=NULL,y='Share of GDI')
p

###### Add intermediates ######
# Use KLEMS: get total intermediate inputs of all industries
gall <- fread(file.path("data", "NIPA", "GDP-by-industry",
                     "grossoutput_decomp_ind.csv"))
g2all <- fread(file.path("data", "NIPA", "GDP-by-industry",
                      "grossoutput_decomp_ind_historical.csv"))

gall <- format_total_intermediates(gall)
g2all <- format_total_intermediates(g2all)

total_intermediates <- rbind(gall, g2all)
setnames(total_intermediates, old = "industry", new = "Name")
total_intermediates[Name == "All industries", Name := "Gross output"]

total_intermediates[, gdi := unique(gdi[, .(year, gdi)])
                    [.SD, on = .(year), x.gdi]]
gdi <- rbind(gdi, total_intermediates[Name == "Intermediate inputs"])
gdi[, gross_output := total_intermediates[Name == "Gross output"]
    [.SD, on = .(year), x.value]]
gdi <- gdi[!is.na(gross_output)]

gdi[, share_in_output := value / gross_output]
gdi[, c("value", "gdi") := NULL]
setnames(gdi, old = "Name", new = "item")
gdi <- gdi[, .(year, item, share_in_output, gross_output)]
setorder(gdi, year, item)

p <- ggplot(gdi[item != "Net operating surplus (government)"],
       aes(year, share_in_output))+
  geom_line()+
  facet_wrap(~item, scales = "free")+
  theme_bw()+
  labs(x = NULL, y = "Share in gross output")
p
ggsave(p, file = file.path("figures", "exploration", "IO-matrices",
                           "aggregate_GO decomposition.png"),
       width = 7, height = 4, units = "in")

write.csv(gdi, file = file.path("data", "constructed data", "IO-matrices",
                                "aggregate_grossoutput_decomposed.csv"),
          row.names = FALSE)


#### Adjust imputed shares based on aggregate shares?

# Conclusion from the below:
# the (imputed) KLEMS shares vs aggregate shares look almost identical.
# So let's leave everything as is (at least for now)

# g <- fread(file.path("data", "constructed data", "IO-matrices",
#                      "industry_output_decomposed_KLEMS.csv"))

gdi
# Aggregating up from KLEMS, including imputed data:
klems_totals <- g[, .(value = sum(share_in_output * gross_output)),
                  by = .(year, item)]
klems_grossoutput <- unique(g[,.(year,
                                 gross_output,
                                 nna_code)])[,.(gross_output =
                                                  sum(gross_output)),
                                             by = .(year)]
klems_totals[, gross_output :=
               klems_grossoutput[.SD, on = .(year), x.gross_output]]

klems_totals[, share_in_output := value / gross_output]

p <- ggplot(klems_totals, aes(year, share_in_output, fill = item))+
  geom_bar(position = "stack", stat = "identity")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(fill = NULL, x = NULL, y = "Share in gross output")+
  ylim(c(-.02, 1.02))
p

#Convert aggregate data to same categories:
gdi[item %in% c("Net operating surplus (government)",
                "Net operating surplus (private)",
                "Spending on fixed capital"),
    item := "Gross operating surplus"]
gdi[item %in% c("Subsidies", "Taxes"),
    item := "Taxes on production and imports less subsidies"]
gdi <- gdi[, .(share_in_output = sum(share_in_output)),
           by = .(year, item, gross_output)]

gdi[, item := factor(item, levels = levels(klems_totals$item))]

p <- ggplot(gdi, aes(year, share_in_output, fill = item))+
  geom_bar(position = "stack", stat = "identity")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(fill = NULL, x = NULL, y = "Share in gross output")+
  ylim(c(-.02, 1.02))
p
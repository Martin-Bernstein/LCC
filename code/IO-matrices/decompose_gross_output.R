setwd(file.path(
  "~", "Dropbox-HarvardUniversity", "Martin Bernstein",
  "Fall 2025", "Other", "LCC"
))
source(file.path("code", "IO-matrices", "NIPA-utility-functions.R"))

#### GO decomp from IO tables ####
aggs <- fread(file.path("data", "constructed data", "IO-matrices",
                        "industry_GO=VA+intermediates.csv"))
nna <- fread(file.path("data", "codes and crosswalks",
                       "NNA_codes.csv"))
cw <- fread(file.path("data", "codes and crosswalks",
                      "NNA_codes_crosswalks.csv"))

aggs[, industry := factor(industry, levels = nna$NNA_industry)]

aggs[, total_output := 
       aggs[aggregate == "Total Industry Output"][.SD, on = 
                                                    .(year, code_industry),
                                                  x.value]]
aggs <- aggs[aggregate != "Total Industry Output"]

aggs[, share_in_output := value / total_output]

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
# Basic plots:
d <- fread(file.path("data", "constructed data",
                     "firm-assets", "qfr_long.csv"))
size_codes <- fread(file.path("data", "QFR", "crosswalks",
                              "asset_size_codes.csv"))
d[, size_description := size_codes[.SD, on = .(year, asset_size_code),
                                   x.description]]

use_industries <- c("20. -39.", "10. -14.",
                    "50. -51.", "53. -59.",
                    "TMFG", "TRET", "TWHS",
                    "TMIN", "TINF", "TPTS")
industry_names <- c("Manufacturing", "Mining",
                    "Wholesale", "Retail",
                    "Manufacturing",
                    "Retail", "Wholesale",
                    "Mining", "Information",
                    "Prof. Services (ex. legal)")
industries <- data.table(industry_code = use_industries,
                         industry_name = industry_names)

dt <- d[industry_code %in% use_industries]
unique(d$industry_code)
dt[, industry_name := industries[.SD, on = .(industry_code), x.industry_name]]
dt <- dt[year >= 1952]
dt <- dt[size_description == "All total asset sizes (universe totals)"]

bsheet_items <- fread(file.path("data", "QFR",
                                "crosswalks", "bsheet_items.csv"))

bsheet_items <- bsheet_items[!is.na(use_baseline) & use_baseline != ""]

dt <- dt[bsheet_item %in% bsheet_items$use_baseline]
dt[, c("side", "item", "maturity") :=
     bsheet_items[.SD, on = .(use_baseline = bsheet_item),
                                       .(x.side, x.names_baseline, x.maturity)]]

dt[, share := value / sum(value), by = .(industry_code, date, side,
                                         asset_size_code)]

setorder(dt, date, side, maturity)
dt[, item := factor(item, levels = unique(dt$item))]

p <- ggplot(dt[side == "asset"], aes(date, share, fill = item)) +
  geom_area(position = "stack") +
  facet_wrap(~industry_name) +
  labs(x = NULL, y = "Share of total assets", fill = "Assets") +
  theme_bw() +
  backg
p
ggsave(p, file = file.path("figures", "exploration", "firm-assets",
                           "QFR_bsheet_assets.png"),
       width = 8, height = 4, units = "in")

# Double check - looks good.
# aggregate <- d[industry_code == "20. -39." & bsheet_item == "TOTASSET" &
#                  size_description == "All total asset sizes (universe totals)"]
# ggplot(dt[side == "asset"], aes(date, value, fill = item)) +
#   geom_bar(position = "stack", stat = "identity") +
#   facet_grid(~industry_code) +
#   geom_line(data = aggregate,
#             mapping = aes(date, value, fill = NULL))

d <- fread(file.path("data", "compustat", "compustat_annual.csv"))

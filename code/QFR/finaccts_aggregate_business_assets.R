# Look at the aggregate nonfinancial business balance sheet over time.
source(file.path("code", "utility-functions.R"))



data <- clean_NA('b103')
corps <- data[[1]]
key <- data[[2]]
# Firm assets we want to track:
assets <- data.table(series = c(key$series[3:6],
                                key$series[8:10],
                                key$series[11:13],
                                key$series[20:21],
                                key$series[22],
                                key$series[23:25],
                                key$series[c(26, 28)],
                                key$series[27]),
                     names = c("real estate", "equipment",
                               "intellectual property",
                               "inventories",
                               rep("deposits", times = 3),
                               rep("safe assets", times = 3),
                               "mortgage loans", "consumer credit",
                               "foreign firm debt",
                               rep("stocks", times = 3),
                               "mutual funds",
                               "miscellaneous",
                               "trade receivables"),
                     type = c(rep("non-financial", times = 4),
                              rep("financial", times = 14),
                              "deferred sales"))
assets[, series := substr(series, 1, (nchar(series)-2))]

corps <- corps[series %in% assets$series]


# One of the largest items is "miscellaneous." Try to unpack.
# data2 <- clean_NA("l103")[[1]]
# miscassets <- data.table(series = c("FL103093005",
#                                     "FL103076005",
#                                     "FL103094705",
#                                     "FL103072005"),
#                          names = c("miscellaneous",
#                                    "insurance receivables",
#                                    "financial subsidiaries",
#                                    "PPP from government"),
#                          type = rep("financial", times = 4))
# data2 <- data2[series %in% miscassets$series]
# 
# # Replace that:
# corps <- corps[series != assets[names == "miscellaneous", series]]
# data2
# 
# data2[, c("asset_name", "asset_type") :=
#         miscassets[.SD, on = .(series), .(x.names, x.type)]]
# Turns out it's still miscellaneous; no way to unpack it further.

# Do the same for noncorps:
data <- clean_NA('b104')
noncorps <- data[[1]]
key <- data[[2]]

# Firm assets we want to track:
noncorpassets <- data.table(series = c(key$series[c(3,6,9,10)],
                                key$series[12:13],
                                key$series[14:15],
                                key$series[19:21],
                                key$series[23],
                                key$series[22]),
                     names = c("real estate", "equipment",
                               "intellectual property",
                               "inventories",
                               rep("deposits", times = 2),
                               rep("safe assets", times = 2),
                               "mortgage loans", "consumer credit",
                               "stocks",
                               "miscellaneous",
                               "trade receivables"),
                     type = c(rep("non-financial", times = 4),
                              rep("financial", times = 8),
                              "deferred sales"))
noncorpassets[, series := substr(series, 1, (nchar(series)-2))]

noncorps <- noncorps[series %in% noncorpassets$series]





corps[, c("asset_name", "asset_type") :=
        assets[.SD, on = .(series), .(x.names, x.type)]]
noncorps[, c("asset_name", "asset_type") :=
        noncorpassets[.SD, on = .(series), .(x.names, x.type)]]

# corps <- rbind(corps, data2)
noncorps[, business_type := "Non-corporate"]
corps[, business_type := "Corporate"]
all <- rbind(corps, noncorps)

all <- all[, .(value = sum(value)), by = .(date, year, gdp,
                                           asset_name, asset_type,
                                           business_type)]
agg <- all[, .(value = sum(value)), by = .(date, year, gdp,
                                           asset_name, asset_type)]

# corps <- corps[, .(value = sum(value)), by = .(date, year, gdp,
#                                                asset_name, asset_type)]
# noncorps <- noncorps[, .(value = sum(value)), by = .(date, year, gdp,
#                                                      asset_name, asset_type)]
# corps[, share := value / sum(value), by = .(date)]
# noncorps[, share := value / sum(value), by = .(date)]


all[, share := value / sum(value), by = .(date, business_type)]
agg[, share := value / sum(value), by = .(date)]


agg[, business_type := "All non-financial businesses"]
all_agg <- rbind(all, agg)

plot_business_bsheet <- function(corps) {
  
  ## 1) Desired vertical order for blocks
  type_order_wanted <- c("non-financial", "deferred sales", "financial")
  type_levels <- intersect(type_order_wanted, unique(corps$asset_type))
  type_levels <- c(type_levels, setdiff(unique(corps$asset_type), type_levels))
  corps[, asset_type := factor(asset_type, levels = type_levels)]
  
  ## 2) Order items within each asset_type (largest avg share first)
  order_within_type <- corps[
    , .(avg_share = mean(share, na.rm = TRUE)), by = .(asset_type, asset_name)
  ][order(factor(asset_type, levels = type_levels), -avg_share)]
  
  lvl_asset <- order_within_type$asset_name
  corps[, asset_name := factor(asset_name, levels = lvl_asset)]
  
  ## 3) Automatic palette per type
  seed_map <- list(
    "non-financial" = c("#E5F5E0", "#A1D99B", "#31A354"),
    "deferred sales" = c("#FEEDDE", "#FD8D3C", "#D94801"),
    "financial"     = c("#DEEBF7", "#9ECAE1", "#08519C")
  )
  fallback_seed <- function(i, n) {
    h <- (i - 1) / n * 360
    c(hcl(h, 35, 95), hcl(h, 50, 70), hcl(h, 60, 45))
  }
  
  counts_by_type <- order_within_type[, .N, by = asset_type]
  types_in_data  <- counts_by_type$asset_type
  
  cols_named <- c()
  for (i in seq_along(types_in_data)) {
    t <- types_in_data[i]
    n_this <- counts_by_type[asset_type == t, N]
    seeds  <- if (!is.null(seed_map[[t]])) seed_map[[t]] else fallback_seed(i, length(types_in_data))
    pal    <- grDevices::colorRampPalette(seeds)(max(1, n_this))
    items  <- order_within_type[asset_type == t, asset_name]
    cols_named <- c(cols_named, setNames(pal, items))
  }
  
  ## 4) Base plot (faceted by business_type)
  p_area <- ggplot(corps, aes(date, share, fill = asset_name)) +
    geom_area(position = "stack") +
    scale_fill_manual(values = cols_named,
                      breaks = levels(corps$asset_name),
                      name = "Assets") +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x = NULL, y = "Share of total assets") +
    theme_bw(base_size = 12) +
    theme(panel.grid.major.x = element_blank(),
          legend.title = element_text(size = 11),
          legend.text  = element_text(size = 10)) +
    facet_wrap(~ business_type)
  
  ## ---------- Labels computed WITHIN business_type ----------
  # Quarters to annotate (first quarter in each year)
  label_years <- c(1980, 2025)
  label_dates <- corps[year %in% label_years,
                       .(date = sort(unique(date))[1L]), by = year][
                         match(label_years, year), date]
  
  # darkest text colors per type
  type_dark <- setNames(vapply(levels(corps$asset_type), \(t) tail(seed_map[[t]], 1), character(1)),
                        levels(corps$asset_type))
  
  # Type-level shares at those dates, BY business_type
  type_shares <- corps[date %in% label_dates,
                       .(share_type = sum(share, na.rm = TRUE)),
                       by = .(business_type, date, asset_type)]
  type_shares[, asset_type := factor(asset_type, levels = type_levels)]
  setorder(type_shares, business_type, date, asset_type)
  
  # Reversed cumsum bottoms to match geom_area stacking (within each facet & date)
  type_shares[, y_bottom := rev(cumsum(rev(share_type))) - share_type,
              by = .(business_type, date)]
  type_shares[, y_mid := y_bottom + share_type / 2]
  
  type_shares[, label := sprintf("%s %s",
                                 tools::toTitleCase(paste0(as.character(asset_type), ":\n")),
                                 scales::percent(share_type, accuracy = 1))]
  
  # Year labels per facet
  year_map <- data.table(date = label_dates, year = c("1980", "2025"))
  year_labels <- merge(
    CJ(business_type = unique(corps$business_type), date = label_dates),
    year_map, by = "date", sort = FALSE
  )
  y_top <- 0.85
  
  # left shift so text stays inside frame
  x_offset <- 0  # adjust as needed (days)
  offset_2025 <- "2021-01-01"
  type_shares[year(date) == 2025, date := as.Date(offset_2025)]
  year_labels[year == 2025, date := as.Date(offset_2025)]
  p_area +
    geom_text(
      data = type_shares,
      aes(x = date + x_offset, y = y_mid, label = label,
          color = asset_type, business_type = business_type),
      fontface = "bold", size = 3.8, show.legend = FALSE, inherit.aes = FALSE
    ) +
    geom_text(
      data = transform(year_labels, y = y_top),
      aes(x = date + x_offset, y = y, label = year, business_type = business_type),
      fontface = "bold", size = 4.2, color = "gray20", inherit.aes = FALSE
    ) +
    scale_color_manual(values = type_dark, guide = "none") +
    coord_cartesian(clip = "off") +
    backg
}


# plot_business_bsheet(all[business_type == "Corporate"])
# plot_business_bsheet(all[business_type == "Non-corporate"])
# plot_business_bsheet(all)
# plot_business_bsheet(agg)
p1 <- plot_business_bsheet(all)
ggsave(p1, file = file.path("figures", "exploration", "firm-assets",
                            "assets_corporate_noncorporate.png"),
       width = 12, height = 6, units = "in")
p2 <- plot_business_bsheet(agg)
ggsave(p2, file = file.path("figures", "exploration", "firm-assets",
                            "assets_allbusiness.png"),
       width = 8, height = 4, units = "in")
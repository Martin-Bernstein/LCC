setwd(file.path(
  "~", "Dropbox-HarvardUniversity", "Martin Bernstein",
  "Fall 2025", "Other", "LCC"
))


library(data.table)
library(plotly)
library(RColorBrewer)

# Load data
io <- fread(file.path("data", "constructed data", "IO-matrices", "IO_matrices_long.csv"))
NNA_codes <- fread(file.path("data", "codes and crosswalks", "NNA_codes_crosswalks.csv"))
NNA_codes <- unique(NNA_codes[, .(NNA_code, NNA_industry)])
io[, industry := factor(industry, levels = NNA_codes$NNA_industry)]
NNA_backwards <- NNA_codes[nrow(NNA_codes):1, ]
io[, commodity := factor(commodity, levels = NNA_backwards$NNA_industry)]

# Tooltip
io[, tooltip := paste0(
  "Industry: ", industry,
  "<br>Commodity: ", commodity,
  "<br>Pct. of output: ", round(share_in_output * 100, 2), "%"
)]

# Color scale in logs
io[share_in_output < 0, share_in_output := 0]
epsilon <- 1e-6
io[, adj_share := share_in_output^(1 / 5)]

# Compute the global min and max share values
adj_min <- min(io$adj_share, na.rm = TRUE)
adj_max <- max(io$adj_share, na.rm = TRUE)

# Custom color ramp from white to lightblue to darkblue
color_func <- colorRamp(c("white", "lightblue", "darkblue"))

p <- plot_ly(
  data = io,
  x = ~industry,
  y = ~commodity,
  z = ~adj_share,
  frame = ~year,
  type = "heatmap",
  colors = color_func,
  zmin = adj_min,
  zmax = adj_max,
  text = ~tooltip,
  hoverinfo = "text",
  showscale = FALSE
) %>%
  layout(
    title = "Input-Output Matrix",
    xaxis = list(title = "Industry", showticklabels = FALSE),
    yaxis = list(title = "Commodity", showticklabels = FALSE),
    margin = list(l = 80, r = 80, t = 80, b = 80)
  )


# Save as HTML
htmlwidgets::saveWidget(
  p,
  file = file.path("figures", "exploration", "IO-matrices", "IO_matrix_dashboard.html"),
  selfcontained = TRUE
)

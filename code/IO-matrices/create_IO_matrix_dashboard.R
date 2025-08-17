setwd(file.path(
  "~", "Dropbox-HarvardUniversity", "Martin Bernstein",
  "Fall 2025", "Other", "LCC"
))

library(data.table)
library(plotly)
library(RColorBrewer)

NNA_codes <- fread(file.path("data", "codes and crosswalks", "NNA_codes_crosswalks.csv"))
NNA_codes <- unique(NNA_codes[, .(NNA_code, NNA_industry)])
NNA_backwards <- NNA_codes[nrow(NNA_codes):1, ]

#### Dashboard for IO Matrices ####
# Load data
io <- fread(file.path("data", "constructed data", "IO-matrices", "IO_matrices_long.csv"))
io[, industry := factor(industry, levels = NNA_codes$NNA_industry)]
io[, commodity := factor(commodity, levels = NNA_backwards$NNA_industry)]

# Tooltip
io[, tooltip := paste0(
  "Industry: ", industry,
  "<br>Commodity: ", commodity,
  "<br>Pct. of output: ", round(share_in_industry_output * 100, 2), "%"
)]

# Color scale in fifth-root (i.e. moderate concave transformation)
io[share_in_industry_output < 0, share_in_industry_output := 0]
io[, adj_share := share_in_industry_output^(1 / 5)]

create_dashboard <- function(io, usetitle = "Input-Output Matrix"){
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
      title = usetitle,
      xaxis = list(title = "Industry", showticklabels = FALSE),
      yaxis = list(title = "Commodity", showticklabels = FALSE),
      margin = list(l = 80, r = 80, t = 80, b = 80)
    )
  
  return(p)
}

p <- create_dashboard(io)

# Save as HTML
htmlwidgets::saveWidget(
  p,
  file = file.path("figures", "exploration", "IO-matrices", "IO_matrix_dashboard.html"),
  selfcontained = TRUE
)


leontief <- fread(file.path("data", "constructed data", "IO-matrices",
                            "leontief_inverse_long.csv"))
leontief[, industry := factor(industry, levels = NNA_codes$NNA_industry)]
leontief[, commodity := factor(commodity, levels = NNA_backwards$NNA_industry)]

# Tooltip
leontief[, tooltip := paste0(
  "Industry: ", industry,
  "<br>Commodity: ", commodity,
  "<br>Coefficient: ", round(value, 3)
)]

#leontief[, adj_share := value]
leontief[, adj_share := value^(1 / 5)]

p2 <- create_dashboard(leontief, usetitle = "Leontief Inverse Matrix")

# Save as HTML
htmlwidgets::saveWidget(
  p2,
  file = file.path("figures", "exploration", "IO-matrices", "Leontief_matrix_dashboard.html"),
  selfcontained = TRUE
)
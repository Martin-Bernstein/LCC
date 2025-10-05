library(plotly)
library(data.table)
library(readxl)
library(htmltools)
library(htmlwidgets)

setwd("/Users/martinbernstein/Library/CloudStorage/GoogleDrive-mbernstein@g.harvard.edu/My Drive/Summer 2025/supply side")
# Source the file with format_IO function
source(file.path('R scripts','NIPA functions.R'))

# Get the bigkey once
bigkey <- get_bigkey()

# Output directory for individual plots
# outdir <- file.path('figures','exploration','disagged IO','dashboard_plots')
outdir <- file.path("~/Dropbox-HarvardUniversity/Martin Bernstein/Fall 2025/Other/LCC",
                    'figures','exploration','IO-matrices','gross-output_dashboard_files')

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Generate and save all plots as HTML files (not self-contained)
years <- 1963:2023
for (year in years) {
  cat("Generating plot for year:", year, "\n")
  if(year <= 1996){
    m <- read_excel(file.path('data','NIPA','IO tables','Historical IO','IOUse_Before_Redefinitions_PRO_1963-1996_Summary.xlsx'),
                    sheet=as.character(year))
    colnames(m) <- m[5,]
  }else{
    m <- read_excel(file.path('data','NIPA','IO tables','AllTablesIO','IOUse_Before_Redefinitions_PRO_1997-2023_Summary.xlsx'),
                    sheet=as.character(year))
    colnames(m) <- m[6,]
  }
  m <- m[7:nrow(m),]
  setDT(m)
  plot <- format_IO(m, year, get_bigkey())
  htmlwidgets::saveWidget(plot, file.path(outdir, paste0('plot_', year, '.html')), selfcontained = FALSE)
}

#Create the more detailed plots
for(year in c(2007,2012,2017)){
  cat("Generating plot for year:", year, "\n")
  m <- read_excel(file.path('data','NIPA','IO tables','AllTablesIO','IOUse_Before_Redefinitions_PRO_2017_Detail.xlsx'),
                  sheet=as.character(year))
  colnames(m) <- m[4,]
  m <- m[6:nrow(m),]
  setDT(m)
  plot <- format_IO(m, year, get_bigkey())
  htmlwidgets::saveWidget(plot, file.path(outdir, paste0('detailplot_', year, '.html')), selfcontained = FALSE)
}

# Years for each dashboard
less_years <- 1963:2023
more_years <- c(2007, 2012, 2017)

# Iframes for less detailed
iframe_divs_less <- lapply(less_years, function(y) {
  tags$iframe(
    id = paste0('plot_', y),
    src = file.path('gross-output_dashboard_files', paste0('plot_', y, '.html')),
    style = if (y == less_years[1]) {
      "width:95%;height:525px;border:none;display:block;"
    } else {
      "width:95%;height:525px;border:none;display:none;"
    }
  )
})

# Iframes for more detailed
iframe_divs_more <- lapply(more_years, function(y) {
  tags$iframe(
    id = paste0('detailplot_', y),
    src = file.path('gross-output_dashboard_files', paste0('detailplot_', y, '.html')),
    style = if (y == more_years[1]) {
      "width:95%;height:525px;border:none;display:block;"
    } else {
      "width:95%;height:525px;border:none;display:none;"
    }
  )
})

# JavaScript for toggling dashboards and sliders
js_code <- sprintf('
  // Years for each dashboard
  const less_years = [%s];
  const more_years = [%s];
  // Elements
  const lessBtn = document.getElementById("lessBtn");
  const moreBtn = document.getElementById("moreBtn");
  const lessDiv = document.getElementById("lessDetailDiv");
  const moreDiv = document.getElementById("moreDetailDiv");
  // Less detailed slider
  const lessSlider = document.getElementById("yearSliderLess");
  const lessDisplay = document.getElementById("yearDisplayLess");
  // More detailed slider
  const moreSlider = document.getElementById("yearSliderMore");
  const moreDisplay = document.getElementById("yearDisplayMore");

  // Toggle dashboard views
  lessBtn.onclick = function() {
    lessDiv.style.display = "block";
    moreDiv.style.display = "none";
    lessBtn.classList.add("active");
    moreBtn.classList.remove("active");
  };
  moreBtn.onclick = function() {
    lessDiv.style.display = "none";
    moreDiv.style.display = "block";
    lessBtn.classList.remove("active");
    moreBtn.classList.add("active");
  };

  // Show correct iframe for less detailed
  function showYearLess(year) {
    less_years.forEach(function(y) {
      document.getElementById("plot_" + y).style.display = (y == year) ? "block" : "none";
    });
    lessDisplay.textContent = year;
  }
  lessSlider.addEventListener("input", function() {
    showYearLess(parseInt(lessSlider.value));
  });
  showYearLess(%d);

  // Show correct iframe for more detailed
  function showYearMore(year) {
    more_years.forEach(function(y) {
      document.getElementById("detailplot_" + y).style.display = (y == year) ? "block" : "none";
    });
    moreDisplay.textContent = year;
  }
  moreSlider.addEventListener("input", function() {
    showYearMore(parseInt(moreSlider.value));
  });
  showYearMore(%d);

  // Set initial view
  lessDiv.style.display = "block";
  moreDiv.style.display = "none";
  lessBtn.classList.add("active");
', 
                   paste(less_years, collapse = ","), 
                   paste(more_years, collapse = ","), 
                   less_years[1], 
                   more_years[1])

# Build dashboard
out_dashboard <- tagList(
  tags$h2("Industry Gross Output Shares"),
  tags$div(
    style = "margin-bottom: 1em; text-align:center;",
    tags$button("Less detailed", id = "lessBtn", style = "margin-right:10px;"),
    tags$button("More detailed", id = "moreBtn")
  ),
  # Less detailed section
  tags$div(
    id = "lessDetailDiv",
    tags$div(
      style = "margin: 20px 0;",
      tags$label("Select Year: "),
      tags$input(
        type = "range",
        id = "yearSliderLess",
        min = min(less_years),
        max = max(less_years),
        value = min(less_years),
        step = 1,
        style = "width: 80%;"
      ),
      tags$span(id = "yearDisplayLess", style = "margin-left: 10px;")
    ),
    iframe_divs_less
  ),
  # More detailed section
  tags$div(
    id = "moreDetailDiv",
    style = "display:none;",
    tags$div(
      style = "margin: 20px 0;",
      tags$label("Select Year: "),
      tags$input(
        type = "range",
        id = "yearSliderMore",
        min = min(more_years),
        max = max(more_years),
        value = min(more_years),
        step = 5,
        style = "width: 80%;"
      ),
      tags$span(id = "yearDisplayMore", style = "margin-left: 10px;")
    ),
    iframe_divs_more
  ),
  tags$script(HTML(js_code)),
  tags$style(HTML('
    body { font-family: sans-serif; margin: 20px; max-width: 1200px; margin: 0 auto; padding: 20px; }
    h2 { margin-bottom: 20px; text-align: center; }
    input[type=\"range\"] { height: 25px; }
    iframe { margin-top: 20px; border: 1px solid #ddd; border-radius: 4px; background: #fff; }
    button.active { background: #007bff; color: #fff; }
    button { padding: 6px 18px; font-size: 16px; border-radius: 4px; border: 1px solid #007bff; background: #fff; color: #007bff; cursor: pointer; }
    button:hover { background: #e6f0ff; }
  '))
)

# Save dashboard HTML
# htmltools::save_html(out_dashboard, file.path('figures','exploration','disagged IO','IO_dashboard.html'))
htmltools::save_html(out_dashboard, file.path("~/Dropbox-HarvardUniversity/Martin Bernstein/Fall 2025/Other/LCC",
                                              'figures','exploration','IO-matrices','IO_grossoutput_dashboard.html'))
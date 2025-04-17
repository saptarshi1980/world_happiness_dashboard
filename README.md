1. Use R version 4.3.3
2. You can use any R Studio version
3. copy-paste this into your R console to install any missing packages automatically:
   # List of required packages
packages <- c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "plotly",
  "DT",
  "corrplot",
  "RColorBrewer"
)

# Install only missing ones
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Install all
invisible(lapply(packages, install_if_missing))

4.   I have used a simple dataset for world happiness report from kaggle (https://www.kaggle.com/datasets/unsdsn/world-happiness?resource=download). There is one zip file containing yearwise csv file. You need to merge the files into one csv file, which will be used by our server.R and ui.R files.
5. You can use    

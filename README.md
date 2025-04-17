1. Use R version 4.3.3
2. You can use any R Studio version
3. Create a new project in R Studio and give the name of the project as "world-happiness-dashboard"
4. Copy all the files from the repository to this projectc folder.
5. There are many dependency libraries rqeuired. copy-paste this into your R console to install any missing packages automatically:
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
5. You can use csv_merge.R file to merge these yearwise csv files.   
6. If the above steps have been executed succesfully then you should be able to get the dashboard by running the command in the R console from project's home directory- shiny::runApp()

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(corrplot)
library(RColorBrewer)

# Load data for use in UI (for choices)
happiness_combined <- read_csv("world_happiness_merged.csv") %>%
  mutate(
    region = as.factor(region),
    country = as.factor(country)
  ) %>%
  mutate(across(c(happiness_score, gdp_per_capita, social_support, 
                  life_expectancy, freedom, generosity, corruption), 
                as.numeric))

choices_x <- choices_y <- choices_dist <- c(
  "Happiness Score" = "happiness_score",
  "GDP per capita" = "gdp_per_capita",
  "Social Support" = "social_support",
  "Life Expectancy" = "life_expectancy",
  "Freedom" = "freedom",
  "Generosity" = "generosity",
  "Corruption" = "corruption"
)

dashboardPage(
  dashboardHeader(title = "World Happiness Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Correlation Analysis", tabName = "correlation", icon = icon("project-diagram")),
      menuItem("Country Rankings", tabName = "rankings", icon = icon("trophy")),
      menuItem("Score Distribution", tabName = "distribution", icon = icon("chart-bar")),
      menuItem("Regional Comparison", tabName = "regional", icon = icon("globe")),
      menuItem("Time Trends", tabName = "trends", icon = icon("line-chart"))
    ),
    selectInput("region", "Select Region:", 
                choices = c("All", unique(na.omit(happiness_combined$region)))),
    sliderInput("year_range", "Select Year Range:",
                min = min(happiness_combined$year, na.rm = TRUE),
                max = max(happiness_combined$year, na.rm = TRUE),
                value = c(max(happiness_combined$year, na.rm = TRUE) - 5, 
                          max(happiness_combined$year, na.rm = TRUE)),
                step = 1),
    selectizeInput("countries", "Select Countries (Trends Tab):",
                   choices = unique(happiness_combined$country),
                   selected = c("Finland", "Denmark", "United States", "India"),
                   multiple = TRUE)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "correlation",
              fluidRow(
                box(title = "Factor Correlations", plotOutput("corrPlot"), width = 12)
              ),
              fluidRow(
                box(selectInput("xvar", "X Variable:", choices = choices_x), width = 6),
                box(selectInput("yvar", "Y Variable:", choices = choices_y, selected = "gdp_per_capita"), width = 6)
              ),
              fluidRow(
                box(title = "Interactive Scatter Plot", plotlyOutput("scatterPlot"), width = 12)
              )
      ),
      tabItem(tabName = "rankings",
              fluidRow(
                box(title = "Top/Bottom Countries by Happiness Score", plotlyOutput("rankingPlot"), width = 12)
              ),
              fluidRow(
                box(selectInput("rank_year", "Select Year:", choices = unique(happiness_combined$year)), width = 6),
                box(numericInput("top_n", "Number of Countries to Show:", value = 15, min = 5, max = 50), width = 6)
              )
      ),
      tabItem(tabName = "distribution",
              fluidRow(
                box(title = "Distribution of Happiness Components", plotlyOutput("distPlot"), width = 12)
              ),
              fluidRow(
                box(selectInput("dist_var", "Select Variable:", choices = choices_dist), width = 6),
                box(checkboxInput("show_density", "Show Density Curve", value = TRUE), width = 6)
              )
      ),
      tabItem(tabName = "regional",
              fluidRow(
                box(title = "Regional Averages Over Time", plotlyOutput("regionalTrendPlot"), width = 12)
              ),
              fluidRow(
                box(title = "Regional Component Comparison", plotlyOutput("regionalBarPlot"), width = 12)
              )
      ),
      tabItem(tabName = "trends",
              fluidRow(
                box(title = "Happiness Score Trends", plotlyOutput("trendPlot"), width = 12)
              ),
              fluidRow(
                box(title = "Component Trends", plotlyOutput("componentTrendPlot"), width = 12)
              )
      )
    )
  )
)

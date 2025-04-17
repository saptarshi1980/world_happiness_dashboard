library(shiny)
library(tidyverse)
library(plotly)
library(corrplot)
library(RColorBrewer)

happiness_combined <- read_csv("world_happiness_merged.csv") %>%
  mutate(
    region = as.factor(region),
    country = as.factor(country)
  ) %>%
  mutate(across(c(happiness_score, gdp_per_capita, social_support, 
                  life_expectancy, freedom, generosity, corruption), 
                as.numeric))

function(input, output, session) {
  
  filtered_data <- reactive({
    data <- happiness_combined %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    if (input$region != "All") {
      data <- data %>% filter(region == input$region)
    }
    
    data
  })
  
  output$corrPlot <- renderPlot({
    corr_data <- filtered_data() %>%
      select(happiness_score, gdp_per_capita, social_support,
             life_expectancy, freedom, generosity, corruption) %>%
      na.omit()
    
    corr_matrix <- cor(corr_data)
    
    corrplot(corr_matrix, method = "color", type = "upper", 
             tl.col = "black", tl.srt = 45,
             addCoef.col = "black", number.cex = 0.7,
             col = colorRampPalette(brewer.pal(8, "RdYlBu"))(25),
             title = "Correlation Between Happiness Factors",
             mar = c(0, 0, 1, 0))
  })
  
  output$scatterPlot <- renderPlotly({
    req(input$xvar, input$yvar)
    
    p <- ggplot(filtered_data(), aes_string(x = input$xvar, y = input$yvar, 
                                            color = "region", text = "country")) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("text", "x", "y")) %>% 
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  output$rankingPlot <- renderPlotly({
    rank_data <- filtered_data() %>%
      filter(year == input$rank_year) %>%
      arrange(desc(happiness_score)) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, country, happiness_score, region)
    
    top_countries <- head(rank_data, input$top_n)
    bottom_countries <- tail(rank_data, input$top_n)
    
    combined <- bind_rows(
      top_countries %>% mutate(Type = "Top"),
      bottom_countries %>% mutate(Type = "Bottom")
    ) %>%
      mutate(country = factor(country, levels = rev(unique(country))))
    
    p <- ggplot(combined, aes(x = country, y = happiness_score, 
                              fill = region, text = paste("Rank:", Rank))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      facet_wrap(~Type, scales = "free_y") +
      labs(title = paste("Happiness Rankings -", input$rank_year),
           x = "", y = "Happiness Score") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("text", "y", "fill"))
  })
  
  output$distPlot <- renderPlotly({
    req(input$dist_var)
    
    p <- ggplot(filtered_data(), aes_string(x = input$dist_var, fill = "region")) +
      geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
      theme_minimal()
    
    if (input$show_density) {
      p <- p + geom_density(aes(y = after_stat(count) * 3), alpha = 0.2, color = "black")
    }
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })
  
  output$regionalTrendPlot <- renderPlotly({
    regional_data <- filtered_data() %>%
      group_by(region, year) %>%
      summarise(Avg_Happiness = mean(happiness_score, na.rm = TRUE), .groups = "drop")
    
    ggplotly(
      ggplot(regional_data, aes(x = year, y = Avg_Happiness, color = region)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(title = "Regional Happiness Trends") +
        theme_minimal()
    )
  })
  
  output$trendPlot <- renderPlotly({
    req(input$countries)
    
    trend_data <- filtered_data() %>%
      filter(country %in% input$countries)
    
    ggplotly(
      ggplot(trend_data, aes(x = year, y = happiness_score, 
                             color = country, group = country)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(title = "Happiness Score Trends") +
        theme_minimal()
    )
  })
  
  output$componentTrendPlot <- renderPlotly({
    req(input$countries)
    
    trend_data <- filtered_data() %>%
      filter(country %in% input$countries) %>%
      select(country, year, gdp_per_capita, social_support, 
             life_expectancy, freedom, generosity, corruption) %>%
      pivot_longer(cols = -c(country, year), names_to = "component", values_to = "value")
    
    ggplotly(
      ggplot(trend_data, aes(x = year, y = value, color = country)) +
        geom_line() +
        facet_wrap(~component, scales = "free_y") +
        labs(title = "Component Trends by Country") +
        theme_minimal()
    )
  })
}

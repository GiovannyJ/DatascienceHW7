#Data Science Homework 7 Fall 2024
library(ggplot2)
library(leaflet)
library(dplyr) # for filter

# Read data from CSV file
data <- read.csv("storms.csv")

# Determine the range of years and split data into 10-year intervals
start_year <- floor(min(data$year) / 10) * 10
end_year <- ceiling(max(data$year) / 10) * 10
half_decade <- seq(start_year, end_year, by = 10)

# Loop over each half decade
for (i in 1:(length(half_decade) - 1)) {
  # Filter data for the current decade
  decade_data <- data %>%
    filter(year >= half_decade[i] & year < half_decade[i + 1])
  
  # Print the time range
  cat("Time Range:", half_decade[i], "-", half_decade[i + 1] - 1, "\n")
  
  # ---- Bar Graphs ----
  
  # Count of storms by year within the time period
  p1 <- ggplot(decade_data, aes(x = year)) +
    geom_bar(stat = "count") +
    labs(title = paste("Storm Count for", half_decade[i], "-", half_decade[i + 1] - 1),
         x = "year", y = "Count") +
    theme_minimal()
  print(p1)
  
  # Count by year and type
  p2 <- ggplot(decade_data, aes(x = year, fill = factor(Type))) +
    geom_bar(stat = "count") +
    scale_fill_discrete(
      name = "Type of Hurricane",
      labels = c("Tropical Storm", "1", "2", "3", "4", "5")
    ) +
    labs(title = paste("Storm Count by year and Type for", half_decade[i], "-", half_decade[i + 1] - 1),
         x = "year", y = "Count") +
    theme_minimal()
  print(p2)
  
  # Display type table for current decade
  data_table <- table(decade_data$Type)
  print(data_table)
  
  # ---- New Category for Regression ----
  # Create new binary category (Type_New) for regression
  decade_data$Type_New <- ifelse(decade_data$type1 == 0, 0, 1)
  cat("Type_New Table:", "\n")
  print(table(decade_data$Type_New))
  
  # ---- Map with Leaflet ----
  # Create a map for the current decade with color coding
  m <- leaflet() %>%
    addTiles() %>%
    addProviderTiles("Esri.OceanBasemap") %>%
    addCircleMarkers(
      lng = decade_data$lon,
      lat = decade_data$lat,
      radius = 2.5,
      color = c("red", "navy")[decade_data$Type_New + 1],
      popup = paste("year:", as.character(decade_data$year))
    ) %>%
    addLegend(
      "topright",
      colors = c("red", "navy"),
      labels = c("Tropical", "Non-Tropical"),
      title = paste("Type of Hurricane ", half_decade[i], "-", half_decade[i + 1] - 1),
      opacity = 1
    )
  print(m)
  
  # ---- Logistic Regression Analysis ----
  log_model <- glm(Type_New ~ lat, data = decade_data, family = "binomial")
  print(summary(log_model))
  
  # Predictions and confidence intervals
  lats <- seq(min(decade_data$lat), max(decade_data$lat), 0.1)
  probs <- predict(log_model, newdata = data.frame(lat = lats), type = "response", se.fit = TRUE)
  pm <- probs$fit
  pu <- probs$fit + probs$se.fit * 1.96 # Upper bound of 95% confidence interval
  pl <- probs$fit - probs$se.fit * 1.96 # Lower bound of 95% confidence interval
  
  # ---- Probability Plot ----
  plot(decade_data$lat, decade_data$Type_New, pch = 16, cex = 1,
       ylab = "Probability", xlab = "Formation Latitude (N)",
       main = paste("Probability of Hurricane Type by Latitude for", half_decade[i], "-", half_decade[i + 1] - 1))
  
  grid()
  
  polygon(c(rev(lats), lats), c(rev(pl), pu), col = "grey90", border = NA)
  lines(lats, pm, lwd = 2)
  lines(lats, pu, lwd = 2, col = "red")
  lines(lats, pl, lwd = 2, col = "red")
  
  abline(h = 0.1, lty = 2)
  abline(h = 0.5, lty = 2)
  abline(h = 0.9, lty = 2)
  
  cat("Logistic regression complete for", half_decade[i], "-", half_decade[i + 1] - 1, "\n\n")
}



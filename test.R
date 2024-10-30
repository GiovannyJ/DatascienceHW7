# Load required libraries
library(ggplot2)
library(leaflet)
library(dplyr)      # For data manipulation
library(htmlwidgets) # For saving leaflet maps

# Read data from CSV file
data <- read.csv("storms.csv")

# Determine the range of years and split data into 5-year intervals
start_year <- floor(min(data$year) / 5) * 5
end_year <- ceiling(max(data$year) / 5) * 5
half_decade <- seq(start_year, end_year, by = 5)

# Loop over each half-decade
for (i in 1:(length(half_decade) - 1)) {
  # Filter data for the current half-decade
  decade_data <- data %>%
    filter(year >= half_decade[i] & year < half_decade[i + 1])
  
  # Create new binary category (Type_New) for regression
  # Adjusting this line to create Type_New for each period
  decade_data$Type_New <- ifelse(decade_data$Type == 0, 0, 1)
  
  # Create a folder for each half-decade period
  folder_name <- paste0("HalfDecade_", half_decade[i], "_", half_decade[i + 1] - 1)
  dir.create(folder_name, showWarnings = FALSE)
  
  # ---- Bar Graphs ----
  
  # Count of storms by year within the time period
  p1 <- ggplot(decade_data, aes(x = year)) +
    geom_bar(stat = "count") +
    labs(title = paste("Storm Count for", half_decade[i], "-", half_decade[i + 1] - 1),
         x = "Year", y = "Count") +
    theme_minimal()
  ggsave(filename = paste0(folder_name, "/Storm_Count_", half_decade[i], "_", half_decade[i + 1] - 1, ".png"), plot = p1)
  
  # Count by year and type
  p2 <- ggplot(decade_data, aes(x = year, fill = factor(Type))) +
    geom_bar(stat = "count") +
    scale_fill_discrete(
      name = "Type of Hurricane",
      labels = c("Tropical Storm", "1", "2", "3", "4", "5")
    ) +
    labs(title = paste("Storm Count by Year and Type for", half_decade[i], "-", half_decade[i + 1] - 1),
         x = "Year", y = "Count") +
    theme_minimal()
  ggsave(filename = paste0(folder_name, "/Storm_Count_Type_", half_decade[i], "_", half_decade[i + 1] - 1, ".png"), plot = p2)
  
  # ---- Map with Leaflet ----
  # Create a map for the current half-decade with color coding
  m <- leaflet() %>%
    addTiles() %>%
    addProviderTiles("Esri.OceanBasemap") %>%
    addCircleMarkers(
      lng = decade_data$lon,
      lat = decade_data$lat,
      radius = 2.5,
      color = c("red", "navy")[decade_data$Type_New + 1],
      popup = paste("Year:", as.character(decade_data$year))
    ) %>%
    addLegend(
      "topright",
      colors = c("red", "navy"),
      labels = c("Tropical", "Non-Tropical"),
      title = paste("Type of Hurricane ", half_decade[i], "-", half_decade[i + 1] - 1),
      opacity = 1
    )
  saveWidget(m, file = paste0(folder_name, "/Map_", half_decade[i], "_", half_decade[i + 1] - 1, ".html"))
  
  # ---- Logistic Regression Analysis ----
  # Perform logistic regression and output summary
  log_model <- glm(Type_New ~ lat, data = decade_data, family = "binomial")
  summary_file <- paste0(folder_name, "/Logistic_Summary_", half_decade[i], "_", half_decade[i + 1] - 1, ".txt")
  sink(summary_file)
  print(summary(log_model))
  sink()
  
  # Predictions and confidence intervals
  lats <- seq(min(decade_data$lat), max(decade_data$lat), 0.1)
  probs <- predict(log_model, newdata = data.frame(lat = lats), type = "response", se.fit = TRUE)
  pm <- probs$fit
  pu <- probs$fit + probs$se.fit * 1.96 # Upper bound of 95% confidence interval
  pl <- probs$fit - probs$se.fit * 1.96 # Lower bound of 95% confidence interval
  
  # ---- Probability Plot ----
  png(filename = paste0(folder_name, "/Probability_Plot_", half_decade[i], "_", half_decade[i + 1] - 1, ".png"))
  plot(decade_data$lat, decade_data$Type_New, pch = 16, cex = 1,
       ylab = "Probability", xlab = "Formation Latitude (N)",
       main = paste("Probability of Hurricane Type by Latitude for", half_decade[i], "-", half_decade[i + 1] - 1))
  grid()
  
  # Add confidence interval polygons
  polygon(c(rev(lats), lats), c(rev(pl), pu), col = "grey90", border = NA)
  lines(lats, pm, lwd = 2)
  lines(lats, pu, lwd = 2, col = "red")
  lines(lats, pl, lwd = 2, col = "red")
  
  # Add reference lines
  abline(h = 0.1, lty = 2)
  abline(h = 0.5, lty = 2)
  abline(h = 0.9, lty = 2)
  
  dev.off()
  
  cat("Logistic regression complete and plots saved for", half_decade[i], "-", half_decade[i + 1] - 1, "\n\n")
}


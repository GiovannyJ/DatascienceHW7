#Data Sceince Homework 7 Fall 2024
# Load required libraries
library(ggplot2)
library(leaflet)

# Read the data
data <- read.csv("storms.csv")

# Add a 'Season' column to categorize months
get_season <- function(month) {
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}

data$Season <- sapply(data$month, get_season)

# Add a new binary column based on hurricane season (1 for hurricane season, 0 otherwise)
data$Type_New <- ifelse(data$Season %in% c("Summer", "Fall"), 1, 0)


# Bar graph for storm count by decade
ggplot(data, aes(x = factor(Season))) +
  geom_bar(stat = "count") +
  xlab("Season") +
  ylab("Count") +
  ggtitle("Storm Counts by Season")


ggplot(data, aes(x = year, fill = factor(Season))) +
  geom_bar(stat = "count") +
  scale_fill_discrete(
    name = "Season of Hurricane",
    labels = c("Winter", "Spring", "Summer", "Fall")
  ) +
  xlab("Season") +
  ylab("Count") +
  ggtitle("Storm Counts by Year and Type")

# Create an interactive map with leaflet, adding markers based on Type_New
options(viewer = NULL)
m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.OceanBasemap")

cols <- c("red", "navy")
m <- m %>%
  addCircleMarkers(lng = data$lon,
                   lat = data$lat,
                   radius = 2.5,
                   color = cols[data$Type_New + 1],
                   popup = paste("Year:", as.character(data$year))
  ) %>%
  addLegend("topright",
            colors = cols,
            labels = c("Non-Hurricane Season", "Hurricane Season"),
            title = "Type of Hurricane",
            opacity = 1
  )

# Display the map
m

# Logistic regression model for Type_New based on latitude and decade
log_model <- glm(Type_New ~ data$lat, data = data, family = "binomial")
summary(log_model)


predict(log_model, newdata = list(lat = c(10, 23.5, 30)), type = "response")

# Set up latitudes for predictions and confidence intervals
lats <- seq(min(data$lat), max(data$lat), 0.1)
probs <- predict(log_model,
                 newdata = data.frame(lat = lats),
                 type = "response",
                 se.fit = TRUE
)

# Extract mean predictions and confidence intervals
pm <- probs$fit
pu <- probs$fit + probs$se.fit * 1.96 # 95% confidence interval upper bound
pl <- probs$fit - probs$se.fit * 1.96 # 95% confidence interval lower bound

# Plot the logistic regression model with confidence intervals
plot(data$lat,
     data$Type_New,
     pch = 16,
     cex = 1,
     ylab = "Probability",
     xlab = "Formation Latitude (N)"
)

grid()

polygon(c(rev(lats), lats), c(rev(pl), pu),
        col = "grey90", border = NA
)

lines(lats, pm, lwd = 2)
lines(lats, pu, lwd = 2, col = "red")
lines(lats, pl, lwd = 2, col = "red")

abline(h = 0.1, lty = 2)
abline(h = 0.5, lty = 2)
abline(h = 0.9, lty = 2)



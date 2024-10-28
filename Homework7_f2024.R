#Data Sceince Homework 7 Fall 2024

data <- read.csv("storms.csv")

ggplot(data, aes(x = year)) +
  geom_bar(stat = "count")

ggplot(data, aes(x = year, fill = factor(Type))) +
  geom_bar(stat = "count") +
  scale_fill_discrete(
    name = "Type of Hurricane",
    labels = c("Tropical Storm", "1", "2","3","4","5")
  )
data_table <- table(data$Type)
data_table


data$Type_New <- ifelse(test = data$type1 == 0, yes = 0, no = 1)
table(data$Type_New)

options(viewer = NULL)
library(leaflet)
m <- leaflet()
m <- addTiles(m)
m <- addProviderTiles(m, "Esri.OceanBasemap")

cols <- c("red", "navy")
m <- addCircleMarkers(m,
                      lng = data$lon,
                      lat = data$lat,
                      radius = 2.5,
                      color = cols[data$Type_New + 1],
                      popup = paste("Year:", as.character(data$Year))
)
m <- addLegend(m,
               "topright",
               colors = cols,
               labels = c("tropical", "non-tropical"),
               title = "Type of Hurricane",
               opacity = 1
)
m

log_model <- glm(Type_New ~ data$lat, data = data, family = "binomial")
summary(log_model)
summary(log_model)$coefficients
exp(coefficients(log_model)[2])
confint.default(log_model)[2, ]
exp(confint.default(log_model)[2, ])
predict(log_model, newdata = list(lat = c(10, 23.5, 30)), type = "response")

lats <- seq(min(data$lat), max(data$lat), 0.1)

probs <- predict(log_model,
                 newdata = data.frame(lat = lats),
                 type = "response",
                 se.fit = TRUE
)

pm <- probs$fit
pu <- probs$fit + probs$se.fit * 1.96 # 95% confidence interval
pl <- probs$fit - probs$se.fit * 1.96 # 95% confidence interval

plot(data$lat,
     data$Type_new,
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



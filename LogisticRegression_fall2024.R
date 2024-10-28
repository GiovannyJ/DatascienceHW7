#A "genetic classification" is a method of categorizing organisms or entities based
#on their evolutionary history and genetic relationships, essentially grouping 
#them together based on their shared ancestry, rather than just their observable traits



install.packages("openxlsx", dependencies = TRUE)
packageVersion("openxlsx")
install.packages('leaflet')

#The goal of the following exercise is to build a model that predicts the group 
#membership of a hurricane, either tropical or non-tropical, based on the latitude of formation.

library(openxlsx)
hurricanes <- read.xlsx("https://userpage.fu-berlin.de/soga/data/raw-data/hurricanes.xlsx")

#First, we inspect the structure of the data set by applying the str function.
#There are 337 observations and 12 variables in the data set. We are primarily 
#interested in the variable Type, which is our response variable, and the 
#variable FirstLat, which corresponds to the latitude of formation

str(hurricanes)

library(ggplot2)
ggplot(hurricanes, aes(x = Year)) +
  geom_bar(stat = "count")

ggplot(hurricanes, aes(x = Year, fill = factor(Type))) +
  geom_bar(stat = "count") +
  scale_fill_discrete(
    name = "Type of Hurricane",
    labels = c("tropical-only", "baroclinic influences", "baroclinic initiation")
  )

#For a numerical representation of the hurricane classes we use the table() function:
hurricanes_table <- table(hurricanes$Type)
hurricanes_table

#In class 0, tropical hurricanes, there are 187 observations, in class 1, 
#baroclinic influences, there are 77 observations and in class 3,
#baroclinic initiation, there are 73 observations. Since we can only deal
#with dichotomous data in logistic regression, we re-code the classes and assign classes 1
#and 3, both being influenced by the outer tropics, the label 1


hurricanes$Type_new <- ifelse(test = hurricanes$Type == 0, yes = 0, no = 1)
table(hurricanes$Type_new)

options(viewer = NULL)
library(leaflet)
m <- leaflet()
m <- addTiles(m)
m <- addProviderTiles(m, "Esri.OceanBasemap")

cols <- c("red", "navy")
m <- addCircleMarkers(m,
                      lng = hurricanes$FirstLon,
                      lat = hurricanes$FirstLat,
                      radius = 2.5,
                      color = cols[hurricanes$Type_new + 1],
                      popup = paste("Year:", as.character(hurricanes$Year))
)
m <- addLegend(m,
               "topright",
               colors = cols,
               labels = c("tropical", "non-tropical"),
               title = "Type of Hurricane",
               opacity = 1
)
m

#We want to build a model that predicts the group membership of a hurricane, either tropical (0)
#or non-tropical (1), based on the latitude of formation of the hurricane. 
#The response variable is the binary variable Type_new and the predictor variable 
#is FirstLat. We build a logit model by applying the glm() function. For the logistic
#regression model we specify family = 'binomial'

log_model <- glm(Type_new ~ FirstLat, data = hurricanes, family = "binomial")
summary(log_model)

#Be aware, that the output of the logistic model is on link-scale (logit). 
#Thus, the numerical output of the model corresponds to the log-odds. For 
#convenience let us write down the logistic regression model with the calculated 
#intercept and coefficient:

summary(log_model)$coefficients

exp(coefficients(log_model)[2])
confint.default(log_model)[2, ]
exp(confint.default(log_model)[2, ])
predict(log_model, newdata = list(FirstLat = c(10, 23.5, 30)), type = "response")

lats <- seq(min(hurricanes$FirstLat), max(hurricanes$FirstLat), 0.1)

probs <- predict(log_model,
                 newdata = data.frame(FirstLat = lats),
                 type = "response",
                 se.fit = TRUE
)

pm <- probs$fit
pu <- probs$fit + probs$se.fit * 1.96 # 95% confidence interval
pl <- probs$fit - probs$se.fit * 1.96 # 95% confidence interval

plot(hurricanes$FirstLat,
     hurricanes$Type_new,
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

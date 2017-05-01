# Libraries
library(ggmap)
library(dplyr)

# Import Data
house.data <- read.csv('kc_house_data.csv', header = TRUE)

# Drop Date From Model 
house.data <- house.data[ ,c(1,3:21)]

# Grab a smaller set of that data
house.data <- house.data[1:5000]

# Make var names available in script
attach(house.data)

##################################
##                              ##
##     1. Data Exploration      ##
##                              ##
##################################

# View the structure of the data
glimpse(house.data)

# View a summary of the house data
summary(house.data)

# Make prices in $100k's
pricesIn100k <- house.data$price / 100000

# Price Distribution
hist(pricesIn100k, 
     data = house.data, 
     main = 'Distribution of Price', 
     xlab = 'Price In $100k', 
     ylab = 'Frequency',
     col = 'blue',
     bins = 10
     )

# Bedrooms Distribution
hist(house.data$bedrooms,
     main = 'Distribution of Bedrooms', 
     xlab = 'Number Of Bedrooms', 
     ylab = 'Frequency',
     col = 'green'
     )

# Distribution Of Condition
hist(house.data$condition,
     main = 'Distribution of Condtion', 
     xlab = 'House Condition', 
     ylab = 'Frequency',
     col = 'yellow'
    )

# Price by sqft
plot(y = price, x = sqft_lot15, main = 'Price By Sqft_lot15', col = 'red', pch = 10 )

# Price by bedrooms
plot(pricesIn100k, 
     bedrooms, 
     data = house.data, 
     main = 'Price By Bedroom', 
     col = 'purple', 
     xlab = 'Price In $100k', 
     ylab = 'Number Of Bedrooms'
     )

##################################
##                              ##
##          2. MLR Model        ##
##                              ##
##################################
# Drop DATE from model, too many facotrs to create dummy variables
# for more complete analysis you would refactor these dates into 
# more meaningful data

# Create baseline model
house.model <- glm(price ~ ., data = house.data)

# Summary of model
summary(house.model)

# Create a Scatter PLot
plot(price ~ yr_built, 
     data = house.data, 
     cex = .2, 
     col = 'red', 
     main = 'Price By Year', 
     xlab = 'Year', 
     ylab = 'Price of house in $100k'
     )

##################################
##                              ##
##           3. Anova           ##
##                              ##
##################################
# I want to see if house prices on
# average vary by quarter centuries

# Grab The Price and year to convert year into decade factor
priceByDecade <- data.frame(Price = house.data$price, Decade = house.data$yr_built)

# Find the Earliest Year Built
min(priceByDecade$Decade)

# Find the Lastest Year Built
max(priceByDecade$Decade)

# Fins the Distribution By Year
hist(priceByDecade$Decade,
      bins = 10,
      main = 'Distribution Of Houses By Year', 
      xlab = 'Decade Built', ylab = 'Count',
      col = c('orange') 
     )

# Create a Break Every 25 Years
for(i in 1:5000){
  if(priceByDecade$Decade[i] < 1925){
    priceByDecade$Decade[i] <- '1900 - 1925'
  }
  else if (priceByDecade$Decade[i] > 1925 && priceByDecade$Decade[i] < 1950){
    priceByDecade$Decade[i] <- '1925 - 1950' 
  }
  else if (priceByDecade$Decade[i] > 1950 && priceByDecade$Decade[i] < 1975){
    priceByDecade$Decade[i] <- '1950 - 1975' 
  }
  else if (priceByDecade$Decade[i] > 1975 && priceByDecade$Decade[i] < 2000){
    priceByDecade$Decade[i] <- '1975 - 2000' 
  }
  else{
    priceByDecade$Decade[i] <- '2000 - current' 
  }
}

# Make Sure Each Year Is 
priceByDecade$Decade <- as.factor(priceByDecade$Decade)

# Now that the Years are grouped into factors of 20year spans, 
# we can run an anova
anova <- aov(Price ~ Decade, data = priceByDecade)

# See Summary Stats On Anova
summary(anova)

# Run Tukey's Test On Anova
TukeyHSD(anova)

# Plot ANOVA Relationship
plot(Price ~ Decade, 
     data = priceByDecade, 
     main = 'ANOVA Price ~ Qtr. Century', 
     xlab = 'Qrt. Century 1925 - Present', 
     ylab = 'Frequency', 
     col = c('orange', 'blue', 'green', 'yellow', 'pink')
     )

##################################
##                              ##
##          4. GGMAPS           ##
##                              ##
##################################

# Find what should be the cutoff point for 'expensive'!
# Cut at 3rd quarter and up
summary(house.data$price)

# Create New Table Of the Most Expensive Houses
mostExpensiveHouses <- house.data[house.data[,2] > 650000,]

# Select Only The Columns You Need
# Price Longitude & Latitude
mostExpensiveHouses <- mostExpensiveHouses[,c(2,17,18)]

# Store Map of King County
map <- get_map("King County, Washington", 
               zoom = 10, 
               maptype = 'hybrid',
               source = 'google',
               color = 'bw'
               )
# Store Map
m <- ggmap(map)

# Plot Map With Data & Add Lables
m + geom_point(data=mostExpensiveHouses, 
               aes(x=mostExpensiveHouses$long, 
               y=mostExpensiveHouses$lat), 
               color="green", 
               size=2, 
               alpha=0.2
               ) + labs(title = 'Most Expensive Houses In King County, Wa', x = '', y = '')



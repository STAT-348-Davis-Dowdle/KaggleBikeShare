##
## Bike Share EDA Code
## 

## libraries
library(vroom)
library(patchwork)

## Read in the data
bike <- vroom("new/stat 348/KaggleBikeShare/train.csv")

# Change factors assigned integers to factors/integers
bike$season <- as.factor(bike$season)
bike$holiday <- as.factor(bike$holiday)
bike$workingday <- as.factor(bike$workingday)
bike$weather <- as.factor(bike$weather)
bike$casual <- as.integer(bike$casual)
bike$registered <- as.integer(bike$registered)
bike$count <- as.integer(bike$count)

# Review variable types
dplyr::glimpse(bike)

# View data completeness
plot1 <- DataExplorer::plot_intro(bike)

# Visualize collinearity
plot2 <- DataExplorer::plot_correlation(bike)

# Display bar charts for categorical variables
DataExplorer::plot_bar(bike)

# Display histograms for continuous variables
DataExplorer::plot_histogram(bike)

# Identify missing data percentage from each variable
plot3 <- DataExplorer::plot_missing(bike)

# Generate scatterplot/heat map hybrid 
GGally::ggpairs(bike)


plot4 <- ggplot(data = bike) +
  geom_point(mapping = aes(x = temp, y = count)) +
  geom_smooth(mapping = aes(x = temp, y = count), se = F) +
  labs(x = "Temperature (Celsius)",
       y = "Number of Rentals")

plot5 <- (plot1 + plot2) / (plot3 + plot4)

ggsave(plot5, filename = "new/stat 348/KaggleBikeShare/panels.png", width = 10, height = 10)


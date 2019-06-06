# Gets the metric functions
source("metric_functions.R")

# Finds the paths of the csv files
file_paths <- list.files("data/prepped", pattern = "*.csv", full.names = TRUE)

# Stores the data in each city csv to a dataframe within a list
city_data = lapply(file_paths, read.csv)

# Defining the city names
cities <- c("Baltimore", "Charleston", "Chicago", "Columbus", "Dayton", "Denver",
            "Kansas City", "Memphis", "Milwaukee", "Oklahoma City", "Pittsburgh",
            "St. Louis", "Syracuse", "Wichita")

# Calculates each metric and stores them to a vector
diss <- sapply(city_data, dissimilarity)
inter <- sapply(city_data, interact)
isol <- sapply(city_data, isolation)
corr <- sapply(city_data, correlation)
propo <- sapply(city_data, proposed)

# Calculating the ranks of the city from most to least segregated
diss_rank <- rank(-diss)
inter_rank <- rank(inter)
corr_rank <- rank(-corr)

# Creating dataframe for the results of the metrics
metrics_data <- data.frame(
  "cities" = cities,
  "dissimilarity" = diss,
  "interaction" = inter,
  "isolation" = isol,
  "correlation" = corr,
  "proposed" = propo,
  "dissimilarity_rank" = diss_rank,
  "interaction_rank" = inter_rank,
  "correlation_rank" = corr_rank
)
propo_data <- data.frame("cities" = cities, "index" = propo)

# Loads the library
library(ggplot2)

# Dissimilarity data in bar graph
diss_chart <- ggplot(metrics_data, aes(x=reorder(cities, -dissimilarity), y=dissimilarity)) + 
  geom_bar(stat="identity", width = 0.5) +
  coord_cartesian(ylim=c(0, 1)) + 
  labs(title="Dissimialrity Index of Cities", x="Cities", y="Dissimilarity Index") +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1, vjust = 1)) +
  geom_text(aes(x=cities, y=dissimilarity + 0.02, label=format(dissimilarity, digits=3)),
            hjust=0.5, size=1.7)

# Interaction data in bar graph
inter_chart <- ggplot(metrics_data, aes(x=reorder(cities, -interaction), y=interaction)) + 
  geom_bar(stat="identity", width = 0.5) +
  coord_cartesian(ylim=c(0, 1)) + 
  labs(title="Interaction Index of Cities", x="Cities", y="Interaction Index") +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1, vjust = 1)) +
  geom_text(aes(x=cities, y=interaction + 0.02, label=format(interaction, digits=3)),
            hjust=0.5, size=1.7)

# Correlation data in bar graph
corr_chart <- ggplot(metrics_data, aes(x=reorder(cities, -correlation), y=correlation)) + 
  geom_bar(stat="identity", width = 0.5) +
  coord_cartesian(ylim=c(0, 1)) + 
  labs(title="Correlation Index of Cities", x="Cities", y="Correlation Index") +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1, vjust = 1)) +
  geom_text(aes(x=cities, y=correlation + 0.02, label=format(correlation, digits=3)),
            hjust=0.5, size=1.7)

# New metric comparisons
dfm <- melt(metrics_data[,c("cities","dissimilarity", "interaction","correlation","proposed")],id.vars = 1)
prop_chart1 <- ggplot(dfm, aes(x=cities, y=value)) + 
  geom_point(aes(color = variable), stat="identity") +
  coord_cartesian(ylim=c(0, 1)) +
  labs(title="Comparison of Proposed Metric Index for Cities", x="Cities", y="Index", color="Metric") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1))

# New metric comparisons to isolation
dfm2 <- melt(metrics_data[,c("cities", "isolation", "proposed")],id.vars = 1)
prop_chart2 <- ggplot(dfm2, aes(x=cities, y=value)) + 
  geom_point(aes(color = variable), stat="identity") +
  coord_cartesian(ylim=c(0, 1)) +
  labs(title="Comparison of Proposed to Isolation Metric for Cities", x="Cities", y="Index", color="Metric") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1))
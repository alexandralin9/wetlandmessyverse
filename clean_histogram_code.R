library(ggplot2)
library(dplyr)

impact_sites <- read.csv('impactsites.csv')

wetland_data <- impact_sites %>%
  filter(grepl('Wetland', Credit.TypeList, ignore.case = TRUE)) %>%
  select(Distance_km) %>%
  na.omit()

remove_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm=TRUE)
  H <- 1.5 * IQR(x, na.rm=TRUE)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  return(x)
}

wetland_data$Distance_km <- remove_outliers(wetland_data$Distance_km)
wetland_data <- na.omit(wetland_data)


ggplot(wetland_data, aes(x=Distance_km)) +
  geom_histogram(bins=50, fill='skyblue', color='black') +
  theme_minimal() +
  labs(title='Distribution of Distances for Wetland Sites',
       x='Distance (km)',
       y='Count') +
  theme(plot.title = element_text(hjust = 0.5))


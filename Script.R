## Reading the raw data file
raw_data = read.csv("cleaned_road_noise.csv")
##Cleaning NA's
data_filtered = raw_data[raw_data$X55.59 != -2,]
data_filtered_2 = data_filtered[data_filtered$X55.59 != -1,]
##Cleaning empty columns
data_filtered_2 = data_filtered_2[,-10]
##Grouping the data by country and summing the population column and the numbers of people exposed to noise coulumns
grouped_data = data_filtered_2 %>% group_by(Country) %>% summarise(Population_Sum = sum(Population), "55-59" = sum(X55.59), "60-64" = sum(X60.64), "65-69" = sum(X65.69), "70-74" = sum(X70.74), "75+" = sum(X.75))
##Cleaning the data:

##Reading the raw data file
raw_data = read.csv("cleaned_road_noise.csv")
##Cleaning NA's
data_filtered = raw_data[raw_data$X55.59 != -2,]
data_filtered_2 = data_filtered[data_filtered$X55.59 != -1,]
##Cleaning empty columns
data_filtered_2 = data_filtered_2[,-10]
##Grouping the data by country and summing the population column and the numbers of people exposed to noise columns
grouped_data = data_filtered_2 %>% group_by(Country) %>% summarise(Population_Sum = sum(Population), Land = sum(Area) , "55-59" = sum(X55.59), "60-64" = sum(X60.64), "65-69" = sum(X65.69), "70-74" = sum(X70.74), "75+" = sum(X.75))
##Removing NA's
no_NAs = na.omit(grouped_data)
##Saving the new data as a CSV
write.csv(no_NAs, "cleaned_data.csv")
cleaned_data = read.csv("cleaned_data.csv")
##Changing Columns names 
names(cleaned_data) = c("Country_Number" , "Country" , "Population", "Area" , "55-59", "60-64", "65-69", "70-74", "75+" )

##calculating the % of people exposed to each noise level
cleaned_data$"55-59 %" =  (cleaned_data$`55-59`) / (cleaned_data$Population) * 100
cleaned_data$"60-64 %" =  (cleaned_data$`60-64`) / (cleaned_data$Population) * 100 
cleaned_data$"65-69 %" =  (cleaned_data$`65-69`) / (cleaned_data$Population) * 100
cleaned_data$"70-74 %" =  (cleaned_data$`70-74`) / (cleaned_data$Population) * 100
cleaned_data$"75+ %" =  (cleaned_data$`75+`) / (cleaned_data$Population) * 100 

#Merging a world population table to the existing table and tidying it up
world_pop = read.csv("2017_world_pop.csv")
names(world_pop) = c("Country", "Population")
combined_table = merge(world_pop, cleaned_data, by = "Country")
names(combined_table) = c("Country" , "Total Population","Country Number", "Measured Population", "Area" , "55-59", "60-64", "65-69", "70-74", "75+", "55-59 %", "60-64 %", "65-69 %", "70-74 %","75+ %"  )
combined_table = combined_table[,-3]
combined_table$"Exposed to any noise %" = ((combined_table$`55-59`) + (combined_table$`60-64`) + (combined_table$`65-69`) + (combined_table$`70-74`) + (combined_table$`75+`)) / (combined_table$`Measured Population`) * 100
combined_table$"Exposed to any noise % of total population" = ((combined_table$`55-59`) + (combined_table$`60-64`) + (combined_table$`65-69`) + (combined_table$`70-74`) + (combined_table$`75+`)) / (combined_table$`Total Population`) * 100
combined_table = combined_table[,-15]
write.csv(combined_table, "combined_cleaned_table.csv")

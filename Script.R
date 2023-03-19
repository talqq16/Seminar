setwd("C:/Users/User/Desktop/Geo-informatics Seminar/Seminar/Data")
library(dplyr)
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
##Merging a world population table to the existing table and tidying it up
world_pop = read.csv("2017_world_pop.csv")
names(world_pop) = c("Country", "Population")
combined_table = merge(world_pop, cleaned_data, by = "Country")
names(combined_table) = c("Country" , "Total Population", "Measured Population", "Area" , "55-59", "60-64", "65-69", "70-74", "75+", "55-59 %", "60-64 %", "65-69 %", "70-74 %","75+ %"  )
combined_table = combined_table[,-3]
combined_table$"Exposed to any noise %" = ((combined_table$`55-59`) + (combined_table$`60-64`) + (combined_table$`65-69`) + (combined_table$`70-74`) + (combined_table$`75+`)) / (combined_table$`Measured Population`) * 100
combined_table$"Exposed to any noise % of total population" = ((combined_table$`55-59`) + (combined_table$`60-64`) + (combined_table$`65-69`) + (combined_table$`70-74`) + (combined_table$`75+`)) / (combined_table$`Total Population`) * 100
combined_table = combined_table[,-15]
write.csv(combined_table, "combined_cleaned_table.csv")
##Calculating the percentage of the population that was measured
combined_table$"% measured popultaion" = (combined_table$`Measured Population`) / (combined_table$`Total Population`) * 100
##Joining a coordinates table for displaying a map 
coridinates = read.csv("coordinates_cities.csv")
coridinates$City_Name = coridinates$ASCII.Name
raw_data$City_Name = raw_data$Agglomeration.Name..in.English.
joined_cor = merge(coridinates, raw_data, by = "City_Name")
rem_dup <- joined_cor[grepl("Europe", joined_cor$Timezone), ]
write.csv(rem_dup ,"only_Checked_Cities.csv")
#Reading all the mental health disorders tables
Depression = read.csv("Depression.csv")
Anxiety = read.csv("Anxiety.csv")
Eating_Disorder = read.csv("Eating Disorder.csv")
Bipolar = read.csv("Bipolar.csv")
Schizophrenia = read.csv("Schizophrenia.csv")
#Removing all the data that isn't from 2017
Depression_2017 = subset(Depression, Year == 2017)
Anxiety_2017 = subset(Anxiety, Year == 2017)
Eating_Disorder_2017 = subset(Eating_Disorder, Year == 2017)
Bipolar_2017 = subset(Bipolar, Year == 2017)
Schizophrenia_2017 = subset(Schizophrenia, Year == 2017)
#Combining all the tables into one
merged_mental_health = merge(Depression_2017, Anxiety_2017, by = "Entity")
merged_mental_health = merge(merged_mental_health, Eating_Disorder_2017, by = "Entity")
merged_mental_health = merge(merged_mental_health, Bipolar_2017, by = "Entity")
merged_mental_health = merge(merged_mental_health, Schizophrenia_2017, by = "Entity")
#Deleting unnecessary columns 
merged_mental_health$Code.x = NULL
merged_mental_health$Year.y = NULL
merged_mental_health$Code.y = NULL
merged_mental_health$Code.x = NULL
merged_mental_health$Code.y = NULL
merged_mental_health$Code = NULL
merged_mental_health$Year.x = NULL
merged_mental_health$Year.x = NULL
merged_mental_health$Year.y = NULL
merged_mental_health$Year = NULL
#Renaming the columns
names(merged_mental_health) = c('Country' , 'Depression %', 'Anxiety %', 'Eating Disorder %', 'Bipolar Disorder %', 'Schizophrenia %')
#Merging the noise pollution and the mental health data
noise_and_mental_health = merge(merged_mental_health,combined_table, by = "Country")
write.csv(noise_and_mental_health, "Noise and Mental Health.csv")

mean_school = read.csv("Mean Years of Schooling.csv")
mean_school$Continent = NULL
mean_school$ISO_Code = NULL
mean_school$Level= NULL
mean_school$GDLCODE = NULL
mean_school$Region = NULL
names(mean_school) = c('Country', 'Avreage Years in School')
noise_mental_education = merge(noise_and_mental_health, mean_school, by = "Country")

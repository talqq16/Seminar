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
world_pop$Country.Code = NULL
world_pop$Indicator.Name = NULL
world_pop$Indicator.Code = NULL
names(world_pop) = c("Country", "Population")
combined_table = merge(world_pop, cleaned_data, by = "Country")
combined_table$Country_Number = NULL
names(combined_table) = c("Country" , "Total Population", "Measured Population", "Area" , "55-59", "60-64", "65-69", "70-74", "75+", "55-59 %", "60-64 %", "65-69 %", "70-74 %","75+ %"  )
combined_table$"Exposed to any noise %" = ((combined_table$`55-59`) + (combined_table$`60-64`) + (combined_table$`65-69`) + (combined_table$`70-74`) + (combined_table$`75+`)) / (combined_table$`Measured Population`) * 100
combined_table$"Exposed to any noise % of total population" = ((combined_table$`55-59`) + (combined_table$`60-64`) + (combined_table$`65-69`) + (combined_table$`70-74`) + (combined_table$`75+`)) / (combined_table$`Total Population`) * 100
write.csv(combined_table, "combined_cleaned_table.csv")
##Calculating the percentage of the population that was measured
combined_table$"% measured popultaion" = (combined_table$`Measured Population`) / (combined_table$`Total Population`) * 100
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
write.csv(merged_mental_health, "merged_mental_health.csv")
merged_mental_health = read.csv("merged_mental_health.csv")
#Renaming the columns
merged_mental_health$X = NULL
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

Gini = read.csv("Gini.csv")
names(Gini) = c("Country","Gini Index")
noise_mental_education_gini = merge(noise_mental_education, Gini , by = "Country")

work = read.csv("work_hours.csv")
work_subset = subset(work , Year == 2017)
work_subset$Code = NULL
work_subset$Year = NULL
names(work_subset) = c("Country", "Annual Working Hours")
noise_mental_education_gini_Work = merge(noise_mental_education_gini, work_subset, by = "Country" )

write.csv(noise_mental_education_gini_Work, "final_table.csv")

distribution_noise = shapiro.test(noise_mental_education_gini_Work$`Exposed to any noise % of total population`)
distribution_depression = shapiro.test(noise_mental_education_gini_Work$`Depression %`)
distribution_anxiety = shapiro.test(noise_mental_education_gini_Work$`Anxiety %`)
distribution_eating = shapiro.test(noise_mental_education_gini_Work$`Eating Disorder %`)
distribution_bipolar = shapiro.test(noise_mental_education_gini_Work$`Bipolar Disorder %`)
distribution_schizophrenia = shapiro.test(noise_mental_education_gini_Work$`Schizophrenia %`)
distribution_gini = shapiro.test(noise_mental_education_gini_Work$`Gini Index`)
distribution_MYS = shapiro.test(noise_mental_education_gini_Work$`Avreage Years in School`)
distribution_work = shapiro.test(noise_mental_education_gini_Work$`Annual Working Hours`)

correlation_depression = cor(noise_mental_education_gini_Work$`Exposed to any noise % of total population`, noise_mental_education_gini_Work$`Depression %`, method = "kendall")
correlation_anxiety = cor(noise_mental_education_gini_Work$`Exposed to any noise % of total population`, noise_mental_education_gini_Work$`Anxiety %`, method = "kendall")
correlation_bipolar = correlation_coefficient = cor(noise_mental_education_gini_Work$`Exposed to any noise % of total population`, noise_mental_education_gini_Work$`Bipolar Disorder %`, method = "kendall")
correlation_eating_disorder = correlation_coefficient = cor(noise_mental_education_gini_Work$`Exposed to any noise % of total population`, noise_mental_education_gini_Work$`Eating Disorder %`, method = "kendall")
correlation_schizophernia = correlation_coefficient = cor(noise_mental_education_gini_Work$`Exposed to any noise % of total population`, noise_mental_education_gini_Work$`Schizophrenia %`, method = "kendall")

par(mfrow = c(1, 2)) 
hist(noise_mental_education_gini_Work$`Exposed to any noise % of total population`, main = "Noise Pollution", xlab = "% of the population exposed to noise pollution", col = "skyblue")
hist(noise_mental_education_gini_Work$`Depression %`, main = "Depression", xlab = "% of the population suffering from depression", col = "pink")
hist(noise_mental_education_gini_Work$`Anxiety %`, main = "Anxiety", xlab = "% of the population suffering from anxiety", col = "purple")
hist(noise_mental_education_gini_Work$`Eating Disorder %`, main = "Eating Disorder %", xlab = "% of the population suffering from eating disorder", col = "red")
hist(noise_mental_education_gini_Work$`Bipolar Disorder %`, main = "Bipolar Disorder %", xlab = "% of the population suffering from Bipolar disorder", col = "green")
hist(noise_mental_education_gini_Work$`Schizophrenia %`, main = "Schizophrenia %", xlab = "% of the population suffering from Schizophrenia ", col = "orange")
hist(noise_mental_education_gini_Work$`Gini Index`, main = "GINI Index", xlab = "Gini Index", col = "blue")
hist(noise_mental_education_gini_Work$`Avreage Years in School`, main = "Avreage Years in School", xlab = "The average number of completed years of education of a population", col = "pink")
hist(noise_mental_education_gini_Work$`Annual Working Hours`, main = "Annual Working Hours", xlab = "The total number of hours an individual works in a year.", col = "brown")

noise_plot <- noise_mental_education_gini_Work$`Exposed to any noise % of total population`
depression_plot <- noise_mental_education_gini_Work$`Depression %`
schizophrenia_plot <- noise_mental_education_gini_Work$`Schizophrenia %`
Anxiety_plot = noise_mental_education_gini_Work$`Anxiety %`
bipolar_plot = noise_mental_education_gini_Work$`Bipolar Disorder %`
eating_plot = noise_mental_education_gini_Work$`Eating Disorder %`

install.packages("ggplot2")
library(ggplot2)

data <- data.frame(
  x = noise_mental_education_gini_Work$`Exposed to any noise % of total population`,
  y1 = noise_mental_education_gini_Work$`Depression %`,
  y2 = noise_mental_education_gini_Work$`Schizophrenia %`,
  y3 = noise_mental_education_gini_Work$`Anxiety %`,
  y4 = noise_mental_education_gini_Work$`Bipolar Disorder %`,
  y5 = noise_mental_education_gini_Work$`Eating Disorder %`
)

library(ggplot2)

# Create the scatter plot
scatter_plot <- ggplot(noise_mental_education_gini_Work, aes(x = `Exposed to any noise % of total population`)) +
  geom_point(aes(y = `Depression %`, color = "Depression"), size = 2, alpha = 0.6) +
  geom_point(aes(y = `Schizophrenia %`, color = "Schizophrenia"), size = 2, alpha = 0.6) +
  geom_point(aes(y = `Anxiety %`, color = "Anxiety"), size = 2, alpha = 0.6) +
  geom_point(aes(y = `Bipolar Disorder %`, color = "Bipolar Disorder"), size = 2, alpha = 0.6) +
  geom_point(aes(y = `Eating Disorder %`, color = "Eating Disorder"), size = 2, alpha = 0.6) +
  labs(x = "Exposed to any noise % of total population", y = "% of the population suffering from menatl disorder") +
  ggtitle("Mental Disorders vs. Noise Exposure") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "green", "black", "brown"),
                     labels = c("Depression", "Schizophrenia", "Anxiety", "Bipolar Disorder", "Eating Disorder"))

# Display the scatter plot
scatter_plot


install.packages("lme4")
library(lme4)
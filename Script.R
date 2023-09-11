setwd("C:/Users/User/Desktop/Geo-informatics Seminar/Seminar/Data")
library(dplyr)
library(knitr)
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

library(ggplot2)

par(mfrow = c(1, 2)) 
hist_noise <- ggplot(data = noise_mental_education_gini_Work, aes(x = `Exposed to any noise % of total population`)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Noise Pollution", x = "% of the population exposed to noise pollution")

hist_depression <- ggplot(data = noise_mental_education_gini_Work, aes(x = `Depression %`)) +
  geom_histogram(fill = "pink", color = "black") +
  labs(title = "Depression", x = "% of the population suffering from depression")

hist_anxiety <- ggplot(data = noise_mental_education_gini_Work, aes(x = `Anxiety %`)) +
  geom_histogram(fill = "purple", color = "black") +
  labs(title = "Anxiety", x = "% of the population suffering from anxiety")

hist_eating_disorder <- ggplot(data = noise_mental_education_gini_Work, aes(x = `Eating Disorder %`)) +
  geom_histogram(fill = "red", color = "black") +
  labs(title = "Eating Disorder %", x = "% of the population suffering from eating disorder")

hist_bipolar <- ggplot(data = noise_mental_education_gini_Work, aes(x = `Bipolar Disorder %`)) +
  geom_histogram(fill = "green", color = "black") +
  labs(title = "Bipolar Disorder %", x = "% of the population suffering from Bipolar disorder")

hist_schizophrenia <- ggplot(data = noise_mental_education_gini_Work, aes(x = `Schizophrenia %`)) +
  geom_histogram(fill = "orange", color = "black") +
  labs(title = "Schizophrenia %", x = "% of the population suffering from Schizophrenia")

hist_gini <- ggplot(data = noise_mental_education_gini_Work, aes(x = `Gini Index`)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "GINI Index", x = "Gini Index")

hist_avg_school_years <- ggplot(data = noise_mental_education_gini_Work, aes(x = `Avreage Years in School`)) +
  geom_histogram(fill = "pink", color = "black") +
  labs(title = "Average Years in School", x = "The average number of completed years of education of a population")

hist_annual_working_hours <- ggplot(data = noise_mental_education_gini_Work, aes(x = `Annual Working Hours`)) +
  geom_histogram(fill = "brown", color = "black") +
  labs(title = "Annual Working Hours", x = "The total number of hours an individual works in a year")

noise_plot <- noise_mental_education_gini_Work$`Exposed to any noise % of total population`
depression_plot <- noise_mental_education_gini_Work$`Depression %`
schizophrenia_plot <- noise_mental_education_gini_Work$`Schizophrenia %`
Anxiety_plot = noise_mental_education_gini_Work$`Anxiety %`
bipolar_plot = noise_mental_education_gini_Work$`Bipolar Disorder %`
eating_plot = noise_mental_education_gini_Work$`Eating Disorder %`


data <- data.frame(
  x = noise_mental_education_gini_Work$`Exposed to any noise % of total population`,
  y1 = noise_mental_education_gini_Work$`Depression %`,
  y2 = noise_mental_education_gini_Work$`Schizophrenia %`,
  y3 = noise_mental_education_gini_Work$`Anxiety %`,
  y4 = noise_mental_education_gini_Work$`Bipolar Disorder %`,
  y5 = noise_mental_education_gini_Work$`Eating Disorder %`
)

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

scatter_plot_with_trendlines <- scatter_plot +
  geom_smooth(aes(y = `Depression %`), method = "lm", color = "blue", se = FALSE) +
  geom_smooth(aes(y = `Schizophrenia %`), method = "lm", color = "red", se = FALSE) +
  geom_smooth(aes(y = `Anxiety %`), method = "lm", color = "green", se = FALSE) +
  geom_smooth(aes(y = `Bipolar Disorder %`), method = "lm", color = "black", se = FALSE) +
  geom_smooth(aes(y = `Eating Disorder %`), method = "lm", color = "brown", se = FALSE)

df <- data.frame(P_Value = c(distribution_noise$p.value, distribution_depression$p.value, distribution_anxiety$p.value, distribution_bipolar$p.value, distribution_eating$p.value,distribution_schizophrenia$p.value, distribution_gini$p.value, distribution_MYS$p.value, distribution_work$p.value))
data_names = c("Noise Pollution", "Depression %", "Anxiety %", "Bipolar", "Eating Disorder", "Schizophrenia", "GINI Index", "Years In School", "Annual Working Hours")
p_values <- cbind(Variable = data_names, df)
formatted_p_values <- format(p_values, scientific = FALSE)

correlation_df = stack(list("Depression" = correlation_depression, "Anxiety " = correlation_anxiety, "Eating Disorder" = correlation_eating_disorder, "Bipolar" = correlation_bipolar, "Schizophrenia" = correlation_schizophernia))
correlation_df <- correlation_df[, c("ind", "values")]
colnames(correlation_df) = c("Mental Health Disorder", "Correlation Coefficient Value")

correlation_education = cor(noise_mental_education_gini_Work$`Exposed to any noise % of total population`, noise_mental_education_gini_Work$`Avreage Years in School`, method = "kendall")

scatter_plot_with_trendlines


linear_model = lm(cbind(`Depression %`,`Anxiety %` )  ~ `Exposed to any noise % of total population` + `Gini Index` + `Avreage Years in School` + `Annual Working Hours`   ,data = noise_mental_education_gini_Work)
summary(linear_model)

linear_model_2 <- glm(`Bipolar Disorder %` ~ `Exposed to any noise % of total population` + `Gini Index` + `Avreage Years in School` + `Annual Working Hours` , data = noise_mental_education_gini_Work, family = poisson(link = "log"))
summary(linear_model_2)

linear_model_3 <- glm(`Eating Disorder %` ~ `Exposed to any noise % of total population` + `Gini Index` + `Avreage Years in School` + `Annual Working Hours` , data = noise_mental_education_gini_Work, family = poisson(link = "log"))
summary(linear_model_3)

linear_model_4 <- glm(`Schizophrenia %` ~ `Exposed to any noise % of total population` + `Gini Index` + `Avreage Years in School` + `Annual Working Hours` , data = noise_mental_education_gini_Work, family = poisson(link = "log"))
summary(linear_model_4)



summary_model_1 = summary(linear_model)
summary_model_2 = summary(linear_model_2)
summary_model_3 = summary(linear_model_3)
summary_model_4 = summary(linear_model_4)

dep_coe = summary_model_1$`Response Depression %`$coefficients
anx_coe = summary_model_1$`Response Anxiety %`$coefficients
dep_coe_df = as.data.frame(dep_coe)
anx_coe_df = as.data.frame(anx_coe)
dep_coe_df$Coefficient = dep_coe_df$Estimate
anx_coe_df$Coefficient = anx_coe_df$Estimate
anx_coe_df$Estimate = NULL
dep_coe_df$Estimate = NULL
anx_coe_df$`t value` = NULL
dep_coe_df$`t value` = NULL
anx_coe_df$`Std. Error` = NULL
dep_coe_df$`Std. Error` = NULL
dep_coe_df$p_value = dep_coe_df$`Pr(>|t|)`
anx_coe_df$p_value = anx_coe_df$`Pr(>|t|)`
dep_coe_df$`Pr(>|t|)` = NULL
anx_coe_df$`Pr(>|t|)` = NULL

bipolar_coe = summary_model_2$coefficients
bipolar_coe_df = as.data.frame(bipolar_coe)
bipolar_coe_df$Coefficient = bipolar_coe_df$Estimate
bipolar_coe_df$p_value = bipolar_coe_df$`Pr(>|z|)`
bipolar_coe_df$Estimate = NULL
bipolar_coe_df$`t value` = NULL
bipolar_coe_df$`Std. Error` = NULL
bipolar_coe_df$`Pr(>|z|)` = NULL
bipolar_coe_df$`z value` = NULL

eating_coe = summary_model_3$coefficients
eating_coe_df = as.data.frame(eating_coe)
eating_coe_df$Coefficient = eating_coe_df$Estimate
eating_coe_df$p_value = eating_coe_df$`Pr(>|z|)`
eating_coe_df$Estimate = NULL
eating_coe_df$`t value` = NULL
eating_coe_df$`Std. Error` = NULL
eating_coe_df$`Pr(>|z|)` = NULL
eating_coe_df$`z value` = NULL

schiz_coe = summary_model_4$coefficients
schiz_coe_df = as.data.frame(schiz_coe)
schiz_coe_df$Coefficient = schiz_coe_df$Estimate
schiz_coe_df$p_value = schiz_coe_df$`Pr(>|z|)`
schiz_coe_df$Estimate = NULL
schiz_coe_df$`t value` = NULL
schiz_coe_df$`Std. Error` = NULL
schiz_coe_df$`Pr(>|z|)` = NULL
schiz_coe_df$`z value` = NULL


summary(linear_model)

plot(linear_model)

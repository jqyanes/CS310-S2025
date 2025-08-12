#Load packages
library(ggplot2)
library(dplyr)
library(BSDA)
library(dichromat)

#Setting directory: WILL HAVE TO BE CHANGED FOR OTHER USERS
setwd("/Users/yanes/Downloads") 
crop_data = read.csv("yield_df.csv")
head(crop_data)
table(crop_data$Area)
#only looking at data from countries with > 100 entries
  #provides a baseline for the amount of representation in the dataset
crop_data = crop_data %>%
  dplyr::group_by(Area) %>%
  dplyr::filter(n() > 100)

#countries of interest for hypothesis test
uganda_data = subset(crop_data, Area == "Uganda")
malawi_data = subset(crop_data, Area == "Malawi")
#comparing countries
mean(uganda_data$hg.ha_yield)
mean(uganda_data$avg_temp)
mean(uganda_data$average_rain_fall_mm_per_year)
mean(malawi_data$hg.ha_yield)
mean(malawi_data$avg_temp)
mean(malawi_data$average_rain_fall_mm_per_year)
#Hypothesis test (z-test)
u_sd = sd(uganda_data$hg.ha_yield)
m_sd = sd(malawi_data$hg.ha_yield)
z_test = z.test(uganda_data$hg.ha_yield,malawi_data$hg.ha_yield,alternative = "less",mu=0,sigma.x=u_sd,sigma.y=m_sd)
print(z_test)


table(crop_data$Item) #Potato = most common dataset item (crop)
#Looking at how average temp and average rainfall affect potato yield
potato_data = subset(crop_data, Item == "Potatoes")
 #average rainfall
rain_colfunc = colorRampPalette(c("lightgoldenrod1", "gray88","lightskyblue4"))
ggplot(potato_data, aes(x=cut_interval(potato_data$average_rain_fall_mm_per_year, 8), y=mean(hg.ha_yield), fill=cut_interval(potato_data$average_rain_fall_mm_per_year, 8))) +
  xlab("Average rainfall (mm/year)") +
  ylab("Average potato yield (hg/ha)") + 
  scale_fill_manual(values=rain_colfunc(8)) +
  theme(legend.position="none") +
  geom_bar(stat = "identity")
 #average temp
temp_colfunc = colorRampPalette(c("dodgerblue4", "mintcream", "tan1"))
ggplot(potato_data, aes(x=cut_interval(potato_data$avg_temp, 8), y=mean(hg.ha_yield), fill = cut_interval(potato_data$avg_temp, 8))) + 
  xlab("Average temperature (°C)") +
  ylab("Average potato yield (hg/ha)") + 
  scale_fill_manual(values=temp_colfunc(8)) +
  theme(legend.position="none") +
  geom_bar(stat = "identity")
#Finding country/countries with "ideal" potato growing conditions
ideal_potato_rainfall_data = crop_data %>%
  dplyr::group_by(Area) %>%
  dplyr::filter(between(mean(average_rain_fall_mm_per_year), 450, 848))
ggplot(ideal_potato_rainfall_data, aes(x=Area, y=avg_temp, fill=Area)) + 
  xlab("Country") + 
  ylab("Temperature (°C)") +
  scale_fill_hue(l=70, c=45) + 
  theme(legend.position="none") +
  geom_boxplot()
#checking that "ideal" temp conditions are within Pakistan's IQR
pakistan_data = subset(crop_data, Area == "Pakistan")
quantile(pakistan_data$avg_temp)









# Import the csv file (Boston_house_prices.csv) and explore it using str and summary functions.
bos_hp <- read.csv("E:/ISM 645/Assignment 2/Boston_house_prices.csv", stringsAsFactors = TRUE, header = TRUE)
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("stringr")
install.packages("GGally")
library("GGally")
library(stringr)
library(tidyverse)
library(ggplot2)
library(tidyr)
str(bos_hp)
head(bos_hp)
summary(bos_hp)

#Spread the data out to multiple columns, with the shop type (Starbucks vs. Dunkin Donuts) being the column name (key) and the shops variable the value (Hint: tidyr).


bos_hp_spread <-spread(bos_hp, shop_type, shops )
head(bos_hp_spread)
View(bos_hp_spread)

#Delete observations that contain at least one missing value or invalud value (e.g. negative income).
str_detect(bos_hp_spread$median_income,"-")
bos_hp_spread$median_income[bos_hp_spread$median_income < 0] <- "NA"
na_omited <-na.omit(bos_hp_spread)
str_detect(na_omited$dunkin_donuts, "NA" )
str_detect(na_omited$starbucks, "NA" )
str_detect(na_omited$median_income,"-")
View(na_omited)



#To examine the relationship between house prices and Starbucks, make a scatter plot using ggplot2.
head(na_omited)


ggplot(na_omited, aes(x= starbucks, y= house_price_index))+
  geom_point( size = 1, shape = 23)+
  geom_smooth(method=lm, se = FALSE, color = "black")
  labs(y= "HOUSE PRICE INDEX", x= "NUMBER OF STARBUCKS SHOPS", title = " RELATIONSHIP BETWEEN HOUSEPRICE AND STARBUCKS")


#Repeat Q2-1 using Dunkin Donuts instead of Starbucks.
  ggplot(na_omited, aes(x= dunkin_donuts, y= house_price_index))+
  geom_point( size = 1, shape = 20, color = "red")+
  geom_smooth(method = lm, se= FALSE, color = "red")+
  labs(y= "HOUSE PRICE INDEX", x= "NUMBER OF DUNKIN DONUT SHOPS", title = " RELATIONSHIP BETWEEN HOUSEPRICE AND DUNKIN DONUTS")


# Make a linear regression model to predict house prices based on the number of Starbucks and Dunkin Donuts.

linearreg <- lm(house_price_index ~ starbucks + dunkin_donuts, na_omited)
#houseprice = 264.486 -1.951 * no of dunkin donut shops + 28.314 * no of starbucks shop
summary(linearreg)

# One might argue that neighborhoods where Starbucks are located are relatively rich.
# Does Starbucks have a predictive power for house prices, even after controlling for household incomes and population?

str(na_omited)
med_as_int <-transform(na_omited, median_income = as.integer(median_income))
str(med_as_int)
log10_values <- med_as_int  %>%
  mutate(log10_pop = log10(population)) %>%
            mutate(log10_inc = log10(median_income))

head(log10_values)


linearreg_withlog <- lm(house_price_index ~ starbucks + dunkin_donuts + log10_inc + log10_pop, log10_values)
summary(linearreg_withlog)

#By controlling the household income and population, Starbucks still has significant impact on determining rise in houseprice



# The dynamics of house prices might vary across counties in Boston.

head(log10_values)

ggplot(na_omited, aes(x= starbucks, y= house_price_index))+
  geom_point( size = 1, shape = 23)+
  geom_smooth(method = lm, se = FALSE, color ="black")+
  labs(y= "HOUSE PRICE INDEX", x= "NUMBER OF STARBUCKS SHOPS")+
  facet_wrap(~county)


ggplot(na_omited, aes(x= dunkin_donuts, y= house_price_index))+
  geom_point( size = 1, shape = 20, color = "red")+
  geom_smooth(method = lm, se = FALSE, color = "red")+
  labs(y= "HOUSE PRICE INDEX", x= "NUMBER OF DUNKIN DONUT SHOPS")+
  facet_wrap(~county)

linearreg_withcounty <- lm(house_price_index ~ starbucks + dunkin_donuts + log10_inc + log10_pop + county, log10_values)
summary(linearreg_withcounty)


# Q6. Based on your analysis, do you agree or disagree that Starbucks is the bellwether of rise in house prices?

#We know that lower the Probability value, more significant is the impact of co-efficient.
# Based on Q5, I found that though population and median income has lesser Pr value than Starbucks,

install.packages("GGally")
library("GGally")

corelation <- na_omited %>% select (starbucks, house_price_index, population)

ggpairs(corelation, title = "relation finder")


#Finding the corelation using tree matrix plot,
#The value shows that Starbucks still has significant impact on the house price value.


ggplot(na_omited, aes(x= starbucks, y= house_price_index))+
  geom_point( size = 1, shape = 23)+
  geom_smooth(method=lm, se = FALSE, color = "black")
labs(y= "HOUSE PRICE INDEX", x= "NUMBER OF STARBUCKS SHOPS", title = " RELATIONSHIP BETWEEN HOUSEPRICE AND STARBUCKS")

#The straight increasing line shows that presence of starbucks shop in a locatily increases the house price


#From all the above, I agree that starbucks is the bellwether of rise in houseprice.













#===================================================================
S

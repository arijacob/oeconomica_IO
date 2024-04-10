library(tidyverse)
library(ggplot2)
library(ggthemes)

df <- read.csv('/Users/rachelalper/Desktop/Desktop Mess/master data.csv')

season_func <- function(month) {
  season <- ifelse(month %in% c(12, 1, 2), "winter",
                   ifelse(month %in% c(3, 4, 5), "spring",
                          ifelse(month %in% c(6, 7, 8), "summer", "fall")))
  return(season)
}


df = df %>%
  mutate(year = year(date), month = month(date)) %>%
  mutate(season = season_func(month)) %>%
  select(-month)

reg = lm(log(dominant_sales) ~ conduct_period + 
           factor(state) + log(temp) + log(milk_price) +
           log(sugar_price) + log(eggs_price) + log(diesel_price) + 
           log(dominant_wage) + log(waffle_cone_price) + log(cpi_less_food_and_energy) + 
           log(unemployment_rate) + factor(year) + factor(season), data = df)

#wages graph
library(ggplot2)
ggplot(df, aes(x = date)) +
  geom_point(aes(y = defendant_1_wage), color = "blue" , size = 0.25) +
  geom_point(aes(y = defendant_2_wage), color = "red", size = 0.25) +
  geom_point(aes(y = defendant_3_wage), color = "green", size = 0.25) +
  geom_point(aes(y = defendant_4_wage), color = "purple", size = 0.25) +
  geom_point(aes(y = defendant_5_wage), color = "darkblue", size = 0.25) +
  geom_point(aes(y = defendant_6_wage), color = "darkgreen", size =0.25) +
  geom_point(aes(y = defendant_7_wage), color = "orange", size =0.25) +
  geom_point(aes(y = dominant_price), color = "black", size =0.25) +
  labs(x = "Date", y = "Wages", title = "Wages Comparison") +
  theme_minimal()

#prices graph

ggplot(df, aes(x = date)) +
  geom_point(aes(y = defendant_1_price), color = "blue" , size = 0.25) +
  geom_point(aes(y = defendant_2_price), color = "red", size = 0.25) +
  geom_point(aes(y = defendant_3_price), color = "green", size = 0.25) +
  geom_point(aes(y = defendant_4_price), color = "purple", size = 0.25) +
  geom_point(aes(y = defendant_5_price), color = "darkblue", size = 0.25) +
  geom_point(aes(y = defendant_6_price), color = "darkgreen", size =0.25) +
  geom_point(aes(y = defendant_7_price), color = "orange", size =0.25) +
  geom_point(aes(y = dominant_price), color = "black", size =0.25) +
  labs(x = "Date", y = "Wages", title = "Defendant Prices Comparison") +
  theme_minimal()

#REGRESSION GRAPH COMPARIOSN

df_zero = df %>%
  mutate(conduct_period = 0)

df = df %>%
  mutate(predicted = predict(reg, newdata = df), 
         predicted_zero =  predict(reg, newdata = df_zero))


# Plotting the data points and the regression lines
ggplot(data = df) +
  geom_line(aes(x = date, y = predicted), color = "blue") +  # Original regression line
  geom_line(data = df, aes(x = date, y = predicted_zero), color = "purple", linetype = "dashed")  # Regression line with 'conduct_period' set to zero

#GRAPH COMPARING ACTUAL SALES TO PREDICTED IF CONDUCT PERIOD WAS 0
ggplot(data = df) +
  geom_line(aes(x = date, y = log(dominant_sales), color = "Actual Sales (logged)")) +  # actual sales logged
  geom_line(data = df, aes(x = date, y = predicted_zero, color = "Predicted Sales if no Midconduct"), linetype = "dashed") + # Regression line with 'conduct_period' set to zero
  scale_color_manual(values = c(
    'Actual Sales (logged)' = 'darkgreen',
    'Predicted Sales if no Midconduct' = 'purple')) +
  labs(color = 'Legend') +
  theme(legend.position="bottom")

#Comparing actual sales to average with rolling mean
library(zoo)
ggplot(data = df) +
  geom_line(aes(x = date, y = rollmean(log(dominant_sales), k=15, align='right',  fill = NA), color = "Actual Sales (logged)")) +  # actual sales logged
  geom_line(data = df, aes(x = date, y = rollmean(predicted_zero, k=15, align='right',  fill = NA), color = "Predicted Sales if no Midconduct")) + # Regression line with 'conduct_period' set to zero
  scale_color_manual(values = c(
    'Actual Sales (logged)' = 'darkgreen',
    'Predicted Sales if no Midconduct' = 'purple')) +
  labs(color = 'Legend') +
  theme(legend.position="bottom") +
  theme_tufte()

library(tidyverse)
library(ggthemes)
library(zoo)
library(stargazer)

df <- read.csv('https://raw.githubusercontent.com/arijacob/oeconomica_IO/main/Oeconomica/Master-data.csv')

df = df %>%
  mutate(date = as.Date(date)) %>%
  mutate(state = as.factor(state))


season_func <- function(month) {
  season <- ifelse(month %in% c(12, 1, 2), "winter",
                   ifelse(month %in% c(3, 4, 5), "spring",
                          ifelse(month %in% c(6, 7, 8), "summer", "fall")))
  return(season)
}


df = df %>%
  mutate(year = year(date), month = month(date)) %>%
  mutate(season = season_func(month)) %>%
  select(-month) %>%
  mutate(year = factor(year),
         season = factor(season),
         state = factor(state))

reg = lm(log(dominant_sales) ~ conduct_period + 
           factor(state) + log(temp) + log(milk_price) +
           log(sugar_price) + log(eggs_price) + log(diesel_price) + 
           log(dominant_wage) + log(waffle_cone_price) + log(cpi_less_food_and_energy) + 
           log(unemployment_rate) + factor(year) + factor(season), data = df)


library(tune)
library(ranger)
library(tidymodels)
lm_model = 
  linear_reg() %>%
  set_engine("lm")


vars_to_log <- c("dominant_sales", "temp", "milk_price", "eggs_price",
                 "sugar_price", "diesel_price", "dominant_wage",
                 "waffle_cone_price", "cpi_less_food_and_energy",
                 "unemployment_rate", "defendant_6_wage", "defendant_4_wage",
                 "defendant_5_wage")

doms_rec = 
  recipe(dominant_sales ~conduct_period + 
           state + temp + milk_price + eggs_price +
           sugar_price+ diesel_price + 
           dominant_wage + waffle_cone_price + cpi_less_food_and_energy + 
           unemployment_rate + year +
           season + defendant_6_wage + defendant_4_wage + 
           defendant_5_wage, data = df) %>%
  step_log(all_of(vars_to_log), offset = 1) %>%
  step_dummy(state, year, season) 

lm_model = linear_reg() %>%
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(doms_rec)

lm_fit <- fit(lm_wflow, df)

#wages graph
ggplot(df, aes(x = date)) +
  geom_point(aes(y = defendant_1_wage), color = "pink" , size = 0.25) +
  geom_point(aes(y = defendant_2_wage), color = "red", size = 0.25) +
  geom_point(aes(y = defendant_3_wage), color = "green", size = 0.25) +
  geom_point(aes(y = defendant_4_wage), color = "purple", size = 0.25) +
  geom_point(aes(y = defendant_5_wage), color = "darkblue", size = 0.25) +
  geom_point(aes(y = defendant_6_wage), color = "darkgreen", size =0.25) +
  #geom_point(aes(y = defendant_7_wage), color = "orange", size =0.25) +
  #geom_point(aes(y = dominant_price), color = "black", size =0.25) +
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

lm_fit_zero <- fit(lm_wflow, df_zero)
  


df = df %>%
  mutate(predicted = predict(reg, newdata = df), 
         predicted_zero =  predict(reg, newdata = df_zero))

df = df %>%
  mutate(predicted_zero =  predict(lm_fit, newdata = df_zero))

#with predicted values
df_predicted = df_predicted %>%
  mutate(predicted_zero = df_predicted_zero$.pred)

# Plotting the data points and the regression lines
ggplot(data = df) +
  geom_line(aes(x = date, y = predicted), color = "blue") +  # Original regression line
  geom_line(data = df, aes(x = date, y = predicted_zero), color = "purple", linetype = "dashed")  # Regression line with 'conduct_period' set to zero


#Comparing actual sales to average with rolling mean (for reg2)
ggplot(data = df, aes(x = date)) +
  geom_line(aes(y = rollmean(log(dominant_sales), k=20, align='right',  fill = NA), color = "Actual Sales")) +  # actual sales logged
  geom_line(aes(y = rollmean(predicted_zero, k=20, align='right',  fill = NA), color = "Predicted Sales assuming no misconduct")) + # Regression line with 'conduct_period' set to zero
  scale_color_manual(values = c(
    'Actual Sales' = 'darkgreen',
    'Predicted Sales assuming no misconduct' = 'purple')) +
  labs(y = "Sales (logged)", color = 'Legend') +
  theme_excel_new()

<<<<<<< Updated upstream
#calculation of damages
=======

#calculation of damages, this is with reg not reg2 bc 
#i ran into issues, but reg is more conservative also hopefully we can fix and
#then just switch to reg2
>>>>>>> Stashed changes
df = df %>%
  mutate(difference = exp(predicted_zero) - dominant_sales)

sum(df$difference[df$conduct_period == 1])

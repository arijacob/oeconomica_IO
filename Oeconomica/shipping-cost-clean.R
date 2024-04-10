# call libraries
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)

shipping_costs <- read_excel("/Users/arijacob/Documents/Oeconomica - IO Cohort - ICSIOCS Data/Shipping costs time series.xlsx")

names(shipping_costs) <- c("date", "state", "truck_diesel", "truck_gasoline")

shipping_costs <- shipping_costs %>%
  mutate(across(c(truck_diesel, truck_gasoline),~str_remove(., " / gallon"))) %>%
  mutate(across(c(truck_diesel, truck_gasoline),~str_remove(., "\\$")))

shipping_costs <- shipping_costs %>%
  mutate(across(c(truck_diesel, truck_gasoline), as.numeric))

shipping_costs <- shipping_costs %>%
  mutate(date = as.Date(date))

ggplot(shipping_costs, aes(x= date, y = truck_diesel)) + 
  geom_line(stat = "identity", position = "identity")+
  geom_line(aes(x = date, y = truck_gasoline))
write.csv(shipping_costs, "/Users/arijacob/Documents/shipping_costs.csv")

library(tidyverse) 
library(lubridate) 
library(janitor)   
library(skimr)     


customers <- read_csv("customer_data.csv")
transactions <- read_csv("transactions_data.csv")
glimpse(customers)
glimpse(transactions)
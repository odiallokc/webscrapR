# Sevare Acute Malnutrition by country 2018

pacman::p_load(tidyverse, hrbrthemes, janitor, scales, lubridate)

df <- read_csv('Measure Numeric_data.csv') |> 
  select(country = 1, value = 3) |> 
  filter(value > 0)

# df |> arrange(desc(value)) |> pull(country)
afr <- c(
  "Nigeria", "Niger", "Ethiopia" , "Democratic Republic of the Congo", "Chad",
  "Sudan", "Somalia", "South Sudan", "Mali" , "Burkina Faso", "Cameroon", 
  "Burundi", "Malawi" , "Sierra Leone", "Senegal", "Uganda", "Liberia",
  "Central African Republic", "Zimbabwe", "United Republic of Tanzania",
  "Benin", "Zambia", "Ghana" , "Guinea", "Côte d'Ivoire" , "South Africa",
  "Congo", "Gambia", "Lesotho", "Guinea-Bissau", "Namibia", "Equatorial Guinea",                              
  "Gabon" , "Djibouti")

afr_df <- df |> 
  filter (country %in% afr) |> 
  mutate(country = fct_reorder(country, value)) 
afr_df |> 
  ggplot(aes(value, country)) + geom_col() +
  theme_ft_rc()


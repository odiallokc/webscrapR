# Beginner’s Guide on Web Scraping in R

pacman::p_load(rvest, tidyverse, lubridate)

# rvest ----------------------------- ####

url <- "https://en.wikipedia.org/wiki/World_population"

paragraphs <- read_html(url)  |>  html_nodes("p")

# parse out those paragraphs into text:
pg <- paragraphs <- read_html(url)  |>  html_nodes("p") |>  html_text()
pg[1:10]


# getting tables:

tabs <- read_html(url) |>  html_nodes("table") |>  html_table()
df <- tabs[[4]] |> 
  rename("Country" = 2, "Pct_worldPop" = 4) |> 
  select(2:5, 1) |> 
  janitor::clean_names() |> 
  mutate(country       = fct_reorder(country, -rank),
         population    = parse_integer(str_remove_all(population, ",")),
         pct_world_pop = parse_number(pct_world_pop),
         date          = dmy(str_replace_all(date, " ", "/")))


# Plotting

df |> 
  ggplot(aes(pct_world_pop, country, fill = country)) +
  geom_col(show.legend = FALSE) +
  hrbrthemes::theme_ipsum() -> p

ggsave('country_pop.png', p, bg = 'white')

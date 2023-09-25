pacman::p_load(gt, gtsummary, gtExtras, flextable, huxtable, tidyverse)

pz <- pizzaplace |> 
  mutate(datetime = ymd_hms(paste(date, time)),
         date = ymd(date),
         year = year(datetime),
         month = month(datetime, label = TRUE, abbr = TRUE),
         day = month(datetime, label = TRUE, abbr = FALSE),
         hour = hour(datetime),
         day = case_when(hour <= 18 ~ 'Day', TRUE ~ 'Night')) |> 
  select(date, name, size, type, price, year, month, day)
pz

tab1 <- pz |> 
  count(day, type, size) |> 
  pivot_wider(names_from = size, values_from = n, values_fill = 0)

# gt package =========================================================================
# # https://gt.rstudio.com/articles/intro-creating-gt-tables.html
gt_tab1 <- tab1 |> 
  gt() |> 
  tab_header(title = md("**PIZZA DISTRIBUTION BY CATEGORY**"),
             subtitle = html("<em style=color:blue>Grouped by time of day and content</em>")) |> 
  tab_footnote(
    footnote = md("Pizza of size *XL* and *XXL* are all of type Classic"),
    locations = cells_body(columns = type, rows = c(2, 6))) |> 
  tab_source_note(source_note = html("<em style=color:grey>Source: gt R package.</em>"))
gt_tab1

gt_tab2 <- gt_tab1 |> 
  cols_hide(day) |> 
  cols_label(
    type =  html("<b>TYPE</b>"), L = md("*L*"), M = md("*M*"),
    S = md("*S*"), XL = md("*XL*"), XXL = md("*XXL*")) |> 
  tab_row_group(label = md("*DAY*"), rows = 1:4) |>
  tab_row_group(label = md("*NIGHT*"), rows = 5:8) |> 
  tab_style(style = cell_fill(color = "bisque"), 
    locations = cells_row_groups()) |>               # groups = "DAY",  groups = 2
  tab_style(style = cell_text(size = pct(110)), 
            locations = cells_row_groups()) |> 
  tab_options(row_group.padding = px(1),
              data_row.padding = px(1)) |> 
  tab_style(style = list(cell_fill(color = "lightblue"),
                         cell_text(weight = "bold")),
            locations = cells_body(columns = type)) |> 
  tab_style(style = list(cell_fill(color = "lightcyan"),
                         cell_text(weight = "bold")),
            locations = cells_body(columns = c(L, M, S, XL, XXL))) |> 
  tab_style(style = list(cell_fill(color = "black"),
                         cell_text(color = "white")),
            locations = cells_column_labels()) |> 
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "navy",
      weight = px(.5),
      style = "solid"),
    locations = cells_body()
  )


gt_tab1
gt_tab2


# gtExtras  ==========================================================
# https://jthomasmock.github.io/gtExtras/

price_by_type <- pz |> 
  filter(day == 'Day', size == 'S', month == 'Jan', wday(date) == 3) |> 
  group_by(type) |> 
  summarise(
    avg = mean(price), 
    std = sd(price),
    price_data = list(price), #summarize(list_data = list(col_name)) 
    .groups = 'drop') |>
  arrange(type)  |>  
  gt() |> 
  fmt_number(columns = c(avg, std), decimals = 2)

## desnity plot ----
price_by_type |> gtExtras::gt_plt_sparkline(price_data)
price_by_type |> 
  gt_plt_dist(price_data, 
              type = "density", 
              line_color = "blue", fill_color = "red")

## Histogram ------
price_by_type |> 
  gt_plt_dist(price_data, 
              type = "histogram", 
              line_color = "purple", fill_color = "wheat", bw = 2)

## H barplot ----
pz |> 
  select(type, size, price) |> 
  head(10) |> 
  gt() |> 
  gt_plt_bar(column = price, keep_column = TRUE, width = 35, color  = 'dodgerblue')

## H barplot percentage -----
pz |> 
  select(type, size, price) |> 
  head(10) |> 
  gt() |> 
  gt_plt_bar_pct(
    column = price, scaled = FALSE, 
    width = 35, 
    fill = 'darkblue', background = 'dodgerblue') |> 
  cols_align("center", contains("price")) %>%
  cols_width(3 ~ px(200))

## Stacked barplot -----
pz_bar <- pz |> 
  filter(size %in% c('L', 'M', 'S')) |> 
  mutate(size = fct_relevel(size, 'S', 'M', 'L')) |> 
  select(type, size, price) |> # summarize(list_data = list(col_name))
  group_by(size, type) |> 
  summarise(price = round(mean(price), 0), .groups = 'drop') 
pz_bar|> 
  group_by(type) |> 
  summarise(tp = price[1:3], list_data = list(price), .groups = 'drop')  |> 
  arrange(tp)  |> 
  gt() |> 
  gt_plt_bar_stack(list_data, width = 50,
                   labels = c("  SMALL  ", "  MEDIUM  ", "  LARGE  "),
                   palette = c("#ff4343", "#bfbfbf", "#0a1c2b")) |> 
  gt_theme_538()

pz_bar|> 
  group_by(type) |> 
  summarise(tp = price[1], list_data = list(price), .groups = 'drop')  |> 
  arrange(tp)  |> 
  gt() |> 
  cols_hide(columns = tp) |> 
  gt_plt_bar_stack(list_data, width = 50,
                   labels = c("  SMALL  ", "  MEDIUM  ", "  LARGE  "),
                   palette = c("#c985c1", "#8d92c7", "#85cca0")) |> 
  gt_theme_espn()
  

## bullet chart --------
tgt <- pz |> 
  filter(size %in% c('L', 'M', 'S')) |> 
  select(size, type, price) |> 
  group_by(type) |> 
  mutate(avg = mean(price)) |> 
  slice_sample(n = 3) |> 
  ungroup() |> 
  arrange(type)
tgt |>  
  gt() |> 
  gt_plt_bullet(column = price, target = avg, width = 45,
                palette = c("lightblue", "black"))

theme_gtsummary_compact()
tgt |>  
  mutate(bars = price) |> 
  gt() |> 
  fmt_number(price, decimals = 2) |> 
  gt_plt_bullet(column = bars, target = avg, width = 45,
                palette = c("lightgreen", "red")) |> 
  gt_theme_espn()
  

# gtsummary  ==========================================================
# https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html

pz_select <- pz |> select(day, type, size, price) 

pz_select |> tbl_summary()

pz_select |> 
  tbl_summary(by = day) |> 
  modify_header(label = "**Measure**") |> 
  bold_labels() |> 
  add_n() |> 
  add_p()

pz_select |> 
  tbl_summary(
    by = day,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,
    label = list(
      type ~ "TYPE",
      size ~ "SIZE",
      price ~ "PRICE"),
    missing_text = "(Missing)") |> 
  bold_labels() |> 
  italicize_levels() |> 
  modify_caption("*Table 1. Pizza Characteristics*") |> # show_header_names()
  modify_spanning_header(c("stat_1", "stat_2") ~ "**TIMING**")
 

pz_select |> 
 tbl_cross(
    row = day,
    col = size,
    percent = "cell") |> 
  add_p()

pz_select |> 
  tbl_cross(
    row = day,
    col = size,
    percent = "cell") |> 
  add_p()

theme_gtsummary_compact()
theme_gtsummary_journal(journal = "lancet") # c("jama", "lancet", "nejm", "qjecon")
pz_select |> 
  tbl_cross(
    row = type,
    col = size) |> 
  bold_labels()
reset_gtsummary_theme()


# Flextable ==========================================================
# https://ardata-fr.github.io/flextable-book/
# https://ardata.fr/en/flextable-gallery/

fb <- pz |> 
  group_by(type, day) |> 
  summarise(
    max  = round(max(price), 0), 
    min = round(min(price), 0), 
    .groups = 'drop')
 
fb1 <- fb |> flextable()
fb2 <- fb1 |> 
  add_header_row(
    colwidths = c(2, 2),
    values = c('', 'Price')) |> 
  theme_zebra() |>   # theme_tron - tron_legacy - vader - vanila
  add_footer_lines("Data: gt package.") |> 
  color(part = "footer", color = "grey70") |> 
  flextable::set_caption(caption = "Table 1:Pizza prices distribution") 

avg = mean(pz$price)
fb3 <- fb2 |> 
  flextable::align(i = 1, part = "header", align = "center") |> 
  flextable::color( ~ day == 'Day', ~ day + max + min, color = "white") |> 
  flextable::bold( ~ day %in% 'Day', ~ day + max + min, bold = TRUE) |> 
  flextable::color( j = ~ type, color = 'royalblue') |> 
  flextable::bold( j = ~ type, part = 'all') |> 
  flextable::bg(~ type == 'chicken', bg = '#c7a475') |> 
  flextable::bg(~ type == 'classic', bg = '#aeb4b5') |> 
  flextable::bg(~ type == 'supreme', bg = '#ed82ad') |> 
  flextable::bg(~ type == 'veggie', bg = '#96b891') |> 
  flextable::italic(part = "all", j = c('max', 'min')) |> 
  set_header_labels(type = 'TYPE', day = 'DAY', max = 'MAX', min  = 'MIN') |> 
  merge_at(i = 1:2, j = 1) |> merge_at(i = 3:4, j = 1) |> 
  merge_at(i = 5:6, j = 1) |> merge_at(i = 7:8, j = 1)






# huxtable : https://hughjonesd.github.io/huxtable/huxtable.html

hx <- pz |> 
  filter(size %in% c('S', 'M', 'L')) |> 
  mutate(size = fct_relevel(size, 'S', 'M', 'L')) |> 
  count(day, type, size) |> 
  pivot_wider(names_from = size, values_from = n) # , values_fill = 0


hx |> huxtable() |> 
  set_background_color(evens, everywhere, "grey80") %>% 
  set_all_borders(brdr(0.4, "solid", "white")) %>% 
  set_outer_padding(4)

hx |> huxtable() |> 
  set_text_color(2:5, 1, "purple") |> 
  set_text_color(6:9, 1, "orange") |> 
  set_italic(everywhere, c('S', 'M', 'L')) |> 
  set_bold(1, everywhere) |> 
  set_bottom_border(1, value = 2) |> 
  set_background_color(2:5, 2:5, '#eaa1f0') |> 
  set_background_color(6:9, 2:5, '#f7da99') |> 
  set_text_color(2:9, 2:5, "black")

hx |> 
  as_hux() |> 
  theme_basic() |>  
  set_tb_padding(10) |> 
  set_contents(1, 1:2, c("DAY", "TYPE")) |> 
  set_bold(1, everywhere) |> 
  #restack_across(rows = 5) |> 
  set_bottom_border(final(1), everywhere) |> 
  set_col_width(1:2, .3) |> 
  insert_row("", "", "SIZE", "", "", after = 0) |> 
  merge_cells(1, 3:5)

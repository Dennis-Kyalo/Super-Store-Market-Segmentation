library(tidyverse)
library(forcats)
library(readxl)
library(lubridate)
library(tidyquant)
library(data.table)
library(broom)

fs::dir_create(path = "")

# visualization
superstore_tbl %>%
  select(customer_segment, order_date, order_quantity, unit_price) %>%
  mutate(
    order_date       = as.Date(order_date, format = "%m/%d/%Y"),
    customer_segment = as_factor(customer_segment),
    total            = order_quantity * unit_price
  ) %>%
  group_by(customer_segment) %>%
  summarise(n = n()) %>% ungroup() %>%
  mutate(n_text = str_glue("{n}")) %>%
  ggplot(aes(fct_reorder(customer_segment, n, .desc = TRUE), n, fill = customer_segment)) +
  geom_col()      +
  scale_fill_tq() +
  theme_tq()      +
  geom_label(aes(label = n_text), fill = "white") +
  labs(
    x = "",
    y = "Number of Customers",
    title = "Customer Segment Distribution Plot",
    subtitle = "Plot showing the number of customers in each segment",
    caption = "There are four types of customers in the dataset
       Corporate having the highest number of customers"
  ) +
  theme(legend.position = "none")



superstore_tbl %>%
  select(customer_segment, order_date, order_quantity, unit_price) %>%
  mutate(
    order_date = as.Date(order_date, format = "%m/%d/%Y"),
    customer_segment = as_factor(customer_segment),
    total = order_quantity * unit_price,
    year  = year(order_date) %>% as_factor
  ) %>%
  group_by(customer_segment, year) %>%
  summarise(sales = sum(total)) %>%
  ungroup() %>%
  mutate(
    sales_text = scales::dollar(sales),
    customer_segment = fct_reorder(customer_segment, sales, .desc = TRUE)
  ) %>%
  ggplot(aes(fct_reorder(year, sales, .desc = FALSE), sales, fill = customer_segment)) +
  geom_col() +
  facet_wrap( ~ customer_segment, scales = "free_y") +
  geom_label(aes(label = sales_text), fill = "white", size = 3) +
  scale_y_continuous(labels = scales::dollar) +
  scale_fill_tq() +
  theme_tq() +
  labs(
    x = "",
    y = "Sales",
    title = "Total Sales per Segment Plot",
    subtitle = "Plot showing the total year sales in each customer segement",
    caption = "Corporate business contributes highest number of sales yearly"
  ) +
  theme(legend.position = "none")















library(tidyverse)
library(forcats)
library(readxl)
library(lubridate)
library(data.table)


# Load the data
superstore_tbl <- fread(file = "data/Superstore.csv")
superstore_tbl <- janitor::clean_names(superstore_tbl)
glimpse(superstore_tbl)

superstore_tbl <- superstore_tbl %>%
  
  # removing columns
  select(!c(row_id,
            discount,
            product_base_margin,
            profit,
            sales)) %>%
  
  mutate(
    order_date       = mdy(order_date),
    
    # Categorical features
    customer_segment = as_factor(customer_segment),
    department       = as_factor(department),
    ship_mode        = as_factor(ship_mode),
    order_priority   = as_factor(order_priority),
    container        = as_factor(container),
    region           = as_factor(region),
    total            = order_quantity * unit_price
  ) %>% glimpse()

write_rds(x = superstore_tbl, file = "data/superstore.rds")  



library(tidyverse)
library(forcats)
library(readxl)
library(lubridate)
library(tidyquant)
library(janitor)
library(scales)
library(plotly)
library(readxl)

kmeans_4_tbl <- read_rds("00_data/kmeans_custseg_tbl.rds")
kmeans_4_tbl <- kmeans_4_tbl %>% 
  select(
    !c(
      item_id,
      order_id,
      customer_id,
      ship_date,
      container,
      shipping_cost
    )
  ) %>%
  select(customer_name, everything())

# 1. PLOTS ----
## 1.1 table for customers in each cluster ----
table_customer_cluster_func <- function(cluster = 1) {
  data <- kmeans_4_tbl %>%
    mutate(
      total_bin = case_when(
        total > 10000 ~ "Very High End",
        total > 5000  ~ "High End",
        total > 1000  ~ "Medium End",
        TRUE          ~ "Low End"
      )
    )

  clustered_data <- data %>% 
    
    filter(.cluster == cluster) %>%
    distinct(.keep_all = TRUE) %>%
    arrange(desc(total)) %>%
    mutate(
      total_prop      = total / sum(total),
      total_prop_text = percent(total_prop, accuracy = 0.01),
      cum_prop        = scales::percent(cumsum(total_prop), accuracy = 0.01),
      label_text      = str_glue(
        "Customer : {customer_name}
                                   Type    : {total_bin}
                                   Spent   : {scales::dollar(total)}
                                   Segment : {customer_segment}
                                   Department : {department}
                                   Item  : {item}
                                   State : {state}
                                   City  : {city}
                                   Postal Code : {postal_code}"
      )
    ) %>%
    
      rename(
      `Customer Name` = customer_name,
      Revenue         = total,
      `Purchase Prop` = total_prop_text,
      Cumulative      = cum_prop,
      Department      = department,
      Item            = item,
      `Postal Code`   = postal_code
    ) %>% 
    
    mutate(Revenue    = scales::dollar(Revenue))
  
  return(clustered_data)
  
}

table_customer_cluster_func() %>% glimpse()

## 1.2 plot for customers in each cluster ----  
plot_customer_cluster_func <- function(cluster = 1) {
  
  g <- table_customer_cluster_func(cluster = cluster) %>% 
    ggplot(aes(x, y, color = as_factor(total_bin))) +
    geom_point(aes(text = label_text)) +
    theme_tq() +
    scale_color_manual(values = c("#2C3E50", "#C40003", "#00C19B", "#1F78B4")) +
    labs(colour = "Amount Spent")
  ggplotly(g, tooltip = "text")
  
}  
  

plot_customer_cluster_func(cluster = 4)

## 1.3 table for all customers and clusters ----
table_customer_noncluster_func <- function() {
  data <- kmeans_4_tbl %>%
    mutate(
      total_bin = case_when(
        total > 10000 ~ "Very High End",
        total > 5000  ~ "High End",
        total > 1000  ~ "Medium End",
        TRUE          ~ "Low End"
      )
    )
  
  nonclustered_data <- data %>%
    distinct(.keep_all = TRUE) %>%
    arrange(desc(total)) %>%
    mutate(
      total_prop      = total / sum(total),
      total_prop_text = percent(total_prop, accuracy = 0.01),
      cum_prop        = scales::percent(cumsum(total_prop), accuracy = 0.01),
      label_text      = str_glue(
        "Customer : {customer_name}
                                   Cluster  : {.cluster}
                                   Type : {total_bin}
                                   Spent   : {scales::dollar(total)}
                                   Segment : {customer_segment}
                                   Department : {department}
                                   Item : {item}
                                   State : {state}
                                   City : {city}
                                   Postal Code : {postal_code}"
      )
    ) %>%
    rename(
      `Customer Name` = customer_name,
      Revenue         = total,
      `Purchase Prop` = total_prop_text,
      Cumulative      = cum_prop,
      Department      = department,
      Item            = item,
      `Postal Code`   = postal_code
    ) %>% 
    mutate(Revenue    = scales::dollar(Revenue))
  
  return(nonclustered_data)
  
}

table_customer_noncluster_func() %>% glimpse()

## 1.4 plot for all customers and clusters ----
plot_customer_noncluster_func <- function(interactive = TRUE) {
  g <- table_customer_noncluster_func() %>%
    ggplot(aes(x, y, color = as_factor(.cluster))) +
    geom_point(aes(text = label_text)) +
    theme_tq() +
    scale_color_manual(values = c("#2C3E50", "#C40003", "#00C19B", "#1F78B4")) +
    labs(col = "Cluster")
  
  if (interactive) {
    ggplotly(g, tooltip = "text")
  } else {
    return(g)
  }
}
plot_customer_noncluster_func(interactive = TRUE)

dump(c( "table_customer_cluster_func",
    "plot_customer_cluster_func",
    "table_customer_noncluster_func",
    "plot_customer_noncluster_func"), 
    file = "00_scripts/cluster_functions.R")

source("scripts/cluster_functions.R")

# 2. TABLES ----
## 2.1 selected columns : table for clustered customers ----
table_customer_cluster_func() %>% 
  select()


## 2.2 selected columns : table for nonclustered customers ----
table_customer_noncluster_func() %>% 
    select()


table_customer_noncluster_func() %>% 
  mutate(label_text2 = str_glue("{customer_name}")) %>% 
  ggplot(aes(x, y, color = as_factor(.cluster))) +
  geom_point() +
  theme_tq() +
  scale_color_manual(values = c("#C40003","#00C19B", "#2C3E50", "#1F78B4")) +
  labs(title    = "Cluster Plot", 
       subtitle = "The cluster plot shows the cluster projections using UMAP",
       caption  = "There are four clusters based on the K-means elbow graph.
                   Number of customers in each cluster:
                   Cluster 1 - 1454, Cluster 2 - 59 ,Cluster 3 - 620, Cluster 4 - 7293", 
       color    = "Cluster") +
  theme(legend.position = "right")
  




















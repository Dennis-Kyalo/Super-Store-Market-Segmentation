library(tidyverse)
library(forcats)
library(readxl)
library(lubridate)
library(tidyquant)
library(data.table)
library(broom)
library(recipes)
library(umap)
library(parsnip)
library(skimr)
library(ggrepel)
library(janitor)

# Load the data ----
set.seed(123)
superstore_tbl <- read_rds(file = "data/superstore.rds") %>% 
  filter(total > 300) %>% 
  sample_n(size = 3000) 
  


# remove some of the columns
superstore_model_tbl <- superstore_tbl %>% 
  select(!c(item_id, order_id, customer_id, postal_code, ship_date)) %>% 
  select(customer_name, everything()) %>% 
  relocate(shipping_cost, .before = order_quantity)

superstore_model_tbl %>% View()


# Data pre-processing ----
superstore_obj <- superstore_model_tbl %>% 
  recipe(customer_name ~ .) %>%
  step_dummy(
    c(
      customer_segment
    ),
    
    one_hot = TRUE
    
  ) %>%
  step_normalize(all_numeric_predictors()) %>%
  prep()

superstore_prep_tbl <-
  bake(superstore_obj, superstore_model_tbl) 


superstore_prep_tbl %>% View


# Modelling ----
## kmeans function used for iteration ----
kmeans_function <- function(centers = 5){
  
  superstore_prep_tbl %>% 
    select_if(is.numeric) %>% 
    kmeans(centers = centers, nstart = 100)
  
} 


## map the kmeans function and glance the features ----
set.seed(123)
kmeans_centers_tbl <- 
  tibble(centers = seq(1:15)) %>%
  mutate(k_means = centers %>% map(kmeans_function)) %>% 
  mutate(glance  = k_means %>% map(glance))


# write_rds(x = kmeans_centers_tbl, file = "data/kmeans_centers_tbl.rds")
kmeans_centers_tbl <- read_rds("data/kmeans_centers_tbl.rds")



## ggplot showing the scree plot ----
kmeans_centers_tbl %>% unnest(glance) %>%
  select(centers, tot.withinss) %>% 
  ggplot(aes(centers, tot.withinss, label = centers)) +
  geom_point() +
  geom_line(size = 1, colour = palette_light()[1]) +
  geom_label_repel(aes(centers)) +
  geom_segment(
    x = 4.5,
    y = 50000,
    xend = 4.,
    yend = 44700,
    arrow = arrow(length = unit(0.03, "npc")),
    size = 0.8,
    colour = palette_light()[2]
  ) +
  scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16)) +
  theme_tq() +
  labs(title    = "K-means Elbow Plot", 
       subtitle = "The plot helps determine the optimal number of clusters.", 
       caption  = "We select the value of k at the elbow point.
                   Thus the optimal number of clusters for the data is 4" )


  
# Applying UMAP ----
set.seed(123)
superstore_prep_tbl1 <- read_rds("data/superstore_prep_tbl1.rds")
umap_obj <- superstore_prep_tbl %>% 
  select_if(is.numeric) %>% 
  umap()

umap_layout_tbl <- umap_obj$layout %>% 
  as_tibble() %>% 
  rename(x = V1, y = V2) %>% 
  bind_cols(superstore_tbl) 

umap_layout_tbl


# write_rds(x = umap_layout_tbl, file = "data/umap_layout_tbl.rds")
umap_layout_tbl <- read_rds("data/umap_layout_tbl.rds")


## plucking the cluster ----
kmeans_obj <- kmeans_centers_tbl %>% 
  pull(k_means) %>% 
  pluck(4)



## joining the clusters to the main table
kmeans_4_tbl <- kmeans_obj %>% 
  broom::augment(superstore_tbl) %>% 
  left_join(umap_layout_tbl) %>%
  mutate(
    total_bin = case_when(
      total > 10000 ~ "Very High End",
      total > 5000  ~ "High End",
      total > 1000  ~ "Medium End",
      TRUE          ~ "Low End"
    )) 


write_rds(kmeans_4_tbl, file = "data/kmeans_4_tbl.rds")
kmeans_4_tbl <- read_rds("data/kmeans_4_tbl.rds")



## K-means table with customer segment only
write_rds(kmeans_4_tbl, file = "data/kmeans_custseg_tbl.rds")



## graph showing the clusters ----
ggplotly(kmeans_4_tbl %>% 
  ggplot(aes(x, y, color = .cluster)) +
  geom_point() +
  theme_tq() +
  scale_color_manual(values = c("#C40003", "#00C19B", "#2C3E50", "#1F78B4" ))) 


kmeans_4_tbl %>% dim()

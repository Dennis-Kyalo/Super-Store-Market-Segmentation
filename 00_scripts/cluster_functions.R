table_customer_cluster_func <-
function(cluster = 1) {
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
plot_customer_cluster_func <-
function(cluster = 1) {
  
  g <- table_customer_cluster_func(cluster = cluster) %>% 
    ggplot(aes(x, y, color = as_factor(total_bin))) +
    geom_point(aes(text = label_text)) +
    theme_tq() +
    scale_color_manual(values = c("#2C3E50", "#C40003", "#00C19B", "#1F78B4")) +
    labs(colour = "Amount Spent")
  ggplotly(g, tooltip = "text")
  
}
table_customer_noncluster_func <-
function() {
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
plot_customer_noncluster_func <-
function(interactive = TRUE) {
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

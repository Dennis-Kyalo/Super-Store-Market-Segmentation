# LIBRARIES ----
library(shiny)
library(shinythemes)
library(shinyjs)

# Tables
library(DT)
library(knitr)
library(kableExtra)

# Core
library(tidyverse)
library(tidyquant)
library(readxl)
library(data.table)
library(scales)
library(plotly)




source("00_scripts/cluster_functions.R", local = TRUE)
kmeans_4_tbl <- read_rds("00_data/kmeans_4_tbl.rds")

# INFO CARD ----
info_card <- function(title, value, sub_value = NULL,
                      main_icon = "chart-line", sub_icon = "arrow-up",
                      bg_color = "default", text_color = "default", 
                      sub_text_color = "success") {
  
  div(
    class = "panel panel-default",
    style = "padding: 0px;",
    div(
      class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
      p(class = "pull-right", icon(class = "fa-4x", main_icon)),
      h4(title),
      h3(value),
      p(
        class = str_glue("text-{sub_text_color}"),
        icon(sub_icon),
        tags$small(sub_value)
      )
    )
  )
  
}


# UI ------------------------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Customer Segmentation App",
  collapsible = TRUE,
  position    = "static-top", 
  inverse     = TRUE, 
  theme       = shinytheme("flatly"),
  
  
  
  
  
  tabPanel( class = "container-fluid",
            title = "Superstore Market Analyzer",
            
            
            # JS ----
            shinyjs::useShinyjs(),
            
            
            # Value Boxes ----
            fluidRow(column(width = 2),
                     
                     div(
                       class = "container-fluid",
                       column(width = 10,
                              div(
                                
                                uiOutput("value_boxes"),
                              ),
                              
                       ) 
                       
                     )
                     
                     
            ),
            
            
            # SIDE PANEL -----------------------------------------------------------------
            
            fluidRow( 
              column(
                
                width = 2,
                
                wellPanel(
                  
                  h4("Customer Segmentation"),
                  
                  HTML("<p>We use this app to apply customer segmentation to our Super Store client's business, 
                       which will assist them in understanding the various categories of customer segments in the
                       store.We perform <strong>Customer Segmentation Analysis</strong> 
                       using <code>K-means</code> to find the optimal number of clusters (which we get 4) and then apply
                       <code>UMAP</code> to project the customers to their respective clusters.</p>"),
                  hr(),
                  
                  # * Selections -----
                  shiny::selectInput(
                    inputId  = "cluster_selection", 
                    label    = "Pick a Cluster",
                    choices  = c("All", 1, 2, 3, 4),
                    selected = "All"
                    
                  ),
                  
                  # * Action Buttons ------
                  shiny::actionButton(inputId = "submit", "Submit", class = "btn-primary"),
                  shiny::actionButton(inputId = "reset",  "Reset", class = "btn-primary pull-right"),
                  
                ),
                
                
              ),
              column (
                width = 10,
                class = "container",
                
                
                # MAIN PANEL --------------------------------------------------------------  
                
                # * Plot Output ----
                fluidRow(
                  column(width = 6,
                         div(
                           class = "well",
                           div(
                             
                             div(class = "panel-heading", h5("Customer Segments")),
                             
                             div(
                               
                               
                               plotlyOutput("ggplot", height = "600px"),
                             )
                           )
                         )
                         
                  ),
                  
                  # * Table Output ----
                  column(width = 6, 
                         
                         div(
                           class = "well",
                           div(
                             
                             div(class = "panel-heading", h5("Superstore Customer Table")),
                             
                             div(
                               dataTableOutput("cluster_table", height = "600px")
                             )
                           )
                         )
                         
                  ),
                  
                  
                  
                )
              )
            )
  )
)



# SERVER ----------------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  observeEvent(eventExpr = input$reset, handlerExpr = {
    
    updateSelectInput(
      session = session, 
      inputId = "cluster_selection", 
      selected = "All")
    
    
    shinyjs::delay(ms = 300, expr = {
      shinyjs::click(id = "submit")
    })
    
    
  })
  
  rv <- reactiveValues()
  
  # * Data Preparation ----
  observeEvent(input$submit, {
    rv$cluster_table <-
      
      if (input$cluster_selection == "All") {
        table_customer_noncluster_func() 
        
        
      } else {
        table_customer_cluster_func(cluster = input$cluster_selection) 
        
      }
    
    rv$ggplot <- if (input$cluster_selection == "All") {
      plot_customer_noncluster_func()
      
      
    } else {
      plot_customer_cluster_func(cluster = input$cluster_selection)
      
    }
    
    rv$office <- rv$cluster_table %>% group_by(Department) %>%
      summarise(dept_sum = n()) %>%
      slice(1) %>%
      ungroup() %>%
      pull(dept_sum)
    
    rv$furniture <-
      rv$cluster_table %>% group_by(Department) %>%
      summarise(dept_sum = n()) %>%
      slice(2) %>%
      ungroup() %>%
      pull(dept_sum)
    
    rv$technology <-
      rv$cluster_table %>% group_by(Department) %>%
      summarise(dept_sum = n()) %>%
      slice(3) %>%
      ungroup() %>%
      pull(dept_sum)
    
    
  }, ignoreNULL = FALSE)
  
  
  
  # * Plot Output ------
  output$ggplot <- renderPlotly({
    
    rv$ggplot
    
  })
  
  # * Table Output ------
  output$cluster_table <- renderDataTable(
    rv$cluster_table %>% 
      select(`Customer Name`,
             Revenue,
             `Purchase Prop`,
             Cumulative,
             Department,
             `Postal Code`)
    
  )
  
  # * Valuebox Output ---------
  output$value_boxes <- renderUI({
    
    office   <- rv$office
    furniture <- rv$furniture
    technology <- rv$technology
    
    tagList(
      column(
        width = 4,
        info_card(
          title = HTML("<span style='color:white;'>Office Supplies</span>"),
          value = HTML(str_glue("<span class='label label-info'>{office}</span>")),
          bg_color  = "primary",
          main_icon = "folder"
        )
      ),
      column(
        width = 4,
        info_card(
          title = HTML("<span style='color:white;'>Furniture</span>"),
          value = HTML(str_glue("<span class='label label-info'>{furniture}</span>")),
          bg_color  = "primary",
          main_icon = "chair"
        )
      ),
      column(
        width = 4,
        info_card(
          title = HTML("<span style='color:white;'>Technology</span>"),
          value = HTML(str_glue("<span class='label label-info'>{technology}</span>")),
          bg_color  = "primary",
          main_icon = "desktop"
        )
      )
    )
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

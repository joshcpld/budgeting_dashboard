library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)
library(janitor)
library(gt)
library(DT)

################################################################################
############################## Import data #####################################
################################################################################



################################################################################
############################## Dashboard UI ####################################
################################################################################



ui <- fluidPage(
  navbarPage("Budgeting Dashboard", 
             
             tabPanel("Overview", 
                      
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          fileInput("data_upload", "Upload your data"),
                          selectInput("summary_column", "Summarise data by:",
                                      choices = c("payee", "up_category"))
                        ),
                        
                        mainPanel(
                          
                          plotlyOutput("spending_share"),
                          
                          fluidRow(
                          
                          column(6,
                                 h3("Needs Table"),
                                 tableOutput("needs_table")),
                          
                          column(6,
                                 h3("Wants Table"),
                                 tableOutput("wants_table"))
                          )
                          
                        )))))


################################################################################
############################## Dashboard server ################################
################################################################################

server <- function(input,output) {
  
  ################################## CODE FOR ALL CALCS ######################## 
  
  data <- reactive({
    req(input$data_upload)
    
    read_csv(input$data_upload$datapath) %>%
      clean_names() %>%
      select(time, payee, transaction_type, description, up_category = category, total = total_aud) %>%
      mutate(category = case_when(
        transaction_type == "Salary" ~ "Income",
        up_category %in% c("Groceries", "Rent & Mortgage", "Rates & Insurance", "Utilities", "Public transport", "Car Insurance, Rego & Maintenance") ~ "Need",
        TRUE ~ "Want"
      ))
  })
  
  
  ##############################################################################
  ############################### OVERVIEW OUTPUTS #############################
  ##############################################################################
  
  
  
  
  ############################### CALC FOR CHART DATA ##########################
  
  # CHART DATA
  
  chart_data <- reactive({
    income <- data() %>% 
      filter(category == "Income") %>% 
      summarise(income = sum(total)) %>% 
      pull(income)
    
    needs <- data() %>% 
      filter(category == "Need") %>% 
      summarise(needs = sum(total) * -1) %>% 
      pull(needs)
    
    wants <- data() %>% 
      filter(category == "Want") %>% 
      summarise(wants = sum(total) * -1) %>% 
      pull(wants)
    
    data.frame(income, needs, wants) %>% 
      mutate(savings = income - needs - wants) %>%
      select(-income) %>% 
      pivot_longer(everything()) %>% 
      mutate(share = round(value / income * 100))
  })
  
  # CHART
  
  output$spending_share <- renderPlotly({
    p <- ggplot(chart_data(), aes(x = reorder(name, -share), share, fill = name)) + 
      geom_col() + 
      theme_minimal() +
      labs(title = "Spending Share")
    
    ggplotly(p)
  })
  
  #####################################  NEEDS TABLE ###########################
  
  
  output$needs_table <- renderTable({
    
    needs_table_data <- data() %>% 
      filter(category == "Need") %>% 
      group_by_(input$summary_column) %>% 
      summarise(total = sum(total)) %>% 
      arrange(total)
    
    gt_table <- gt(needs_table_data)
    
    return(gt_table)
    
  })
  
  
  
  ##################################### WANTS TABLE ############################
  
  output$wants_table <- renderTable({
  
    wants_table_data <- data() %>% 
      filter(category == "Want") %>% 
      group_by_(input$summary_column) %>% 
      summarise(total = sum(total)) %>% 
      arrange(total)
    
    gt_table <- gt(wants_table_data)
    
    return(gt_table)
    
  })



}

################################################################################
############################## Deploy dashboard ################################
################################################################################


shiny::shinyApp(ui,server)
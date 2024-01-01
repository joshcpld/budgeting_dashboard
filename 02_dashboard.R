library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)
library(janitor)
library(gt)
library(DT)

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
                                 h3("Needs Table", align = "center"),
                                 tableOutput("needs_table")),
                          
                          column(6,
                                 h3("Wants Table", align = "center"),
                                 tableOutput("wants_table"))
                          )
                          
                        ))),
             
             tabPanel("Data",
                      
                      fluidPage(
                        mainPanel(
                          DTOutput("data_edited")
                        
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
      filter(!(transaction_type %in% c("Transfer", "Scheduled Transfer"))) %>% 
      mutate(category = case_when(
        transaction_type == "Salary" ~ "Income",
        up_category %in% c("Groceries", "Rent & Mortgage", "Rates & Insurance", 
                           "Utilities", "Public Transport", "Car Insurance, Rego & Maintenance") ~ "Need",
        TRUE ~ "Want"
      )) %>% 
      mutate(up_category = str_replace_all(up_category, "&", "and")) %>% 
      mutate(up_category = ifelse(is.na(up_category), "N/A", as.character(up_category)))
  })
  
  ##############################################################################
  ############################### DATA OUTPUTS #################################
  ##############################################################################
  
  # CREATING DATA TABLE FROM UPLOADED DATA
  
  output$data_edited <- renderDT({
    if (!is.null(data())) {
      datatable(data(), options = list(pageLength = 10), editable = TRUE)
    }
  })
  
  # ALLOWING EDITS I MAKE IN THIS TAB TO BE REFLECT IN OVERVIEW TAB
  
  rv <- reactiveValues(data_edited = NULL)
  
  observeEvent(input$data_edited_cell_edit, {
    info = input$data_edited_cell_edit
    str(info)
    modified_data <- rv$data_edited
    modified_data[info$row, info$col] <- info$value
    rv$data_edited <- modified_data
  })
  
  observe({
    if (!is.null(data())) {
      rv$data_edited <- data()
    }
  })
  
  output$data_edited <- renderDT({
    if (!is.null(rv$data_edited)) {
      datatable(rv$data_edited, options = list(pageLength = 10), editable = TRUE)
    }
  })
  
  ##############################################################################
  ############################### OVERVIEW OUTPUTS #############################
  ##############################################################################
  
  
  
  
  ############################### CALC FOR CHART DATA ##########################
  
  # CHART DATA
  
  chart_data <- reactive({
    income <- rv$data_edited %>% 
      filter(category == "Income") %>% 
      summarise(income = sum(total)) %>% 
      pull(income)
    
    needs <- rv$data_edited %>% 
      filter(category == "Need") %>% 
      summarise(needs = sum(total) * -1) %>% 
      pull(needs)
    
    wants <- rv$data_edited %>% 
      filter(category == "Want") %>% 
      summarise(wants = sum(total) * -1) %>% 
      pull(wants)
    
    data.frame(income, needs, wants) %>% 
      mutate(savings = income - needs - wants) %>%
      pivot_longer(everything()) %>% 
      mutate(share = round(value / income * 100)) %>% 
      mutate(name = as_factor(name)) %>% 
      mutate(name = fct_relevel(name, c("income", "needs", "wants","savings"))) %>% 
      arrange(name) %>%
      mutate(target_value = case_when(
        
        name == "income" ~ NA_real_,
        name == "needs" ~ income * 0.5,
        name %in% c("wants", "savings") ~ income * 0.25
        
      ))
  })
  
  # CHART
  
  output$spending_share <- renderPlotly({
    p <- ggplot(chart_data(), aes(x = name, y = value, fill = name)) + 
      geom_col() + 
      geom_point(aes(x = name, y = target_value), color = "black", size = 3, fill = "black") +
      theme_minimal() +
      labs(x = NULL) +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    
    p <- ggplotly(p) 
    p <- layout(p, annotations = list(
      list(x = 0, y = -0.11, xref = "paper", yref = "paper",
           text = "Note: dots reflect target values (50:25:25 rule).",
           showarrow = FALSE,
           font = list(size = 11),
           xanchor = "left",
           yanchor = "bottom")))
    
    p
    
  })
  
  #####################################  NEEDS TABLE ###########################
  
  
  output$needs_table <- renderTable({
    
    needs_table_data <- rv$data_edited %>% 
      filter(category == "Need") %>% 
      group_by_(input$summary_column) %>% 
      summarise(total = sum(total)) %>% 
      arrange(total)
    
    gt_table <- gt(needs_table_data)
    
    return(gt_table)
    
  })
  
  
  
  ##################################### WANTS TABLE ############################
  
  output$wants_table <- renderTable({
  
    wants_table_data <- rv$data_edited %>% 
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
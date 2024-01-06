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
                          DTOutput("data_edited")))),
             
             
             tabPanel("Instructions",
                      fluidRow(
                        box(title = "Instructions",
                            uiOutput("instructions_text"),
                            solidHeader = TRUE, status = "info"),
                        # Add other content for Tab 2
                      )
             )))


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
      mutate(up_category = ifelse(is.na(up_category), "N/A", as.character(up_category))) %>% 
      mutate(total = round(total,1))
  })
  
  ##############################################################################
  ############################### DATA OUTPUTS #################################
  ##############################################################################
  
  # CREATING DATA TABLE FROM UPLOADED DATA
  
  output$data_edited <- renderDT({
    if (!is.null(data())) {
      datatable(data(), options = list(pageLength = 10),
                editable = list(target = 'cell', editor = list(select = TRUE)),
                selection = 'none')
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
  # 
  # output$data_edited <- renderDT({
  #   if (!is.null(rv$data_edited)) {
  #     datatable(rv$data_edited, options = list(pageLength = 10), editable = TRUE)
  #   }
  # })
  
  
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
    
    income <- rv$data_edited %>% 
      filter(category == "Income") %>% 
      summarise(income = sum(total)) %>% 
      pull(income)
    
    needs_table_data <- rv$data_edited %>% 
      filter(category == "Need") %>% 
      group_by_(input$summary_column) %>% 
      summarise(total = sum(total)) %>%
      mutate(income_share = round(total / income * 100, 1)) %>%
      arrange(total)
    
    gt_table <- gt(needs_table_data)
    
    return(gt_table)
    
  })
  
  
  
  ##################################### WANTS TABLE ############################
  
  output$wants_table <- renderTable({
  
    income <- rv$data_edited %>% 
      filter(category == "Income") %>% 
      summarise(income = sum(total)) %>% 
      pull(income)
    
    wants_table_data <- rv$data_edited %>% 
      filter(category == "Want") %>% 
      group_by_(input$summary_column) %>% 
      summarise(total = sum(total)) %>%
      mutate(income_share = round(total / income * 100, 1)) %>%
      arrange(total)
    
    gt_table <- gt(wants_table_data)
    
    return(gt_table)
    
  })

  ################################################################################
  ############################## INSTRUCTIONS ####################################
  ################################################################################
  
  
  output$instructions_text <- renderText({
    # Your logic to dynamically generate instructions for Tab 2

    HTML("Step 1: Upload your statement from Up (only data from Up Bank can be used for this dashboard). <br><br>
    
    
         Step 2: Review the chart and tables on the overview page to make sure all transactions seem categorised correctly. <br><br>
         
         Step 3: If it looks like there are some errors, find the relevant transactions in the Data tab and edit the payee or up_category columns accordingly by double clicking any cell. 
          Any edits you make here will be reflected in the chart and tables. <br><br>
         
         Notes: <br>
         - This dashboard is intended to be an easy way to check if you're adhering to the 50:25:25  personal finance rule (refer here for context: https://www.unfcu.org/financial-wellness/50-30-20-rule/). <br>
         - To be registered as income in this dashboard, a payment needs to be recognised as your salary on Up. <br>
         - Savings are calculated as what's leftover from your income after subtracting needs and wants transcations. This means 
         that transfers from/to other accounts/banks can make the chart look misleading - it's helpful to make these transactions zero
          for a true representation of what you've actually spent in a given period.")
    
      })

}





################################################################################
############################## Deploy dashboard ################################
################################################################################


shiny::shinyApp(ui,server)
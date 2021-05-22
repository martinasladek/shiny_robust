
library(DT)
library(ggplot2)
library(magrittr)
library(shiny)
library(shinydashboard)

source("../shiny_robust/scripts/helpers.R") 

viridis <- readRDS("../shiny_robust/objects/viridis.rds")

# Define UI for application that draws a histogram
ui <- dashboardPage( 
    
    # Application title
    header = dashboardHeader(title = "Robust Statistics"),
    
    # Sidebar with a slider input for number of bins 
    sidebar = dashboardSidebar(
        sidebarMenu(id = "sidebar_menu",
                    menuItem("Data", tabName = "data", icon = icon("archive")),
                    menuItem("Robust regression", tabName = "robust_regression", icon = icon("chart-line"))
        )
    ), 
    
    # Show a plot of the generated distribution
    body  = dashboardBody(
        tabItems(
            tabItem(tabName = "data", 
                    fluidRow(
                        fileInput(inputId = "upload_data","Upload data", accept = ".csv"), 
                        align = "center"
                    ), 
                    fluidRow(
                        dataTableOutput(outputId = "display_data")
                    )
            ), 
            tabItem(tabName = "robust_regression", 
                    fluidRow(
                        column(2,
                               selectInput(inputId = "outcome", label = "Outcome:", 
                                           choices = NULL, multiple = FALSE), 
                               selectInput(inputId = "predictor", label = "Predictor(s):",
                                           choices = NULL, multiple = TRUE),
                               numericInput(inputId = "n_interactions", label = "Number of interactions", value = 1),
                               selectInput(inputId = "interaction", label = "Interaction term(s):",
                                           choices = NULL, multiple = TRUE, 
                                            # options = list(
                                            #     create = TRUE 
                                            #     )
                                           ),
                               br(), 
                               br(),
                               fluidRow(
                                   actionButton(inputId = "run_analysis", label = "Run (to the hills...)"), 
                                   align = "center"
                               ),
                        ), 
                        column(5, 
                               fluidRow(
                                   box(
                                       tableOutput(outputId = "lmrob_output"), 
                                       title = "Robust model", solidHeader = FALSE, status = "primary", 
                                       width = 12
                                   )
                               ),
                               fluidRow(
                                   box(
                                       tableOutput(outputId = "lm_output"), 
                                       title = "OLS model", solidHeader = FALSE, status = "primary", 
                                       width = 12
                                   )
                               )
                        ), 
                        column(5, 
                               fluidRow(
                                   # box(
                                   #     plotOutput(outputId = "lm_plot"), 
                                   #     width = 12
                                   # ), 
                                   box(
                                       htmlOutput(outputId = "debug_box"), 
                                       width = 12
                                   )
                               )
                        )
                    )
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    # Upload data and save as a reactive value
    rvs <- reactiveValues(data = NULL, n_interactions = 1)
    
    observe({
        req(input$upload_data)
        rvs$data <- read.csv(input$upload_data$datapath)
        rvs$n_interactions <- input$n_interactions
        
        updateSelectInput(inputId = "outcome", choices = names(rvs$data))
        updateSelectInput(inputId = "predictor", choices = names(rvs$data))
        updateSelectInput(inputId = "interaction", choices = rep(c(names(rvs$data), "+"), rvs$n_interactions))
    })
    
    
    # Display data 
    output$display_data <- renderDataTable({
        DT::datatable(
            rvs$data, 
            selection = 'none',
            #editable = TRUE,
            class = "display",
            options = list(
                paging = FALSE, 
                scrollY = "500px"
            )
        )
    })
    
    # Run linear model 
    observe({
        
        req(input$run_analysis, input$predictor)
        
        if(is.null(input$interaction)){
            interaction = ""
        } else if(stringr::str_sub(input$interaction[length(input$interaction)], -1) == "+") {
            interaction = ""
        } else if(check_multi_plus(input$interaction) == TRUE){
            interaction == ""
        } else {
            interaction = paste0("+", 
                                 process_int_string(input$interaction)
            )
        }
        
        
        formula = paste0(
            input$outcome, "~", 
            paste0(input$predictor, collapse = "+"), 
            interaction
        )
        
        # fit OLS model 
        lm_model <- lm(formula = formula(paste(formula, collapse = " ")), data = rvs$data)
        output$lm_output <- renderTable({
            model_output(lm_model)
        }, digits = 4)
        
        # fit robust model 
        lmrob_model <- robustbase::lmrob(formula(paste(formula, collapse = " ")), data = rvs$data, setting = "KS2014")
        output$lmrob_output <- renderTable({
            model_output(lmrob_model)
        }, digits = 4)
        
        # pred_lm <- predict(lm_model, interval = "confidence") %>%
        #     as.data.frame()
        # 
        # pred_lmrob <- predict(lmrob_model, interval = "confidence") %>%
        #     as.data.frame() 
        
        
        # output$lm_plot <- renderPlot({
        # 
        #     ggplot2::ggplot(data = rvs$data, aes(x = get(input$predictor), 
        #                                          y = get(input$outcome))) +
        #         geom_point(colour = viridis$blue_3, alpha = 0.5) +
        #         #geom_line(data = pred_lm, aes(y = fit), size = 1, colour = viridis$purple_1) +
        #         #geom_line(data = pred_lmrob, aes(y = fit), size = 1, colour = viridis$green_1) +
        #         theme_minimal()
        # 
        # })
        
        output$debug_box <- renderUI({
            HTML(
                paste0(
                    interaction
                )
            )
        })
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

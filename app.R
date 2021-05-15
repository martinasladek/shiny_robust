
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
                               varSelectInput(inputId = "outcome", label = "Outcome:", data = NULL, multiple = TRUE), 
                               varSelectInput(inputId = "predictor", label = "Predictor(s):", data = NULL, multiple = TRUE),
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
                                   box(
                                       plotOutput(outputId = "lm_plot"), 
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
    rvs <- reactiveValues(data = NULL)
    
    observe({
        req(input$upload_data)
        rvs$data <- read.csv(input$upload_data$datapath) %>% 
            tibble::rownames_to_column()
        
        updateVarSelectInput(inputId = "outcome", data = rvs$data)
        updateVarSelectInput(inputId = "predictor", data = rvs$data)
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
        
        req(input$run_analysis)
        
        formula = paste0(input$outcome, "~", input$predictor)
        
        # fit OLS model 
        lm_model <- lm(formula = as.formula(formula), data = rvs$data)
        output$lm_output <- renderTable({
            model_output(lm_model)
        }, digits = 4)
        
        # fit robust model 
        lmrob_model <- robustbase::lmrob(formula = as.formula(formula), data = rvs$data)
        output$lmrob_output <- renderTable({
            model_output(lmrob_model)
        }, digits = 4)
        

        pred_lm <- predict(lm_model, interval = "confidence") %>%
            as.data.frame() %>%
            tibble::rownames_to_column()

        pred_lmrob <- predict(lmrob_model, interval = "confidence") %>%
            as.data.frame() %>%
            tibble::rownames_to_column()
    
        
        output$lm_plot <- renderPlot({

            ggplot2::ggplot(data = rvs$data, aes()) +
                #geom_point(colour = viridis$blue_3, alpha = 0.5) +
                #geom_line(aes(y = pred_lm$fit), size = 1, colour = viridis$purple_1) +
                #geom_line(aes(y = pred_lmrob$fit), size = 1, colour = viridis$green_1) +
                theme_minimal()

        })

    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


library(DT)
library(ggplot2)
library(magrittr)
library(palmerpenguins)
library(shiny)
library(shinydashboard)
library(shinyjs)

source("../shiny_robust/scripts/helpers.R") 

viridis <- readRDS("../shiny_robust/objects/viridis.rds")

# Define UI for application that draws a histogram
ui <- dashboardPage( 
    
    shinyjs::useShinyjs(),
    
    # Application title
    header = dashboardHeader(title = "Robust Statistics"),
    
    # Sidebar with a slider input for number of bins 
    sidebar = dashboardSidebar(
        sidebarMenu(id = "sidebar_menu",
                    menuItem("Data", tabName = "data", icon = icon("archive")),
                    menuItem("Robust linear model", tabName = "robust_regression", icon = icon("chart-line"))
        )
    ), 
    
    # Show a plot of the generated distribution
    body  = dashboardBody(
        tabItems(
            tabItem(tabName = "data", 
                    fluidRow(
                        fileInput(inputId = "upload_data","Upload data", accept = ".csv"), 
                        actionButton(inputId = "load_demo_data", "Load demo data"),
                        actionButton(inputId = "reset_data", "Reset data"),
                        align = "center"
                    ), 
                    fluidRow(
                        dataTableOutput(outputId = "display_data")
                    )
            ), 
            tabItem(tabName = "robust_regression", 
                    fluidRow(
                        column(4,
                               box(
                                   selectInput(inputId = "outcome", label = "Outcome:", 
                                               choices = NULL, multiple = FALSE), 
                                   selectInput(inputId = "predictor", label = "Predictor(s):",
                                               choices = NULL, multiple = TRUE),
                                   uiOutput(outputId = "interaction_vars"), 
                                   br(),
                                   fluidRow(
                                       actionButton("add_interaction", "Next interaction"),
                                       actionButton("reset", "Reset"), 
                                       align = "center"
                                   ),
                                   br(),
                                   htmlOutput("interaction_terms"),
                                   br(),
                                   # fluidRow(
                                   #     actionButton(inputId = "run_analysis", label = "Run analysis"), 
                                   #     align = "center"
                                   # ),
                                   width = 12
                               ), 
                               box(
                                   htmlOutput(outputId = "code_box"), 
                                   tags$style(HTML("
                                          code {
                                            color: #007aa6;
                                            background-color: white;
                                          } 
                                       ")),
                                   width = 12
                               )
                        ), 
                        column(8, 
                               fluidRow(
                                   box(
                                       tableOutput(outputId = "lmrob_output"), 
                                       title = "Robust model", solidHeader = FALSE, status = "primary", 
                                       width = 10
                                   )
                               ),
                               fluidRow(
                                   box(
                                       tableOutput(outputId = "lm_output"), 
                                       title = "OLS model", solidHeader = FALSE, status = "primary", 
                                       width = 10
                                   )
                               )
                        ), 
                        column(4, 
                               fluidRow(
                                   # box(
                                   #     plotOutput(outputId = "lm_plot"), 
                                   #     width = 12
                                   # ), 
                                   
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
    rvs <- reactiveValues(data = NULL, variables = NULL, interaction = NULL, interaction_display = NULL)
    chosen_vars <- reactiveVal(c())
    
    observeEvent(input$reset, chosen_vars(c()))
 
    observe({
        if(input$load_demo_data != 0){shinyjs::disable("upload_data")}
        req(input$upload_data)
        rvs$data <- read.csv(input$upload_data$datapath)
        rvs$variables <- names(rvs$data)
        
        updateSelectInput(inputId = "outcome", choices = rvs$variables)
        updateSelectInput(inputId = "predictor", choices = rvs$variables)

    })
    
    observe({
        if(!is.null(input$upload_data)){shinyjs::disable("load_demo_data")}
        req(input$load_demo_data)
        rvs$data <- palmerpenguins::penguins
        rvs$variables <- names(rvs$data)
        
        updateSelectInput(inputId = "outcome", choices = rvs$variables)
        updateSelectInput(inputId = "predictor", choices = rvs$variables)
        
    })
    
    observeEvent(input$reset_data, {
        shinyjs::refresh()
    })
    
    output$interaction_vars <- renderUI({
        # Take a dependency on chosen_vars to re-render when an option is selected.
        chosen_vars()
        selectInput(inputId = "interaction", label = "Interacting variables:",
                    choices = rvs$variables[!rvs$variables %in% input$outcome], multiple = TRUE, selectize = FALSE
        )
    })
    
    observeEvent(input$interaction, {
        chosen_vars(c(chosen_vars(), input$interaction))
    })
    
    observeEvent(input$add_interaction, {
        chosen_vars(c(chosen_vars(), "+"))
    })
    
    observe({
        rvs$interaction <- process_int_string(chosen_vars())
    })
    
    output$interaction_terms <- renderPrint(
        HTML("<b> Interaction term(s):</b> <br><br>", rvs$interaction %>% 
                 stringr::str_replace_all(., pattern = ":", replacement = " ✕ ") %>% 
                 stringr::str_replace_all(., pattern = "[+]", replacement = "</br> +")
        )
    )
    
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
        
        req(input$predictor)
        
        if(is.null(rvs$interaction)){
            interaction = ""
        } else if(stringr::str_sub(rvs$interaction[length(rvs$interaction)], -1) == "+") {
            interaction = ""
        } else if(check_multi_plus(rvs$interaction) == TRUE){
            interaction == ""
        } else {
            interaction = rvs$interaction
        }
        
        if(nchar(interaction) == 0){
            connect = ""
        } else {connect = "+"}
        
        formula = paste0(
            input$outcome, "~", 
            paste0(input$predictor, collapse = "+"), connect,
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
        
        output$code_box <- renderUI({
            tags$code(
                HTML(
                    '<b># R code for the robust model:</b> <br><br>',
                    'robustbase::lmrob( <br>
                    &nbsp formula = ', stringr::str_replace_all(formula, "[+]", " + "),', <br> 
                    &nbsp data = uploaded_data) <br>
                    &nbsp setting = "KS2014" <br>
                    ) <br><br>',
                    
                    '<b># R code for the OLS model:</b> <br><br>',
                    'lm( <br>
                    &nbsp formula = ', stringr::str_replace_all(formula, "[+]", " + "),', <br> 
                    &nbsp data = uploaded_data) <br>
                    ) <br><br>'
                )
            )
        })
        
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

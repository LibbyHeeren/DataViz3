# Elizabeth Heeren, DATAVIZ HW3, app deployment -> https://libbyheeren.shinyapps.io/dv_hw3/

library(ggplot2)
library(shiny)
library(haven)


ui <- fluidPage(
    
    titlePanel("Apple Financials"),
    
    sidebarLayout(
        sidebarPanel(
            
            fileInput("sasfile", "Upload SAS Data:"),
            
            selectInput("xvar",
                        "X-Axis Variable:",
                        choices = c("Sales", 
                                    "Cash",
                                    "Assets",
                                    "Profits",
                                    "R&D",
                                    "SG&A")),
            
            selectInput("yvar",
                        "Y-Axis Variable:",
                        choices = c("Sales", 
                                    "Cash",
                                    "Assets",
                                    "Profits",
                                    "R&D",
                                    "SG&A"),
                        selected = "R&D"),
            
            selectInput("scale",
                        "Choose the Scale:",
                        choices = c("Levels", 
                                    "Log 10")),
            
            radioButtons("model", 
                         "Choose the Model:", 
                         choices = c("Linear Model",
                                     "LOESS", 
                                     "None"),
                         selected = "LOESS"),
            
            checkboxInput("se", "Standard Error Ribbon", TRUE)
            
        ),
        
        
        mainPanel(
            plotOutput("plot")
        )
    )
)


server <- function(input, output) {
    
    
    output$plot <- renderPlot({
        
        instruct <- "Please upload a SAS data file (sas7bdat extension)
 Make sure that it has the following variables:
 SALEQ, CHEQ, ATQ, OIADPQ, XRDQ, XSGAQ"
        
        validate(
            need(input$sasfile != "", instruct)
        )
        
        sas <- read_sas(input$sasfile$datapath) 
        
        xvar <- switch(input$xvar,
                       "Sales" = "SALEQ",
                       "Cash" = "CHEQ",
                       "Assets" = "ATQ",
                       "Profits" = "OIADPQ",
                       "R&D" = "XRDQ",
                       "SG&A" = "XSGAQ")
        
        xtitle <- switch(input$xvar,
                         "Sales" = "Sales (million $)",
                         "Cash" = "Cash (million $)",
                         "Assets" = "Assets (million $)",
                         "Profits" = "Profits (million $)",
                         "R&D" = "R&D (million $)",
                         "SG&A" = "SG&A (million $)")
        
        yvar <- switch(input$yvar,
                       "Sales" = "SALEQ",
                       "Cash" = "CHEQ",
                       "Assets" = "ATQ",
                       "Profits" = "OIADPQ",
                       "R&D" = "XRDQ",
                       "SG&A" = "XSGAQ")
        
        ytitle <- switch(input$yvar,
                         "Sales" = "Sales (million $)",
                         "Cash" = "Cash (million $)",
                         "Assets" = "Assets (million $)",
                         "Profits" = "Profits (million $)",
                         "R&D" = "R&D (million $)",
                         "SG&A" = "SG&A (million $)")
        model <- switch(input$model,
                        "Linear Model" = "lm",
                        "LOESS" = "loess",
                        "None" = "")
        if (input$scale == "Levels") {
            
            ggplot(sas, aes_string(xvar, yvar)) +
                geom_point() +
                geom_smooth(method = model, se = input$se, color = "white") +
                labs(x = xtitle, y = ytitle) +
                hrbrthemes::theme_modern_rc() +
                theme(legend.position = "none") +
                scale_x_continuous() +
                scale_y_continuous()
            
        } else {
            
            ggplot(sas, aes_string(xvar, yvar)) +
                geom_point() +
                geom_smooth(method = model, se = input$se, color = "white") +
                labs(x = xtitle, y = ytitle) +
                hrbrthemes::theme_modern_rc() +
                theme(legend.position = "none") +
                scale_x_log10() +
                scale_y_log10()
        }
    })
}


shinyApp(ui = ui, server = server)

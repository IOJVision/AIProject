library(shiny)
library(shinydashboard)
library(neuralnet)
#library(shinyWidgets)
# Define User Interface logic

ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "AssignmentAI"),
    dashboardSidebar(
        #disable = TRUE,
        sliderInput(
            "cement",
            "Cement :",
            min = 0.4091,
            max = 1,
            value = 0.4091
            
        ),
        sliderInput(
            "slag",
            "Slag :",
            min = 0.20561,
            max = 1,
            value = 0.20561
        ),
        sliderInput(
            "ash",
            "Ash :",
            min = 0.2708,
            max = 1,
            value = 0.2708
        ),
        sliderInput(
            "water",
            "Water :",
            min = 0.4774,
            max = 1,
            value = 0.4774
        ),
        sliderInput(
            "superplastic",
            "Superplastic :",
            min = 0.1927,
            max = 1,
            value = 0.1927
        ),
        sliderInput(
            "coarseagg",
            "Coarseagg :",
            min = 0.4998,
            max = 1,
            value = 0.4998
        ),
        sliderInput(
            "fineagg",
            "Fineagg :",
            min = 0.4505,
            max = 1,
            value = 0.4505
        ),
        sliderInput(
            "age",
            "Age :",
            min = 0.1227,
            max = 1,
            value = 0.1227
        )
    ),
    dashboardBody(
        tabItem(tabName = "dashboard",
                fluidRow(
                    box(
                        title = "Concrete Line Plot",
                        "Blue line - predicted_strength2",br(),
                        "Red line - concrete_test$strength",br(),
                        textOutput("cor"),
                        status = "primary",
                        solidHeader = TRUE,
                        plotOutput("linePlot", height = 500)
                    ),
                    box(
                        title = "Neural Network",
                        "Hidden = 5",br(),br(),
                        status = "primary",
                        solidHeader = TRUE,
                        plotOutput("nnPlot", height = 500)
                    )
                ),
                fluidRow(
                    box(
                        title = "Data Frame",
                        status = "primary",
                        solidHeader = TRUE,
                        verbatimTextOutput("dtframe")
                    ),
                    box(
                        title = "Output",
                        status = "primary",
                        solidHeader = TRUE,
                        strong("Predicted Strength Based on User Input"),
                        textOutput("summary3")
                    )
                )
        )
    )
)

# Define Server logic

server <- function(input, output) {
    concrete <- read.csv("concrete.csv")
    
    #str(concrete)
    
    normalize <- function(x) {
        return((x - min(x)) / (max(x) - min(x)))
    }
    
    concrete_norm <- as.data.frame(lapply(concrete, normalize))
    
    #summary(concrete_norm$strength)
    
    #summary(concrete$strength)
    
    concrete_train <- concrete_norm[1:773, ]
    concrete_test <- concrete_norm[774:1030, ]
    
    #Train with 5 hidden layer
    concrete_model2 <- neuralnet(strength ~ cement + slag +
                                     ash + water + superplastic +
                                     coarseagg + fineagg + age,
                                 data = concrete_train, hidden = 5)
    
    
    model_results2 <- compute(concrete_model2, concrete_test[1:8])
    
    predicted_strength2 <- model_results2$net.result
    
    #function to gather the composition
    
    top  <- reactive({
        
        cement <- input$cement
        slag <- input$slag
        ash<- input$ash
        water<-input$water
        superplastic<-input$superplastic 
        coarseagg<-input$coarseagg
        fineagg<-input$fineagg
        age<-input$age
        
        userInput <- cbind(cement,slag,ash,water,superplastic,coarseagg,fineagg,age )
        
        concrete_testInp1 <- as.data.frame(userInput) 
        model_results3 <- compute(concrete_model2 , concrete_testInp1)
        top <- model_results3$net.result
    })
    
    
    
    
    
    output$summary3 <- renderText({
        top()
    })
    
    output$linePlot <- renderPlot({
        plot(predicted_strength2,  typ='l', col = "red" , ylab = "Values")
        lines(concrete_test$strength,  typ='l', col = "blue")
        
    })  
    output$nnPlot <- renderPlot({
        plot(concrete_model2)
    })  
    output$dtframe <- renderPrint({
        str(concrete)
    })
    output$cor <- renderText({
        paste(
            "Correlation - ",
            correlate <- cor(predicted_strength2, concrete_test$strength)
        )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
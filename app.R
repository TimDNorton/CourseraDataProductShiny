#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(datasets)
library(stats)
library(nnet)
library(Metrics)
library(e1071)
library(caret)
library(shiny)
library(car)
ChickWeight

set.seed(123)

ChickWeight$Chick<-as.numeric(ChickWeight$Chick)
ChickWeight$Diet<-as.numeric(ChickWeight$Diet)

split_on<-floor(.75 * nrow(ChickWeight))
train_ind <- sample(seq_len(nrow(ChickWeight)), size = split_on)

train<-ChickWeight[train_ind,]
test<-ChickWeight[-train_ind,]
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Multivariate Analysis of Chick Weights and Diet"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          selectInput(inputId = "vars", label = "Independent Variables",
                      choices =c("Diet, Chick and Time", "Time and Diet", "Time","Time and Chick", "Diet", "Diet and Chick", "Chick", "Support Vector Machine"),
                      selected = "Diet, Chick and Time")
      ),
    mainPanel(
        
        tabsetPanel(type="tabs",
                    tabPanel("Exploratory Data Analysis",
                             
                             tableOutput("view"),
                             verbatimTextOutput("sum"),
                             plotOutput("core"),
                             textOutput("var")),
                    tabPanel("Model Summary",
                             verbatimTextOutput("mod")),
                    
                    tabPanel("Root Mean Squared Error",
                             textOutput("freetext"),
                             verbatimTextOutput("rmse")),
                    tabPanel("Residual Plots",
                             plotOutput("resd"))
                    
        
        
       
        
)         
))
)
      # Show a plot of the generated distribution





# Define server logic required to draw a histogram
server <- function(input, output) {
    

output$view<-renderTable({head(ChickWeight, 10)})
output$sum<-renderPrint({summary(ChickWeight)})
output$core<-renderPlot({
    pairs(ChickWeight)})

fit<-reactive({input$vars})

output$mod<-renderPrint({
    if (fit()=="Diet, Chick and Time"){
        fit.1<-lm(weight~Diet+Chick+Time, data=train)
        summary(fit.1)
    }
    else if(fit()=="Time and Diet"){
        fit.2<-lm(weight~Time+Diet, data=train)
        summary(fit.2)
    }
    else if(fit()=="Time"){
        fit.3<-lm(weight~Time, data=train)
        summary(fit.3)
    }
    else if(fit()=="Time and Chick"){
        fit.4<-lm(weight~Time+Chick, data=train)
        summary(fit.4)
        
    }
    else if(fit()=="Diet"){
        fit.5<-lm(weight~Diet, data=train)
        summary(fit.5)
    }
    else if(fit()=="Diet and Chick"){
        fit.6<-lm(weight~Diet+Chick, data=train)
        summary(fit.6)
    }
    else if(fit()=="Chick"){
        fit.7<-lm(weight~Chick, data=train)
        summary(fit.7)
    }
    else if(fit()=="Support Vector Machine"){
        svm.fit<-svm(weight~Time+Diet, data=train, kernel="radial")
        summary(svm.fit)
    }
    
    
})
output$freetext<-renderText({"Root Mean Squared Error"})
output$rmse<-renderPrint({
    if (fit()=="Diet, Chick and Time"){
        fit.1<-lm(weight~Diet+Chick+Time, data=train)
        pred.1<-predict(fit.1, test)
        
        rmse(pred.1, test$weight)
    }
    else if (fit()=="Time and Diet"){
        fit.2<-lm(weight~Diet+Time, data=train)
        pred.2<-predict(fit.2, test)
        
        rmse(pred.2, test$weight)
    }
    else if (fit()=="Time"){
        fit.3<-lm(weight~Time, data=train)
        pred.3<-predict(fit.3, test)
        
        rmse(pred.3, test$weight)
    }
    else if (fit()=="Time and Chick"){
        fit.4<-lm(weight~Chick+Time, data=train)
        pred.4<-predict(fit.4, test)
        
        rmse(pred.4, test$weight)
    }
    else if (fit()=="Diet"){
        fit.5<-lm(weight~Diet, data=train)
        pred.5<-predict(fit.5, test)
        
        rmse(pred.5, test$weight)
    }
    else if (fit()=="Diet and Chick"){
        fit.6<-lm(weight~Diet+Chick, data=train)
        pred.6<-predict(fit.6, test)
        
        rmse(pred.6, test$weight)
    }
    else if (fit()=="Chick"){
        fit.7<-lm(weight~Chick, data=train)
        pred.7<-predict(fit.7, test)
        
        rmse(pred.7, test$weight)
    }
    else if (fit()=="Support Vector Machine"){
        svm.fit<-svm(weight~Time+Diet, data=train, kernel="radial")
        svm.pred<-predict(svm.fit, test)
        
        rmse(svm.pred, test$weight)
    }
})

output$resd<-renderPlot({
    if (fit()=="Diet, Chick and Time"){
        fit.1<-lm(weight~Diet+Chick+Time, data=train)
        par(mfrow=c(2,2))
            plot(fit.1)
        
    }
    else if (fit()=="Time and Diet"){
        fit.2<-lm(weight~Diet+Time, data=train)
        par(mfrow=c(2,2))
        plot(fit.2)
    }
    else if (fit()=="Time"){
        fit.3<-lm(weight~Time, data=train)
        par(mfrow=c(2,2))
        plot(fit.3)
    }
    else if (fit()=="Time and Chick"){
        fit.4<-lm(weight~Chick+Time, data=train)
        par(mfrow=c(2,2))
        plot(fit.4)
    }
    else if (fit()=="Diet"){
        fit.5<-lm(weight~Diet, data=train)
        par(mfrow=c(2,2))
        plot(fit.5)
    }
    else if (fit()=="Diet and Chick"){
        fit.6<-lm(weight~Diet+Chick, data=train)
        par(mfrow=c(2,2))
        plot(fit.6)
    }
    else if (fit()=="Chick"){
        fit.7<-lm(weight~Chick, data=train)
        par(mfrow=c(2,2))
        plot(fit.7)
    }
    else if (fit()=="Support Vector Machine"){
        svm.fit<-svm(weight~Time+Diet, data=train, kernel="radial")
        pred.7<-predict(svm.fit, test)
        plot(pred.7, test$weight)
        abline(a=0, b=1)
    }
})
}

# Run the application 
shinyApp(ui = ui, server = server)


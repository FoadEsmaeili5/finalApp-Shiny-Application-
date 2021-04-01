library(shiny)
library(formattable)
library(flextable)
library(broom)
library(xtable)
library(tableHTML)
library(webshot)
library(rmarkdown)
library(dplyr)
library(ggplot2)
library(shinyBS)
library(shinydashboard)


ui_1sample_t_test=function(id){
  ns=NS(id)
  tagList(
   fluidRow(
     column(
       width = 2,offset = 0,
       uiOutput(ns("selectVar1")),
       numericInput(ns("num2compare"),label = "mean",value = 0),
       verbatimTextOutput(ns("txt")),
       selectInput(inputId = ns("typeTest"),label = "test type",
                   choices = c("two.sided","less","greater"),multiple = F),
       actionButton(ns("submit"),"submit")),
     column(
       width = 6,
       bsCollapsePanel("test output", verbatimTextOutput(ns("modelOut"))),
       bsCollapsePanel("summary of the variable",
                       ui_model2table_and_download(ns("meanTable"),T)
                       ),
       bsCollapsePanel("test table",
                       ui_model2table_and_download(ns("model_2show"),T)
                       )
     ),
     column(width = 4
            # bsCollapsePanel("introducing 1 sample t test",
            #                 tags$iframe(style="height:600px; width:100% ; scrolling:yes",
            #                             src="one-sample-t-test.pdf")
            # )
     )
  )
  )
}
server_1sample_t_test=function(input,output,session,data){
  ns=session$ns
  
  output$selectVar1=renderUI(
    varSelectInput(ns("selectvar1"),label = "variable",data = data(),multiple = F)
  )
  meanVar=eventReactive(input$selectvar1,{
    X=data()%>%
      select(as.character(input$selectvar1))%>%
       summarise_all(list(n="length",mean="mean",variance="var",sd="sd"))
    cbind(variable=as.character(input$selectvar1),X)
  })
  observeEvent(meanVar(),{callModule(server_model2table_and_download,"meanTable",meanVar())})
  Model=eventReactive(input$submit,{
    data()%>%
      select(as.character(input$selectvar1))%>%
    t.test(mu = input$num2compare,alternative = input$typeTest)
  })
  output$modelOut=renderPrint(Model())
  
  observeEvent(Model(),callModule(server_model2table_and_download,"model_2show",Model()))
}


# application -------------------------------------------------------------

# ui=fluidPage(
#   ui_1sample_t_test("foad1")
# )
# server=function(input,output){
#   callModule(server_1sample_t_test,"foad1",iris)
# }
# shinyApp(ui,server)

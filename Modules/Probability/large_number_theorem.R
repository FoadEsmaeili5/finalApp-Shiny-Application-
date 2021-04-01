# source("Modules/Probability/dist_selecter.R")
# 
# source("Modules/Probability/dist_parameters.R")

library(shiny)
library(plotly)
library(shinydashboard)
ui_large_numbar_theorem=function(id){
  ns=NS(id)
  tagList(
  ui_dist_selecter(ns("selecter")),
  ui_dist_parameters(ns("distParameter")),
  sliderInput(ns("n"),label = "Sample size",min=10,max=3e3,value=1e2,animate = T,step = 10),
  valueBox(uiOutput(ns("orderNum")),"Mathematical Expectation",width = "500px"),
  plotlyOutput(ns("plt"))
  #verbatimTextOutput(ns("txt"))
  )
}

server_large_number_theorem=function(input,output,session){
  ns=session$ns
  Dist=reactive(callModule(server_dist_selecter,"selecter"))
  Parameter=eventReactive(Dist(),callModule(server_dist_parameters,"distParameter",Dist(),sliderT=F))
  
  output$orderNum=renderText(
    prettyNum(Parameter()$mean,big.mark = ",")
  )
  
  smpl=reactive({
    n=1:input$n
    sapply(n,function(x){
      set.seed(123)
    mean(frandom(Dist(),params = Parameter(),n = x))
  })
  })
  output$plt=renderPlotly({
    n=1:input$n
    data.frame(n=n,x=smpl())%>%
      plot_ly(x=~n,y=~x)%>%
      add_lines(color=I("black"))%>%
      add_lines(y=I(Parameter()$mean),color=I("red"))
  })
  #output$txt=renderPrint(reactiveValuesToList(Parameter()))
  
}

ui=fluidPage(
  ui_large_numbar_theorem("foad")
)
server=function(input,output){
  callModule(server_large_number_theorem,"foad")
}
shinyApp(ui,server)

# setwd("C:/Users/Foad/OneDrive/book/lessens/home works/applications/R/learning by myselph/shiny/magazin/finalAPP")
# source("Modules/models/model2table_and_download.R")

library(shiny)
library(shinyWidgets)
library(dplyr)
library(htmlTable)

kurtosis=e1071::kurtosis
skew=e1071::skewness

quantile_25=function(x){quantile(x,.25)}
quantile_50=function(x){quantile(x,.50)}
quantile_75=function(x){quantile(x,.75)}

f_group=function(x){
is.character(x)|is.factor(x)
}
ui_data_summary=function(id){
  ns=NS(id)
  tagList(
    uiOutput(ns("selectVal1")),
    uiOutput(ns("selectVal2")),
    actionButton(ns("btn"),label = "submit"),
    verbatimTextOutput(ns("txt")),
    ui_model2table_and_download(ns("foad"))
  )
}

server_data_summary=function(input,output,session,data){
  ns=session$ns
  
  output$selectVal1=renderUI({
    dat=data()%>%select_if(is.numeric)
    selectInput(ns("selectval1"),label = "first parameter",choices=names(dat),multiple = F,selected = 1)
  })
    
  output$selectVal2=renderUI({
    dat=data()%>%select_if(f_group)
    selectInput(ns("selectval2"),label="second parameter",choices=c("none",names(dat)))
  })
  x=eventReactive(input$btn,{
    if(!input$selectval2%in%names(data())){
      data()%>%select(input$selectval1)%>%
        summarise_if(is.numeric,list("mean"="mean","variance"="var","sd"="sd","min"="min","max"="max",
                                     "kurtosis"="kurtosis","skewness"="skew","Q_25"="quantile_25",
                                     "Q_50"="quantile_50","Q_75"="quantile_75"))
    }else{
    data()%>%select(input$selectval1,input$selectval2)%>% group_by_if(f_group)%>%
    summarise_if(is.numeric,list("mean"="mean","variance"="var","sd"="sd","min"="min","max"="max",
                       "kurtosis"="kurtosis","skewness"="skew","Q_25"="quantile_25",
                       "Q_50"="quantile_50","Q_75"="quantile_75"))%>%
        tidyr::gather(key="variable",value="value",input$selectval2)
    }
  })
  xx=eventReactive(input$btn,{
    if(input$selectval2%in%names(data())){
    select(x(),"value","variable",everything())
    }else{
    x()}
  })
  #output$txt=renderPrint(xx())
  observeEvent(xx(),callModule(server_model2table_and_download,"foad",xx()))
 }

# ui=fluidPage(
#   ui_data_summary("dd")
# )
# server=function(input,output){
#   callModule(server_data_summary,"dd",reactive(iris))
# }
# shinyApp(ui,server)

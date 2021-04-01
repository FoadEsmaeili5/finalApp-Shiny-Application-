library(shiny)
library(data.table)
library(foreign)
library(readxl)
library(shinyWidgets)
library(DT)
library(shinydashboard)
library(stringr)

#---
DATA=data(package="datasets")
DATA=DATA$results[,3]
x=str_which(DATA," ")
DATA=DATA[-x]

UIinput=function(id){
  ns=NS(id)
  tagList(
    materialSwitch(inputId = ns("fileOrData"),label = "Import dataset",value = TRUE,right = T,status = "info"),
    
    # inside dataset #----
    conditionalPanel("!input.fileOrData",ns=ns,
                     pickerInput(
                       inputId = ns("datasetPickerInput"),
                       label = "Data within 'datasets' package", 
                       choices =as.list(DATA),multiple = F,selected = "iris",
                       options = list(
                         `live-search` = TRUE)
                     ),
                     actionButton(inputId = ns("okDataPickerInput"),label = "Submit")
    ),
    # end inside dataset #----
    # fileInput as dataset #----
    conditionalPanel("input.fileOrData",ns=ns,
                     
                     radioGroupButtons(inputId = ns("dataType"),label = "Choose data format",
                                       choiceNames =c(".csv",".xlsx",".sav",".txt"),
                                       choiceValues = c("csv","xlsx","sav","txt"),selected = "csv"),
                     
                     
                     conditionalPanel( "input.dataType== 'csv'",ns = ns,
                                       fileInput(inputId = ns("file_csv"),label = "import.csv Data")
                                       
                     ),
                     conditionalPanel("input.dataType=='xlsx'",ns=ns,
                                      fileInput(inputId = ns("file_xlsx"),label = "import.xlsx Data")
                                      
                     ),
                     conditionalPanel("input.dataType=='sav'",ns=ns,
                                      fileInput(inputId = ns("file_sav"),label="import .sav Data")
                     ),
                     conditionalPanel("input.dataType=='txt'",ns=ns,
                                      fileInput(inputId = ns("file_txt"),label = "import .txt Data"),
                                      checkboxInput(ns("HeaderT"),"First row as header names",value = T),
                                      textInput(inputId = ns("Seprator"),label = "Seprator",value = ""),
                                      actionButton(ns(ns("btn")),label = "submit txt Data")
                                      )
                     
                     
    
    # end fileInput as dataset #----
    )
  )
}


serverInput=function(input,output,session){
  # common values #----
  dataa=reactiveValues(Value=NULL,
                       class=NULL)
  
  # end common values #----
  # inside dataset #----
  dataName=reactive(input$datasetPickerInput)
  
  observeEvent(input$okDataPickerInput,{
    data(list=dataName())
    dataa$class=class(get(dataName()))
    dataa$Value=as.data.frame(get(dataName()))
  })
  # end inside dataset #----
  
  # fileInput as dataset #----
  
  # .csv data
  observeEvent(input$file_csv,{
    dataa$Value=read.csv(input$file_csv$datapath,header = T)
    dataa$class=class(dataa$Value)
  })
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # .xlsx data
  observeEvent(input$file_xlsx,{
    dataa$Value=read_xlsx(input$file_xlsx$datapath)
    dataa$class=class(dataa$Value)
  })
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # .sav data 
  observeEvent(input$file_sav,{
    dataa$Value=read.spss(input$file_sav$datapath,to.data.frame = T)
    dataa$class=class(dataa$Value)
  })
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # .txt data
  observeEvent(input$file_txt,{
    dataa$Value=read.table(input$file_txt$datapath,header = input$HeaderT,sep = input$Seprator)
    dataa$class=class(dataa$Value)
  })
  # end fileInput as dataset #----
  # test #----
   output$txt=renderPrint(dataa$Value)
  # end test
  # reuturn #----
  
  return(dataa)
}


# test
ui=fluidPage(
  UIinput("foad"),
  verbatimTextOutput("txt")
)
server=function(input,output){
  Data=callModule(serverInput,"foad")
  output$txt=renderPrint(Data$Value)
}
shinyApp(ui,server)
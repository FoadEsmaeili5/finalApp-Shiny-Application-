library(shiny)
library(dplyr)

ui_selectvar4model=function(id,showFormula=T){
  ns=NS(id)
  tagList(
    if(showFormula){
    checkboxInput(ns("formulaT"),"formula",value = T)
      },
    uiOutput(ns("selectVal1")),
    uiOutput(ns("selectVal2")),
    uiOutput(ns("selectsub1")),
    uiOutput(ns("selectsub2"))
   # actionButton(ns("btn"),"submit")
  )
}
server_selectvar4model=function(input,output,session,data){
  ns=session$ns

  
  output$selectVal1=renderUI(
    selectInput(inputId = ns("selectval1"),label = "1st parameter",choices = 
                  names(data)[which("factor"!=sapply(1:length(data), function(i)class(data[,i])))])
  )
  
  subdata=reactive({ # choices for select variable2
    if(input$formulaT){
      # names(which(unlist(data%>%summarise_all(is.factor|is.character))))
      names(data)[which(sapply(1:length(data), function(i){is.factor(data[,i])|is.character(data[,i])}))]
    }else{
      # names(data)[-(which(unlist(data%>%summarise_all(is.factor|is.character))))]
      names(data)[-which(sapply(1:length(data), function(i){is.factor(data[,i])|is.character(data[,i])} ))]
      
    }
  })
  
  output$selectVal2=renderUI({
    if(length(subdata())>1)
      selectInput(inputId = ns("selectval2"),label = "2nd parameter",choices = subdata())
  })
  inputselectval2=reactive({
    if(length(subdata())>1){
      input$selectval2
    }else{
      subdata()
    }
  })
  
  # select subgroups --------------------------------------------------------
  
  
  subName=reactive({
    names(table(data[inputselectval2()]))
  })
  
  output$selectsub1=renderUI({
    if(input$formulaT)
      selectInput(inputId=ns("selectlevel1"),label="1st level",choices = subName())
  })
  output$selectsub2=renderUI({
    if(input$formulaT)
      selectInput(inputId=ns("selectlevel2"),label="2nd level",choices = subName())
  })


# making formula ----------------------------------------------------------

  newData=reactive({
    # data[which(data[inputselectval2()()]==input$selectlevel1 | data[inputselectval2()()]==input$selectlevel2),]%>%
    #   select(c(input$selectval1,inputselectval2()))
    if(input$formulaT){
    data%>%
      select(c(input$selectval1,inputselectval2()))%>%
      filter(get(inputselectval2())==input$selectlevel1 | get(inputselectval2())==input$selectlevel2)
    }else{
      return(data)
    }
  })
# return part -------------------------------------------------------------
  #return(res.formual)
  res=reactiveValues(var1=ifelse(is.null(input$selectval1),1,input$selectval1),
                     var2=ifelse(is.null(input$selectval2),1,input$selectval2),
                     subval1=ifelse(is.null(input$selectlevel1),1,input$selectlevel1),
                     subval2=ifelse(is.null(input$selectlevel2),1,input$selectlevel2),
                     formulaT=input$formulaT,
                     data=newData(),
                     formula=DF2formula(newData())
                     )

  return(reactiveValuesToList(res))
}
# 
# ui=fluidPage(
#   ui_selectvar4model("foad"),
#   verbatimTextOutput("out")
# )
# 
# server=function(input,output){
#   res=reactive(callModule(server_selectvar4model,"foad",iris))
#   output$out=renderPrint(res())
# }
# shinyApp(ui,server)

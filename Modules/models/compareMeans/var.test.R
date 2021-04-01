#
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
# ui part -----------------------------------------------------------------

ui_var.test=function(id){
  ns=NS(id)
  tagList(
    # input ui partition ------------------------------------------------------
    
    fluidRow(
      column(width = 2,offset = 0,
             checkboxInput(inputId = ns("formulaT"),label = "vs 2nd parameter"),
             #selectInput(ns("sel"),"test",choices = "foad"),
             uiOutput(ns("selectVal1")),
             uiOutput(ns("selectVal2")),
             uiOutput(ns("selectsub1")),
             uiOutput(ns("selectsub2")),
             selectInput(inputId = ns("typeTest"),label = "test type",choices = c("two.sided","less","greater"),multiple = F),
             actionButton(ns("submit"),"submit")),
      column(width = 6,
             bsCollapsePanel("test output", verbatimTextOutput(ns("modelOut"))),
             bsCollapsePanel("mean and variance table", 
                             ui_model2table_and_download(ns("tblmean2show"))
                             # formattableOutput(ns("tblMean")),
                             # actionButton(ns("meantblLatex"),label = "latex Code"),
                             # downloadButton(ns("meantblWord"),label="download as word file"),
                             # actionButton(ns("meantblPNGdown"),label="download as image")
                             ),
             bsCollapsePanel("chart",plotOutput(ns("plt")),
                             actionButton(ns("pltDownload"),label = "download image")),
             bsCollapsePanel("test table",
                             # formattableOutput(ns("tbl")),
                             # actionButton(ns("latexCode"),"latex Code"),
                             # #actionButton(ns("htmlCode"),"HTML Code"),
                             # downloadButton(ns("wordDown"),label = "download as word file"),
                             # downloadButton(ns("pngDown"),label = "download as image")
               ui_model2table_and_download(ns("tbl_model"))
               )
      ),
      column(width = 4
             # bsCollapsePanel("introducing F test"
             # )
      )
      
      # output ui partition -----------------------------------------------------
      
    )
  )
}


# server part ---------------------------------------------------------

server_var.test=function(input,output,session,data){
  ns=session$ns
  
  # select variables --------------------------------------------------------
  
  output$selectVal1=renderUI(
    selectInput(inputId = ns("selectval1"),label = "1st parameter",choices = 
                  names(data())[which("factor"!=sapply(1:length(data()), function(i)class(data()[,i])))])
  )
  
  subdata=reactive({ # choices for select variable2
    if(input$formulaT){
      # names(which(unlist(data%>%summarise_all(is.factor|is.character))))
      names(data())[which(sapply(1:length(data()), function(i){is.factor(data()[,i])|is.character(data()[,i])}))]
    }else{
      # names(data())[-(which(unlist(data()%>%summarise_all(is.factor|is.character))))]
      names(data())[-which(sapply(1:length(data()), function(i){is.factor(data()[,i])|is.character(data()[,i])} ))]
      
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
    names(table(data()[inputselectval2()]))
  })
  
  output$selectsub1=renderUI({
    if(input$formulaT)
      selectInput(inputId=ns("selectlevel1"),label="1st level",choices = subName())
  })
  output$selectsub2=renderUI({
    if(input$formulaT)
      selectInput(inputId=ns("selectlevel2"),label="2nd level",choices = subName())
  })
  
  # defining model ----------------------------------------------------------
  
  
  # new data for model ------------------------------------------------------
  
  newData=reactive({
    # data[which(data()[inputselectval2()()]==input$selectlevel1 | data()[inputselectval2()()]==input$selectlevel2),]%>%
    #   select(c(input$selectval1,inputselectval2()))
    
    data()%>%
      select(c(input$selectval1,inputselectval2()))%>%
      filter(get(inputselectval2())==input$selectlevel1 | get(inputselectval2())==input$selectlevel2)
  })
  
  model=eventReactive(input$submit,{
    if(!input$formulaT){
      var.test(data()%>%
               select(input$selectval1)%>%
                 unlist(),
             data()%>%
               select(input$selectval2)%>%
               unlist(),
             alternative = input$typeTest)
    }else{
      var.test(newData()%>%
               DF2formula(),data=newData(),na.rm=T,alternative = input$typeTest)
    }
  })
  
  
  # plot for mean -----------------------------------------------------------
  
  observeEvent(input$submit,{
    output$plt=renderPlot({
      if(!input$formulaT){
        data()%>%
          select(c(input$selectval1,inputselectval2()))%>%
          reshape2::melt()%>%
          ggplot(aes(variable,value))+geom_boxplot()
      }else{
        ggplot(newData(),aes_string(inputselectval2(),input$selectval1))+geom_boxplot()
      }
    })
  })
  
  
  # show the R model output -------------------------------------------------
  
  output$modelOut=renderPrint({
    model()
    # data%>%
    #   select(c(input$selectval1,inputselectval2()))%>%
    #   filter(inputselectval2()==as.character(input$selectlevel1))
  })
  
  # mean table --------------------------------------------------------------
  
  tblmean=reactive({
    if(input$formulaT){
      newData() %>%
               group_by(group=get(inputselectval2()))%>%
               summarise_at(input$selectval1,list(mean=mean,variance=var),na.rm=T)
    }else{
      data()%>%
               select(c(input$selectval1,inputselectval2()))%>%
               summarise_all(list(mean= mean,variance= var))
    }
  })
  observeEvent(input$submit,callModule(server_model2table_and_download,"tblmean2show",tblmean()))
  # observeEvent(input$submit,{
  #   output$tblMean= renderFormattable({
  #     formattable(tblmean())
  #   })
  # })
  # 
  # output$meantblWord=downloadHandler(
  #   filename = function(){
  #     paste("mean_table",Sys.Date(),".docx",sep = "")
  #   },
  #   content = function(file){
  #     doc=officer::read_docx()
  #     doc=body_add_flextable(x = doc,value = flextable(tblmean()))
  #     print(doc,target = file)
  #   }
  # )
  # 
  # output$meantblPNGdown=downloadHandler(
  #   filename = function(){
  #     paste("data-",Sys.Date(),".png",sep="")
  #   },
  #   content = function(file){
  #     tableHTML_to_image(tableHTML =tableHTML(tblmean(),round = 4,class = "myClass"),file = file,type = "png" )
  #   }
  # )
  # 
  # # mean table download buttons ---------------------------------------------
  # observeEvent(input$meantblLatex,{
  #   showModal(modalDialog(
  #     title="table Latex Code ",
  #     verbatimTextOutput(ns("meanlatextxt"))
  #   )
  #   )
  # })
  # output$meanlatextxt=renderPrint(
  #   print(tblmean())
  # )
  
  # making tables for output ------------------------------------------------
  
  observeEvent(input$submit,callModule(server_model2table_and_download,"tbl_model",model()))
  # Xtab=reactive({
  #   xtable(tidy(model()))
  # })
  # flextab=reactive(
  #   flextable(Xtab())
  # )
  # output$tbl=renderFormattable(
  #   formattable(Xtab())
  # )
  # 
  # # latex ouptut ------------------------------------------------------------
  # 
  # observeEvent(input$latexCode,{
  #   showModal(modalDialog(
  #     title="table Latex Code ",
  #     verbatimTextOutput(ns("latextxt"))
  #   )
  #   )
  # })
  # 
  # output$latextxt=renderPrint(
  #   print.xtable(Xtab())
  # )
  # 
  # # html output -------------------------------------------------------------
  # 
  # observeEvent(input$htmlCode,{
  #   showModal(modalDialog(
  #     title = "table HTML Code",
  #     verbatimTextOutput(ns("htmltxt"))
  #   ))
  # })
  # 
  # output$htmltxt=renderPrint(
  #   formattable(Xtab())
  # )
  # 
  # # word table download -----------------------------------------------------
  # 
  # output$wordDown=downloadHandler(
  #   filename = function(){
  #     #tempfile(fileext = ".docx")
  #     paste("data-",Sys.Date(),".docx",sep="")
  #   },
  #   content = function(file){
  #     doc=officer::read_docx()
  #     doc=body_add_flextable(x = doc,value = flextab())
  #     print(doc,target = file)
  #   }
  # )
  # 
  # # png table download ------------------------------------------------------
  # 
  # output$pngDown=downloadHandler(
  #   filename = function(){
  #     paste("data-",Sys.Date(),".png",sep="")
  #   },
  #   content = function(file){
  #     tableHTML_to_image(tableHTML = tableHTML(Xtab(), round = 4, class = "myClass"), file = file,type = "png" )
  #   }
  # )
}

# end of the module -------------------------------------------------------

# 
# ui=dashboardPage(header = dashboardHeader(),sidebar = dashboardSidebar(),body = dashboardBody(
#   ui_var.test("foad")
# ))
# server=function(input,output){
#   callModule(server_var.test,"foad",iris)
# }
# 
# shinyApp(ui,server)

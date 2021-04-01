library(shiny)
library(xtable)
library(broom)
library(formattable)
library(flextable)
library(tableHTML)
library(gridExtra)
ui_model2table_and_download=function(id,ButtonsShow=T){
  ns=NS(id)
  tagList(
    fluidRow(
       formattableOutput(ns("tbl"))
    ),
    if(ButtonsShow){
    uiOutput(ns("btns"))}
    )
}
server_model2table_and_download=function(input,output,session,model){
  ns=session$ns
  # output=session$output
  
  output$btns=renderUI(
    flowLayout(
      actionButton(ns("latexCode"),"Latex code"),
      downloadButton(ns("wordDown"),"download as .docx"),
      downloadButton(ns("pdfDown"),"download as .pdf"),
      downloadButton(ns("pngDown"),"download as .png"),
      downloadButton(ns("jsonDown"),"download as .json")
    )
  )
  
  output$txt=renderPrint(
    model
  )
  tidyModel=reactive({
    if(is.data.frame(model)){
      model
    }else{
      tidy(model)
    }
    })
  Xtab=reactive({
    xtable(tidyModel())
  })
  flextab=reactive(
    flextable(Xtab())
  )
  # showing table
  output$tbl=renderFormattable(
    formattable(Xtab())
  )

  # latex ouptut ------------------------------------------------------------

  observeEvent(input$latexCode,{
    showModal(modalDialog(
      title="table Latex Code ",
      verbatimTextOutput(ns("latextxt"))
    )
    )
  })

  output$latextxt=renderPrint(
    print.xtable(Xtab())
  )

  # # word table download -----------------------------------------------------

  output$wordDown=downloadHandler(
    filename = function(){
      #tempfile(fileext = ".docx")
      paste("data-",Sys.Date(),".docx",sep="")
    },
    content = function(file){
      doc=officer::read_docx()
      doc=body_add_flextable(x = doc,value = flextab())
      print(doc,target = file)
    }
  )

  # png table download ------------------------------------------------------

  output$pngDown=downloadHandler(
    filename = function(){
      paste("data-",Sys.Date(),".png",sep="")
    },
    content = function(file){
      png(file,height = 50*nrow(tidyModel()),width = 200*ncol(tidyModel()))
      grid.table(tidyModel())
      dev.off()
      # tableHTML_to_image(tableHTML = tableHTML(Xtab(), round = 4, class = "myClass"), file = file,type = "png" )
    }
  )
  output$pdfDown=downloadHandler(
    filename = function(){
      paste("data-",Sys.Date(),".pdf",sep="")
    },
    content = function(file){
      pdf(file,height = 11,width = 10)
      grid.table(tidyModel())
      dev.off()
      # tableHTML_to_image(tableHTML = tableHTML(Xtab(), round = 4, class = "myClass"), file = file,type = "png" )
    }
  )
  
  output$jsonDown=downloadHandler(
    filename = function(){
      paste("data",Sys.Date(),".json",sep="")
    },
    content = function(file){
      jsonlite::write_json(tidyModel(),file)
    }
  )
}




# ui=fluidPage(
#   h4("compare means"),
#   ui_model2table_and_download("foad1",T),
#   h4("regression models \n"),
#   ui_model2table_and_download("foad2",T),
#   h4("anova table \n"),
#   ui_model2table_and_download("foad3",T)
# )
# server=function(input,output){
#   model1=with(iris,t.test(Sepal.Length,Sepal.Length))
#   callModule(server_model2table_and_download,"foad1",model1)
#   
#   model2=lm(Sepal.Length~Petal.Length,data=iris)
#   callModule(server_model2table_and_download,"foad2",model2)
#   
#   model3=aov(Sepal.Length~Species,data=iris)
#   callModule(server_model2table_and_download,"foad3",model3)
# }
# shinyApp(ui,server)
# 

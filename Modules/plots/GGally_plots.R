
library(shiny)
library(ggplot2)
library(GGally)
library(shinycssloaders)
library(shinyBS)
library(shinydashboardPlus)
ui_GGpairs=function(id){
  ns=NS(id)
  tagList(
    fluidRow(

# input page --------------------------------------------------------------

      # input page
      column(width = 2,
             verticalLayout(
          # column1 select inputs
          verticalLayout( h3("Matrix plots"),
                          br(),
                          #h3("Main selectors"),
            uiOutput(ns("slider")),
            uiOutput(ns("color"))
            ),
          # column2 select inputs
          bsCollapsePanel("Upper triangular", 
            selectInput(ns("continuesup"),label = "Continues vs Continues",choices = c(
              "points","smooth","density","cor","blank"
              )),
            selectInput(ns("comboup"),label="Categorical vs Continues",choices = c(
              "box","dot","facethist","facetdensity","denstrip","blank"
              )),
            selectInput(ns("discreteup"),label = "Categorical vs Categorical",choices = c(
              "facetbar","ratio","blank"
              ))),
          # column3 select inputs 
          bsCollapsePanel("Lower triangular", 
            selectInput(ns("continuesdown"),label = "Continues vs Continues",choices = c(
              "points","smooth","density","cor","blank"
            )),
            selectInput(ns("combodown"),label="Categorical vs Continues",choices = c(
              "box","dot","facethist","facetdensity","denstrip","blank"
            )),
            selectInput(ns("discretedown"),label = "Categorical vs Categorical",choices = c(
              "facetbar","ratio","blank")
              )),
          bsCollapsePanel("Diagonal", 
                           selectInput(ns("continuesdiag"),label = "Continues vs Continues",choices = c(
                             "densityDiag","barDiag","blankDiag"
                           )),
                           selectInput(ns("discretediag"),label = "Categorical vs Categorical",choices = c(
                             "barDiag","blankDiag")
                           )
            )
            # uiOutput(ns("Xselect")),
            # uiOutput(ns("Yselect"))
            )
          ),

# output page -------------------------------------------------------------

      column(width = 10,
          bsCollapse(
            bsCollapsePanel(title = "Correlation matrix",
                            withSpinner(plotOutput(ns("corMatrix")))
                            ),
            bsCollapsePanel(title = "Matrix plot",
                            withSpinner(plotOutput(ns("plt")))
                            )
            ),
          bsCollapsePanel(title = "Specific Panel",
                          flowLayout(
                            uiOutput(ns("Xselect")),
                            uiOutput(ns("Yselect"))
                          ),
                         withSpinner(plotOutput(ns("choosedPlot"))
                                     )
                          )
          )
    )
  )
}
server_GGpairs=function(input,output,session,Data){
  ns=session$ns
  output$slider=renderUI({
    varSelectInput(ns("var2show"),label = "Variables to be included:",data = Data(),multiple = T,selected = names(Data())[1:3])
  })
  output$color=renderUI({
    selectInput(ns("Color"),label="With respect to:",choices = 
                  c("none",names(Data())[sapply(1:ncol(Data()), function(i)is.factor(as.data.frame(Data())[,i])|is.character(as.data.frame(Data())[,i]) )]),
                multiple = F)
  })
  
  col2show=reactive({
    which(names(Data())%in%unlist(input$var2show))
  })
  output$Xselect=renderUI({
    selectInput(ns("xSel"),label="row",choices = seq(length(col2show())))
  })
  output$Yselect=renderUI({
    selectInput(ns("ySel"),label="column",choices = seq(length(col2show())))
  })
  Plot=reactive({
    if(!input$Color%in%names(Data())){
     ggpairs(Data(),columns = col2show(),upper =
               list(continuous = input$continuesup, combo = input$comboup,discrete=input$discreteup),lower = 
               list(continuous = input$continuesdown, combo = input$combodown,discrete=input$discretedown),diag = 
               list(continuous = input$continuesdiag,discrete=input$discretediag)
             )
      }else{
      ggpairs(Data(),columns = col2show(),mapping = aes_string(color=input$Color),upper =
                list(continuous = input$continuesup, combo = input$comboup,discrete=input$discreteup),lower = 
                list(continuous = input$continuesdown, combo = input$combodown,discrete=input$discretedown),diag = 
                list(continuous = input$continuesdiag,discrete=input$discretediag)
              )
        }
  })
  output$corMatrix=renderPlot({
    ggcorr(Data()[,sapply(1:ncol(Data()), function(i)!is.factor(Data()[,i]))],label = T)
  })
  output$plt=renderPlot({
    Plot()
  })
  output$choosedPlot=renderPlot({
    getPlot(Plot(),str2lang(input$xSel),str2lang(input$ySel))
  })
}

# 
ui=fluidPage(
  ui_GGpairs("foad")
)
server=function(input,output){
  callModule(server_GGpairs,"foad",iris)
}
shinyApp(ui,server)



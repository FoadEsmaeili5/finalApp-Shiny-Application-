library(shiny)
library(plotly)
library(esquisse)
library(dplyr)
library(grDevices)

f_as.numeric=function(x){
  if(is.null(x)){
    NULL
  }else{
    as.numeric(x)
  }
}
ui_Single_plotly_plots=function(id){
  ns=NS(id)
  tagList(
    # tabsetPanel(
    #   tabPanel("html plot",
  uiOutput(ns("dragula")),
  flowLayout(
  selectInput(ns("pltType"),label = "Plot Type",choices = 
                c('scatter', 'bar', 'box', 'heatmap', 'histogram',
                  'histogram2d', 'histogram2dcontour', 'contour',
                  'violin', 'waterfall', 'scatter3d','mesh3d',
                   'scattergl', 'pointcloud','heatmapgl', 'area'),
              selected = 'scatter',multiple = F
                  ),
  selectInput(ns("cl"),label = "colors",choices =
                c("black","gray","red","orange","yellow","green","blue"),multiple = T),
  checkboxInput(ns("jitter"),label = "jittering",value = F),
  sliderInput(ns("alphaSlider"),label = "Opacity",min = .05,max=1,value=1),
  downloadButton(ns("down2json"),label = "download as .json")
  ),plotlyOutput(ns("plt"),height = "600px")
  )
  
  # tabPanel("ggplot",
  #          div(style="height:700px ;",
  #   esquisserUI(id = ns("ggplot_plot"),header = F,choose_data = F)
  # )
  # )
  #   )
  #)
}

server_Single_plotly_plots = function(input,output,session,data){
  ns=session$ns
  
  output$dragula=renderUI(
    dragulaInput(inputId = ns("Data_input"),
                 sourceLabel = "Variables",
                 targetsLabels = 
                   c("X", "Y","Z","size", "Color", "point Type",
                     "Line Type", "Split","frame"
                     
                   ),
                 targetsIds = 
                   c("x", "y","z","size", "color", "pointType",
                     "lineType", "split","frame"
                     
                   ),
                 choices = names(data()), replace = TRUE
    )
  )
  x=reactive({
    x=data()%>%
      select(x=input$Data_input$target$x)
    if(input$jitter){
      as.data.frame(jitter(x$x))
    }else{
      x
    }
  })
  y=reactive({
    x=data()%>%
      select(y=input$Data_input$target$y)
    if(input$jitter){
      as.data.frame(jitter(x$y))
    }else{x}
  })
  z=reactive({
    x=data()%>%
      select(y=input$Data_input$target$z)
    if(input$jitter){
      as.data.frame(x$z)
    }else{x}
  })
  # va=reactive({
  #   data%>%count(input$Data_input$target$value)%>%select(n)
  #   
  # })
  size=reactive({
    data()%>%
      select(size=input$Data_input$target$size)#%>%mutate(size=as.numeric(size))
  })
  
  
  color=reactive({
    data()%>%
      select(color=input$Data_input$target$color)
  })
  pointType=reactive({
    data()%>%
      select(color=input$Data_input$target$pointType)
  })
  lineType=reactive({
    data()%>%
      select(color=input$Data_input$target$lineType)
  })
  
  split=reactive({
    data()%>%
      select(color=input$Data_input$target$split)
  })
  
  frame=reactive({
    data()%>%
      select(color=input$Data_input$target$frame)
  })
  #ccl=reactive({input})
  cl=reactive({
    if(is.vector(input$cl)){
    colorRampPalette(input$cl)(12)
    }else{
    colorRampPalette(c("red","green","blue"))(12)
    }
  })
  p=reactive({
    plot_ly(x=unlist(x()),y=unlist(y()),z=unlist(z()),
            color=unlist(color()),colors = cl(),
            symbol = unlist(pointType()),
            linetype = unlist(lineType()),split = unlist(split()),
            frame=unlist(frame()),alpha = input$alphaSlider,
            type = input$pltType,size = f_as.numeric(unlist(size())))
  })
  output$plt=renderPlotly({
    p()
  })
   output$down2json=downloadHandler(
           filename = function(){
                        paste("plot",Sys.Date(),".json")},
           content = function(file){
                        write(plotly_json(p(),jsonedit = F),file)
             }
  )
  
  
  # data()_a=reactiveValues(data=data,name="class")
  # callModule(esquisserServer,"ggplot_plot",data=data_a)
}
ui=fluidPage(
  ui_Single_plotly_plots("foad")
)
server=function(input,output){
  callModule(server_Single_plotly_plots,"foad",reactive(iris))
 }
shinyApp(ui,server)







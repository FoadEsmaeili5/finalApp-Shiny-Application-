library(shiny)

ui_norm_t_cauchy=function(id){
  ns=NS(id)
  tagList(
  sliderInput(inputId =ns("df4tdist"),"df for t distribution",min=.25,max=100,value=.5,animate = T,step = .5,),
  #sliderInput(inputId = ns("df4cauchy"),"df for cauchy dist",min=.1,max=30,value = 1,animate = T,step = .1),
  plotOutput(ns("plt"),height = "600px")
  )
}
server_norm_t_cauchy=function(input,output,session){
  output$plt=renderPlot({
    par(cex=2,lwd=2)
    curve(dnorm(x),-4,4)
    curve(dt(x,df = input$df4tdist),-4,4,add=T,col="red")
    curve(dcauchy(x,scale = 1),add=T,col="blue")
    legend("topright",c("standard normal","t student","cauchy"),text.col = c("black","red","blue"),text.width = 2)
  })
}
ui=fluidPage(
  ui_norm_t_cauchy("foad")
)
server=function(input,output){
  callModule(server_norm_t_cauchy,"foad")
}
shinyApp(ui,server)
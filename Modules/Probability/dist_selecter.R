# names of the distributions:
#options(encoding = "UTF-8")
library(shiny)
ui_dist_selecter=function(id){
  ns=NS(id)
  tagList(
    selectInput(inputId = ns("inp"),label = "choose distribution",choices = list(
      "Continues"=list(
      "Beta"="beta",
      "Cauchy"="cauchy",
      "Chisquare"="chisq",
      "Exponential"="exp",
      "Fisher"="f",
      "Gamma"="gamma",
      "Log normal"="lnorm",
      "Normal"="norm",
      "T student"="t",
      "Uniform"="unif",
      "Weibull"="weibull"),
      "Discrete"=list(      
      "Binomial"="binom",
      "Geometric"="geom",
      "Hyper Geometric"="hyper",
      "Negative Binomial"="nbinom",
      "Poisson"="pois")
    ))
  )
}
server_dist_selecter=function(input,output,session){
  return(input$inp)
}
# 
# ui=fluidPage(
#   ui_dist_selecter("foad")
# )
# server=function(input,output){
#  reactive( callModule(server_dist_selecter,"foad"))
# }
# shinyApp(ui,server)
library(shiny)

# source("Modules/Probability/dist_selecter.R")
# source("Modules/Probability/dist_parameters.R")
# source("Modules/Rscripts/dist_curve.R")
# source("Modules/Rscripts/dist_polygon.R")

library(dplyr)
frandom=function(dens,params,n){
  #n=n*100
# switch(dens,
#        "beta" = rbeta(n,params$par1,params$par2),
#        "cauchy"=rcauchy(n,params$par1,params$par2),
#        "chisq"=rchisq(n,params$par1),
#        "exp"=rexp(n,params$par1),
#        "f"=rf(n,params$par1,params$par2),
#        "gamma"=rgamma(n,params$par1,params$par2),
#        "lnorm"=rlnorm(n,params$par1,params$par2),
#        "norm"=rnorm(n,params$par1,params$par2),
#        "t"=rt(n,params$par1),
#        "unif"=runif(n,params$par1,params$par2),
#        "weibull"=rweibull(n,params$par1,params$par2)
# )
  dens=paste("r",dens,sep="")
  if(is.null(params$par2)){
    eval(call(dens,n,params$par1))
  }else{if(is.null(params$par3)){
    eval(call(dens,n,params$par1,params$par2))
  }else{
    eval(call(dens,n,params$par1,params$par2,params$par3))
  }}
}
ui_central_limit_theorem=function(id){
  ns=NS(id)
  tagList(
  fluidPage(
   ui_dist_selecter(ns("Dist")),
   ui_dist_parameters(ns("sliders")),
    uiOutput(ns("Sliders")),
  sliderInput(ns("nn"),"Sample Size",min = 1,max=5e2,value=1e2,animate = T,step = 5),
  verbatimTextOutput(ns("txt")),
  fluidRow(
    column(width=6,
  plotOutput(ns("meanplt"),height = "500px")
    ),
  column(width=6,
         plotOutput(ns("orginplt"),height = "500px")
         )
  )
))}
server_central_limit_theorem=function(input,output,session){
  ns=session$ns
  dist=reactive(callModule(server_dist_selecter,"Dist"))
  params=eventReactive(dist(),callModule(server_dist_parameters,"sliders",dist(),sliderT=F))
  
  param=reactive(reactiveValuesToList(params()))
  
  x=reactive({
    frandom(dist(),param(),n=input$nn*100)%>%
      matrix(nrow=100)
    })
  xx=reactive({
    (rowMeans(x())-param()$mean)/(sqrt(param()$variance/input$nn))
  })
  output$meanplt=renderPlot({
    hist(xx(),freq = F,main="Mean",breaks = "scott",ylim=c(0,1))
     curve(dnorm(x,mean=0,sd=1),add=T)
    })
  # output$txt=renderPrint(x())
  
  output$orginplt=renderPlot({
    if(param()$discrete){
      barplot(table(x()))
    }else{
    hist(x(),freq = F,breaks = "scott",main = "Origin")
    dist_Curve(dist(),params = param(),add=T)
    }
  })
}

# ui=fluidPage(ui_central_limit_theorem("foad"))
# server=function(input,output){
#   callModule(server_central_limit_theorem,"foad")
# }
# #
# shinyApp(ui,server)

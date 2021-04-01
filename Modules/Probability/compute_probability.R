library(shiny)
library(latex2exp)
 # setwd("C:/Users/Foad/OneDrive/book/lessens/home works/applications/R/learning by myselph/shiny/magazin/finalAPP")
 # 
 #  source("Modules/Probability/dist_selecter.R")
 #  source("modules/Probability/dist_parameters.R")
 #  source("Modules/Rscripts/dist_curve.R")
 #  source("Modules/Rscripts/dist_polygon.R")
 #functions ---------------------------------------------------------------

f_prob=function(dens,n,params){
  # switch(dens,
  #        "dbeta" = pbeta(n,params$par1,params$par2),
  #        "dcauchy"=pcauchy(n,params$par1,params$par2),
  #        "dchisq"=pchisq(n,params$par1),
  #        "dexp"=pexp(n,params$par1),
  #        "df"=pf(n,params$par1,params$par2),
  #        "dgamma"=pgamma(n,params$par1,params$par2),
  #        "dlnorm"=plnorm(n,params$par1,params$par2),
  #        "dnorm"=pnorm(n,params$par1,params$par2),
  #        "dt"=pt(n,params$par1),
  #        "dunif"=punif(n,params$par1,params$par2),
  #        "dweibull"=pweibull(n,params$par1,params$par2)
  # )
  dens=paste("p",dens,sep="")
  if(is.null(params$par2)){
    eval(call(dens,n,params$par1))
  }else{if(is.null(params$par3)){
    eval(call(dens,n,params$par1,params$par2))
  }else{
    eval(call(dens,n,params$par1,params$par2,params$par3))
  }}
}
f_d=function(dens,n,params){
  dens=paste("d",dens,sep="")
  if(is.null(params$par2)){
    eval(call(dens,n,params$par1))
  }else{if(is.null(params$par3)){
    eval(call(dens,n,params$par1,params$par2))
  }else{
    eval(call(dens,n,params$par1,params$par2,params$par3))
  }}
}

# application -------------------------------------------------------------

ui_compute_probability=function(id){
ns=NS(id)
  tagList(
fluidPage(
  ui_dist_selecter(ns("dist_select")),
  ui_dist_parameters(ns("params")),
  fluidRow(
  column(width = 6,
         numericInput(ns("num"),label = "computing probability",min=-50,max=50,value=0),
         textOutput(ns("txtFor")),
         verbatimTextOutput(ns("ansprob1")),
         plotOutput(ns("plt")),
         plotOutput(ns("plt3"))),
  column(width = 6,
         numericInput(ns("num1"),label="upper number",min=-50,max=50,value=0),
         numericInput(ns("num2"),label="lower number",min=-50,max=50,value = 0),
         verbatimTextOutput(ns("ansprob2")),
         plotOutput(ns("plt2")),
         plotOutput(ns("plt4"))
         )
  )
)
)
}
server_compute_probability=function(input,output,session){
  ns=session$ns
  dist=reactive(callModule(server_dist_selecter,"dist_select"))
  p=eventReactive(dist(),callModule(server_dist_parameters,"params",dist()))
  parameters=reactive(reactiveValuesToList(p()))
  Number2compute=reactive(req(input$num,cancelOutput = T))
  output$txtFor=renderPrint(noquote(paste0("F(",input$num,")=")))
  Ansprob1=reactive({f_prob(dist(),Number2compute(),parameters())})
  output$ansprob1=renderPrint(Ansprob1())
  # probabilitiy distribution
  output$plt=renderPlot({
    
    dist_Curve(dist(),parameters(),ylab="",xlab="x")
    dist_polygon(dist(),parameters(),Number2compute())
  })
  prob1=reactive({f_prob(dist(),input$num1,parameters())})
  prob2=reactive({f_prob(dist(),input$num2,parameters())})
  Ansprob2=reactive(
    if(input$num1>=input$num2){
      if(parameters()$discrete){
        if(input$num1==input$num2){
          f_d(dist(),input$num1,parameters())
        }else{
          f_prob(dist(),input$num1,parameters()) - f_prob(dist(),(input$num2-1),parameters())
        }
    }else{
    prob1()-prob2()
    }
  }else{
      stop(safeError(
        paste0("Number1 must be greater than number2")
      ))
    }
      )
  output$ansprob2=renderPrint(
   Ansprob2()
  )
  output$plt2=renderPlot( # probability distribution
    if(input$num1>=input$num2){
      if(!parameters()$discrete)
    dist_Curve(dist(),parameters(),ylab="",xlab="x")
        dist_polygon(dens = dist(),params = parameters(),xxmax = input$num1,xxmin = input$num2) 
    }else{
      stop(safeError(
        paste0("Number1 must be greater than number2")
      ))
    }
  )
  output$plt3=renderPlot({
    dist_Curve(dist(),parameters(),pdf=F,ylab="",xlab="x")
    points(input$num,Ansprob1(),col="red",pch=18,cex=2)
    segments(x0=c(input$num,parameters()$xmin),x1=c(input$num,input$num),y0=c(0,Ansprob1()),
             y1=c(Ansprob1(),Ansprob1()),lwd=1,lty=2,col="red")
    #segments(x0=parameters()$xmin,x1=input$num,y0=Ansprob1(),y1=Ansprob1(),lwd=1,lty=2,col = "red")
  })
  
  output$plt4=renderPlot({
    if(input$num1>=input$num2){
    dist_Curve(dist(),parameters(),pdf=F,ylab="",xlab="x")
    points(c(input$num1,input$num2),c(prob1(),prob2()),col="red",pch=18,cex=2)
    segments(x0=c(input$num1,parameters()$xmin),x1=c(input$num1,input$num1),y0=c(0,prob1()),
             y1=c(prob1(),prob1()),lwd=1,lty=2,col="red")
    segments(x0=c(input$num2,parameters()$xmin),x1=c(input$num2,input$num2),y0=c(0,prob2()),
             y1=c(prob2(),prob2()),lwd=1,lty=2,col="red")
    #abline(v=c(input$num1,input$num2),h=c(prob1(),prob2()),lwd=1,col="red",lty=2)
  }else{
    stop(safeError(
      paste0("Number1 must be greater than number2")
    ))
  }
  })
}

ui=fluidPage(ui_compute_probability("foad"))
server=function(input,output){
  callModule(server_compute_probability,"foad")
}
shinyApp(ui,server)



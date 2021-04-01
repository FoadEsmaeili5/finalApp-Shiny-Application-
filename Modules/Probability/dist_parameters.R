library(shiny)

ui_dist_parameters=function(id){
  ns=NS(id)
  tagList(
    uiOutput(ns("xscale")),
    uiOutput(ns("param1")),
    uiOutput(ns("param2")),
    uiOutput(ns("param3"))
  )
}

server_dist_parameters=function(input,output,session,dens,probshow=T,sliderT=T){
  ns=session$ns
  xScales=reactiveValues(min=NULL,max=NULL)
  par1Scale=reactiveValues(min=NULL,max=NULL)
  par2Scale=reactiveValues(min=NULL,max=NULL)
  par3Scale=reactiveValues(min=NULL,max=NULL)
  distParam=reactiveValues(mean=NULL,variance=NULL)
  # xScale ------------------------------------------------------------------
  
  observe({
    if(probshow){
      switch (dens,
              "beta"={xScales$min=-50; xScales$max=50},
              "cauchy"={xScales$min=-50; xScales$max=50},
              "chisq"={xScales$min=-50; xScales$max=50},
              "exp"={xScales$min=-50; xScales$max=50},
              "f"={xScales$min=-50; xScales$max=50},
              "gamma"={xScales$min=-50; xScales$max=50},
              "lnorm"={xScales$min=-50; xScales$max=50},
              "norm"={xScales$min=-50; xScales$max=50},
              "t"={xScales$min=-50; xScales$max=50},
              "unif"={xScales$min=-50; xScales$max=50},
              "weibull"={xScales$min=-50; xScales$max=50},
              
              "binom"={xScales$min=0; xScales$max=50},
              "geom"={xScales$min=0; xScales$max=50},
              "hyper"={xScales$min=0; xScales$max=50},
              "nbinom"={xScales$min=0; xScales$max=50},
              "pois"={xScales$min=0; xScales$max=50}
              
      )
    }else{
      switch (dens,
              "beta"={xScales$min=-5; xScales$max=5},
              "cauchy"={xScales$min=-5; xScales$max=5},
              "chisq"={xScales$min=-5; xScales$max=5},
              "exp"={xScales$min=-5; xScales$max=5},
              "f"={xScales$min=-5; xScales$max=5},
              "gamma"={xScales$min=-5; xScales$max=5},
              "lnorm"={xScales$min=-5; xScales$max=5},
              "norm"={xScales$min=-5; xScales$max=5},
              "t"={xScales$min=-5; xScales$max=5},
              "unif"={xScales$min=-5; xScales$max=5},
              "weibull"={xScales$min=-5; xScales$max=5},
              
              "binom"={xScales$min=0; xScales$max=50},
              "geom"={xScales$min=0; xScales$max=50},
              "hyper"={xScales$min=0; xScales$max=50},
              "nbinom"={xScales$min=0; xScales$max=50},
              "pois"={xScales$min=0; xScales$max=50}
      )}
  })
  
  
  # par1Scale ---------------------------------------------------------------
  
  observe({
    switch (dens,
            "beta"={par1Scale$min=0; par1Scale$max=50},
            "cauchy"={par1Scale$min=-50; par1Scale$max=50},
            "chisq"={par1Scale$min=0; par1Scale$max=50},
            "exp"={par1Scale$min=0; par1Scale$max=50},
            "f"={par1Scale$min=0; par1Scale$max=50},
            "gamma"={par1Scale$min=0; par1Scale$max=50},
            "lnorm"={par1Scale$min=-50; par1Scale$max=50},
            "norm"={par1Scale$min=-50; par1Scale$max=50},
            "t"={par1Scale$min=0.001; par1Scale$max=50},
            "unif"={par1Scale$min=-50; par1Scale$max=50},
            "weibull"={par1Scale$min=0; par1Scale$max=50},
            
            "binom"={par1Scale$min=0; par1Scale$max=50},#n
            "geom"={par1Scale$min=0; par1Scale$max=1},#
            "hyper"={par1Scale$min=0; par1Scale$max=50},
            "nbinom"={par1Scale$min=0; par1Scale$max=1},
            "pois"={par1Scale$min=0; par1Scale$max=50}
    )
  })
  
  # par2Scale ---------------------------------------------------------------
  
  observe({
    switch (dens,
            "beta"={par2Scale$min=0; par2Scale$max=5},
            "cauchy"={par2Scale$min=0.001; par2Scale$max=20},
            "chisq"={par2Scale$min=NULL; par2Scale$max=NULL},
            "exp"={par2Scale$min=NULL; par2Scale$max=NULL},
            "f"={par2Scale$min=0.001; par2Scale$max=20},
            "gamma"={par2Scale$min=0; par2Scale$max=20},
            "lnorm"={par2Scale$min=0.001; par2Scale$max=5},
            "norm"={par2Scale$min=0.001; par2Scale$max=5},
            "t"={par2Scale$min=NULL; par2Scale$max=NULL},
            "unif"={par2Scale$min=(input$par1+0.01); par2Scale$max=(input$par1+20)},
            "weibull"={par2Scale$min=0; par2Scale$max=20},
            #-------------------------------------------
            "binom"={par2Scale$min=0; par2Scale$max=1},
            "geom"={par2Scale$min=NULL; par2Scale$max=NULL},
            "hyper"={par2Scale$min=0; par2Scale$max=50},
            "nbinom"={par2Scale$min=0; par2Scale$max=1},
            "pois"={par2Scale$min=NULL; par2Scale$max=NULL}
    )
  })
  observe({
    if(dens=="hyper"){par3Scale$min=0;par3Scale$max=50}else{
      par3Scale$min=NULL;par3Scale$max=NULL
    }
  })
  

# define variance and mean of the distributions ---------------------------
Par1=reactive(input$par1)
Par2=reactive(input$par2)
Par3=reactive(input$par3)
  observe({
    switch (dens,
            "beta"={
              distParam$mean={Par1()/(Par1()+Par2())}
             distParam$variance=(Par1()*Par2())/((Par1()+Par2()+1)*(Par1()+Par2())^2)},
            "cauchy"={distParam$mean=NULL; distParam$variance=NULL},
            "chisq"={distParam$mean=NULL; distParam$variance=NULL},
            "exp"={distParam$mean=1/Par1(); distParam$variance=1/Par1()^2},
            "f"={distParam$mean={
              Par2()/(Par2()-2)
            }; distParam$variance={
              2*Par2()^2*(Par1()+Par2()-2)/(Par1()* (Par2()-2)^2 *(Par2()-4))
            }},
            "gamma"={distParam$mean={Par1()/Par2()}
             distParam$variance=Par1()/Par2()^2},
            "lnorm"={distParam$mean={exp(Par1()+.5*Par2()^2)};
             distParam$variance=exp(Par2())*(exp(Par2()-1)*exp(2*Par1()))},
            "norm"={distParam$mean=Par1(); distParam$variance=Par2()^2},
            "t"={distParam$mean=0; distParam$variance=(Par1()/Par1()-2)},
            "unif"={distParam$mean=(Par1()+Par2())/2; distParam$variance=(Par2()-Par1())^2/12},
            "weibull"={distParam$mean={
              gamma(1+(1/Par1()))/Par2()^(1/Par1())
            }; distParam$variance={
              (gamma(1+2/Par1())-gamma(1+1/Par1())^2)/Par2()^(2/Par1())
            }},
            #-------------------------------------------
            "binom"={distParam$mean=Par1()*Par2(); distParam$variance=Par1()*Par2()*(1-Par2())},
            "geom"={distParam$mean=(1-Par1())/Par1(); distParam$variance=(1-Par1())/Par1()^2},
            "hyper"={distParam$mean=NULL; distParam$variance=NULL},
            "nbinom"={distParam$mean=Par1()*(1-Par2())/Par2()
             distParam$variance=Par1()*(1-Par2())/Par2()^2},
            "pois"={distParam$mean=Par1(); distParam$variance=Par1()}
    )
  })
  
  
  # ui outputs --------------------------------------------------------------
  
  output$xscale=renderUI({
    if(sliderT)
    sliderInput(inputId=ns("sliderScale"),label = "xscale",min = xScales$min,max=xScales$max,value=c(0,1),width = 1000)
  })
  output$param1=renderUI({
    #if(!is.null(par1Scale$min))
    sliderInput(inputId = ns("par1"),label="1st parameter",min=par1Scale$min,max=par1Scale$max,value = par1Scale$min,width = 1000,animate = T)
  })
  
  output$param2=renderUI({
    if(!is.null(par2Scale$min))
      sliderInput(inputId = ns("par2"),label="2nd parameter",min=par2Scale$min,max=par2Scale$max,value = par2Scale$min,width = 1000,animate = T)
  })
  
  output$param3=renderUI({
    if(!is.null(par3Scale$min))
      sliderInput(inputId = ns("par3"),label="3rd parameter",min = par3Scale$min,max = par3Scale$max,value = par3Scale$min,width = 1000,animate = T)
  })
  
  
  # output of the function --------------------------------------------------
  otpt=isolate({dens
    reactiveValues(xmin=NULL,
                      xmax=NULL,
                      par1=NULL,
                      par2=NULL,
                      par3=NULL,
                      mean=NULL,
                      variance=NULL,
                   discrete=NULL
                   )})
  # observeEvent(dens,{
  #   otpt$xmin=NULL
  #   otpt$xmax=NULL
  #   otpt$par1=NULL
  #   otpt$par2=NULL
  #   otpt$par3=NULL
  #   otpt$mean=NULL
  #   otpt$variance=NULL
  # })
  par2.2=reactive({
    if(is.null(par2Scale$min)){
      NULL
    }else{
      input$par2
    }
  })
  par3.3=reactive({
    if(is.null(par3Scale$min)){
      NULL
    }else{
      input$par3
    }
  })
  DIS=reactive({
    ifelse(dens%in%c("binom","geom","hyper","nbinom","pois"),T,F)
  })
  observe({
    otpt$xmin=input$sliderScale[1]
    otpt$xmax=input$sliderScale[2]
    otpt$par1=input$par1
    otpt$par2= par2.2()
    otpt$par3=par3.3()
    otpt$mean=distParam$mean
    otpt$variance=distParam$variance
    otpt$discrete=DIS()
  })
  return(otpt)
}

# ui=fluidPage(
#   ui_dist_selecter("foadS"),
#   ui_dist_parameters("foad"),
#               verbatimTextOutput("txt"),
#    verbatimTextOutput("txt2"),
#   plotOutput("plt")
#              )
# 
# 
# 
# 
# server=function(input,output){
#   dist=reactive(callModule(server_dist_selecter,"foadS"))
#   params=eventReactive(dist(),callModule(server_dist_parameters,"foad",dist()))
#   output$txt2=renderPrint(reactiveValuesToList(params()))
#   
# }
# 
# shinyApp(ui,server)

# shiny ui for densities:
 source("Modules/Probability/dist_parameters.R")
# source("Modules/Rscripts/dist_polygon.R")
 source("Modules/Probability/dist_selecter.R")
# # application -------------------------------------------------------------
# 
# 
# # ui ----------------------------------------------------------------------
 source("Modules/Rscripts/dist_curve.R")

ui_dist_detail=function(id){
  ns=NS(id)
  tagList(
  fluidPage(
  shiny::verticalLayout(#width=12,
    ui_dist_selecter(ns("inp"))
  ),
  splitLayout(
    ###
    #column(width = 6,
    shiny::verticalLayout(
          h3("parameter Sliders"),
           ui_dist_parameters(ns("foad"))
           ),
  verticalLayout(#column(#width=6,
      h3("Probability distribution function"),
                          sliderInput(ns("ylim1"),label ="y scale limit",min = 0,max =10,value=3,step = .1),
                         plotOutput(ns("plt") ,height = "500px"
                                    )),
  verticalLayout(h3("Cumulative distribution function"),
                          sliderInput(ns("ylim2"),label ="y scale limit",min = 0,max =10,value=1,step = .1),
                         plotOutput(ns("hi"),height = "500px"
                                    ))
                         
  #)
  )
  )
  )
  }

# server ------------------------------------------------------------------


server_dist_detail=function(input,output,session){
  ns=session$ns
  s=reactive(callModule(server_dist_selecter,"inp"))
  otpt=eventReactive(s(),{callModule(server_dist_parameters,"foad",s(),T)})
  #otpt2=eventReactive(s(),{callModule(server_dist_parameters,"foad2",s(),F)})
  output$hi=renderPlot({
    dist_Curve(s(),reactiveValuesToList(otpt(),all.names = T),pdf = F,ylab="",ylim=c(0,input$ylim2))
    })
  output$plt=renderPlot({
    dist_Curve(s(),reactiveValuesToList(otpt(),all.names = T),pdf=T,ylab="",ylim=c(0,input$ylim1))
  })
  output$txt=renderPrint(reactiveValuesToList(otpt()))
}
ui=fluidPage(
  ui_dist_detail("foad")
)
server=function(input,output){
  callModule(server_dist_detail,"foad")
}
shinyApp(ui,server)

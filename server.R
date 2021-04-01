
library(shiny)
library(GGally)

shinyServer(function(input, output) {

# file input page ---------------------------------------------------------

Data=reactive(callModule(serverInput,"INPUT_FILE"))

output$str2show=renderPrint(str(Data()$Value))
observeEvent(Data(),{
output$tblShowData=renderDT(
    datatable(Data()$Value)
)}
)
# pair plot page ----------------------------------------------------------

observe(callModule(server_GGpairs,"ggpairshow",reactive(Data()$Value)))


# single graph ------------------------------------------------------------
observe(callModule(server_Single_plotly_plots,"singlePlot",reactive(Data()$Value)))

# data summary ------------------------------------------------------------


observe(callModule(server_data_summary,"datasummary",reactive(Data()$Value)))
# density show page --------------------------------------------------------

callModule(server_dist_detail,"distDetails")
# central limit theorem page ----------------------------------------------

callModule(server_central_limit_theorem,"centralPage")


# Largen number theorem ---------------------------------------------------

callModule(server_large_number_theorem,"LargeNumbers")
# comparing normal and t and cuachy distributions -------------------------

callModule(server_norm_t_cauchy,"normtca")
# compute probability page ------------------------------------------------

callModule(server_compute_probability,"computeProbPage")


# F test for variance test ------------------------------------------------

observe(callModule(server_var.test,"varTest4equality",reactive(as.data.frame(Data()$Value))))
# 1 sample t test page ----------------------------------------------------

observe(callModule(server_1sample_t_test,"1sample_t.test",reactive(as.data.frame(Data()$Value))))
# 2 sample t test page ----------------------------------------------------

observe(callModule(server_2sample_t_test,"2sample_t.test",reactive(as.data.frame(Data()$Value))))

})

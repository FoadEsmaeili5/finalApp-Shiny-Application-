 #setwd("C:/Users/Foad/OneDrive/book/lessens/home works/applications/R/learning by myselph/shiny/magazin/finalAPP")


library(shiny)
library(shinydashboard)
library(DT)
library(formattable)
library(flextable)
library(broom)
library(xtable)
library(tableHTML)
library(webshot)
library(rmarkdown)
library(dplyr)
library(ggplot2)
library(shinyBS)

source("Modules/InputPage/FileInputCodes.R")
# for GGpair plot
source("Modules/plots/GGally_plots.R")
source("Modules/plots/Single_plotly_plots.R")
# for distributions
source("Modules/Probability/dist_selecter.R")

source("Modules/Probability/dist_parameters.R")
#source("dist_parameters.R")
source("Modules/Rscripts/dist_curve.R")

source("Modules/Rscripts/dist_polygon.R")

source("Modules/Probability/dist_detail.R")

source("Modules/Probability/central_limit_thorem.R")

source("Modules/Probability/compute_probability.R")

source("Modules/Probability/norm_t_cauchy.R")

source("Modules/models/data_summary.R")

source("Modules/Probability/large_number_theorem.R")
# for models --------------------------------------------------------------
source("Modules/models/model2table_and_download.R")
# compare means
source("Modules/models/compareMeans/2sample_t_test.R")
source("Modules/models/compareMeans/1sample_t_test.R")
source("Modules/models/compareMeans/var.test.R")
# application
shinyUI(
    dashboardPage(
        header = dashboardHeader(),
        sidebar = dashboardSidebar(

# sidebar -----------------------------------------------------------------

            sidebarMenu(
                menuItem("MainPage",tabName = "MainPage"),
                menuItem("Data input",tabName = "fileInput"),
                menuItem("Discriptive Statistics",
                         menuItem("Graphs",
                         menuSubItem("Matirx plots",tabName = "pairPlots"),
                         menuSubItem("Single Graphs",tabName = "singleGRAPH")
                         ),
                         menuSubItem("Data Summary",tabName = "dataSUMMARY")
                         ),
                menuItem("Distributions",
                         menuSubItem("Density curves",tabName = "densityShow"),
                         menuSubItem("Centeral limit theorem","centeralTheorem"),
                         menuSubItem("Law of large numbers","largeNumberTheorem"),
                         menuSubItem("Comparing normal and t",tabName = "normtchauchy")
                         ),
                menuItem("Basic Probabilities",
                         menuItem("Compute probability",tabName = "computeProbability")
                         ),
                menuItem("Compare means",
                        menuSubItem("F Test for variance",tabName = "varianceTest"),
                        menuSubItem("1 Sample t-test",tabName = "1smplt_test"),
                        menuSubItem("2 Sample t-test",tabName = "2smplt_test")
                )
            )
        ),

# body --------------------------------------------------------------------


        body = dashboardBody(
            tabItems(
                tabItem("MainPage",
                        # img(src="uni arm.png",height="100%",width="100%")
                        shiny::flowLayout(
                        img(src = "2659.jpg",height = "150px",width = "120px"),
                        fluidPage(h2("Creator:"),h4(strong(" Foad Esmaeili")),
                        a("foadesameili5@gmail.com",href = "mailto:foadesmaeili5@gmail.com"))),
                        flowLayout(
                            img(src = "Dr_Salehi.jpg",height = "150px",width = "150px"),
                        fluidPage(h2("Supervision:"),
                                  h4(strong("Dr Mahdi Salehi")),
                        a("salehi2sms@gmail.com",href = "mailto:salehi2sms@gmail.com"))),
                        flowLayout(
                            img(src = "arm-english.png",height = "150px",width = "150px"),
                            fluidPage(
                        h3("Department of Mathematics and Statistics"),
                        h3("University of Neyshabur, Iran")
                        ))),

# input page --------------------------------------------------------------

                tabItem("fileInput",
                        UIinput("INPUT_FILE"),
                        verbatimTextOutput("str2show"),
                        dataTableOutput("tblShowData")
                        ),

# pairplot page -----------------------------------------------------------

                tabItem("pairPlots",
                        ui_GGpairs("ggpairshow")
                        ),

# single graph ------------------------------------------------------------

tabItem("singleGRAPH",style="font-size=24px",
        ui_Single_plotly_plots("singlePlot")
        ),
# dataSUMMARY -------------------------------------------------------------

                tabItem("dataSUMMARY",
                        ui_data_summary("datasummary")
                        ),
# densities details -------------------------------------------------------

                tabItem("densityShow",
                        ui_dist_detail("distDetails")
                        ),

# centeral limit theorem page ---------------------------------------------

                tabItem("centeralTheorem",
                        ui_central_limit_theorem("centralPage")
                        ),

# large number theorem ----------------------------------------------------

                tabItem("largeNumberTheorem",
                        ui_large_numbar_theorem("LargeNumbers")
                        ),

# comparing normal and t and cauchy distributions -------------------------

                tabItem("normtchauchy",
                        ui_norm_t_cauchy("normtca")
                        ),

# compute probability page ------------------------------------------------

                tabItem("computeProbability",
                        ui_compute_probability("computeProbPage")
                        ),

# F test for variance test ------------------------------------------------

tabItem("varianceTest",
        ui_var.test("varTest4equality")
        ),
# 1 sample t test ---------------------------------------------------------

                tabItem(tabName = "1smplt_test",
                        ui_1sample_t_test("1sample_t.test")
                        ),

# 2 sample t test ---------------------------------------------------------

                tabItem(tabName = "2smplt_test",
                        ui_2sample_t_test("2sample_t.test")
                        ),

# one way anova -----------------------------------------------------------

                tabItem(tabName = "onewayAnove",
                        h3("one way anova")
                        ),


# variance equality test --------------------------------------------------

                tabItem(tabName = "covTest",
                        h3("covariance test")
                )
            )
        )
    )
)

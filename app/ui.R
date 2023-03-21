# Define UI for application
ui <- fluidPage(
  withMathJax(),
  
  # CSS code for black horizontal lines
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  # Application title
  titlePanel("Mann-Whitney and Wilcoxon Signed Rank Distributions"),
  
  tabsetPanel(type = "tabs",
              tabPanel("Mann-Whitney Distributions",
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput(inputId = "mw_m",
                                       label = "m",
                                       min = 3,
                                       max = 10,
                                       value = 5,
                                       step = 1
                           ),
                           sliderInput(inputId = "mw_n",
                                       label = "n",
                                       min = 3,
                                       max = 10,
                                       value = 6, 
                                       step = 1
                           ),
                           radioButtons(inputId = "mw_tails",
                                        label = "Alternative Hypothesis",
                                        choices = c("lower tail" = -1,
                                                    "two tail" = 2,
                                                    "upper tail" = 1),
                                        selected = -1,
                                        inline = TRUE
                           ),
                           radioButtons(inputId = "mw_significance",
                                        label = "Significance Level",
                                        choices = c("10%" = 0.1,
                                                    "5%" = 0.05,
                                                    "2.5%" = 0.025,
                                                    "1%" = 0.01,
                                                    "None" = 0),
                                        selected = 0,
                                        inline = TRUE
                           ),
                           textOutput("mw_critical_value"),
                           hr(), # horizontal line
                           radioButtons(inputId = "mw_pv",
                                        label = NULL,
                                        choices = c("Display probabilities" = FALSE,
                                                    "Display p-values" = TRUE),
                                        selected = FALSE,
                                        inline = TRUE),
                           numericInput(inputId = "mw_statistic",
                                        label = "Value of observed rank sum, Wm",
                                        value = NA,
                                        step = 1
                           ),
                           uiOutput("mw_probability")
                         ),
                         mainPanel(plotOutput("mw_plot"))
                       )
              ),
              
              tabPanel("Wilcoxon Signed Rank Distributions",
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput(inputId = "w_n",
                                       label = "n",
                                       min = 4,
                                       max = 13,
                                       value = 10, 
                                       step = 1
                           ),
                           radioButtons(inputId = "w_tails",
                                        label = "Alternative Hypothesis",
                                        choices = c("lower tail" = -1,
                                                    "two tail" = 2,
                                                    "upper tail" = 1),
                                        selected = -1,
                                        inline = TRUE
                           ),
                           radioButtons(inputId = "w_significance",
                                        label = "Significance Level",
                                        choices = c("10%" = 0.1,
                                                    "5%" = 0.05,
                                                    "2.5%" = 0.025,
                                                    "1%" = 0.01,
                                                    "None" = 0),
                                        selected = 0,
                                        inline = TRUE
                           ),
                           textOutput("w_critical_value"),
                           hr(), # horizontal line
                           radioButtons(inputId = "w_pv",
                                        label = NULL,
                                        choices = c("Display probabilities" = FALSE,
                                                    "Display p-values" = TRUE),
                                        selected = FALSE,
                                        inline = TRUE),
                           numericInput(inputId = "w_statistic",
                                        label = "Value of observed rank sum, W",
                                        value = NA,
                                        step = 1
                           ),
                           uiOutput("w_probability")
                         ),
                         mainPanel(plotOutput("w_plot"))
                       )
              )
  )
)# end of fluidPage
###########################################################################
##R Shiny App to visualize sampling distributions
##Justin Post - Fall 2015
###########################################################################

#Load package
library(shiny)


# Define UI for application that draws the prior and posterior distributions given a value of y
shinyUI(fluidPage(
  
  # Application title
  titlePanel("MCMC Demonstration App"),
  
  # Sidebar with a slider input for the number of successes
  fluidRow(
    column(3,br(),
	numericInput(inputId="y",label="Number of Success",value=13,step=1,max=20),
	numericInput(inputId="thetastart",label="Starting Value for Theta",value=0.5,step=0.1,min=0,max=1),
	numericInput(inputId="thetaproposalsd",label="Standard Deviation of Normal Jumping Distribution",value=0.05,min=0.000001,max=1),
	sliderInput(inputId="step", label=h4("Draw Number"), 
                  min = 1, max = 10000, value = 1, step = 1,
                  animate=list(TRUE, interval=100,loop=FALSE)),br()
    ),

    #Show a plot of the poseterior
    column(9,
           fluidRow(
                     plotOutput("posteriorPlot")
           ),br(),
            fluidRow(
                       tableOutput("drawTable")
            )
    )
  )
))


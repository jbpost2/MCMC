###########################################################################
##R Shiny App to visualize sampling distributions
##Justin Post - Fall 2015 (updated 2017)
###########################################################################

#Load package
library(shiny)
library(shinydashboard)

# Define UI for application that displays an about page and the app itself

dashboardPage(skin="red",
              
  #add title
  dashboardHeader(title="MCMC Demonstration App",titleWidth=750),
  
  #define sidebar items
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("archive")),
    menuItem("Application", tabName = "app", icon = icon("laptop"))
  )),
  
  #define the body of the app
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "about",
        fluidRow(
          #add in latex functionality if needed
          withMathJax(),
          
          #two columns for each of the two items
          column(6,
            #Description of App
            h1("What does this app do?"),
            #box to contain description
            box(background="red",width=12,
              h4("This application demonstrates a Markov Chain Monte Carlo (MCMC) sampling algorithm.  The purpose of MCMC sampling is to obtain draws from the appropriate posterior distribution."),
              h4("The random parameter of interest is \\(\\Theta\\)."),
              h4("The prior distribution is denoted by \\(f_{\\Theta}(\\theta)\\)."),
              h4("The likelihood is denoted by \\(f_{Y|\\Theta}(y|\\theta)\\)."),
              h4("The posterior is denoted by \\(f_{\\Theta|Y}(\\theta|y)\\propto f_{\\Theta}(\\theta)f_{Y|\\Theta}(y|\\theta)\\).  Often the normalizing constant for the posterior distribution is difficult to obtain."),
              h4("The app implements a Metropolis-Hastings algorithm to instead obtain draws from the posterior, eliminating the need for the constant.  The idea here is to follow the algorithm below:"),
              tags$ol(
                tags$li("Draw (or choose) a starting value in the appropriate range for the posterior, \\(\\theta^{1}\\)."), 
                h5("At step t"),
                tags$li("Sample \\(\\theta^{*}\\) from a proposal (or jumping) distribution, \\(J_{\\Theta}(\\theta|\\theta^{t-1})\\)."), 
                tags$li("Find the ratio \\(r=\\frac{f_{\\Theta|Y}(\\theta^{*}|y)/J_{\\Theta}(\\theta^{*}|\\theta^{t-1})}{f_{\\Theta|Y}(\\theta^{t-1}|y)/J_{\\Theta}(\\theta^{t-1}|\\theta^{*})}\\).  Plugging in using the relationship between the posterior, the prior, and the likelihood, this simplifies to \\(r=\\frac{f_{\\Theta}(\\theta^{*})f_{Y|\\Theta}(y|\\theta^{*})/J_{\\Theta}(\\theta^{*}|\\theta^{t-1})}{f_{\\Theta}(\\theta^{t-1})f_{Y|\\Theta}(y|\\theta^{t-1})/J_{\\Theta}(\\theta^{t-1}|\\theta^{*})}\\), which does not require the constant."),

                tags$li("Set \\(\\theta^{t}\\) equal to \\(\\theta^{*}\\) with probability \\(min(r,1)\\) and leave it as \\(\\theta^{t-1}\\) otherwise.")
              ),
              h4("For this example, the prior distribution is a Standard Uniform distribution.  The likelihood is a Binomial distribution, yielding a Beta distribution for the posterior distribution.  As the form of the posterior is known, there is no reason to do the MCMC sampling other than for demonstration purposes.")
            )
          ),
          
          column(6,
            #How to use the app
            h1("How to use the app?"),
            #box to contain description
            box(background="red",width=12,
              h4("The controls for the app are located to the left and the visualization and information are available on the right."),
              h4("To change the likelihood values there are two input boxes on the top left.  There you can change the sample size and the number of successes observed (for example the number of coins tossed and the number landing head side up)."),
              h4("A Normal distribution is used as the jumping distribution.  The mean is determined by the \\(\\theta\\) values in the MCMC chain.  The standard deviation of this jumping distribution can be changed using the input box on the middle left."),
              h4("Below this box, is a box to change the initial value for the chain."),
              h4("Lastly, the draws can be cycled through using the play button on the slider on the bottom left.  To advance only one draw, you can double click the play button."),
              h4("The graph on the right displays the true posterior distribution, the jumping distribution (centered at the current value in the chain), a candidate value, and a histogram of values accepted so far in the chain."),
              h4("Below the graph is information about the Metropolis-Hastings algorithm for the most recent draws.")
            )
          )
        )
      ),
      
      #actual app layout      
      tabItem(tabName = "app",
        fluidRow(
          column(width=3,
            h4("Binomial Distribution (Likelihood) set-up"),
            numericInput(inputId="n",label="Number of Trials",value=20,step=1,min=1,max=5000),
      	    numericInput(inputId="y",label="Number of Success",value=13,step=1,max=20,min=0),
            h4("Jumping Distribution is a Normal Distribution"),
            numericInput(inputId="thetaproposalsd",label="Standard Deviation of Jumping Distribution",value=0.05,min=0.000001,max=0.2,step=0.01),
          	numericInput(inputId="thetastart",label="Starting Value for Theta",value=0.5,step=0.1,min=0,max=1),
          	sliderInput(inputId="step", label=h4("Draw Number"), 
              min = 1, max = 10000, value = 1, step = 1,
              animate=list(TRUE, interval=100,loop=FALSE)),
      	    br()
          ),
          
          #Show a plot of the poseterior
          column(9,
            fluidRow(
              plotOutput("posteriorPlot")
            ),
            br(),
            fluidRow(
              tableOutput("drawTable")
            )
          )
        )
      )
    )
  )
)




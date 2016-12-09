library(shiny)

# Define UI for random distribution application 
fluidPage(
  
  # Application title
  titlePanel("The Boy Girl Paradox"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Simulation type:",
                   c("All Families*" = "nosamp",
                     "Families with at least one boy**" = "oneboy",
                     "Families where you meet a boy***" = "meetboy")),
      br(),
      
      p("* This simulates 1,000 families."),
      p("** From all families with two children, at least one of whom is a boy, a family is chosen at random."),
      p("*** From all families with two children, one child is selected at random, and the sex of that child is specified to be a boy.")
      
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("plot")), 
                  tabPanel("Summary", verbatimTextOutput("summary")), 
                  tabPanel("Table", tableOutput("table"))
      )
    )
  )
)
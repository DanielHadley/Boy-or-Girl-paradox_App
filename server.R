give_birth <- function(){
  sample(c("boy", "girl"), size = 1, prob = c(.5,.5))
}

child_one <- replicate(1000, give_birth())
child_two <- replicate(1000, give_birth())

families <- data_frame(child_one, child_two) %>% 
  mutate(pairs = paste(child_one, child_two))

# Define server logic for random distribution application
function(input, output) {
  
  data <- reactive({
    dist <- switch(input$dist,
                   nosamp = function(){
                     return(families)
                   },
                   onegirl = function(){
                     families %>% 
                       filter(pairs != "boy boy")
                   },
                   meetgirl = function(){
                     m <- c()
                     for(i in 1:1000){
                       which_child <- sample(c(1,2), size = 1, prob = c(.5,.5))
                       which_family <- sample(c(1:nrow(families)), size = 1)
                       y <- families[which_family, which_child]
                       if(y == "girl"){m <- c(m, which_family)}
                     }
                     return(families[m,])
                   },
                   unif = meeting,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist()
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    dist <- input$dist
    
    ggplot(data(), aes(factor(pairs))) + geom_bar()
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })
  
}

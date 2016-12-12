# First we simulate the creation of 1000 families (leaving out all the messy drama of real births)
# This is stochastic and the sample is only 1k, so just like in real life, there may be more of a certain gender
give_birth <- function(){
  sample(c("boy", "girl"), size = 1, prob = c(.5,.5))
}

child_one <- replicate(1000, give_birth())
child_two <- replicate(1000, give_birth())

# Put the children into families
families <- tibble(child_one, child_two) %>% 
  mutate(pairs = paste(child_one, child_two))


function(input, output) {
  
  data <- reactive({
    dist <- switch(input$dist,
                   
                   #### The first is the scenario where you do not sample. This is just the data created above
                   nosamp = function(){
                     return(families)
                   },
                   
                   #### The Second scenario is where you randomly sample families from a sample frame where all pairs of "boy boy" are removed.
                   oneboy = function(){
                     families %>% 
                       filter(pairs != "girl girl")
                   },
                   
                   #### In this scenario, you meet one child from a two-child family and that child is specified to be a boy
                   meetboy = function(){
                     m <- c()
                     for(i in 1:1000){
                       # randomly pick which child you meet
                       which_child <- sample(c(1,2), size = 1, prob = c(.5,.5))
                       # And from which family
                       which_family <- sample(c(1:nrow(families)), size = 1)
                       y <- families[which_family, which_child]
                       # If the child you meet is a boy, add the family to your list
                       if(y == "boy"){m <- c(m, which_family)}
                     }
                     return(families[m,])
                   })
    
    dist()
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  output$plot <- renderPlot({
    dist <- input$dist
    
    ggplot(data(), aes(pairs)) + geom_bar() +
      ggthemes::theme_wsj()
  })
  
  # Generate a summary of the data
  output$summary <- renderText({
    "test"
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })
  
}

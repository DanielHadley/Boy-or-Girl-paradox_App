# First we simulate the creation of 10,000 families (leaving out all the messy drama of real births)
# This is stochastic and the sample is only 10k, so just like in real life, there may be more of a certain gender
give_birth <- function(){
  sample(c("boy", "girl"), size = 1, prob = c(.5,.5))
}

child_one <- replicate(10000, give_birth())
child_two <- replicate(10000, give_birth())

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
                     met_boy <- c()
                     for(i in 1:10000){
                       # randomly pick which child you meet
                       which_child <- sample(c(1,2), size = 1, prob = c(.5,.5))
                       # and from which family
                       which_family <- sample(c(1:nrow(families)), size = 1)
                       # the meeting
                       you_meet <- families[which_family, which_child]
                       # If the child you meet is a boy, add the family to your list
                       if(you_meet == "boy"){met_boy <- c(met_boy, which_family)}
                     }
                     return(families[met_boy,])
                   })
    
    dist()
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  output$plot <- renderPlot({
    
    ggplot(data(), aes(pairs)) + geom_bar() +
      ggthemes::theme_wsj()
    
  })
  
  # Generate a summary of the data
  output$summary <- renderText({
    
    str_c(
      if (input$dist == "nosamp") "In this simulation, all families of two are kept. There should be approximately 2,500 of each possible combo (BG, BB, GB, GG). The reason for discrepancies between these numbers and the actual values in the chart is that the simulation is stochastic, meaning boys and girls are chosen randomly with 50% probability, just as they are in real life. Moreover, the sample is small enough that occasionally the number of one gender will far exceed the other. You can always refresh to re-simulate",
      if (input$dist == "oneboy") "In this simulation, families with two girls are filtered out, leaving only families with at least one boy. This has the effect of increasing the odds to 33% that both children are boys. This represents the scenario where Mr. Smith is pulled randomly from a group of two-child families with at least one boy.",
      if (input$dist == "meetboy") "Finally, this simulates the scenario in which you encounter Mr. Smith on the street and he is with only one of his children who happens to be a boy. We assume that Mr. Smith is equally likely to walk with his sons as his daughters, so meeting him with his son increases the odds that both children are boys to 50%",
      "."
    )
    
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })
  
}

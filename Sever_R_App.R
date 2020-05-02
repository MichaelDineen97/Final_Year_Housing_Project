library(shiny)

shinyServer(
  
  function(input, output, session){
    
      output$Plot_One <- renderPlot({
        
        distType <- input$Distribution
        size <- input$sampleSize
        
        
        if(distType == "Normal"){
          
          randomvec <-  rnorm(size, mean, as.numeric(input$mean), sd=as.numeric(input$sd))
          
  }
      else{
        
        randomvec <- rexp(size, rate = 1/ as.numeric(input$lambda))
        
      }
        hist(randomvec, col = "Blue ")
        
        
        
      }
        
        
        
        
      )
    
  }
  
    
    
  )
    
  
  
  


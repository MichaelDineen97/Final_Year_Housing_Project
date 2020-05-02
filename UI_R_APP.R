

shinyUI(
  
  pageWithSidebar(
    
    headerPanel("Analysis and Investigation of the PropertyPrice Register "),
    
    sidebarPanel("filter"),
    mainPanel("Main")
  ),
  
  mainPanel(plotOutput("Plot_One") 
            
            )
)

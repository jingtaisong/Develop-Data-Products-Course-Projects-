library(shiny)
##The list of names of national parks in the Southwest
park.names=c("Arches",                      
             "Big.Bend",                    
             "Black.Canyon.of.the.Gunnison",
             "Bryce.Canyon",               
             "Canyonlands",             
             "Capitol.Reef" ,             
             "Carlsbad.Caverns",            
             "Channel.Islands",             
             "Grand.Canyon",                
             "Grend.Teton",                 
             "Great.Basin",                 
             "Great.Sand.Dunes",            
             "Guadalupe.Mountains",         
             "Joshua.Tree",                 
             "Kings.Canyon",                
             "Lassen.Volcanic",             
             "Mesa.Verde",                  
             "Petrified.Forest",            
             "Redwood",                     
             "Rocky.Mountain",              
             "Saguaro",                     
             "Sequoia",                     
             "Yellowstone",                 
             "Yosemite",                    
             "Zion")

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
    headerPanel("National Parks Travel Planner"),
    sidebarPanel(
        selectInput(inputId="start", label="Choose a park you want to start from", choices=park.names, selected = NULL,
                    multiple = FALSE),
        selectInput(inputId="end", label="Choose a park you want to stop at", choices=park.names, selected = NULL,
                    multiple = FALSE),
        sliderInput(inputId="percentage", label="How much extra way (in percentage) are you willing to consider in addition to the direct distance?", min=0, max=25, value=0, step=0.1),
        submitButton("Submit")
    ),
    mainPanel(
        h3("Basic User Guide:"),
        p(HTML("Jumping into vacations for national parks in the southwest but got scared when planning the trip? This trip planner will help you choose your tailored route in a minute! <br/> Choose your origin and destination from the menu on the left side and tell us by how long in percentage are you willing to add on the journey so that you could visit other national parks on your way, </br> then you will find our recommended routes on the right side!")),
        h3("Recommended Routes:"),
        htmlOutput("Routes")
    )
))
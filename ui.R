shinyUI(fluidPage(
    
    fluidRow(
        
        absolutePanel(
            
            helpText(h5("Submit new text.")),
            wellPanel(
                column(4, textInput('myText', "Gene symbol",value="")),
                
                actionButton("submit1", strong("Submit"))
            ),
            
            textOutput("mytext20"),
            
            #verbatimTextOutput("DataFrame"),
            tableOutput("DataFrame"),
            
            
            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid
             blue;'></div></b>")),
            br(),
            
            right=5, left=10
        )
    )
    
))


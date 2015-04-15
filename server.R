shinyServer(function(input, output) {
    
    generateDataFrame <- reactive({
        if (input$submit1>0) {
            df.gene <- data.frame(Symbol=input$myText,
                                  stringsAsFactors=FALSE)
            
        } else {NULL}
    })
    
    
    output$DataFrame = renderTable ({generateDataFrame()})
    output$mytext20 = renderText({
        input$submit1
    })
}





s^4=3
    
    0.25^4+1
sqrt(-1)

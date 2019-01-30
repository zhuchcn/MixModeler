dataInit = function(input, data){
    dataInit = data.frame(
        matrix(nrow = input$nrow, ncol = input$ncol, ""),
        stringsAsFactors = FALSE
    )
    data$data = dataInit
    data$nrow = input$nrow
    data$ncol = input$ncol
    data$coltype = sapply(dataInit, class)
}

alertModal = function(type, msg){
    modalDialog(
        title = type,
        msg,
        easyClose = TRUE,
        footer = NULL
    )
}

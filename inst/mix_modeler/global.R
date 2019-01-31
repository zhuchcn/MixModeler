# This defines the data init step
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

# Define the Modal dialog generator
alertModal = function(type, msg){
    div(
        id = "shiny-modal", class = "modal fade", tabindex = "-1",
        div(
            class = "modal-dialog",
            div(
                class = "modal-content",
                div(
                    class = "modal-header",
                    class = switch(type, "error"="bg-danger", "success"="bg-success"),
                    tags$h4(class = "modal-title", type)
                ),
                div(class = "modal-body py-2", msg)
            )
        ),
        tags$script("$('#shiny-modal').modal().focus();")
    )
}

# The rHandsontable scrolling bars only work fine when the styling is added to
# the attribute of the tag. So I have to do this in a ugly way
jsCode = '
shinyjs.addInlineCss = function(){
    const div = document.getElementById("dataTable");
    div.style["overflow"] = "hidden";
    div.style["height"] = "750px";
    div.style["width"] = "auto";
}
'

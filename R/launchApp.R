#' @title Launch the mix_modeler app
#' @export
#' @examples
#' MixModeler::launch()
launch = function(){
    shiny::shinyAppFile(
        system.file(
            paste0("mix_modeler", "/app.R"),
            package = 'MixModeler',
            mustWork = TRUE
        )
    )
}

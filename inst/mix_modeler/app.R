pkgs = c("rhandsontable", "shiny", "bs4Dash")
for (pkg in pkgs){
    library(pkg, character.only = T)
}

source("global.R", local = TRUE)

ui = bs4DashPage(
    navbar = bs4DashNavbar(
        "Mix Modeler",
        rightUi = tagList(
            tags$ul(
                class = "navbar-nav mr-auto",
                tags$li(
                    class = "nav-item activate px-2 my-auto",
                    tags$a(
                        calss = "nav-link",
                        href = "http://www.chenghaozhu.net",
                        tags$i(
                            class = "fas fa-home fa-2x"
                        )
                    )
                ),
                tags$li(
                    class = "nav-item activate px-2 my-auto",
                    tags$a(
                        calss = "nav-link",
                        href = "http://www.github.com/zhuchcn/MixModeler",
                        tags$i(
                            class = "fab fa-github fa-2x"
                        )
                    )
                )
            )
        )
    ),
    sidebar = bs4DashSidebar(

    ),
    body = bs4DashBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href="styles.css")
        ),
        shinyjs::useShinyjs(),
        fluidRow(
            class = "pt-2",
            tags$div(
                class = "col-2-md col-3-sm",
                bs4Card(
                    width = 12,
                    height = "auto",
                    tags$div(
                        class = "my-control-panel",
                        numericInput("nrow", "Number of Rows",
                                     min = 1, max = 1000, step = 1, value = 1),
                        numericInput("ncol", "Number of Columns",
                                     min = 1, max = 1000, step = 1, value = 1),
                        actionButton("init", "Initialize it", class="btn-outline-primary btn-block pb-2"),
                        actionButton("data_submit_btn", "Submit It", class="btn-primary btn-block pb-2"),
                        actionButton("coltype_btn", "Column Types", class="btn-outline-primary btn-block pb-2"),
                        actionButton("colnames_btn", "Column Names", class="btn-outline-primary btn-block pb-2"),
                        tags$hr(),
                        textInput("formula", "Formula",
                                  placeholder = "Input the formula"),
                        actionButton("test", "Test it", class="btn-danger btn-block pb-2"),
                        tags$hr(),
                        actionButton("showHelp", "Help", class="btn-info btn-block pb-2")
                    )
                )
            ),
            tags$div(
                class = "col-10-md col-9-sm",
                tags$div(
                    id = "my-data-table",
                    bs4Card(
                        width = 12,
                        rHandsontableOutput("dataTable")
                    )
                ),
                tags$div(
                    id = "help-page",
                    bs4Card(
                        width = 12,
                        tags$ul(
                            calss = "list-group list-group-flush",
                            tags$li(
                                class = "list-group-item",
                                "Step 1: input number of columns and rows, and click 'Init it'"
                            ),
                            tags$li(
                                class = "list-group-item",
                                "Step 2: Copy paste your data from Excel, and click 'Submit it'"
                            ),
                            tags$li(
                                class = "list-group-item",
                                "Step 3: Click 'Column Types' and change the types of your variables"
                            ),
                            tags$li(
                                class = "list-group-item",
                                "Step 4: Click 'Column Names' and change the names of your variables"
                            ),
                            tags$li(
                                class = "list-group-item",
                                "Step 5: input a linear model formula, and click 'Test it'"
                            )
                        ),
                        tags$div(
                            class = "text-right",
                            actionButton("hideHelp", "Kick off",
                                         class = "btn-success")
                        )
                    )
                )
                # bs4Card(
                #     width = 12,
                #     tableOutput("debug")
                # )
            )
        )
    ),
    sidebar_collapsed = TRUE
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    shinyjs::hide(id = "my-data-table")
    # Initialize the data
    data = reactiveValues(
        nrow = 1, ncol = 1,
        coltype = NULL,
        data=as.data.frame(matrix(NA))
    )
    # Update data
    observeEvent(input$data_submit_btn, {
        newData = as.data.frame(hot_to_r(input$dataTable))
        if(nrow(newData) == data$nrow & ncol(newData) == data$ncol){
            data$data = newData
            showModal(modalDialog(
                tagList(
                    tags$p("Data submitted!")
                ),
                easyClose = TRUE,
                footer = NULL
            ))
        } else {
            showModal(modalDialog(
                tagList(
                    tags$p("Something went wrong..")
                ),
                easyClose = TRUE,
                footer = NULL
            ))
            dataInit(input, data)
        }
    })

    # Define the output of the data table
    output$dataTable = renderRHandsontable(
        rhandsontable(data$data) %>%
            hot_cols(colWidths = 100)
    )
    # Initialize it with empty data
    observeEvent(input$init, {
        shinyjs::show(id="my-data-table")
        shinyjs::hide(id="help-page")
        dataInit(input, data)
    })
    # Define the column type modal
    observeEvent(input$coltype_btn, {
        showModal(modalDialog(
            rHandsontableOutput("colTypeData"),
            footer = tagList(
                actionButton("coltype_submit", "Submit"),
                modalButton("Cancel")
            )
        ))
    })
    # Define the output of column types
    output$colTypeData = renderRHandsontable({
        DF = data.frame(t(data$coltype), stringsAsFactors = FALSE)
        htb = rhandsontable(DF, height = 200)
        type_list = c("numeric", "character", "integer", "factor")
        for(i in names(data$coltype)) {
            htb = htb %>%
                hot_col(col = i, type = "dropdown", source = type_list)
        }
        htb %>% hot_cols(colWidths = 100)
    })
    # Column type modal submit event
    observeEvent(input$coltype_submit, {
        newTypes = as.character(hot_to_r(input$colTypeData))
        if( any(newTypes != data$coltype)){
            for(i in seq_along(newTypes)) {
                if(class(data$data[,i]) != newTypes[i]){
                    if(any(data$data[,i] != "")){
                        if(newTypes[i] == "numeric"){
                            data$data[,i] = as.numeric(data$data[,i])
                        } else if (newTypes[i] == "character") {
                            data$data[,i] = as.character(data$data[,i])
                        } else if (newTypes[i] == "integer") {
                            data$data[,i] = integer(data$data[,i])
                        } else if (newTypes[i] == "factor") {
                            data$data[,i] = factor(data$data[,i])
                        }
                    } else {
                        if(newTypes[i] == "numeric"){
                            data$data[,i] = numeric(data$nrow)
                        } else if (newTypes[i] == "character") {
                            data$data[,i] = character(data$nrow)
                        } else if (newTypes[i] == "integer") {
                            data$data[,i] = integer(data$nrow)
                        }
                    }
                }
            }
            data$coltype = newTypes
        }
        removeModal()
    })
    # Do the linear model
    observeEvent(input$test, {
        output$modalResult = renderText({
            fit = lm(formula = as.formula(input$formula), data = data$data)
            paste0(capture.output(summary(fit)), collapse = "\n")
        })
        showModal(modalDialog(
            title = "Linear Modal Result:",
            verbatimTextOutput("modalResult"),
            size = 'l',
            easyClose = TRUE,
            footer = NULL
        ))
    })
    # Define the modal for changing of column names
    observeEvent(input$colnames_btn, {
        # Define the table output for colnames changing
        output$hst_colnames = renderRHandsontable({
            coldf = as.data.frame(
                t(colnames(data$data)),
                stringsAsFactors = FALSE
            )
            colnames(coldf) = colnames(data$data)
            rhandsontable(coldf) %>%
                hot_cols(colWidths = 100)
        })
        # Show modal
        showModal(modalDialog(
            rHandsontableOutput("hst_colnames"),
            footer = tagList(
                actionButton("colname_submit", "Submit"),
                modalButton("Cancel")
            )
        ))
    })
    # Define the column name change submit
    observeEvent(input$colname_submit, {
        colnames(data$data) = as.character(hot_to_r(input$hst_colnames))
        removeModal()
    })
    # Show help page
    observeEvent(input$showHelp, {
        shinyjs::show(id = "help-page")
        shinyjs::hide(id = "my-data-table")
    })
    # Hide help page
    observeEvent(input$hideHelp, {
        shinyjs::show(id = "my-data-table")
        shinyjs::hide(id = "help-page")
    })
    # Remove btn-default from all btn
    shinyjs::removeClass(class = "btn-default", selector = ".btn")
    # Hide the sidebar
    shinyjs::addClass(class="d-none", selector = ".main-sidebar")
    # Always collapse the sidebar
    shinyjs::removeClass(class="sidebar-open", selector = "body")
    shinyjs::addClass(class="sidebar-collapse", selector = "body")

}

shinyApp(ui = ui, server = server)

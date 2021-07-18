#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny tablerDash waiter promises
#' @importFrom dplyr `%>%`
#' @noRd
app_ui <- function(request) {
    main_input <- tablerCard(
        closable = FALSE,
        zoomable = FALSE,
        fileInput(
            inputId = "file_upload",
            label = "Upload DD2875s",
            multiple = TRUE,
            accept   = c(".pdf", "application/pdf", ".zip"),
            buttonLabel = tablerIcon("upload")
        ),
        shinyjs::disabled(
            textInput("domaintext", "Active Directory Server")
        ),
        checkboxInput("domainbox", "Create AD Users"),
        shinyjs::disabled(
            textInput("exchangetext", "Exchange Server")
        ),
        checkboxInput("exchangebox", "Create Exchange Mailboxes"),
        actionButton(
            inputId = "submit",
            label = tags$span(
                tablerIcon("check-circle"),
                "Submit"
            ),
            style = "margin: auto; display: block;",
            class = "btn-primary"
        ),
        width = 12,
        status = "info"
    ) %>%
        tagAppendAttributes(class = "h-100")

    main_input$children[[1]] <-
        main_input$children[[1]] %>%
        tagAppendAttributes(class = "h-100")

    main_output <- tablerCard(
        # shiny::tableOutput("file_data"),
        # shiny::uiOutput("users"),
        shiny::uiOutput("csv_data", class = "row"),
        shiny::uiOutput("csv_download", class = "text-center"),
        width = 12
    ) %>%
        tagAppendAttributes(class = "h-100")

    main_output$children[[1]] <-
        main_output$children[[1]] %>%
        tagAppendAttributes(class = "h-100")

    main_output$children[[1]]$children <-
        main_output$children[[1]]$children %>%
        tagAppendChild(
            tags$div(
                textOutput("file_data"),
                class = "ribbon ribbon-bookmark"
            )
        )

    tagList(
        golem_add_external_resources(),
        tablerDashPage(
            title = "ActiveForms",
            body = tablerDashBody(
                tablerTabItems(
                    tablerTabItem(
                        tabName = "Main",
                        tags$h1(
                            tablerIcon("layers"), "ActiveForms",
                            style = "text-align: center; font-weight: 300;"
                        ),
                        fluidRow(
                            column(
                                width = 4,
                                main_input
                            ),
                            column(
                                width = 8,
                                main_output
                            )
                        )
                    )
                )
            ),
            footer = tags$div(style = "display: none;")
        )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
    add_resource_path(
        "www", app_sys("app/www")
    )

    tags$head(
        favicon(),
        bundle_resources(
            path = app_sys("app/www"),
            app_title = "ActiveForms"
        ),
        waiter::useWaiter(),
        shinyjs::useShinyjs(),
        waiterPreloader(html = spin_2(), fadeout = TRUE, color = "#f4f6fa")
    )
}

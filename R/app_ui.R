#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny tablerDash waiter promises shinyjs
#' @importFrom dplyr `%>%`
#' @noRd
app_ui <- function(request) {
    main_input <- tablerDash::tablerCard(
        closable = FALSE,
        zoomable = FALSE,
        shiny::fileInput(
            inputId     = "file_upload",
            label       = "Upload DD2875s",
            multiple    = TRUE,
            accept      = c(".pdf", "application/pdf", ".zip"),
            buttonLabel = tablerDash::tablerIcon("upload")
        ),
        shinyjs::disabled(
            shiny::textInput("domaintext", "Active Directory Server")
        ),
        checkboxInput("domainbox", "Create AD Users"),
        shinyjs::disabled(
            shiny::textInput("exchangetext", "Exchange Server")
        ),
        shiny::checkboxInput("exchangebox", "Create Exchange Mailboxes"),
        shiny::actionButton(
            inputId = "submit",
            label   = shiny::tags$span(
                tablerDash::tablerIcon("check-circle"),
                "Submit"
            ),
            style = "margin: auto; display: block;",
            class = "btn-primary"
        ),
        width = 12,
        status = "info"
    ) %>%
        shiny::tagAppendAttributes(class = "h-100")

    main_input$children[[1]] <-
        main_input$children[[1]] %>%
        shiny::tagAppendAttributes(class = "h-100")

    main_output <- tablerDash::tablerCard(
        shiny::uiOutput("csv_data", class = "row"),
        shiny::uiOutput("csv_download", class = "text-center"),
        width = 12
    ) %>%
        shiny::tagAppendAttributes(class = "h-100")

    main_output$children[[1]] <-
        main_output$children[[1]] %>%
        shiny::tagAppendAttributes(class = "h-100")

    main_output$children[[1]]$children <-
        main_output$children[[1]]$children %>%
        shiny::tagAppendChild(
            shiny::tags$div(
                shiny::textOutput("file_data"),
                class = "ribbon ribbon-bookmark"
            )
        )

    shiny::tagList(
        golem_add_external_resources(),
        tablerDash::tablerDashPage(
            title = "ActiveForms",
            body = tablerDash::tablerDashBody(
                tablerDash::tablerTabItems(
                    tablerDash::tablerTabItem(
                        tabName = "Main",
                        shiny::tags$h1(
                            tablerDash::tablerIcon("layers"), "ActiveForms",
                            style = "text-align: center; font-weight: 300;"
                        ),
                        shiny::fluidRow(
                            shiny::column(
                                width = 4,
                                main_input
                            ),
                            shiny::column(
                                width = 8,
                                main_output
                            )
                        )
                    )
                )
            ),
            footer = shiny::tags$div(style = "display: none;")
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
    golem::add_resource_path(
        "www", app_sys("app/www")
    )

    shiny::tags$head(
        golem::favicon(),
        golem::bundle_resources(
            path      = app_sys("app/www"),
            app_title = "ActiveForms"
        ),
        waiter::useWaiter(),
        shinyjs::useShinyjs(),
        waiter::waiterPreloader(
            html    = waiter::spin_2(),
            fadeout = TRUE,
            color   = "#f4f6fa"
        )
    )
}

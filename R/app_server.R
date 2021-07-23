#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    w <- waiter::Waiter$new(id = "csv_data")

    output$file_data <- shiny::renderText({
        if (is.null(input$file_upload)) {
            "0 DD2875s"
        } else {
            paste(nrow(input$file_upload), "DD2875s")
        }
    })

    shiny::observeEvent(input$submit, {
        shiny::validate(
            need(input$file_upload, "At least one file needs to be uploaded.")
        )

        w$show()

        file_upload <- input$file_upload

        zipped_files <- dplyr::filter(
            file_upload,
            fs::path_ext(datapath) == "zip"
        )

        if (nrow(zipped_files) > 0) {
            unzipped_files <- unlist(lapply(
                zipped_files$datapath,
                utils::unzip,
                exdir = dirname(zipped_files$datapath[1])
            ))
        } else {
            unzipped_files <- NULL
        }

        csv_data <-
            promises::future_promise({
                dplyr::filter(
                    file_upload,
                    stringr::str_detect(name, "2875")
                ) %>%
                `[[`("datapath") %>%
                c(unzipped_files) %>%
                lapply(extract_2875) %>%
                dplyr::bind_rows() %>%
                dplyr::select(-PDF_PATH) %>%
                dplyr::distinct()
            }, seed = TRUE)

        csv_cards <- csv_data %...>%
                     create_user_cards()

        output$csv_data <- shiny::renderUI({
            csv_cards
        })

        promises::then(
            csv_data,
            onFulfilled = function(promise_data) {
                output$.download <<- shiny::downloadHandler(
                    filename = function() {
                        paste0("DD2875_", Sys.Date(), ".csv")
                    },
                    content = function(con) {
                        print(promise_data)
                        readr::write_csv(promise_data, con)
                    }
                )
            }
        )

        output$csv_download <- renderUI({
            shiny::downloadButton(
                ".download",
                label = " Download .csv",
                icon  = htmltools::tagAppendAttributes(
                    tablerDash::tablerIcon("download"),
                    style = "margin-right: 0.5rem;"
                )
            )
        })

        w$hide()
    })
}

#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#' @param futurePlan `future` plan function. Defaults to `future::multisession`.
#'
#' @export
#' @importFrom shiny shinyApp
run_app <- function(onStart = NULL,
                    options = list(),
                    enableBookmarking = NULL,
                    uiPattern = "/",
                    futurePlan = future::multisession,
                    ...) {
    future::plan(futurePlan)

    waiter::waiter_set_theme(
        html  = waiter::spin_2(),
        color = waiter::transparent(0.5)
    )

    shiny::shinyApp(
        ui                = app_ui,
        server            = app_server,
        onStart           = onStart,
        options           = options,
        enableBookmarking = enableBookmarking,
        uiPattern         = uiPattern
    )
}

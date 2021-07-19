#' @title Build ActiveForms as app via Electron
#' @param build_path Directory to build the app to
#' @export
build_app <- function(build_path) {

    if (!dir.exists(build_path)) {
        dir.create(build_path)
    }

    electricShine::electrify(
        app_name          = "ActiveForms",
        short_description = "DoD PDF Parser built in R",
        semantic_version  = "0.0.1",
        build_path        = build_path,
        function_name     = "run_app",
        cran_like_url     = "https://cran.r-project.org",
        local_package_path = system.file(package = "ActiveForms"),
        package_install_opts = list(
            type = "binary",
            dependencies = c("Depends", "Imports")
        )
    )
}
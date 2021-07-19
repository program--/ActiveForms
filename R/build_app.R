#' @title Build ActiveForms as app via Electron
#' @param build_path Directory to build the app to
#' @export
build_app <- function(build_path) {

    if (!dir.exists(build_path)) {
        dir.create(build_path)
    }
    
    platform <- "win"
    arch     <- "x64"

    electricShine::electrify(
        app_name          = "ActiveForms",
        short_description = "DoD PDF Parser built in R",
        semantic_version  = "0.0.1",
        build_path        = build_path,
        function_name     = "run_app",
        cran_like_url     = "https://cran.r-project.org",
        git_host          = "github",
        git_repo          = "program--/ActiveForms",
        package_install_opts = list(
            type = "binary",
            dependencies = c("Depends", "Imports")
        ),
        permission = TRUE
    )
}

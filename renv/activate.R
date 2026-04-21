local({
  renv_bootstrap <- function(project) {
    if (!nzchar(Sys.getenv("RENV_PATHS_ROOT", unset = ""))) {
      Sys.setenv(RENV_PATHS_ROOT = file.path(project, "renv"))
    }

    renv_env <- new.env(parent = emptyenv())
    renv_env$project <- project

    renv_download <- function(url, destfile) {
      if (requireNamespace("utils", quietly = TRUE)) {
        try(utils::download.file(url, destfile, quiet = TRUE), silent = TRUE)
      }
    }

    # Default to a known-good CRAN release (avoid 404s from stale pins).
    renv_version <- Sys.getenv("RENV_VERSION", unset = "1.2.0")
    renv_tarball <- sprintf("renv_%s.tar.gz", renv_version)
    renv_url <- sprintf("https://cran.r-project.org/src/contrib/%s", renv_tarball)

    library_paths <- unique(c(project, Sys.getenv("R_LIBS_USER")))
    library_paths <- library_paths[nzchar(library_paths)]
    library_paths <- normalizePath(library_paths, winslash = "/", mustWork = FALSE)

    for (lib in library_paths) {
      if (!dir.exists(lib)) dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    }

    if (!requireNamespace("renv", quietly = TRUE)) {
      destfile <- file.path(tempdir(), renv_tarball)
      renv_download(renv_url, destfile)
      if (file.exists(destfile)) {
        try(utils::install.packages(destfile, repos = NULL, type = "source", lib = library_paths[[1]]), silent = TRUE)
      }
    }

    # Fallback: install the latest available 'renv' from CRAN if the pinned
    # tarball isn't available (or download/install failed).
    if (!requireNamespace("renv", quietly = TRUE)) {
      try(
        utils::install.packages(
          "renv",
          repos = "https://cloud.r-project.org",
          lib = library_paths[[1]]
        ),
        silent = TRUE
      )
    }

    if (requireNamespace("renv", quietly = TRUE)) {
      renv::load(project = project, quiet = TRUE)
    }
  }

  project <- getwd()
  renv_bootstrap(project)
})


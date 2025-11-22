# Shipped-in packages: qetl, fets (built tarballs now live under each repo's builds directory)
pkg_patterns <- c(
  qetl = "../qetl/builds/qetl_*.tar.gz",
  fets = "../fets/builds/fets_*.tar.gz"
)

for (pkg in names(pkg_patterns)) {
  pkg_files <- Sys.glob(pkg_patterns[[pkg]])
  if (length(pkg_files) == 0) {
    stop(sprintf("Package tarball not found for %s (pattern: %s)", pkg, pkg_patterns[[pkg]]))
  }

  pkg_path <- tail(sort(pkg_files), 1)
  pkg_search <- paste0("package:", pkg)

  # Fully unload everything
  if (pkg %in% loadedNamespaces()) {
    try(unloadNamespace(pkg), silent = TRUE)
  }
  if (pkg_search %in% search()) {
    try(detach(pkg_search, unload = TRUE, character.only = TRUE), silent = TRUE)
  }

  # Remove from renv
  renv::remove(pkg)

  # Install clean
  remotes::install_local(pkg_path, prompt = FALSE)

  # Attach clean version
  library(pkg, character.only = TRUE)
}

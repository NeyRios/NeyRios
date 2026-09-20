required <- c("terra", "plumber", "jsonlite", "png")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  install.packages(missing)
}

message("Dependencias listas: ", paste(required, collapse = ", "))

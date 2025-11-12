render_mermaid <- function(path, replacements) {
  template <- readLines(path)
  for (nm in names(replacements)) {
    template <- gsub(
      paste0("<<", nm, ">>"),
      replacements[[nm]],
      template,
      fixed = TRUE
    )
  }
  cat("```{mermaid}\n")
  cat(paste(template, collapse = "\n"))
  cat("\n```\n")
}

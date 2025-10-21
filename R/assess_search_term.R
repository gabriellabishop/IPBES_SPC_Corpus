#' Assess Search Term
#'
#' This function assesses the search term by counting the number of occurrences in a given text corpus.
#'
#' @param st The search term to be assessed. Each line should be one sub-term. Usually, these are combined by `OR`.
#' @param remove A regular expression pattern to remove from the search term.
#' @param excl_others Logical indicating whether to exclude other search terms from the count.
#' @param and_term Optional additional term to be combined with the search term using `AND`.
#' @param types Optional vector of types to filter the search results. If `NULL`, no filtering is applied.
#'
#' @return A data frame with the search term and the corresponding count.
#'
#' @importFrom pbmcapply pbmclapply
#' @importFrom openalexR oa_fetch
#'
#' @md
#'
#' @examples
#' assess_search_term(list("climate OR", "change"))
#'
#' @keywords internal
assess_search_term <- function(
  st = NULL,
  remove = "OR$|AND$|^[()]$",
  excl_others = FALSE,
  and_term = NULL,
  types = NULL,
  verbose = FALSE
) {
  st <- st |>
    trimws() |>
    gsub(pattern = remove, replacement = "") |>
    trimws() |>
    gsub(pattern = remove, replacement = "")

  st <- st[st != ""]

  result <- data.frame(
    term = st,
    count = pbapply::pblapply(
      st,
      function(x) {
        if (excl_others) {
          excl <- st[!(st %in% x)]
          searchterm <- paste0(
            "(",
            x,
            ") NOT (",
            paste0(excl, collapse = " OR "),
            ")"
          )
        } else {
          searchterm <- x
        }
        if (!is.null(and_term)) {
          searchterm <- paste0(searchterm, " AND ", and_term)
        }
        if (is.null(types)) {
          count <- openalexR::oa_fetch(
            title_and_abstract.search = searchterm,
            output = "list",
            count_only = TRUE,
            api_key = openalexPro::oap_apikey(),
            mailto = openalexPro::oap_mail(),
            verbose = verbose
          )$count
        } else {
          count <- openalexR::oa_fetch(
            title_and_abstract.search = searchterm,
            output = "list",
            count_only = TRUE,
            type = types,
            api_key = openalexPro::oap_apikey(),
            mailto = openalexPro::oap_mail(),
            verbose = verbose
          )$count
        }
        return(count)
      }
    ) |>
      unlist()
  )
  return(result)
}

assess_search_term_both <- function(
  st = NULL,
  remove = "OR$|AND$|^[()]$",
  excl_others = FALSE,
  and_term = NULL,
  types = NULL,
  verbose = FALSE
) {
  incl <- assess_search_term(
    st = st,
    remove = remove,
    excl_others = FALSE,
    and_term = and_term,
    types = types,
    verbose = verbose
  )
  excl <- assess_search_term(
    st = st,
    remove = remove,
    excl_others = TRUE,
    and_term = and_term,
    types = types,
    verbose = verbose
  )
  result <- data.frame(
    term = incl$term,
    count = incl$count,
    count_excl = excl$count
  )
  return(result)
}

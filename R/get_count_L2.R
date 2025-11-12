get_count_L2 <- function(
  sets,
  types_filter,
  publication_year_from,
  publication_year_to,
  st_AND
) {
  queries <- lapply(
    sets,
    function(section) {
      lapply(
        section,
        function(s) {
          openalexPro::pro_query(
            title_and_abstract.search = paste0(
              "(",
              st_AND,
              ") AND (",
              st_compact(s),
              ")"
            ),
            type = types_filter,
            from_publication_date = publication_year_from,
            to_publication_date = publication_year_to
          )
        }
      )
    }
  )

  result <- lapply(
    names(queries),
    function(sect_nm) {
      result <- lapply(
        names(queries[[sect_nm]]),
        function(nm) {
          message("Retrieving ", sect_nm, " - ", nm, " ...")
          result <- c(
            count = NA_integer_,
            db_response_time_ms = NA_integer_,
            page = NA_integer_,
            per_page = NA_integer_
          )
          try(
            result <- queries[[sect_nm]][nm] |>
              openalexR::oa_request(
                count_only = TRUE,
                verbose = TRUE
              ) |>
              unlist(),
            silent = FALSE
          )
          return(result)
        }
      ) |>
        do.call(what = cbind) |>
        t() |>
        as.data.frame() |>
        dplyr::select(count)
      rownames(result) <- names(queries[[sect_nm]])
      return(result)
    }
  )
  names(result) <- names(queries)

  return(result)
}

library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)


booking_id <- "218f95d7c697"
token <- httr::get_token(client_secret = client_secret)

get_booking_details <- function(booking_id, token){
    base_url <- "https://kubkalender.kb.dk/1.1"
    url <- paste0(base_url, "/space/booking/", booking_id)

    resp <- httr2::request(url) |>
            httr2::req_headers(
                      Authorization = paste("Bearer", token),
                      Accept = "application/json"
                    ) |>
            httr2::req_url_query(
                      form_answers    = 1,
                      internal_notes  = 1,
                      check_in_status = 1
                    ) |>
            httr2::req_perform()

    if (resp_status(resp) >= 400) {-
        stop(sprintf("HTTP %s\n%s", resp_status(resp), resp_body_string(resp)),
        call. = FALSE)
      }

    raw_json <- httr2::resp_body_string(resp)

    # 1) Parse med flatten=TRUE (giver ofte en pæn "bred" struktur med list-cols tilbage)
    x <- fromJSON(raw_json, simplifyVector = TRUE, flatten = TRUE)

    # Sørg for at vi ender i en tibble (x kan være liste, df, eller liste med "booking"-felt)
    as_tbl <- function(obj) {
                if (is.data.frame(obj)) return(as_tibble(obj))
                if (is.list(obj) && length(obj) == 1 && is.data.frame(obj[[1]])) return(as_tibble(obj[[1]]))
                if (is.list(obj)) return(tibble(data = list(obj)))  # fallback: 1 række med listekolonne
              tibble(value = obj)
              }

    tbl <- as_tbl(x)

    # 2) “Unnest alt” helper: udvider named-lists bredt og lister af records langt
    unnest_everything <- function(df, max_iters = 50) {
                            iter <- 0
                            repeat {
                              iter <- iter + 1
                              if (iter > max_iters) break

                              list_cols <- names(df)[vapply(df, is.list, logical(1))]
                              if (length(list_cols) == 0) break

                              changed <- FALSE

                            for (col in list_cols) {
                            v <- df[[col]]

                            # tom / NULL -> drop kolonnen
                            if (all(map_lgl(v, ~ is.null(.x) || (is.list(.x) && length(.x) == 0)))) {
                            df <- df |> select(-all_of(col))
                            changed <- TRUE
                            next
                              }

      # Hvis cellerne er named lists (dvs. record/objekt), så unnest_wider
      is_named_record <- function(x) is.list(x) && length(x) > 0 && !is.null(names(x))
      if (all(map_lgl(v, ~ is.null(.x) || is_named_record(.x)))) {
        df <- df |>
          unnest_wider(all_of(col), names_sep = ".")
        changed <- TRUE
        next
      }

      # Hvis cellerne er "lister af records" (fx en array af objekter), så unnest_longer
      is_list_of_records <- function(x) {
        is.list(x) && length(x) > 0 && all(map_lgl(x, ~ is.null(.x) || is_named_record(.x)))
      }
      if (any(map_lgl(v, is_list_of_records))) {
        df <- df |>
          tidyr::unnest_longer(all_of(col), keep_empty = TRUE) |>
          tidyr::unnest_wider(all_of(col), names_sep = ".")
        changed <- TRUE
        next
      }

      # Hvis cellerne er lister af atomics (array af værdier), så unnest_longer
      is_list_of_atomic <- function(x) {
        is.list(x) && length(x) > 0 && all(map_lgl(x, ~ is.atomic(.x) && length(.x) == 1))
      }
      if (any(map_lgl(v, is_list_of_atomic))) {
        df <- df |> tidyr::unnest_longer(all_of(col), keep_empty = TRUE)
        changed <- TRUE
        next
      }

      # Ellers: lad den stå (vi vil ikke ødelægge data ved for aggressive heuristikker)
    }

    if (!changed) break
  }
  df
}

unnest_everything(tbl)
}






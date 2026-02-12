library(httr2)
library(lubridate)
library(purrr)
library(dplyr)
library(tibble)

dato <- as.character(lubridate::today() %m-% months(24))

fetch_space_bookings_all <- function(
  token,
  base_url = "https://kubkalender.kb.dk",
  lid = 3087,
  date = dato,
  days = 365,
  limit = 500,
  form_answers = 1,
  check_in_status = 1,
  internal_notes = TRUE,
  verbose = TRUE
) {

  req0 <- request(base_url) |>
    req_url_path_append("1.1", "space", "bookings") |>
    req_headers(Authorization = paste("bearer", token)) |>
    req_url_query(
      lid = lid,
      date = date,
      days = days,
      limit = limit,
      form_answers = form_answers,
      check_in_status = check_in_status,
      internal_notes = internal_notes
    )

  pages <- list()
  page <- 1L

  repeat {
    if (verbose) message("Henter page=", page)

    resp <- req0 |>
      req_url_query(page = page) |>
      req_perform() |>
      resp_check_status()

    items <- resp_body_json(resp, simplifyVector = FALSE)

    n <- length(items)
    if (n == 0) break

    pages[[page]] <- items

    if (n < limit) break

    page <- page + 1L
  }

  all_items <- list_flatten(pages)

  # ---- HER håndteres NULL korrekt ----

  item_to_row <- function(x) {

    # Sørg for at alle NULL-felter bliver list(NULL)
    x <- purrr::imap(x, \(value, name) {
      if (is.null(value)) {
        list(NULL)
      } else {
        list(value)
      }
    })

    tibble::as_tibble(x)
  }

  df <- dplyr::bind_rows(purrr::map(all_items, item_to_row))

  df
}

# ---- KØR ----

all_bookings <- fetch_space_bookings_all(token = token)

glimpse(all_bookings)
all_bookings |> dplyr::bind_rows()

get_old_bookings <- function(token){

dato <- as.character(lubridate::today() %m-% months(36))
fetch_space_bookings_all <- function(
  token,
  base_url = "https://kubkalender.kb.dk",
  lid = 3087,
  date = dato,
  days = as.integer(lubridate::today() - (lubridate::today() %m-% months(36))), # henter default for sidste tre år
  limit = 500,
  form_answers = 1,
  check_in_status = 1,
  internal_notes = TRUE,
  verbose = TRUE
) {

  req0 <- httr2::request(base_url) |>
    httr2::req_url_path_append("1.1", "space", "bookings") |>
    httr2::req_headers(Authorization = paste("bearer", token)) |>
    httr2::req_url_query(
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
      httr2::req_url_query(page = page) |>
      httr2::req_perform() |>
      httr2::resp_check_status()

    items <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    n <- length(items)
    if (n == 0) break

    pages[[page]] <- items

    if (n < limit) break

    page <- page + 1L
  }

  all_items <- purrr::list_flatten(pages)

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


all_bookings |> 
  dplyr::bind_rows() |> 
  tidyr::unnest(cols = c(bookId, id, eid, cid, lid, fromDate, toDate, q1752, q1753, created, firstName, lastName, email,
   account, status, location_name, cancelled, check_in_status, nickname, category_name, item_name)) |> 
 tidyr::unnest_wider(event, names_sep  = "_") |> 
  tidyr::unnest(internal_notes, keep_empty = TRUE) |> 
 tidyr::unnest_wider(internal_notes, names_sep = "_") 
  
}


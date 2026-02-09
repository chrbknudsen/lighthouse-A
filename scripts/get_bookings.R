# Henter bookings fra en specifik - hvad det nu hedder, library lighthouse. Det niveau

# Det er 3087 vi interesserer os for her.
# Vi skal tænke over hvilket tidsinterval vi skal have fat på.
# Det særligt relevante vi får retur her er booking-id'er
# Vi får også oplysninger tilbage om hvem der bookede. Men vi supplerer med get_booking_details.
# Vi får også oplysninger om evt. aflysning. Herunder hvornår bookingen blev aflyst.

library(lubridate)
library(httr)
library(dplyr)
  dato <- lubridate::today() %m-% lubridate::months(2)
  dato <- as.character(dato)
dato

  url <- httr::modify_url(
    url = "https://kubkalender.kb.dk",
    path = c("1.1", "space" ,"bookings"),
    query = list(
      lid = 3087,
      date = dato,
      days = 365,
      limit = 500
    )
  )
test_data <-   httr::GET(url, add_headers('Authorization' = paste("bearer", token))) |> 
    httr::content()

length(test_data)

test_data |> dplyr::bind_rows() 

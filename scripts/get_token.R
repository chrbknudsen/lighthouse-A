# henter et token - og efterlader "token" i environment
library(tidyverse)
library(httr)
if(str_detect(here::here(), "Users/cbk")){
  client_secret <- keyring::key_get("libcal")
}else{
  client_secret <- Sys.getenv("CLIENT_SECRET")
}

# Get token
# returnerer et access-token. Tager client_secret som input.
get_token <- function(client_secret){
  token_endpoint <- "https://kubkalender.kb.dk/1.1/oauth/token"
  client_id <- "110"
  token <- httr::POST(token_endpoint,
                body = list(grant_type = "client_credentials",
                            client_id = client_id,
                            client_secret = client_secret)) %>% 
    httr::content() 
  token[["access_token"]]
  
}

# get_token ----
token <- get_token(client_secret = client_secret)
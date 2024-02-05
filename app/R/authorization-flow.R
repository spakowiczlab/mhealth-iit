library(shiny)
library(curl)
library(httr)
library(httr2)
library(tidyverse)

url.authorize <- "https://www.fitbit.com/oauth2/authorize"
url.token <- "https://api.fitbit.com/oauth2/token"

pkce.codes <- oauth_flow_auth_code_pkce()

test.auth.url <- paste0(url.authorize,
                        "?client_id=[INSERT FITBIT APP ID HERE]",
                        "&response_type=code&code_challenge=", pkce.codes$challenge,
                        "&code_challenge_method=S256",
                        "&scope=activity%20cardio_fitness%20heartrate%20nutrition%20oxygen_saturation%20profile%20respiratory_rate%20settings%20sleep%20temperature%20weight")

grabAccessInfo <- function(authcode){
  
  tmp.authcode <- gsub(".*code=(.*)#_=_", "\\1", authcode)
  test.verify <- POST(url = url.token,
                      add_headers(`Content-Type` = "application/x-www-form-urlencoded"),
                      body = paste0("client_id=[INSERT FITBIT APP ID HERE]",
                                    "&code=", tmp.authcode,
                                    "&code_verifier=", pkce.codes$verifier,
                                    "&grant_type=authorization_code"))
  
  user_access <- content(test.verify)
  access_table <- as.data.frame(unlist(user_access)) %>%
    t() %>%
    as.data.frame() %>%
    remove_rownames() %>%
    select(user_id, access_token, refresh_token)
  
  return(access_table)
}


# Save this for later, it would be used with data pulls
# test.refresh <- POST(url = url.token,
#                      add_headers(`Content-Type` = "application/x-www-form-urlencoded"),
#                      body = paste0("client_id=", client.id,
#                                    "&refresh_token=", user_access$refresh_token,
#                                    "&grant_type=refresh_token"))

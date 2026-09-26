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

# Read the OAuth code from a Fitbit redirect URL. The code is the query
# parameter `code`; a trailing #_=_ fragment is optional.
extractAuthCode <- function(authcode) {
  redirect.message <- "Paste the full address-bar URL from the Fitbit redirect."
  authcode <- trimws(authcode)
  without.fragment <- sub("#.*$", "", authcode)
  query <- if (grepl("?", without.fragment, fixed = TRUE)) {
    sub("^[^?]*\\?", "", without.fragment)
  } else {
    without.fragment
  }
  parts <- strsplit(query, "&", fixed = TRUE)[[1]]
  code.parts <- parts[startsWith(parts, "code=")]
  if (length(code.parts) < 1) {
    stop(redirect.message, call. = FALSE)
  }
  code <- utils::URLdecode(sub("^code=", "", code.parts[[1]]))
  if (!nzchar(code)) {
    stop(redirect.message, call. = FALSE)
  }
  code
}

grabAccessInfo <- function(authcode){
  redirect.message <- "Paste the full address-bar URL from the Fitbit redirect."
  tmp.authcode <- extractAuthCode(authcode)
  test.verify <- POST(url = url.token,
                      add_headers(`Content-Type` = "application/x-www-form-urlencoded"),
                      body = paste0("client_id=[INSERT FITBIT APP ID HERE]",
                                    "&code=", utils::URLencode(tmp.authcode, reserved = TRUE),
                                    "&code_verifier=", pkce.codes$verifier,
                                    "&grant_type=authorization_code"))

  if (http_error(test.verify)) {
    stop(
      paste("Fitbit rejected the authorization code.", redirect.message),
      call. = FALSE
    )
  }

  user_access <- content(test.verify)
  if (is.null(user_access$user_id) ||
      is.null(user_access$access_token) ||
      is.null(user_access$refresh_token)) {
    stop(
      paste("Fitbit rejected the authorization code.", redirect.message),
      call. = FALSE
    )
  }

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

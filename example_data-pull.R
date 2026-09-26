# Code for pulling and formatting data from fitbit.
# Sourcing this file only defines functions. It does not call the API.
#
# Load httr and tidyverse first. getTokens() also needs url.token
# (https://api.fitbit.com/oauth2/token) and client.id in the environment.
# The pull* functions take the one-row data frame returned by getTokens.
# Dates are YYYY-MM-DD.

# Getting refreshed tokens from a user's refresh token.
# Fitbit rotates the refresh token; keep the refresh_token in the result.
getTokens <- function(reftok){
  test.refresh <- POST(url = url.token,
                       add_headers(`Content-Type` = "application/x-www-form-urlencoded"),
                       body = paste0("client_id=", client.id,
                                     "&refresh_token=", reftok,
                                     "&grant_type=refresh_token"))

  if (http_error(test.refresh)) {
    stop("Fitbit rejected the refresh token.", call. = FALSE)
  }

  refresh.cont <- as.data.frame(unlist(content(test.refresh))) %>%
    t() %>%
    as.data.frame() %>%
    remove_rownames() %>%
    select(user_id, refresh_token, access_token)
  
  return(refresh.cont)
}

tokenFields <- function(toks) {
  user_id <- toks$user_id
  access_token <- toks$access_token
  if (length(user_id) != 1 || length(access_token) != 1) {
    stop("toks must be one row with user_id and access_token.", call. = FALSE)
  }
  list(user_id = user_id, access_token = access_token)
}

fitbitGet <- function(url, access_token) {
  response <- GET(
    url = url,
    add_headers(authorization = paste("Bearer", access_token))
  )
  stop_for_status(response)
  response
}

# Day summaries.

pullDailySteps <- function(toks, startdate, enddate) {
  fields <- tokenFields(toks)
  response <- fitbitGet(
    paste0("https://api.fitbit.com/1/user/",
           fields$user_id,
           "/activities/steps/date/",
           startdate, "/",
           enddate, ".json"),
    fields$access_token
  )
  bind_rows(content(response))
}

pullDailyActiveZoneMinutes <- function(toks, startdate, enddate) {
  fields <- tokenFields(toks)
  response <- fitbitGet(
    paste0("https://api.fitbit.com/1/user/",
           fields$user_id,
           "/activities/active-zone-minutes/date/",
           startdate, "/",
           enddate, ".json"),
    fields$access_token
  )
  azm.ls <- content(response)$`activities-active-zone-minutes`
  azm.df <- lapply(azm.ls, function(x) x$value) %>%
    bind_rows()
  azm.df$dateTime <- unlist(lapply(azm.ls, function(x) x$dateTime))
  azm.df
}

pullDailyHeartRateZones <- function(toks, startdate, enddate) {
  fields <- tokenFields(toks)
  response <- fitbitGet(
    paste0("https://api.fitbit.com/1/user/",
           fields$user_id,
           "/activities/heart/date/",
           startdate, "/",
           enddate, ".json"),
    fields$access_token
  )
  heart.ls <- content(response)$`activities-heart`
  heart.df <- lapply(heart.ls, function(x) bind_rows(x$value$heartRateZones))
  names(heart.df) <- unlist(lapply(heart.ls, function(x) x$dateTime))
  lapply(names(heart.df), function(x) {
    heart.df[[x]] %>% mutate(dateTime = x)
  }) %>%
    bind_rows()
}

# Intraday series for one day, at 5-minute resolution.
# These endpoints need intraday access enabled on the Fitbit application.

pullIntradayHeartRate <- function(toks, date) {
  fields <- tokenFields(toks)
  response <- fitbitGet(
    paste0("https://api.fitbit.com/1/user/",
           fields$user_id,
           "/activities/heart/date/",
           date,
           "/1d/5min.json"),
    fields$access_token
  )
  content(response)$`activities-heart-intraday`$dataset %>%
    bind_rows()
}

pullIntradayActiveZoneMinutes <- function(toks, date) {
  fields <- tokenFields(toks)
  response <- fitbitGet(
    paste0("https://api.fitbit.com/1/user/",
           fields$user_id,
           "/activities/active-zone-minutes/date/",
           date,
           "/1d/5min.json"),
    fields$access_token
  )
  azm.ls <- content(response)$`activities-active-zone-minutes`[[1]]
  as.data.frame(cbind(
    minute = unlist(lapply(azm.ls$minutes, function(x) x$minute)),
    azm = unlist(lapply(azm.ls$minutes, function(x) x$value$activeZoneMinutes))
  ))
}

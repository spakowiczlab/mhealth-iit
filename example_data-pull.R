# Code for pulling and formatting data from fitbit

# Getting refreshed tokens from a user's refresh token
getTokens <- function(reftok){
  test.refresh <- POST(url = url.token,
                       add_headers(`Content-Type` = "application/x-www-form-urlencoded"),
                       body = paste0("client_id=", client.id,
                                     "&refresh_token=", reftok,
                                     "&grant_type=refresh_token"))
  
  refresh.cont <- as.data.frame(unlist(content(test.refresh))) %>%
    t() %>%
    as.data.frame() %>%
    remove_rownames() %>%
    select(user_id, refresh_token, access_token)
  
  return(refresh.cont)
}

# Example code for a variety of fitbit variables - day summaries. Date variables are in the format YYYY-MM-DD.

activity.steps <- GET(url = paste0("https://api.fitbit.com/1/user/",
                                   newtoks$user_id,
                                   "/activities/steps/date/",
                                   startdate, "/",
                                   enddate, ".json"),
                      add_headers(authorization = paste("Bearer", newtoks$access_token)))
steps <- bind_rows(content(activity.steps))

activity.azm <- GET(url = paste0("https://api.fitbit.com/1/user/",
                                 newtoks$user_id,
                                 "/activities/active-zone-minutes/date/",
                                 startdate, "/",
                                 enddate, ".json"),
                    add_headers(authorization = paste("Bearer",
                                                      newtoks$access_token)))
azm.ls <- content(activity.azm)$`activities-active-zone-minutes`
azm.df <- lapply(azm.ls, function(x) x$value) %>%
  bind_rows()
azm.df$dateTime <- unlist(lapply(azm.ls, function(x) x$dateTime))

activity.heart <- GET(url = paste0("https://api.fitbit.com/1/user/",
                                   newtoks$user_id,
                                   "/activities/heart/date/",
                                   startdate, "/",
                                   enddate, ".json"),
                      add_headers(authorization = paste("Bearer",
                                                        newtoks$access_token)))
heart.ls <- content(activity.heart)$`activities-heart`
heart.df <- lapply(heart.ls, function(x) bind_rows(x$value$heartRateZones))
names(heart.df) <- unlist(lapply(heart.ls, function(x) x$dateTime))
heart.df <- lapply(names(heart.df), function(x)
  heart.df[[x]] %>% mutate(dateTime = x)) %>%
  bind_rows()

# Examples for grabbing intraday data
pmr.heartrate <- GET(url = paste0("https://api.fitbit.com/1/user/",
                                  newtoks$user_id,
                                  "/activities/heart/date/",
                                  pmr.date.choose, 
                                  "/1d/5min.json"),
                     add_headers(authorization = paste("Bearer",
                                                       newtoks$access_token)))
pmr.df <- content(pmr.heartrate)$`activities-heart-intraday`$dataset %>%
  bind_rows()

pt.dazm <- GET(url = paste0("https://api.fitbit.com/1/user/",
                            newtoks$user_id,
                            "/activities/active-zone-minutes/date/",
                            "2023-02-01", "/1d/5min.json"),
               add_headers(authorization = paste("Bearer",
                                                 newtoks$access_token)))
pt.azm.ls <- content(pt.dazm)$`activities-active-zone-minutes`[[1]]
pt.azm.df <- as.data.frame(cbind(minute = unlist(lapply(pt.azm.ls$minutes, function(x) x$minute)),
                                 azm = unlist(lapply(pt.azm.ls$minutes, function(x) x$value$activeZoneMinutes)))
)

pt.heartrate <- GET(url = paste0("https://api.fitbit.com/1/user/",
                                 newtoks$user_id,
                                 "/activities/heart/date/",
                                 "2023-02-01", 
                                 "/1d/5min.json"),
                    add_headers(authorization = paste("Bearer",
                                                      newtoks$access_token)))
pt.heart.df <- content(pt.heartrate)$`activities-heart-intraday`$dataset %>%
  bind_rows()
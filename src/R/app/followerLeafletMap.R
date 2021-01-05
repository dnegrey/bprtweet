followerLeafletMap <- function(x) {
    x <- filter(x, !is.na(lat) & !is.na(lon))
    y <- leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addMarkers(
            data = filter(x, !is_retweeter),
            group = "FALSE",
            lng = ~lon,
            lat = ~lat,
            label = ~name,
            labelOptions = labelOptions(
                textsize = "12px"
            ),
            popup = ~paste(
                sprintf(
                    fmt = "<span style='text-decoration: underline;font-weight: bold;'>%s</span>",
                    paste0("@", screen_name)
                ),
                sprintf("<b>Name: </b>%s", name),
                sprintf("<b>Location: </b>%s", location),
                sprintf("<b>Joined: </b>%s", format(account_created_at, "%B %d, %Y")),
                sprintf("<b>Following: </b>%s", formatNumeric(friends_count, "num")),
                sprintf("<b>Followers: </b>%s", formatNumeric(followers_count, "num")),
                sprintf("<b>Tweets: </b>%s", formatNumeric(statuses_count, "num")),
                sprintf("<b>Likes: </b>%s", formatNumeric(favourites_count, "num")),
                sep = "<br>"
            ),
            clusterOptions = markerClusterOptions()
        ) %>%
        addMarkers(
            data = filter(x, is_retweeter),
            group = "TRUE",
            lng = ~lon,
            lat = ~lat,
            label = ~name,
            labelOptions = labelOptions(
                textsize = "12px"
            ),
            popup = ~paste(
                sprintf(
                    fmt = "<span style='text-decoration: underline;font-weight: bold;'>%s</span>",
                    paste0("@", screen_name)
                ),
                sprintf("<b>Name: </b>%s", name),
                sprintf("<b>Location: </b>%s", location),
                sprintf("<b>Joined: </b>%s", format(account_created_at, "%B %d, %Y")),
                sprintf("<b>Following: </b>%s", formatNumeric(friends_count, "num")),
                sprintf("<b>Followers: </b>%s", formatNumeric(followers_count, "num")),
                sprintf("<b>Tweets: </b>%s", formatNumeric(statuses_count, "num")),
                sprintf("<b>Likes: </b>%s", formatNumeric(favourites_count, "num")),
                sep = "<br>"
            ),
            clusterOptions = markerClusterOptions()
        ) %>%
        addLayersControl(
            overlayGroups = c("FALSE", "TRUE"),
            position = "topright",
            options = layersControlOptions(
                collapsed = FALSE
            )
        )
    return(y)
}

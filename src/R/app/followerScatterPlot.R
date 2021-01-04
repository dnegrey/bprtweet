followerScatterPlot <- function(x, vx, vy, vz) {
    xv <- switch(
        EXPR = vx,
        "Followers" = "followers_count",
        "Following" = "friends_count",
        "Tweets" = "statuses_count",
        "Likes" = "favourites_count"
    )
    yv <- switch(
        EXPR = vy,
        "Followers" = "followers_count",
        "Following" = "friends_count",
        "Tweets" = "statuses_count",
        "Likes" = "favourites_count"
    )
    zv <- switch(
        EXPR = vz,
        "Followers" = "followers_count",
        "Following" = "friends_count",
        "Tweets" = "statuses_count",
        "Likes" = "favourites_count"
    )
    x$VarX <- x[, xv]
    x$VarY <- x[, yv]
    x$VarZ <- x[, zv]
    y <- plot_ly(x) %>%
        add_trace(
            x = ~VarX,
            y = ~VarY,
            type = "scatter",
            mode = "markers",
            size = ~VarZ,
            color = ~is_retweeter,
            colors = c("#0D2935", "#32B1E6"),
            marker = list(
                opacity = 0.70,
                sizemode = "diameter",
                line = list(
                    color = "rgba(255, 255, 255, 1.00)",
                    width = 2
                )
            ),
            hoverinfo = "text",
            text = ~paste(
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
            )
        )%>%
        layout(
            xaxis = list(
                tickformat = ",",
                title = sprintf("<b>%s</b>", vx),
                rangemode = "tozero"
            ),
            yaxis = list(
                tickformat = ",",
                title = sprintf("<b>%s</b>", vy),
                rangemode = "tozero"
            )
        ) %>%
        config(
            displayModeBar = FALSE
        )
    y$elementId <- NULL
    return(y)
}

tweetCountPlot <- function(x) {
    x$EndDate <- lead(x$CreateDate) - 1
    x$DateMessage <- ifelse(
        is.na(x$EndDate),
        sprintf("Since %s", format(x$CreateDate, "%B %d, %Y")),
        sprintf(
            fmt = "%s - %s",
            format(x$CreateDate, "%B %d, %Y"),
            format(x$EndDate, "%B %d, %Y")
        )
    )
    ycv <- 5*ceiling(max(x$Total)/5)
    y <- plot_ly(x, x = ~CreateDate) %>%
        add_trace(
            y = ~Total,
            name = "<b>Total</b>",
            type = "scatter",
            mode = "lines",
            line = list(
                color = "#0D2935"
            ),
            fill = "tozeroy",
            fillcolor = "#0D2935",
            hoverinfo = "text",
            text = ~paste(
                sprintf("<b style='text-decoration: underline;'>%s</b>", DateMessage),
                sprintf("<b>Total: </b>%s", formatNumeric(Total, "num")),
                sprintf("<b>Original: </b>%s", formatNumeric(Original, "num")),
                sprintf("<b>Quote: </b>%s", formatNumeric(Quote, "num")),
                sprintf("<b>Retweet: </b>%s", formatNumeric(Retweet, "num")),
                sep = "<br>"
            )
        ) %>%
        add_trace(
            y = ~Original,
            name = "<b>Original</b>",
            type = "scatter",
            mode = "lines",
            line = list(
                color = "#32B1E6"
            ),
            hoverinfo = "none",
            visible = "legendonly"
        ) %>%
        add_trace(
            y = ~Quote,
            name = "<b>Quote</b>",
            type = "scatter",
            mode = "lines",
            line = list(
                color = "#F9D230"
            ),
            hoverinfo = "none",
            visible = "legendonly"
        ) %>%
        add_trace(
            y = ~Retweet,
            name = "<b>Retweet</b>",
            type = "scatter",
            mode = "lines",
            line = list(
                color = "#D230F9"
            ),
            hoverinfo = "none",
            visible = "legendonly"
        ) %>%
        layout(
            xaxis = list(
                title = ""
            ),
            yaxis = list(
                title = "",
                range = c(0, ycv)
            ),
            hovermode = "x unified",
            margin = list(b = 0, l = 0)
        ) %>%
        config(
            displayModeBar = FALSE
        )
    y$elementId <- NULL
    return(y)
}

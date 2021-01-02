tweetTypePlot <- function(x) {
    x$Type <- sprintf("<b>%s </b>", x$Type)
    y <- plot_ly(x) %>%
        add_trace(
            x = ~Percent,
            y = ~Type,
            type = "bar",
            marker = list(
                color = "#32B1E6"
            ),
            hoverinfo = "text",
            text = ~sprintf(
                fmt = "%s (%s)",
                formatNumeric(Freq, "num"),
                formatNumeric(Percent, "pct", 0)
            )
        ) %>%
        layout(
            xaxis = list(
                tickformat = "%",
                title = ""
            ),
            yaxis = list(
                title = "",
                autorange = "reversed"
            ),
            margin = list(b = 0, l = 0)
        ) %>%
        config(
            displayModeBar = FALSE
        )
    y$elementId <- NULL
    return(y)
}

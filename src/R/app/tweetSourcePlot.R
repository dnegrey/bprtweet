tweetSourcePlot <- function(x, barColor = "#32B1E6") {
    x$Source <- paste0(x$Source, " ")
    x$Source <- factor(x$Source, levels = rev(x$Source))
    y <- plot_ly(x) %>%
        add_trace(
            x = ~Percent,
            y = ~Source,
            type = "bar",
            marker = list(
                color = barColor
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
                title = ""
            ),
            margin = list(b = 0, l = 0)
        ) %>%
        config(
            displayModeBar = FALSE
        )
    y$elementId <- NULL
    return(y)
}

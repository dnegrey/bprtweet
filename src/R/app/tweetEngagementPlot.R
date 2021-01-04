tweetEngagementPlot <- function(x, barColor = "#32B1E6") {
    x$Metric <- paste0(x$Metric, " ")
    x$Metric <- factor(x$Metric, levels = rev(x$Metric))
    y <- plot_ly(x) %>%
        add_trace(
            x = ~Percent,
            y = ~Metric,
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
            margin = list(b = 0, l = 0),
            annotations = list(
                x = 0.5,
                y = "Neither ",
                text = "<b style='font-size: smaller;'>* Plot excludes retweets</b>",
                showarrow = FALSE,
                xanchor = "left",
                font = list(
                    color = "#0D2935"
                )
            )
        ) %>%
        config(
            displayModeBar = FALSE
        )
    y$elementId <- NULL
    return(y)
}

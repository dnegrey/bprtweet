tweetTypeSummary <- function(x = main$timeline) {
    y <- data.frame(
        Type = c("Original", "Quote", "Retweet"),
        Freq = c(
            sum(!x$is_quote & !x$is_retweet),
            sum(x$is_quote),
            sum(x$is_retweet)
        ),
        stringsAsFactors = FALSE
    ) %>%
        arrange(desc(Freq))
    y$Percent <- y$Freq/sum(y$Freq)
    return(y)
}

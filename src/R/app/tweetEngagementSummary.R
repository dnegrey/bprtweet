tweetEngagementSummary <- function(x = main$timeline) {
    y <- x %>%
        filter(!is_retweet)
    z <- data.frame(
        Metric = c(
            "Liked",
            "Retweeted",
            "Liked & Retweeted",
            "Neither"
        ),
        Freq = c(
            sum(y$favorite_count > 0),
            sum(y$retweet_count > 0),
            sum(y$favorite_count > 0 & y$retweet_count > 0),
            sum(y$favorite_count == 0 & y$retweet_count == 0)
        ),
        stringsAsFactors = FALSE
    )
    z$Total <- nrow(y)
    z$Percent <- z$Freq/z$Total
    return(z)
}

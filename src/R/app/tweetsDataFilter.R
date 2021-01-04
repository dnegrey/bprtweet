tweetsDataFilter <- function(x, dateRange, tweetType,
                             liked, retweeted, tweetSource) {
    if (liked == "Both") {
        liked <- c("Yes", "No")
    }
    if (retweeted == "Both") {
        retweeted <- c("Yes", "No")
    }
    if (is.null(tweetType)) {
        tweetType <- character()
    }
    if (length(tweetSource) == 0) {
        tweetSource <- uniqueValues(x$source)
    }
    y <- x %>%
        mutate(
            Date = datepart(created_at),
            Type = ifelse(
                is_retweet, "Retweet",
                ifelse(
                    is_quote, "Quote", "Original"
                )
            ),
            Liked = ifelse(favorite_count > 0, "Yes", "No"),
            Retweeted = ifelse(retweet_count > 0, "Yes", "No")
        ) %>%
        filter(
            Date >= dateRange[1],
            Date <= dateRange[2],
            Type %in% tweetType,
            Liked %in% liked,
            Retweeted %in% retweeted,
            source %in% tweetSource
        ) %>%
        select(
            status_id,
            created_at,
            source,
            is_quote,
            is_retweet,
            hashtags,
            mentions_screen_name,
            favorite_count,
            retweet_count,
            text
        )
    return(y)
}

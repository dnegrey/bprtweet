hashtagSummary <- function(x, dateRange, tweetType,
                           liked, retweeted, minCount) {
    if (liked == "Both") {
        liked <- c("Yes", "No")
    }
    if (retweeted == "Both") {
        retweeted <- c("Yes", "No")
    }
    if (is.null(tweetType)) {
        tweetType <- character()
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
            Retweeted %in% retweeted
        )
    yh <- unlist(y$hashtags)
    if (!is.null(yh)) {
        z <- data.frame(
            word = yh,
            stringsAsFactors = FALSE
        ) %>%
            group_by(word) %>%
            summarise(freq = n()) %>%
            data.frame() %>%
            filter(!is.na(word)) %>%
            arrange(desc(freq)) %>%
            filter(freq >= minCount)
        yh0 <- nrow(z) == 0
    } else {
        yh0 <- TRUE
    }
    if (yh0) {
        z <- data.frame(
            word = c("NULL", ""),
            freq = c(2, 1),
            stringsAsFactors = FALSE
        )
    }
    return(z)
}

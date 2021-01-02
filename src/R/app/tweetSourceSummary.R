tweetSourceSummary <- function(x = main$timeline) {
    y <- x %>%
        mutate(Source = source) %>%
        group_by(Source) %>%
        summarise(Freq = n()) %>%
        data.frame() %>%
        arrange(desc(Freq))
    y$Percent <- y$Freq/sum(y$Freq)
    return(y)
}

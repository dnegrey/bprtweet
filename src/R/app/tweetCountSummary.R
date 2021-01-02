tweetCountSummary <- function(unit, x = main$timeline) {
    y <- x %>%
        mutate(
            CreateDate = floor_date(datepart(created_at), unit = unit),
            Original = !is_quote & !is_retweet,
            Quote = is_quote,
            Retweet = is_retweet,
            Total = 1
        ) %>%
        group_by(CreateDate) %>%
        summarise(
            Original = sum(Original),
            Quote = sum(Quote),
            Retweet = sum(Retweet),
            Total = sum(Total)
        ) %>%
        data.frame()
    yd <- range(y$CreateDate)
    z <- data.frame(
        CreateDate = seq.Date(yd[1], yd[2], 1)
    ) %>%
        mutate(
            CreateDate = floor_date(CreateDate, unit = unit)
        ) %>%
        unique() %>%
        left_join(y, "CreateDate") %>%
        mutate(
            Original = ifelse(is.na(Original), 0, Original),
            Quote = ifelse(is.na(Quote), 0, Quote),
            Retweet = ifelse(is.na(Retweet), 0, Retweet),
            Total = ifelse(is.na(Total), 0, Total)
        ) %>%
        arrange(CreateDate)
    return(z)
}

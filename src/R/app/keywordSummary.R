keywordSummary <- function(x, type, mincount = NULL) {
    xt <- switch(
        EXPR = type,
        "h" = "hashtags",
        "m" = "mentions_screen_name"
    )
    y <- data.frame(
        word = unlist(x[, xt]),
        stringsAsFactors = FALSE
    ) %>%
        group_by(word) %>%
        summarise(count = n()) %>%
        data.frame() %>%
        filter(!is.na(word)) %>%
        arrange(desc(count))
    if (!is.null(mincount)) {
        y <- y %>%
            filter(count >= mincount)
    }
    return(y)
}

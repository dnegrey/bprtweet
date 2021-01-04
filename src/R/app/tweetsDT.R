tweetsDT <- function(x) {
    # link templates
    sl <- "https://twitter.com/blastpointinc/status/%s"
    sl <- sprintf(
        fmt = "<a href='%s' target=_blank>%s</a>",
        sl,
        "%s"
    )
    hl <- "https://twitter.com/search?q=%s%s"
    hl <- sprintf(
        fmt = "<a href='%s' target=_blank>#%s</a>",
        hl,
        "%s"
    )
    al <- "https://twitter.com/%s"
    al <- sprintf(
        fmt = "<a href='%s' target=_blank>@%s</a>",
        al,
        "%s"
    )
    y <- x
    y$status_id <- sprintf(sl, y$status_id, y$status_id)
    y$created_at <- format(y$created, "%F %r")
    y$hashtags <- unlist(lapply(y$hashtags, function(v, tmp = hl){
        vo <- ""
        if (!all(is.na(v))) {
            v <- v[!is.na(v)]
            va <- sprintf(tmp, "%2523", v, v)
            va <- paste0("<li>", va, "</li>", collapse = "")
            vo <- sprintf(
                fmt = "<ul style='font-weight: bold;padding-left: 20px;'>%s</ul>",
                va
            )
        }
        return(vo)
    }))
    y$mentions_screen_name <- unlist(lapply(y$mentions_screen_name, function(v, tmp = al){
        vo <- ""
        if (!all(is.na(v))) {
            v <- v[!is.na(v)]
            va <- sprintf(tmp, v, v)
            va <- paste0("<li>", va, "</li>", collapse = "")
            vo <- sprintf(
                fmt = "<ul style='font-weight: bold;padding-left: 20px;'>%s</ul>",
                va
            )
        }
        return(vo)
    }))
    z <- datatable(
        data = y,
        class = "row-border hover nowrap cell-border",
        escape = FALSE,
        selection = "none",
        extensions = "Scroller",
        options = list(
            dom = "lftipr",
            pageLength = 25,
            scrollY = 650,
            scrollCollapse = "FALSE",
            autoWidth = TRUE,
            scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c(
            "Status",
            "Created",
            "Source",
            "Quote",
            "Retweet",
            "Hashtags",
            "Mentions",
            "Liked",
            "Retweeted",
            "Text"
        )
    )
    return(z)
}

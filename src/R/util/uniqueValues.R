uniqueValues <- function(x, na.rm = TRUE) {
    y <- unique(x)
    y <- y[order(y)]
    if (na.rm) {
        y <- y[!is.na(y)]
    }
    return(y)
}

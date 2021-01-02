getData <- function(dir) {
    # load twitter api token
    token <- readRDS("~/.twitter_token.rds")
    # user screen name
    user <- "blastpointinc"
    # get timeline
    RDataCreate(
        dir = dir,
        x = "timeline",
        fun = get_timeline,
        args = list(
            user = user,
            n = 2000,
            token = token
        )
    )
    # get followers
    RDataCreate(
        dir = dir,
        x = "followers",
        fun = lookup_users,
        args = list(
            users = get_followers(
                user = user,
                token = token
            )$user_id,
            token = token
        )
    )
    # get friends
    RDataCreate(
        dir = dir,
        x = "friends",
        fun = lookup_users,
        args = list(
            users = get_friends(
                users = user,
                token = token
            )$user_id,
            token = token
        )
    )
    # geo-code followers based on location
    RDataCreate(
        dir = dir,
        x = "followers_geocode",
        fun = function(followers) {
            x <- followers[c("user_id", "location")]
            x <- split(x, x$user_id)
            y <- lapply(x, function(v){
                ygc <- try(
                    getGeoCode(v$location),
                    silent = TRUE
                )
                lat <- as.numeric(NA)
                lon <- lat
                if (!inherits(ygc, "try-error")){
                    lat <- ygc[["lat"]]
                    lon <- ygc[["lon"]]
                }
                yd <- data.frame(
                    user_id = v$user_id,
                    lat = lat,
                    lon = lon,
                    stringsAsFactors = FALSE
                )
                return(yd)
            })
            y <- do.call(rbind, y)
            row.names(y) <- NULL
            return(y)
        },
        args = list(
            followers = RDataUse(dir, "followers")
        )
    )
    # get favorites
    RDataCreate(
        dir = dir,
        x = "favorites",
        fun = get_favorites,
        args = list(
            user = user,
            n = 2000,
            token = token
        )
    )
    # get retweets
    RDataCreate(
        dir = dir,
        x = "retweets",
        fun = function(timeline, token) {
            x <- timeline %>%
                filter(retweet_count > 0 & !is_retweet) %>%
                select(status_id, retweet_count, is_quote) %>%
                arrange(retweet_count, is_quote)
            for (i in 1:nrow(x)) {
                if (i %% 75 == 1 & i != 1) {
                    catn(sprintf("retweet iteration value is %s", i))
                    Sys.sleep(910)
                }
                xi <- x[i, ]
                xiu <- get_retweeters(xi$status_id, token = token)
                if (nrow(xiu) != xi$retweet_count) {
                    xiu <- xi
                    xiu$user_id <- NA_character_
                } else {
                    xiu <- data.frame(xi, xiu)
                }
                if (i == 1) {
                    y <- xiu
                } else {
                    y <- rbind(y, xiu)
                }
            }
            row.names(y) <- NULL
            return(y)
        },
        args = list(
            timeline = RDataUse(dir, "timeline"),
            token = token
        )
    )
    # get retweeters
    RDataCreate(
        dir = dir,
        x = "retweeters",
        fun = lookup_users,
        args = list(
            users = unique(filter(RDataUse(dir, "retweets"), !is.na(user_id))$user_id),
            token = token
        )
    )
}

analyzeData <- function(dir) {
    # load earlier obtained datasets
    xtl <- RDataUse(dir, "timeline")
    xfr <- RDataUse(dir, "friends")
    xfo <- RDataUse(dir, "followers")
    xfg <- RDataUse(dir, "followers_geocode")
    xfv <- RDataUse(dir, "favorites")
    xtr <- RDataUse(dir, "retweeters")
    xtw <- RDataUse(dir, "retweets")
    bpg <- getGeoCode("128 N Highland Ave, Pittsburgh, PA 15206")
    # main data object
    RDataCreate(
        dir = dir,
        x = "main",
        fun = list,
        args = list(
            user = xtl %>%
                select(
                    user_id,
                    screen_name,
                    account_created_at,
                    name,
                    location,
                    description,
                    profile_expanded_url,
                    profile_banner_url,
                    profile_image_url,
                    followers_count,
                    friends_count,
                    statuses_count,
                    favourites_count
                ) %>% unique() %>% data.frame() %>%
                mutate(
                    lat = bpg[1],
                    lon = bpg[2]
                ),
            followers = xfo %>%
                select(
                    user_id,
                    screen_name,
                    account_created_at,
                    name,
                    location,
                    description,
                    profile_expanded_url,
                    profile_banner_url,
                    profile_image_url,
                    followers_count,
                    friends_count,
                    statuses_count,
                    favourites_count
                ) %>% unique() %>% data.frame() %>%
                left_join(xfg, "user_id"),
            friends = xfr %>%
                select(
                    user_id,
                    screen_name,
                    account_created_at,
                    name,
                    location,
                    description,
                    profile_expanded_url,
                    profile_banner_url,
                    profile_image_url,
                    followers_count,
                    friends_count,
                    statuses_count,
                    favourites_count
                ) %>% unique() %>% data.frame(),
            favorites = xfv %>%
                select(
                    status_id,
                    created_at,
                    source,
                    text,
                    hashtags,
                    mentions_screen_name,
                    is_quote,
                    is_retweet,
                    favorite_count,
                    retweet_count,
                    user_id,
                    screen_name,
                    account_created_at,
                    name,
                    location,
                    description,
                    profile_expanded_url,
                    profile_banner_url,
                    profile_image_url,
                    followers_count,
                    friends_count,
                    statuses_count,
                    favourites_count
                ) %>% data.frame(),
            timeline = xtl %>%
                select(
                    status_id,
                    created_at,
                    source,
                    text,
                    hashtags,
                    mentions_screen_name,
                    is_quote,
                    is_retweet,
                    favorite_count,
                    retweet_count,
                    user_id
                ) %>% data.frame(),
            retweets = xtw %>%
                select(status_id, user_id) %>% data.frame(),
            retweeters = xtr %>%
                select(
                    user_id,
                    screen_name,
                    account_created_at,
                    name,
                    location,
                    description,
                    profile_expanded_url,
                    profile_banner_url,
                    profile_image_url,
                    followers_count,
                    friends_count,
                    statuses_count,
                    favourites_count
                ) %>% unique() %>% data.frame()
        )
    )
}

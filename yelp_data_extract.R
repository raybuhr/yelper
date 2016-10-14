
source(".httr-oauth")

library(httr)
library(jsonlite)

yelpy <- oauth_app("YELP", key = Sys.getenv("YELP_Consumer_Key"), secret = Sys.getenv("YELP_Consumer_Secret"))
sig <- sign_oauth1.0(yelpy, token = Sys.getenv("YELP_Token"), token_secret = Sys.getenv("YELP_Token_Secret"))
# Use a local file to cache OAuth access credentials between R sessions?
# Not necessary if sourcing .httr-oauth
# 1: Yes
# 2: No
# 2

yelp_query <- function(base_url="https://api.yelp.com/v2/search/",
                       search_term="",
                       location="11 e madison chicago il",
                       offset=NULL,
                       limit=20,
                       radius_filter=1000) {
    if (is.null(offset)) {
        query <- paste0(base_url,
                        "?term=", URLencode(search_term),
                        "&location=", URLencode(location),
                        "&limit=", limit,
                        "&radius_filter=", radius_filter)
    }
    else {
        query <- paste0(base_url,
                        "?term=", URLencode(search_term),
                        "&location=", URLencode(location),
                        "&offset=", offset,
                        "&limit=", limit,
                        "&radius_filter=", radius_filter)
    }
    return(query)
}

yelp_data <- function(query) {
    data <- GET(query, sig)
    data_content <- content(data)
    df_json <- fromJSON(toJSON(data_content), flatten = TRUE)
    df <- data.frame(df_json)
    return(df)
}

yelp_data_lean <- function(query) {
    data <- GET(query, sig)
    data_content <- content(data)
    df_json <- fromJSON(toJSON(data_content), flatten = TRUE)
    df <- data.frame(df_json)
    keep <- c("businesses.name", "businesses.display_phone", "businesses.url",
              "businesses.location.address", "businesses.location.postal_code", "businesses.location.neighborhoods",
              "businesses.rating", "businesses.review_count", "businesses.categories")
    df <- df[, keep]
    return(df)
}

# desired data fields
keep <- c("businesses.name", "businesses.display_phone", "businesses.url",
          "businesses.location.address", "businesses.location.postal_code", "businesses.location.neighborhoods",
          "businesses.rating", "businesses.review_count", "businesses.categories")
cnames <- c("business_name", "phone_number", "yelp_link",
            "address", "zip_code", "neighborhood",
            "yelp_rating", "yelp_reviews", "business_categories")

# yelp_df1 <- yelp_data(query = yelp_query())
# yelp_df2 <- yelp_data(query = yelp_query(offset = 21))
# yelp_df3 <- yelp_data(query = yelp_query(offset = 41))
# yelp_df <- bind_rows(yelp_df1[, keep], yelp_df2[, keep], yelp_df2[, keep])

# yelp_df1 <- yelp_data_lean(query = yelp_query())

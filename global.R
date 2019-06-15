library(shiny)
library(plotly)
library(data.table)
library(rPackedBar)

TWITTER_KEYS = jsonlite::read_json("keys.json")
DEFAULT_USER = "@ASpannbauer"
EMPTY_TWEET_DT = data.table::data.table(time  = character(0),
                                        text  = character(0),
                                        user  = character(0),
                                        fav_n = integer(0),
                                        rt_n  = integer(0))

sign_oauth = function() {
  auth = httr::oauth_app("twitter",
                         key = TWITTER_KEYS$api_key,
                         secret = TWITTER_KEYS$api_secret_key)

  httr::sign_oauth1.0(auth,
                      token = TWITTER_KEYS$access_token,
                      token_secret = TWITTER_KEYS$access_token_secret)
}


unescape_html <- function(str) {
  if (is.na(str)) return(NA_character_)
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}


build_request_url = function(user, count, max_id = NULL) {
  max_id = if (is.null(max_id)) "" else sprintf("&max_id=%s", max_id)
  url_template = "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=%s&count=%s%s"
  sprintf(url_template, user, count, max_id)
}


get_user_tweets = function(user, n) {
  oauth_sig = sign_oauth()

  timeline = list()
  max_id = NULL
  n_left = n
  i = 0
  while (n_left > 0) {
    i = i + 1

    tweet_count = min(200, n_left)
    request_url = build_request_url(user, tweet_count, max_id)
    r = httr::GET(request_url, oauth_sig)
    timeline_i = httr::content(r)

    if (length(timeline_i) == 0) break

    timeline = c(timeline, timeline_i)
    max_id = min(vapply(timeline_i, function(tweet) tweet$id, numeric(1)))
    n_left = n_left - tweet_count
  }
  return(timeline)
}


get_user_tweet_info = function(user, n = 1000) {
  tweets = get_user_tweets(user, n)

  tweet_dt_list = lapply(tweets, function(tweet) {
    is_retweet = grepl("^RT", tweet$text)
    if (is_retweet) return(EMPTY_TWEET_DT)

    data.table::data.table(time  = tweet$created_at,
                           text  = unescape_html(tweet$text),
                           user  = tweet$user$screen_name,
                           fav_n = tweet$favorite_count,
                           rt_n  = tweet$retweet_count)
  })
  tweet_dt = data.table::rbindlist(tweet_dt_list)

  tweet_dt[, total_fav_rt := fav_n + rt_n]
  tweet_dt = tweet_dt[order(-total_fav_rt), ]

  tweet_dt[, text_preview := paste0(substr(text, 1, 20), "...")]

  return(tweet_dt)
}


safe_get_user_tweet_info = function(user, n = 1000, default_value = EMPTY_TWEET_DT) {
  tweet_info_dt = default_value
  try({
    tweet_info_dt = get_user_tweet_info(user, n)
  })
  return(tweet_info_dt)
}


# TODO: Fix bug in rPackedBar::packedBarOutput() and delete these functions
packedBarOutput = function(outputId, width = "100%", height = "400px", inline = FALSE,
                            clickedBarInputId = paste0(outputId, "_clicked")) {
  plotly_out = plotly::plotlyOutput(outputId)
  shiny::div(get_clicked_packed_bar(outputId, clickedBarInputId),
             plotly_out)
}


get_clicked_packed_bar = function(outputId, inputId) {
  shiny::tags$script(
    shiny::HTML(
      sprintf(
        "$(document).on('plotly_click', '#%s', function() {",
        outputId
      ),
      sprintf(
        "out = document.querySelector(\"#%s > div > div > svg:nth-child(5) > g.hoverlayer > g.hovertext > text > tspan:nth-child(1)\").innerHTML;",
        outputId
      ),
      sprintf(
        "Shiny.onInputChange('%s', out);",
        inputId
      ),
      "});"
      )
    )
}

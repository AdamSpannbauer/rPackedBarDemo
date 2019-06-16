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


unescape_html = function(str) {
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


guess_bar_count_details = function(x) {
  fit_x = seq_along(x)
  fit_y = sort(x, decreasing = TRUE)

  range_fit_x = c(utils::head(fit_x, 1), utils::tail(fit_x, 1))
  range_fit_y = c(utils::head(fit_y, 1), utils::tail(fit_y, 1))

  fit_df = data.frame(x = range_fit_x, y = range_fit_y)
  fit = stats::lm(y ~ x, data = fit_df)

  details_dt_list = lapply(seq_along(x), function(i) {
    new_obs = data.frame(x = i)
    pred = stats::predict(fit, new_obs)
    obs = fit_y[i]

    data.table::data.table(
      x = i,
      fit = pred,
      actual = obs,
      distance = stats::dist(rbind(pred, obs))[1]
    )
  })

  details_dt = data.table::rbindlist(details_dt_list)
  details_dt[, is_elbow := distance == max(distance)]

  elbow_ind = which(details_dt$is_elbow)[1]
  details_dt[-elbow_ind, is_elbow := FALSE]

  return(details_dt)
}


plot_guess = function(x, min_bar = 3, max_bar = 25) {
  details_dt = guess_bar_count_details(x)

  plot_dt = data.table::melt(details_dt,
                             id.vars = c("x", "is_elbow"),
                             measure.vars = c("fit", "actual"))

  plot_dt[, min_bar := min_bar]
  plot_dt[, max_bar := max_bar]
  guess_value = details_dt[!!is_elbow, x]


  plotly::plot_ly(details_dt,
                  x = ~x,
                  y = ~actual,
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "#00aced"),
                  name = "Actual Data") %>%
    plotly::add_trace(details_dt,
                      x = ~x,
                      y = ~fit,
                      type = "scatter",
                      mode = "lines",
                      line = list(color = "#A9A9A9"),
                      name = "Fit") %>%
    plotly::add_segments(x = min_bar,
                         xend = min_bar,
                         y = 0,
                         yend = max(details_dt$actual),
                         line = list(color = "#51001b", dash = "dash"),
                         name = "Min Bar Count") %>%
    plotly::add_segments(x = max_bar,
                         xend = max_bar,
                         y = 0,
                         yend = max(details_dt$actual),
                         line = list(color = "#51001b", dash = "dash"),
                         name = "Max Bar Count") %>%
    plotly::add_trace(data = details_dt[!!is_elbow, ],
                      x = ~x,
                      y = ~actual,
                      type = "scatter",
                      mode = "markers",
                      line = NULL,
                      marker = list(size = 10, color = "#022f56"),
                      name = sprintf("Guessed Elbow*<br><sub>*Max Distance from Fit at x=%s</sub>", guess_value)) %>%
    plotly::layout(xaxis = list(title = "Index"),
                   yaxis = list(title = "Value"),
                   title = "Visualize Guess",
                   plot_bgcolor = "rgb(245, 245, 245)",
                   paper_bgcolor = "rgb(245, 245, 245)") %>%
    plotly::config(displayModeBar = FALSE)
}


# TODO: Delete these functions when rPackedBar 0.2.2 on CRAN
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


guess_bar_count = function(x, min_bar = 3, max_bar = 25) {
  fit_x = seq_along(x)
  fit_y = sort(x, decreasing = TRUE)

  range_fit_x = c(utils::head(fit_x, 1), utils::tail(fit_x, 1))
  range_fit_y = c(utils::head(fit_y, 1), utils::tail(fit_y, 1))

  fit_df = data.frame(x = range_fit_x, y = range_fit_y)

  # Creating straight line between the max values
  fit = stats::lm(y ~ x, data = fit_df)

  # Distance from point to line
  distances = vapply(seq_along(x), function(i) {
    new_obs = data.frame(x = i)
    pred = c(i, stats::predict(fit, new_obs))
    obs = c(i, fit_y[i])

    stats::dist(rbind(pred, obs))[1]
  }, numeric(1))  # nolint

  # Max distance point
  elbow_ind = which.max(distances) - 1

  if (min_bar > elbow_ind) elbow_ind = min_bar
  if (max_bar < elbow_ind) elbow_ind = max_bar

  return(elbow_ind)
}

shinyServer(function(input, output, session) {

  output$user_name_input_ui = renderUI({
    queried_user = parseQueryString(session$clientData$url_search)[["user"]]
    default_user = if (is.null(queried_user)) DEFAULT_USER else queried_user

    div(
      textInput(
        inputId = "user_name_input",
        label = "Twitter Username",
        value = default_user,
        placeholder = default_user
      ),
      actionButton(
        inputId = "get_tweet_button",
        label = "Search Twitter",
        icon = icon("twitter"),
        style = "color: #fff; background-color: #00aced; border-color: #2e6da4"
      )
    )
  })


  tweet_dt = eventReactive(input$get_tweet_button, {
    user_name = gsub("^\\@", "", input$user_name_input)

    setTimeLimit(elapsed = 15)
    tweet_info_dt = safe_get_user_tweet_info(user_name, 1000)
    setTimeLimit(elapsed = Inf)

    if (nrow(tweet_info_dt) == 0) return(NULL)
    tweet_info_dt[total_fav_rt > 0, ]
  }, ignoreNULL = FALSE)


  most_popular_tweet = reactive({
    req(tweet_dt())
    tweet_dt()[order(-total_fav_rt), ][1, ]
  })


  packed_bar_plot = reactive({
    req(tweet_dt())

    set.seed(42)
    if (nrow(tweet_dt()) < 10) return(plotly::plotly_empty())

    p = rPackedBar::plotly_packed_bar(input_data = tweet_dt(),
                                      label_column = "text_preview",
                                      value_column = "total_fav_rt",
                                      plot_title = "Tweet Interactions<br><sub>(click a bar to view more about tweet)</sub>",
                                      xaxis_label = "Favorites & RTs",
                                      hover_label = "Favs & RTs",
                                      min_label_width = .03,
                                      color_bar_color = "#00aced",
                                      label_color = "white")

    plotly::config(p, displayModeBar = FALSE)
  })


  output$rendered_packed_bar = plotly::renderPlotly({
    packed_bar_plot()
  })


  output$packed_bar_ui = renderUI({
    req(tweet_dt())

    if (nrow(tweet_dt()) < 10) {
      HTML("<h3>Error getting tweets ¯\\_(ツ)_/¯</h3>")
    } else {
      fluidRow(packedBarOutput("rendered_packed_bar"))
    }
  })


  output$clicked_tweet_ui = renderUI({
    req(packed_bar_plot())

    filter_text = input$rendered_packed_bar_clicked
    filter_text = if (is.null(filter_text)) NA_character_ else filter_text
    filter_text = unescape_html(filter_text)

    tweet = tweet_dt()[text_preview == filter_text, .(text, rt_n, fav_n)][1, ]
    if (nrow(tweet) == 0 | is.na(filter_text)) {
      tweet = most_popular_tweet()[, .(text, rt_n, fav_n)]
    }

    if (anyNA(tweet)) return(HTML(""))

    wellPanel(
      HTML(
        sprintf(
          paste(
            "<h4 align='left'>%s</h4>",
            "<h5 align='right'>",
                "%s<i class='fa fa-retweet'></i>",
                "&nbsp;&nbsp;&nbsp;",
                "%s<i class='fa fa-heart'></i>",
            "</h5>"
          ),
          tweet$text, tweet$rt_n, tweet$fav_n
        )
      )  #  HTML
    )  # wellPanel
  })
})  # shinyServer

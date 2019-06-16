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


  plot_n_color_bar = eventReactive(input$replot_packed_bars, {
    if (isTRUE(input$guess_bar_count)) {
      rPackedBar:::guess_bar_count(tweet_dt()$total_fav_rt,
                                   input$guess_bar_count_range[1],
                                   input$guess_bar_count_range[2])
    } else {
      input$select_bar_count
    }
  })


  packed_bar_plot = reactive({
    req(tweet_dt())

    set.seed(42)
    if (nrow(tweet_dt()) < 10) return(plotly::plotly_empty())

    plot_n_row = "guess"
    try({plot_n_row = plot_n_color_bar()})

    p = rPackedBar::plotly_packed_bar(input_data = tweet_dt(),
                                      label_column = "text_preview",
                                      value_column = "total_fav_rt",
                                      number_rows = plot_n_row,
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


  output$guess_bar_plot = plotly::renderPlotly({
    req(input$guess_bar_count_range)

    min_bar = input$guess_bar_count_range[1]
    max_bar = input$guess_bar_count_range[2]

    plot_guess(tweet_dt()$total_fav_rt, min_bar, max_bar)
  })


  output$select_n_bars = renderUI({
    req(nrow(tweet_dt()) > 0)

    column(
      width = 8,
      offset = 2,
      wellPanel(
        h3("Fine Tune Plot"),
        p(
          "These controls allow you to adjust the number of ",
          "rows in the Packed Bar Chart.",
          br(),
          "This corresponds to the",
          code("number_rows"),
          "option in",
          code("rPackedBar::plotly_packed_bar()"),
          br(),
          "The default chart uses the guess option."
        ),
        hr(),
        fluidRow(
          sliderInput(inputId = "select_bar_count",
                      label = "Select Chart Row Count",
                      min = 1,
                      max = 50,
                      step = 1,
                      value = rPackedBar:::guess_bar_count(tweet_dt()$total_fav_rt),
                      width = "50%"),
          sliderInput(inputId = "guess_bar_count_range",
                      label = "Guess Range Bounds",
                      min = 1,
                      max = 50,
                      step = 1,
                      value = c(3, 25),
                      width = "50%")
        ),  # fluidRow
        br(),
        fluidRow(
          checkboxInput(inputId = "guess_bar_count",
                        label = "Guess Row Count",
                        value = TRUE)
        ),  # fluidRow
        fluidRow(
          actionButton(inputId = "replot_packed_bars",
                       label = "Re-Draw Plot",
                       icon = icon("bar-chart"))
        ),  # fluidRow
        br(),
        hr(),
        br(),
        fluidRow(
          plotly::plotlyOutput("guess_bar_plot")
        )  # fluidRow
      )  # wellPanel
    )  # column
  })
})  # shinyServer

shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme("cosmo"),
    fluidRow(
      column(
        width = 8,
        offset = 2,
        align = "center",
        HTML(
          paste0("<br><h2>Packed Bar Charts using the",
                 " <a href='https://CRAN.R-project.org/package=rPackedBar'",
                 " target='_blank'>rPackedBar</a> package</h2>",
                 "<h6>this is just a small demo app; stuff might break</h6><hr><br>")
        )
      )  # column
    ),  # fluidRow
    fluidRow(
      column(
        width = 4,
        offset = 1,
        align = "left",
        uiOutput("user_name_input_ui")
      ),  # column
      column(
        width = 6,
        align = "right",
        uiOutput("clicked_tweet_ui")
      )  # column
    ),  # fluidRow
    br(),
    fluidRow(
      column(
        width = 10,
        offset = 1,
        align = "center",
        shinycssloaders::withSpinner(
          type = 6,
          color = "#A9A9A9",
          uiOutput("packed_bar_ui")
        )
      )  # column
    ),  # fluidRow
    br(),
    br()
  )  # fluidPage
)  # shinyUI

server <- function(input, output, session) {

  # navigation
  observe(nav_select("container", input$nav))

  # data science
  ppServer("pp")
  aggrServer("aggr")
  vsServer("vs")

  # mass tools
  enviPatServer("enviPat")
  tfServer("tf")

  # toolkits
  md5Server("md5")

}
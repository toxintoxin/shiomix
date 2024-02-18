ui <- tagList(
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
  useShinyjs(),
  page_navbar(
    theme = bs_theme(bootswatch = "minty"),
    title = "Shiomix",
    nav_panel("Preprocessing", ppUI("pp")),
    nav_panel("Visualization", vsUI("vs")),
    nav_panel("Aggregate", aggrUI("aggr")),
    nav_panel("TraceFinder's export to Matrix", tfUI("tf")),
    nav_panel("enviPat", enviPatUI("enviPat")),
    nav_panel("MD5 Check", md5UI("md5"))
  )
)
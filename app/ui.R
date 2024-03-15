ui <- tagList(
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
  useShinyjs(),
  page_fillable(
    theme = bs_theme(preset = "bootstrap"),
    title = "Shiomix",
    navset_pill_list(widths = c(2, 10),
      nav_panel("Introduction", includeMarkdown("homepage.md")),
      "Data Science",
      nav_panel("Introduction", includeMarkdown("data-science/data-science.md")),
      # nav_panel("Aggregate", aggrUI("aggr")),
      nav_menu("Statistics", align = "right",
        nav_panel("ttest", ttestUI("ttest"))
      ),
      nav_menu("Visualization", align = "right",
        nav_panel("饼图", pieUI("pie")),
        nav_panel("柱状图", barUI("bar")),
        nav_panel("火山图", volcanoUI("volcano")),
        nav_panel("南丁格尔玫瑰图", nightingaleUI("nightingale"))
      ),
      "Omics",
      nav_panel("Introduction", includeMarkdown("omics/omics.md")),
      nav_panel("Data Preprocessing", ppUI("pp")),
      "Mass tools",
      nav_panel("Introduction", includeMarkdown("mass-tools/mass-tools.md")),
      nav_panel("enviPat", enviPatUI("enviPat")),
      nav_panel("TraceFinder export to Matrix", tfUI("tf")),
      "Toolkits",
      nav_panel("Introduction", includeMarkdown("toolkits/toolkits.md")),
      nav_panel("MD5 Check", md5UI("md5"))
    )
  )
)
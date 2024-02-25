ui <- tagList(
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
  useShinyjs(),
  page_fillable(padding = 0,
    theme = bs_theme(bootswatch = "minty"),
    title = "Shiomix",
    layout_sidebar(border_radius = FALSE, class = "p-0",
      sidebar = sidebar(width = "250px", open = "always",
        title = "Navigation",
        HTML(
          '<div id="nav" class="form-group shiny-input-radiogroup shiny-input-container shiny-bound-input" role="radiogroup" aria-labelledby="nav-label">
              <label class="control-label shiny-label-null" for="nav" id="nav-label"></label>
              <div class="shiny-options-group">
                <div class="radio">
                  <label><input type="radio" name="nav" value="Introduction" checked="checked"><b>Introduction</b></label>
                </div>
                <div class="radio">
                  <label><input type="radio" name="nav" value="Data Science""><b>Data Science</b></label>
                </div>
                <div class="radio">
                  <label><input type="radio" name="nav" value="Aggregate"><span>Aggregate</span></label>
                </div>
                <div class="radio">
                  <label><input type="radio" name="nav" value="Visualization"><span>Visualization</span></label>
                </div>
                <div class="radio">
                  <label><input type="radio" name="nav" value="Omics"><b>Omics</b></label>
                </div>
                <div class="radio">
                  <label><input type="radio" name="nav" value="Data Preprocessing"><span>Data Preprocessing</span></label>
                </div>
                <div class="radio">
                  <label><input type="radio" name="nav" value="Mass tools"><b>Mass tools</b></label>
                </div>
                <div class="radio">
                  <label><input type="radio" name="nav" value="enviPat"><span>enviPat</span></label>
                </div>
                <div class="radio">
                  <label><input type="radio" name="nav" value="TraceFinder export to Matrix"><span>TraceFinder export to Matrix</span></label>
                </div>
                <div class="radio">
                  <label><input type="radio" name="nav" value="Toolkits"><b>Toolkits</b></label>
                </div>
                <div class="radio">
                  <label><input type="radio" name="nav" value="MD5 Check"><span>MD5 Check</span>
                  </label>
                </div>
              </div>
            </div>'
        )
        # radioButtons(
        #   inputId = "nav",
        #   label = NULL,
        #   choices = c("Preprocessing", "Visualization", "Aggregate", "TraceFinder export to Matrix", "enviPat", "MD5 Check")
        # )
      ),
      border = FALSE,
      navset_hidden(
        id = "container",
        nav_panel_hidden("Introduction", includeMarkdown("homepage.md")),
        nav_panel_hidden("Data Science", includeMarkdown("data-science/data-science.md")),
        nav_panel_hidden("Aggregate", aggrUI("aggr")),
        nav_panel_hidden("Visualization", vsUI("vs")),
        nav_panel_hidden("Omics", includeMarkdown("omics/omics.md")),
        nav_panel_hidden("Data Preprocessing", ppUI("pp")),
        nav_panel_hidden("Omics", includeMarkdown("mass-tools/mass-tools.md")),
        nav_panel_hidden("enviPat", enviPatUI("enviPat")),
        nav_panel_hidden("TraceFinder export to Matrix", tfUI("tf")),
        nav_panel_hidden("Omics", includeMarkdown("toolkits/toolkits.md")),
        nav_panel_hidden("MD5 Check", md5UI("md5"))
      )
    )
  )
)
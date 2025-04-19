#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {

  addResourcePath("www", system.file("app/www", package = "shiomix"))

  tagList(
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),

      tags$title("Shiomix"),

      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      # bootstrap css:
      tags$link(
        href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css",
        rel = "stylesheet",
        integrity = "sha384-9ndCyUaIbzAi2FUVXJi0CjmCapSmO7SnpJef0486qhLnuZ2cdeRhO02iuK6FUUVM",
        crossorigin = "anonymous"
      ),
      # google fonts:
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Quicksand:wght@300;400;500;600;700&display=swap",
        rel = "stylesheet"
      ),
    ),
    # shinyjs::useShinyjs(),
    page_fillable(
      padding = 0,
      layout_sidebar(
        sidebar = sidebar(
          radioButtons(
            inputId = "radio",
            label = "Panels",
            choices = list(

              "Data preprocessing" = "pp",
              "minimal-test" = "minimal_test",
              "PCA Analysis" = "pca",
              "Option 2" = "B",
              "Option 3" = "C",
              "Option 4" = "D",
              "Option 5" = "E",
              "Option 6" = "F",
              "Option 7" = "G",
              "Option 8" = "H",
              "Option 9" = "I",
              "Option 10" = "J",
              "Option 11" = "K",
              "Option 12" = "L",
              "Option 13" = "M",
              "Option 14" = "N",
              "Option 15" = "O",
              "Option 16" = "P",
              "Option 17" = "Q",
              "Option 18" = "R",
              "Option 19" = "S",
              "Option 20" = "T",
              "Option 21" = "U",
              "Option 22" = "V",
              "Option 23" = "W",
              "Option 24" = "X",
              "Option 25" = "Y",
              "Option 26" = "Z"

            )
          )
        ),
        navset_hidden(
          id = "container",
          nav_panel_hidden("pp", pp_ui("pp")),
          nav_panel_hidden("pca", pca_ui("pca")),
          nav_panel_hidden("minimal_test", minimal_test_ui("mini")),
          !!!lapply(LETTERS, function(letter) {
            nav_panel_hidden(letter, paste("Here is panel", letter))
          })
        )
      )
    ),

    # tags$body(
    #   # class = "bg-light",
    #   bootstrapLib(theme = bslib::bs_theme(version = 5)),
    #   suppressDependencies("bootstrap"),
    #   tags$div(
    #     class = "d-flex vh-100",
    #     # sidebar
    #     tags$div(
    #       class = "flex-shrink-0 p-3 bg-white border-end shadow-sm",
    #       style = "width: 260px; overflow-y: auto;",
    #       tags$a(
    #         class = paste(
    #           "d-flex align-items-center pb-3 mb-3 link-body-emphasis",
    #           "text-decoration-none border-bottom"
    #         ),
    #         tags$span(
    #           style = "font-size: 2rem; font-family: 'Courier New', monospace; color: #333; font-weight: bold;",
    #           "Shiomix"
    #         )
    #       ),
    #       tags$ul(
    #         class = "list-unstyled ps-0",
    #         tags$li(
    #           tags$span(
    #             class = "fs-5 fw-semibold ps-2",
    #             "Data Science"
    #           ),
    #         ),
    #         tags$li(
    #           lass = "mb-1",
    #           create_sidebar_menu_header(
    #             title = "Statistics",
    #             data_bs_target = "#statistics_collapse"
    #           ),
    #           tags$div(
    #             class = "collapse",
    #             id = "statistics_collapse",
    #             tags$ul(
    #               class = "btn-toggle-nav list-unstyled fw-normal pb-1 small",
    #               create_sidebar_link(id = "ttest", label = "ttest"),
    #             )
    #           )
    #         ),
    #         tags$li(
    #           lass = "mb-1",
    #           create_sidebar_menu_header(
    #             title = "Visualization",
    #             data_bs_target = "#visualization_collapse"
    #           ),
    #           tags$div(
    #             class = "collapse",
    #             id = "visualization_collapse",
    #             tags$ul(
    #               class = "btn-toggle-nav list-unstyled fw-normal pb-1 small",
    #               create_sidebar_link(id = "pie", label = "饼图"),
    #               create_sidebar_link(id = "bar", label = "柱状图"),
    #               create_sidebar_link(id = "volcano", label = "火山图"),
    #               create_sidebar_link(id = "nightingale", label = "南丁格尔玫瑰图"),
    #             )
    #           )
    #         ),
    #         tags$li(class = "border-top my-3"),
    #         tags$li(
    #           tags$span(
    #             class = "fs-5 fw-semibold ps-2",
    #             "Omics"
    #           ),
    #         ),
    #         tags$li(
    #           lass = "mb-1",
    #           create_sidebar_menu_header(
    #             title = "Universal",
    #             data_bs_target = "#universal_collapse"
    #           ),
    #           tags$div(
    #             class = "collapse",
    #             id = "universal_collapse",
    #             tags$ul(
    #               class = "btn-toggle-nav list-unstyled fw-normal pb-1 small",
    #               create_sidebar_link(id = "data_preprocessing", label = "Data Preprocessing"),
    #             )
    #           )
    #         ),
    #         # minimal test
    #         tags$li(
    #           lass = "mb-1",
    #           create_sidebar_menu_header(
    #             title = "MINIMAL TEST",
    #             data_bs_target = "#minimal_test_collapse"
    #           ),
    #           tags$div(
    #             class = "collapse",
    #             id = "minimal_test_collapse",
    #             tags$ul(
    #               class = "btn-toggle-nav list-unstyled fw-normal pb-1 small",
    #               create_sidebar_link(id = "minimal_test", label = "minimal test"),
    #             )
    #           )
    #         ),
    #         # ------------------------------------------------------------------
    #         tags$li(class = "border-top my-3"),
    #         tags$li(
    #           tags$span(
    #             class = "fs-5 fw-semibold ps-2",
    #             "Mass tools"
    #           ),
    #         ),
    #         tags$li(
    #           lass = "mb-1",
    #           create_sidebar_menu_header(
    #             title = "enviPat",
    #             data_bs_target = "#enviPat_collapse"
    #           ),
    #           tags$div(
    #             class = "collapse",
    #             id = "enviPat_collapse",
    #             tags$ul(
    #               class = "btn-toggle-nav list-unstyled fw-normal pb-1 small",
    #               create_sidebar_link(id = "formula_handle", label = "formula handle"),
    #             )
    #           )
    #         ),
    #         tags$li(class = "border-top my-3"),
    #         tags$li(
    #           tags$span(
    #             class = "fs-5 fw-semibold ps-2",
    #             "Toolkits"
    #           ),
    #         ),
    #         tags$li(
    #           class = "mb-1",
    #           create_sidebar_menu_header(
    #             title = "Mess",
    #             data_bs_target = "#mess_collapse"
    #           ),
    #           tags$div(
    #             class = "collapse",
    #             id = "mess_collapse",
    #             tags$ul(
    #               class = "btn-toggle-nav list-unstyled fw-normal pb-1 small",
    #               create_sidebar_link(id = "md5_check", label = "MD5 check"),
    #             )
    #           )
    #         ),
    #       )
    #     ),
    #     # main
    #     tags$div(
    #       class = "main flex-grow-1",
    #       style = "padding: 10px; overflow-y: auto;",
    #       tabsetPanel(
    #         id = "tabs",
    #         type = "hidden",
    #         tabPanelBody(
    #           value = "ttest",
    #           tags$h3("here is ttest panel")
    #         ),
    #         tabPanelBody(
    #           value = "minimal_test",
    #           minimal_test_ui("mini")
    #         ),
    #         tabPanelBody(
    #           value = "data_preprocessing",
    #           pp_ui("pp")
    #         ),
    #       )
    #     )
    #   )
    # )
  )

}

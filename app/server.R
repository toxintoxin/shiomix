server <- function(input, output, session) {

  rv <- reactiveValues()

  ppServer("pp", rv)

  vsServer("vs", rv)

  aggrServer("aggr", rv)

  tfServer("tf", rv)

  enviPatServer("enviPat", rv)

  md5Server("md5", rv)

}
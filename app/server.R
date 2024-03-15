server <- function(input, output, session) {

  # data science
  # aggrServer("aggr")
  statServer("stat")
  lapply(stat_types, function(stat_type) {
    get(paste0(stat_type, "Server"))(stat_type)
  })
  vsServer("vs")
  lapply(vs_types, function(vs_type) {
    get(paste0(vs_type, "Server"))(vs_type)
  })

  # omics
  ppServer("pp")

  # mass tools
  enviPatServer("enviPat")
  tfServer("tf")

  # toolkits
  md5Server("md5")

}
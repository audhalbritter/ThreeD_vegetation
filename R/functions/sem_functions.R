# Structural equation model functions

plot_sem <- function(fit, lay){

  # nod <- data.frame(name = c("warming", "nitrogen", "grazing", "biomass", "diversity"),
  #                   colour = c("#FD6467", "darkslateblue", "darkgoldenrod1",
  #                              "olivedrab3", "darkcyan"))

  graph_data <- prepare_graph(model = fit, layout = lay)
  plot(graph_data)

}

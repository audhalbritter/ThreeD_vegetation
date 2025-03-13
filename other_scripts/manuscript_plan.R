#
manuscript_plan <- list(

  # manuscript
  tar_quarto(name = manuscript,
             path = "Main.qmd"),

  # SI
  tar_quarto(name = si,
             path = "SI.qmd")

)

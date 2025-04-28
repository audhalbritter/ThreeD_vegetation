#
manuscript_plan <- list(

  # manuscript
  tar_quarto(name = manuscript,
             path = "manuscript/main_manuscript.qmd"),

  # SI
  tar_quarto(name = si,
             path = "manuscript/SI.qmd")

)

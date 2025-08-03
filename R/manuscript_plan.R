# manuscript

manuscript_plan <- list(

  # manuscript
  tar_quarto(name = ms,
    path = "manuscript/main_manuscript.qmd"),

  # SI
  tar_quarto(name = si,
    path = "manuscript/SI.qmd")

  # tar_target(
  #   name = render_manuscript_pdf,
  #   command = {
  #     quarto::quarto_render(
  #       input = "manuscript/main_manuscript.qmd",
  #       output_format = "pdf"
  #     )
  #   }
  # ),

  # tar_target(
  #   name = render_manuscript_docx,
  #   command = {
  #     quarto::quarto_render(
  #       input = "manuscript/main_manuscript.qmd",
  #       output_format = "docx"
  #     )
  #   }
  # )

)
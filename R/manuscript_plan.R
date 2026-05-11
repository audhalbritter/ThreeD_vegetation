# manuscript

manuscript_plan <- list(

  tar_target(
    name = manuscript_stat_files,
    command = c(
      "manuscript/title_page.qmd",
      "manuscript/abstract.qmd",
      "manuscript/introduction.qmd",
      "manuscript/methods.qmd",
      "manuscript/SEM_output.qmd",
      "manuscript/discussion.qmd"
    ),
    format = "file"
  ),

  tar_target(
    name = manuscript_stats,
    command = {
      source("R/functions/manuscript_stats.R", local = TRUE)
      count_manuscript_stats(manuscript_files = manuscript_stat_files)
    }
  ),

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
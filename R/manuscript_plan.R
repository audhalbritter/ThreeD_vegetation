#
manuscript_plan <- list(

  # # bibliography
  # tar_target(
  #   name = bibliography,
  #   command = "Manuscript/bibliography.bib",
  #   format = "file"
  # ),
  #
  # # add packages to bibliography
  # tar_target(
  #   name = biblio2,
  #   command = rjt.misc::package_citations(
  #     packages = c("targets", "tidyverse", "rmarkdown"),
  #     old_bib = bibliography,
  #     new_bib = "Manuscript/bibliography2.bib"),
  #   format = "file"
  # ),

  # Results
  tar_render(name = results,
             path = "Manuscript/Main.qmd")


)

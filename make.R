library("targets")
#source("other_scripts/load_libraries.R")

# make the targets that are out of date
# looks for a file called "_targets.R" in the working directory
targets::tar_make()

# tar_make(-manuscript, -si)
tar_load_everything()
# view pipeline and show which targets are out of date
tar_visnetwork()

# fs::file_show("manuscript/manuscript.pdf")#display pdf

# remotes::install_github("claudiozandonella/trackdown",
#                         build_vignettes = TRUE)
# library(trackdown)
# # copy client number
# trackdown_auth_configure(path = "")
# trackdown::upload_file(file = "test_file.qmd",
#                        hide_code = TRUE,
#                        path_output = "test_file.html")

# -----------------------------------------------------------------------------
# Export manuscript / SI figures (300 dpi PNG).
# Main text: Figure 3 onward (Figures 1-2 are static files under manuscript/figures/).
# SI: Figure S1 onward in the order of figure chunks in manuscript/SI.qmd.
# -----------------------------------------------------------------------------
figure_out <- "output/figures"
dir.create(figure_out, recursive = TRUE, showWarnings = FALSE)

savefig <- function(path, plot, width, height,
                    dpi = 300,
                    bg = "white") {
  ggplot2::ggsave(
    filename = file.path(figure_out, path),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = bg
  )
}

# --- Main manuscript (Figure 3+, manuscript/SEM_output.qmd) ---
tar_load(bio_div_figure)
savefig("Figure_3_biomass_diversity.png", bio_div_figure, width = 8, height = 7.5)
tar_load(cut_final_diversity)
savefig("Figure_4_SEM_clipping_Shannon_diversity.png", cut_final_diversity[[1]], width = 10, height = 5)
tar_load(traits_biomass_plot)
tar_load(traits_warming_plot)
tar_load(traits_nitrogen_plot)
fig_toenails <- (traits_biomass_plot + ggplot2::labs(tag = "a) Community biomass")) +
  (traits_warming_plot + ggplot2::labs(tag = "b) Warming")) +
  (traits_nitrogen_plot + ggplot2::labs(tag = "c) Nitrogen")) +
  patchwork::plot_layout(ncol = 1, guides = "collect") &
  ggplot2::theme(
    legend.position = "none",
    legend.box = "vertical",
    plot.tag.position = c(0.12, 1.08),
    plot.tag = ggplot2::element_text(size = 12, hjust = 0, vjust = 1),
    plot.margin = ggplot2::margin(t = 10, r = 5, b = 5, l = 5)
  )
savefig("Figure_5_environmental_affinities.png", fig_toenails, width = 8, height = 10)

# --- Supporting Information (Figure S1+, manuscript/SI.qmd chunk order) ---
tar_load(daily_climate_figure)
tar_load(climate_figure)
tar_load(standing_biomass_back_fig)
tar_load(div_index_figure)
tar_load(cut_final_richness_evenness)
tar_load(graz_final_all_div)
tar_load(traits_clipping_plot)
tar_load(standingB_div_final_figure)
savefig("Figure_S1_daily_microclimate.png", daily_climate_figure, width = 7, height = 6)
savefig("Figure_S2_microclimate_treatments.png", climate_figure, width = 7, height = 7)
savefig("Figure_S3_estimated_vs_collected_biomass.png", standing_biomass_back_fig, width = 7, height = 7)
savefig("Figure_S4_richness_evenness.png", div_index_figure, width = 8, height = 7)
savefig("Figure_S5_SEM_clipping_richness_evenness.png", cut_final_richness_evenness[[1]], width = 10, height = 10)
savefig("Figure_S6_SEM_grazing_all_indices.png", graz_final_all_div[[1]], width = 10, height = 15)
savefig("Figure_S7_trait_distributions_clipping.png", traits_clipping_plot, width = 8, height = 5)
savefig("Figure_S8_biomass_diversity_SEM_raw.png", standingB_div_final_figure, width = 7, height = 5)

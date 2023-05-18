# calculate colonization extinction

calculate_colo_ext <- function(cover){

  # get Vikesland control
  con <- dbConnect(SQLite(), dbname = "data/seedclim.sqlite")

  vik_dest_community <- tbl(con, "turf_community")  |>
    select(-cf, -flag) |>
    left_join(tbl(con, "turfs"), by = "turfID") |>

    # only control plots
    filter(TTtreat %in% c("TTC")) |>
    select(-RTtreat, -GRtreat, -destinationPlotID) |>

    # join plot, block and site IDs
    left_join(tbl(con, "plots"), by = c("originPlotID" = "plotID")) |>
    rename("plotID" = originPlotID) |>
    select(-aspect, -slope) |>
    left_join(tbl(con, "blocks"), by = c("blockID")) |>
    select(-aspect, -slope) |>
    left_join(tbl(con, "sites"), by = c("siteID")) |>
    select(-comment, -norwegian_name, -site_code, -c(biogeographic_zone:precipitation_level)) |>

    # filter 2 sites, and last year
    filter(siteID == "Vikesland",
           year == 2019) |>
    left_join(tbl(con, "taxon"), by = "species") |>
    group_by(siteID, species_name) |>
    summarise(cover = mean(cover)) |>
    rename(species = species_name, destSiteID = siteID) |>
    collect() |>
    mutate(origSiteID = "Sub-alpine",
           warming = "Ambient",
           nitrogen = "no",
           grazing = "Control") |>
    ungroup() |>
    select(origSiteID, warming, nitrogen, grazing, species, cover)


  #select data
  dat <- cover |>
    # select control grazing and 4 N levels
    filter(origBlockID %in% c(1, 3, 5, 6),
           grazing != "Natural") |>
    mutate(nitrogen = case_when(origBlockID == 1 ~ "no",
                                origBlockID == 3 ~ "low",
                                origBlockID == 6 ~ "medium",
                                origBlockID == 5 ~ "high"))
  #colonization and extinction
  #first and last year transplant comm by treatment
  #get first_transplant (year 1)
  first_transplant = dat %>%
    ungroup() |>
    filter(year == 2019) %>%
    select(origSiteID, warming, nitrogen, grazing, year, species, cover)

  # get last_transplant (last year)
  last_transplant = dat %>%
    ungroup() |>
    filter(year == 2022) %>%
    select(origSiteID, warming, nitrogen, grazing, year, species, cover)

  #extinction = first - last year
  extinction = anti_join(first_transplant, last_transplant, by = c("origSiteID", "warming", "nitrogen", "grazing", "species")) %>%
    group_by(origSiteID, warming, nitrogen, grazing) %>%
    summarise(n = n(),
              abundance = sum(cover))

  #colonization = last - first year
  colonization = anti_join(last_transplant, first_transplant, by = c("origSiteID", "warming", "nitrogen", "grazing", "species")) %>%
    group_by(origSiteID, warming, nitrogen, grazing) %>%
    summarise(n = n(),
              abundance = sum(cover))


  ### expected colonization and extinction
  # expected extinction
  #first year destination site only controls
  first_dest_control = dat %>%
    ungroup() |>
    # Alpine: Joa controls, need renaming to Alpine because they are at Sub-alpine
    filter(year %in% c(2019),
           origSiteID == "Sub-alpine",
           warming == "Ambient",
           nitrogen == "no",
           grazing == "Control") %>%
    mutate(origSiteID = recode(origSiteID, "Sub-alpine" = "Alpine")) |>
    select(origSiteID, warming, nitrogen, species, cover) |>
    # Sub-alpine: use Vik controls from SeedClim
    bind_rows(vik_dest_community)

  # difference between first year dest control and first year transplant
  expected_extinction = anti_join(first_transplant, first_dest_control, by = c("origSiteID", "warming", "nitrogen", "grazing", "species")) %>%
    group_by(origSiteID, warming, nitrogen, grazing) %>%
    summarise(n = n(),
              abundance = sum(cover))

  #expected colonization
  expected_colonization = first_dest_control %>%
    select(-warming, -nitrogen, -grazing) %>%
    crossing(first_transplant %>% distinct(warming, nitrogen, grazing)) %>%
    anti_join(first_transplant, by = c("origSiteID", "warming", "nitrogen", "grazing", "species")) %>%
    group_by(origSiteID, warming, nitrogen, grazing) %>%
    summarise(n = n(),
              abundance = sum(cover))


  predicted = bind_rows(
    extinction = expected_extinction,
    colonization = expected_colonization,
    .id = "process") %>%
    rename("predicted_nr" = n, "predicted_abundance" = abundance) |>
    pivot_wider(names_from = process, values_from = c(predicted_nr, predicted_abundance)) |>
    mutate(nitrogen = factor(nitrogen, levels = c("no", "low", "medium", "high"))) |>
    filter(grazing == "Control",
           origSiteID == "Alpine")


  # library("grid")
  # ex = textGrob("\u2190 Extinctions", gp = gpar(fontsize = 9), rot = 90)
  # col = textGrob(paste0("Colonizations ", "\u2192"), gp = gpar(fontsize = 9), rot = 90)

  legend = tibble(x1 = c(4, 4), x2 = c(4, 4),
                  y1 = c(7.5, 3), y2 = c(7.5, 3),
                  t = c("expected","realized"))

  colo_ext_figure <- bind_rows(
    extinction = extinction,
    colonization = colonization,
    .id = "process"
  ) %>%
    select(-abundance) |>
    pivot_wider(names_from = process, values_from = n) %>%
    ungroup() |>
    filter(grazing == "Control",
           origSiteID == "Alpine") |>
    mutate(nitrogen = factor(nitrogen, levels = c("no", "low", "medium", "high"))) |>
    ggplot(aes(x = nitrogen)) +
    geom_col(aes(x = nitrogen, y = predicted_nr_colonization, colour = nitrogen), fill = "white", data = predicted) +
    geom_col(aes(x = nitrogen, y = -predicted_nr_extinction, colour = nitrogen), fill = "white", data = predicted) +
    geom_col(aes(y = colonization, fill = nitrogen), alpha = 0.6) +
    geom_col(aes(y = - extinction, fill = nitrogen), alpha = 0.6) +
    scale_colour_manual(values = c("grey70", "darkolivegreen2", "darkolivegreen3", "darkolivegreen4")) +
    scale_fill_manual(values = c("grey70", "darkolivegreen2", "darkolivegreen3", "darkolivegreen4")) +
    geom_hline(yintercept = 0, colour = "grey") +
    #geom_text(data = legend, aes(x = x2, y = y2, label = t), size = 5) +
    labs(x = "Nitrogen level", y = "Nr extinctions / colonization") +
    facet_grid(origSiteID ~ warming) +
    theme_minimal() +
    theme(text = element_text(size = 20),
          legend.position = "none")

  return(colo_ext_figure)

}




predicted_2 = bind_rows(
  extinction = expected_extinction,
  colonization = expected_colonization,
  .id = "process") %>%
  rename("predicted_nr" = n, "predicted_abundance" = abundance) |>
  pivot_wider(names_from = process, values_from = c(predicted_nr, predicted_abundance)) |>
  mutate(nitrogen = factor(nitrogen, levels = c("no", "low", "medium", "high")),
         GN = paste(nitrogen, grazing, sep = "_")) |>
  filter(origSiteID == "Alpine")

 grazing_plot <- bind_rows(
    extinction = extinction,
    colonization = colonization,
    .id = "process"
  ) %>%
    select(-abundance) |>
    pivot_wider(names_from = process, values_from = n) %>%
    ungroup() |>
    filter(origSiteID == "Alpine") |>
    mutate(nitrogen = factor(nitrogen, levels = c("no", "low", "medium", "high")),
           GN = paste(nitrogen, grazing, sep = "_")) |>
    ggplot(aes(x = nitrogen)) +
    geom_col(aes(x = nitrogen, y = predicted_nr_colonization, colour = GN), fill = "white", data = predicted_2) +
    geom_col(aes(x = nitrogen, y = -predicted_nr_extinction, colour = GN), fill = "white", data = predicted_2) +
    geom_col(aes(y = colonization, fill = GN), alpha = 0.6) +
    geom_col(aes(y = - extinction, fill = GN), alpha = 0.6) +
   scale_colour_manual(values = c("darkolivegreen4", "coral4", "coral4", "darkolivegreen2", "coral", "coral", "darkolivegreen3", "coral2", "coral2", "grey70", "grey70", "grey70")) +
   scale_fill_manual(values = c("darkolivegreen4", "coral4", "coral4", "darkolivegreen2", "coral", "coral", "darkolivegreen3", "coral2", "coral2", "grey70", "grey70", "grey70")) +
    # scale_colour_manual(values = c("grey70", "coral", "coral4")) +
    # scale_fill_manual(values = c("grey70", "coral", "coral4")) +
    geom_hline(yintercept = 0, colour = "grey") +
    #geom_text(data = legend, aes(x = x2, y = y2, label = t), size = 3) +
    labs(x = "Grazing intensity", y = "Nr extinctions / colonization") +
    facet_grid(grazing ~ warming) +
    theme_minimal() +
    theme(text = element_text(size = 20),
          legend.position = "none")

 ggsave(grazing_plot, filename = "output/grazing_plot.png", width = 8, height = 6, bg = "white")

# # abundance
#   bind_rows(
#     extinction = extinction,
#     colonization = colonization,
#     .id = "process"
#   ) %>%
#     select(-n) |>
#     pivot_wider(names_from = process, values_from = abundance) %>%
#     mutate(nitrogen = factor(nitrogen, levels = c("no", "low", "medium", "high"))) |>
#     ggplot(aes(x = nitrogen)) +
#     geom_col(aes(x = nitrogen, y = predicted_abundance_colonization, colour = nitrogen), fill = "white", data = predicted) +
#     geom_col(aes(x = nitrogen, y = -predicted_abundance_extinction, colour = nitrogen), fill = "white", data = predicted) +
#     geom_col(aes(y = colonization, fill = nitrogen), alpha = 0.6) +
#     geom_col(aes(y = - extinction, fill = nitrogen), alpha = 0.6) +
#     scale_color_brewer(palette = "Greens") +
#     scale_fill_brewer(palette = "Greens") +
#     # scale_fill_manual(name = "", values = c("grey", "orange")) +
#     # scale_colour_manual(name = "", values = c("grey", "orange")) +
#     geom_hline(yintercept = 0, colour = "grey") +
#     #geom_text(data = legend, aes(x = x2, y = y2, label = t), size = 3) +
#     labs(x = "", y = "", title = "Abundnace") +
#     #annotation_custom(ex, xmin = 0.1, xmax= 0.1, ymin = -10, ymax = -10) +
#     #annotation_custom(col, xmin = 0.1, xmax= 0.1, ymin = 2, ymax = 2) +
#     #coord_cartesian(xlim = c(1, 6), clip="off") +
#     facet_grid(origSiteID ~ warming) +
#     theme_bw()


  # #colonization and extinction over time
  # transplant_all_years = cover %>%
  #   filter(year != 2012,
  #          TTtreat != "control") %>%
  #   select(turfID, destBlockID, TTtreat, year, species)
  #
  # #extinciton = first - last year
  # extinction_all = anti_join(crossing(first_transplant %>% select(-year), year), transplant_all_years,
  #                            by = c("destBlockID", "TTtreat", "species", "year")) %>%
  #   count(destBlockID, TTtreat, year) %>%
  #   group_by(TTtreat, year) %>%
  #   summarise(nr_species = mean(n),
  #             se = sd(n)/sqrt(n()),
  #             .groups = "drop_last")
  #
  # #colonization = last - first year
  # year = c(2013, 2014, 2015, 2016)
  # colonization_all = anti_join(transplant_all_years, crossing(first_transplant %>% select(-year), year),
  #                              by = c("destBlockID", "TTtreat", "species", "year")) %>%
  #   group_by(destBlockID, TTtreat, year) %>%
  #   count() %>%
  #   group_by(TTtreat, year) %>%
  #   summarise(nr_species = mean(n),
  #             se = sd(n)/sqrt(n()))
  #

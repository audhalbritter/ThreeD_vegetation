### RUN MODEL WITH THREE WAY INTERACTIONS


# colour palette
NxW_col_palette <- c("#009E73", "#CC79A7")
NxWxG_col_palette <- c("#009E73", "#CC79A7", "#E69F00")

range <- cover_model_output |>
  filter(origSiteID == "Alpine") |>
  ungroup() |>
  summarise(min = min(lwr),
            max = max(upr))

p2 <- cover_model_output |>
  filter(origSiteID == "Alpine",
         grazing == "Control") |>
  ggplot(aes(x = Namount_kg_ha_y, y = .response, color = warming, linetype = grazing, shape = grazing)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = warming), alpha = 0.1, linetype = 0) +
  geom_hline(yintercept = 0, colour = "lightgrey") +
  geom_point(size = 2) +
  geom_line(aes(y = prediction), size = 0.9) +
  ylim(ymin = range$min, ymax = range$max) +
  labs(x = bquote(Nitrogen~kg~ha^-1~y^-1),
       y = "Change in cover") +
  scale_colour_manual(name = "Warming", values = NxW_col_palette) +
  scale_fill_manual(name = "Warming", values = NxW_col_palette) +
  scale_linetype_manual(name = "Grazing", values = c("solid", "dashed", "dotted")) +
  scale_shape_manual(name = "Grazing", values = c(16, 0, 2)) +
  facet_wrap(~ functional_group) +
  theme_bw() +
  theme(legend.position = "top",
        legend.box="vertical",
        text = element_text(size = 17))
ggsave(p2, filename = "output/alpine_cover_2.png",width = 10, height = 5.5, bg = "white")
p3 <- cover_model_output |>
  filter(origSiteID == "Alpine") |>
  ggplot(aes(x = Namount_kg_ha_y, y = .response, color = warming, linetype = grazing, shape = grazing)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = warming), alpha = 0.1, linetype = 0) +
  geom_hline(yintercept = 0, colour = "lightgrey") +
  geom_point(size = 2) +
  geom_line(aes(y = prediction), size = 0.9) +
  ylim(ymin = range$min, ymax = range$max) +
  labs(x = bquote(Nitrogen~kg~ha^-1~y^-1),
       y = "Change in cover") +
  scale_colour_manual(name = "Warming", values = NxW_col_palette) +
  scale_fill_manual(name = "Warming", values = NxW_col_palette) +
  scale_linetype_manual(name = "Grazing", values = c("solid", "dashed", "dotted")) +
  scale_shape_manual(name = "Grazing", values = c(16, 0, 2)) +
  facet_wrap(~ functional_group) +
  theme_bw() +
  theme(legend.position = "top",
        legend.box="vertical",
        text = element_text(size = 17))
ggsave(p3, filename = "output/alpine_cover_3.png",width = 10, height = 5.5, bg = "white")


# make figure

div1 <- diversity_model_output |>
  filter(origSiteID == "Alpine",
         diversity_index != "diversity",
         grazing == "Control"
         ) |>
  ggplot(aes(x = Namount_kg_ha_y, y = .response, color = warming, linetype = grazing, shape = grazing)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = warming), alpha = 0.1, linetype = 0) +
  geom_point(size = 2) +
  geom_line(aes(y = prediction), size = 0.9) +
  geom_hline(yintercept = 0, colour = "lightgrey") +
  labs(x = bquote(Nitrogen~kg~ha^-1~y^-1),
       y = "Change in diversity index") +
  scale_colour_manual(name = "Warming", values = NxW_col_palette) +
  scale_fill_manual(name = "Warming", values = NxW_col_palette) +
  scale_linetype_manual(name = "Grazing", values = c("solid", "dashed", "dotted")) +
  scale_shape_manual(name = "Grazing", values = c(16, 0, 2)) +
  facet_wrap(~ diversity_index, scales = "free") +
  theme_bw() +
  theme(legend.position = "top",
        legend.box="vertical",
        text = element_text(size = 17))
ggsave(div1, filename = "output/alpine_diversity_1.png",width = 10, height = 5.5, bg = "white")


div2 <- diversity_model_output |>
  filter(origSiteID == "Alpine",
         diversity_index != "diversity") |>
  ggplot(aes(x = Namount_kg_ha_y, y = .response, color = warming, linetype = grazing, shape = grazing)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = warming), alpha = 0.1, linetype = 0) +
  geom_line(aes(y = prediction), size = 0.9) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, colour = "lightgrey") +
  labs(x = bquote(Nitrogen~kg~ha^-1~y^-1),
       y = "Change in diversity index") +
  scale_colour_manual(name = "Warming", values = NxW_col_palette) +
  scale_fill_manual(name = "Warming", values = NxW_col_palette) +
  scale_linetype_manual(name = "Grazing", values = c("solid", "dashed", "dotted")) +
  scale_shape_manual(name = "Grazing", values = c(16, 0, 2)) +
  facet_wrap(~ diversity_index, scales = "free") +
  theme_bw() +
  theme(legend.position = "top",
        legend.box="vertical",
        text = element_text(size = 17))
ggsave(div2, filename = "output/alpine_diversity_2.png",width = 10, height = 5.5, bg = "white")


# productivity
range <- productivity_model_output |>
  filter(year == 2022,
         origSiteID == "Alpine") |>
  summarise(min = min(lwr),
            max = max(upr))


pro_1 <- productivity_model_output |>
  filter(origSiteID == "Alpine",
         year == 2022,
         warming == "Ambient",
         grazing == "Control") |>
  ggplot(aes(x = Namount_kg_ha_y, y = .response, color = warming, linetype = grazing, shape = grazing)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = warming), alpha = 0.1, linetype = 0) +
  geom_hline(yintercept = 0, colour = "lightgrey") +
  geom_point(size = 2) +
  geom_line(aes(y = prediction), size = 0.9) +
  ylim(ymin = range$min, ymax = range$max) +
  labs(x = bquote(Nitrogen~kg~ha^-1~y^-1),
       y = bquote(Annual~productivity~g~m^-2~y^-1)) +
  scale_colour_manual(name = "Warming", values = NxW_col_palette) +
  scale_fill_manual(name = "Warming", values = NxW_col_palette) +
  scale_linetype_manual(name = "Grazing", values = c("solid", "dashed", "dotted")) +
  scale_shape_manual(name = "Grazing", values = c(16, 0, 2)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.box="vertical",
        text = element_text(size = 17))
ggsave(pro_1, filename = "output/pro_1.png", width = 8, height = 4, bg = "white")

pro_2 <- productivity_model_output |>
  filter(origSiteID == "Alpine",
         year == 2022,
         grazing == "Control") |>
  ggplot(aes(x = Namount_kg_ha_y, y = .response, color = warming, linetype = grazing, shape = grazing)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = warming), alpha = 0.1, linetype = 0) +
  geom_hline(yintercept = 0, colour = "lightgrey") +
  geom_point(size = 2) +
  geom_line(aes(y = prediction), size = 0.9) +
  ylim(ymin = range$min, ymax = range$max) +
  labs(x = bquote(Nitrogen~kg~ha^-1~y^-1),
       y = bquote(Annual~productivity~g~m^-2~y^-1)) +
  scale_colour_manual(name = "Warming", values = NxW_col_palette) +
  scale_fill_manual(name = "Warming", values = NxW_col_palette) +
  scale_linetype_manual(name = "Grazing", values = c("solid", "dashed", "dotted")) +
  scale_shape_manual(name = "Grazing", values = c(16, 0, 2)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.box="vertical",
        text = element_text(size = 17))
ggsave(pro_2, filename = "output/pro_2.png", width = 8, height = 4, bg = "white")

pro_3 <- productivity_model_output |>
  filter(origSiteID == "Alpine",
         year == 2022) |>
  ggplot(aes(x = Namount_kg_ha_y, y = .response, color = warming, linetype = grazing, shape = grazing)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = warming), alpha = 0.1, linetype = 0) +
  geom_hline(yintercept = 0, colour = "lightgrey") +
  geom_point(size = 2) +
  geom_line(aes(y = prediction), size = 0.9) +
  ylim(ymin = range$min, ymax = range$max) +
  labs(x = bquote(Nitrogen~kg~ha^-1~y^-1),
       y = bquote(Annual~productivity~g~m^-2~y^-1)) +
  scale_colour_manual(name = "Warming", values = NxW_col_palette) +
  scale_fill_manual(name = "Warming", values = NxW_col_palette) +
  scale_linetype_manual(name = "Grazing", values = c("solid", "dashed", "dotted")) +
  scale_shape_manual(name = "Grazing", values = c(16, 0, 2)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.box="vertical",
        text = element_text(size = 17))
ggsave(pro_3, filename = "output/pro_3.png", width = 8, height = 4, bg = "white")

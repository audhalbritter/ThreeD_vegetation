# Fancy stats dictionary

fancy_stats <- function(dat){

  dat <- dat %>%
    mutate(term = recode(term,
                         "(Intercept)" = "Intercept",
                         "warmingWarming" = "W",
                         ".grazing" = "G",
                         ".grazingNatural" = "G",
                         "Namount_kg_ha_y" = "N",
                         "poly(Namount_kg_ha_y, 2)1" = "N",
                         "poly(Namount_kg_ha_y, 2)2" = "N\U00B2",
                         "warmingWarming:.grazing" = "WxG",
                         "warmingWarming:.grazingNatural" = "WxG",
                         "warmingWarming:Namount_kg_ha_y" = "WxN",
                         "warmingWarming:poly(Namount_kg_ha_y, 2)1" = "WxN",
                         "warmingWarming:poly(Namount_kg_ha_y, 2)2" = "WxN\U00B2",
                         ".grazing:Namount_kg_ha_y" = "GxN",
                         ".grazingNatural:Namount_kg_ha_y" = "GxN",
                         ".grazing:poly(Namount_kg_ha_y, 2)1" = "GxN",
                         ".grazing:poly(Namount_kg_ha_y, 2)2" = "GxN\U00B2",
                         ".grazingNatural:poly(Namount_kg_ha_y, 2)1" = "GxN",
                         ".grazingNatural:poly(Namount_kg_ha_y, 2)2" = "GxN\U00B2",
                         "warmingWarming:.grazing:Namount_kg_ha_y" = "WxGxN",
                         "warmingWarming:.grazingNatural:Namount_kg_ha_y" = "WxGxN",
                         "warmingWarming:.grazing:poly(Namount_kg_ha_y, 2)1" = "WxGxN",
                         "warmingWarming:.grazing:poly(Namount_kg_ha_y, 2)2" = "WxGxN\U00B2",
                         "warmingWarming:.grazingNatural:poly(Namount_kg_ha_y, 2)1" = "WxGxN",
                         "warmingWarming:.grazingNatural:poly(Namount_kg_ha_y, 2)2" = "WxGxN\U00B2",
                         "Nitrogen_log" = "N",
                         "warmingWarming:Nitrogen_log" = "WxN",
                         ".grazing:Nitrogen_log" = "GxN",
                         "warmingWarming:.grazing:Nitrogen_log" = "WxGxN",
                         ".grazingNatural:Nitrogen_log" = "GxN",
                         "warmingWarming:.grazingNatural:Nitrogen_log" = "WxGxN"),
    # sort
    term = factor(term, levels = c("Intercept", "W", "G", "N", "N\U00B2",
                                   "WxG", "WxN", "WxN\U00B2", "GxN", "GxN\U00B2",
                                   "WxGxN", "WxGxN\U00B2")))
  return(dat)
}

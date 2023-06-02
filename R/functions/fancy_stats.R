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
                         "poly(Namount_kg_ha_y, 2)2" = "N^2",
                         "warmingWarming:.grazing" = "WxG",
                         "warmingWarming:.grazingNatural" = "WxG",
                         "warmingWarming:Namount_kg_ha_y" = "WxN",
                         "warmingWarming:poly(Namount_kg_ha_y, 2)1" = "WxN",
                         "warmingWarming:poly(Namount_kg_ha_y, 2)2" = "WxN^2",
                         ".grazing:Namount_kg_ha_y" = "GxN",
                         ".grazingNatural:Namount_kg_ha_y" = "GxN",
                         ".grazing:poly(Namount_kg_ha_y, 2)1" = "GxN",
                         ".grazing:poly(Namount_kg_ha_y, 2)2" = "GxN^2",
                         ".grazingNatural:poly(Namount_kg_ha_y, 2)1" = "GxN",
                         ".grazingNatural:poly(Namount_kg_ha_y, 2)2" = "GxN^2",
                         "warmingWarming:.grazing:Namount_kg_ha_y" = "WxGxN",
                         "warmingWarming:.grazingNatural:Namount_kg_ha_y" = "WxGxN",
                         "warmingWarming:.grazing:poly(Namount_kg_ha_y, 2)1" = "WxGxN",
                         "warmingWarming:.grazing:poly(Namount_kg_ha_y, 2)2" = "WxGxN^2",
                         "warmingWarming:.grazingNatural:poly(Namount_kg_ha_y, 2)1" = "WxGxN",
                         "warmingWarming:.grazingNatural:poly(Namount_kg_ha_y, 2)2" = "WxGxN^2",
                         "Nitrogen_log" = "N",
                         "warmingWarming:Nitrogen_log" = "WxN",
                         ".grazing:Nitrogen_log" = "GxN",
                         "warmingWarming:.grazing:Nitrogen_log" = "WxGxN",
                         ".grazingNatural:Nitrogen_log" = "GxN",
                         "warmingWarming:.grazingNatural:Nitrogen_log" = "WxGxN"))
  return(dat)
}

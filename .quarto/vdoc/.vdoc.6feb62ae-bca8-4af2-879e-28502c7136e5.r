#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: packages
#| echo: false
#| include: false


if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, 
               sjmisc, 
               sjPlot, 
               lme4, 
               here, 
               performance,
               marginaleffects,
               texreg, 
               ggdist,
               kableExtra,
               ggalluvial, 
               shadowtext,
               MetBrewer,
               patchwork,
               sjlabelled,
               summarytools,
               interactions,
               cowplot,
               ggpubr)


options(scipen=999)
rm(list = ls())
#
#
#
#| label: data
#| echo: false
#| include: false 

load(file=here::here("input/proc/data.RData"))
load(file=here::here("input/proc/digitalization.RData"))
load(file=here::here("input/proc/country_data.RData"))
#
#
#
#
#
#| label: fig-cleveland
#| echo: false
#| fig-cap: Mean of perceived redistributive justice by OECD country
#| fig-cap-location: top
#| fig-subcap: "Source: own elaboration with data by OCDE, Risks That Matter survey 2024. La justicia redistributiva percibida corresponde al grado de acuerdo con el ítem sobre recibir una parte justa de los beneficios públicos dados los impuestos y contribuciones (escala 1–5). The vertical gray line shows the OECD mean of redistributive justice"

data_cleveland <- data %>%
  mutate(country = haven::as_factor(country, levels = "labels"))

promedios <- data_cleveland %>%
  group_by(country) %>%
  summarise(mean_dis = round(mean(distributive, na.rm = TRUE),2))

media_global <- mean(promedios$mean_dis)

promedios %>%
  mutate(
    sobre_promedio = mean_dis > media_global
  ) %>%
  ggplot(aes(
    x = mean_dis,
    y = reorder(country, mean_dis)
  )) +
  geom_segment(
    aes(
      x = 1,
      xend = mean_dis,
      yend = country
    ),
    colour = "grey70"
  ) +
  geom_vline(
    xintercept = media_global,
    colour = "grey30",
    linewidth = 0.6
  ) +
  geom_point(
    aes(fill = sobre_promedio),
    shape = 21,
    size = 3,
    colour = "black",
    stroke = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = "white",
      "FALSE" = "black"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(1, 4),
    breaks = c(1, 2, 2.73, 3, 4)
  ) +
  labs(
    x = "Perceived redistributive justice",
    y = NULL
  ) +
  theme_bw()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

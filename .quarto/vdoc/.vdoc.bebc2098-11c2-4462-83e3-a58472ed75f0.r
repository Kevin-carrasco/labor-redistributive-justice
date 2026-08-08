#
#
#
#
#
#| label: tbl-classification
#| tbl-cap: "Welfare regime classification and sample sizes by country"
#| echo: false
#| warning: false
#| message: false

if (!require("pacman")) install.packedads("pacman") # instalar pacman
                            # cargar librerias
pacman::p_load(dplyr,       # Manipulacion de datos
               knitr,
               kableExtra,
               summarytools,
               ggplot2,
               ggeffects,
               haven,
               sjlabelled,
               lme4,
               sjmisc,
               sjPlot,
               ggrepel,
               ggpubr
               )

data_original<- haven::read_dta(here::here("input/original/OECD_RTM_2024.dta"))
load(file=here::here("input/proc/data.RData"))

data <- data %>% select(ctrcode,country, distributive, sex, age, educ, income, selfemploy, unnemployment, platform, tecnoestres, pos_tecnoestres, skills, deservingness, welfare_regime)

data <- na.omit(data)

# Mapa código -> país -> régimen (idéntico al case_when del pipeline)
regime_map <- tibble::tribble(
  ~code, ~country,          ~regime,
   3,    "Canada",          "Liberal",
   4,    "Chile",           "Liberal",
   6,    "Estonia",         "Liberal",
  11,    "Ireland",         "Liberal",
  15,    "Latvia",          "Liberal",
  16,    "Lithuania",       "Liberal",
  17,    "Mexico",          "Liberal",
  26,    "United Kingdom",  "Liberal",
  27,    "United States",   "Liberal",
   5,    "Denmark",         "Social-democratic",
   7,    "Finland",         "Social-democratic",
  18,    "Netherlands",     "Social-democratic",
  19,    "Norway",          "Social-democratic",
   1,    "Austria",         "Conservative",
   2,    "Belgium",         "Conservative",
   8,    "France",          "Conservative",
   9,    "Germany",         "Conservative",
  10,    "Greece",          "Conservative",
  12,    "Israel",          "Conservative",
  13,    "Italy",           "Conservative",
  14,    "Korea",           "Conservative",
  20,    "Poland",          "Conservative",
  21,    "Portugal",        "Conservative",
  22,    "Slovenia",        "Conservative",
  23,    "Spain",           "Conservative",
  24,    "Switzerland",     "Conservative",
  25,    "Türkiye",         "Conservative"
) %>%
  dplyr::mutate(regime = factor(regime,
                                levels = c("Liberal",
                                           "Social-democratic",
                                           "Conservative")))

# N por país antes y después de la eliminación de casos perdidos, vía sjmisc::frq
n_original <- sjmisc::frq(data_original$country)[[1]] %>%
  dplyr::filter(!is.na(val)) %>%
  dplyr::mutate(code = as.numeric(as.character(val))) %>%
  dplyr::select(code, n_original = frq)

n_final <- sjmisc::frq(data$country)[[1]] %>%
  dplyr::filter(!is.na(val)) %>%
  dplyr::mutate(code = as.numeric(as.character(val))) %>%
  dplyr::select(code, n_final = frq)

tabla_paises <- regime_map %>%
  dplyr::left_join(n_original, by = "code") %>%
  dplyr::left_join(n_final,    by = "code") %>%
  dplyr::arrange(regime, country) %>%
  dplyr::select(regime, country, n_original, n_final)

# Chequeos compactos: 27 países cubiertos, sin códigos huérfanos en los datos
stopifnot(nrow(tabla_paises) == 27,
          !anyNA(tabla_paises$n_original),
          !anyNA(tabla_paises$n_final))
#setdiff(unique(as.numeric(as.character(data$country))), regime_map$code) # debe ser numeric(0)

tabla_paises %>%
  dplyr::mutate(regime = as.character(regime)) %>%
  dplyr::bind_rows(
    dplyr::summarise(.,
                     regime     = "Total",
                     country    = paste(dplyr::n(), "countries"),
                     n_original = sum(n_original),
                     n_final    = sum(n_final))
  ) %>%
  flextable::flextable() %>%
  flextable::set_header_labels(regime     = "Welfare regime",
                               country    = "Country",
                               n_original = "Original N",
                               n_final    = "Final N") %>%
  flextable::merge_v(j = "regime") %>%
  flextable::valign(j = "regime", valign = "top") %>%
  flextable::colformat_num(j = c("n_original", "n_final"), big.mark = ",") %>%
  flextable::align(j = c("n_original", "n_final"), align = "right", part = "all") %>%
  flextable::hline(i = 27, part = "body") %>%
  flextable::bold(i = 28, part = "body") %>%
  flextable::add_footer_lines("Note: Original N refers to all respondents in the 2024 wave of the Risks That Matter survey; Final N to the analytical sample after listwise deletion of missing values. Sources of the classification: Esping-Andersen (1993); Ferrera (1996); Fenger (2007); Aidukaite (2011); Martínez Franzoni (2008).") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::set_table_properties(layout = "autofit")
#
#
#

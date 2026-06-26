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
#| label: tbl-summary
#| tbl-cap: "Main descriptive statistics"
#| tbl-cap-location: top
#| results: asis
#| echo: false

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
load(file=here::here("input/proc/data.RData"))

data <- data %>% select(ctrcode,country, distributive, sex, age, educ, income, selfemploy, unnemployment, platform, tecnoestres, pos_tecnoestres, skills, deservingness, gdp_2024, welfare_regime)

data <- na.omit(data)

data$distributive  <- set_label(x = data$distributive, label = "Redistributive justice")
data$tecnoestres  <- set_label(x = data$tecnoestres, label = "Technological displacement	")
data$pos_tecnoestres  <- set_label(x = data$pos_tecnoestres, label = "Perceived technological benefit")
data$skills  <- set_label(x = data$skills, label = "Lose job due skills")
data$unnemployment  <- set_label(x = data$unnemployment, label = "Lose job")

data$deservingness <- as.numeric(as_factor(data$deservingness))
data$deservingness  <- set_label(x = data$deservingness, label = "Deservingness of others")
data$gdp_2024  <- set_label(x = data$gdp_2024, label = "GDP per-capita")

data$selfemploy  <- set_label(x = data$selfemploy, label = "Self-empleoyment worker")
data$platform  <- set_label(x = data$platform, label = "Platform worker")

data$sex  <- set_label(x = data$sex, label = "Sex")
data$age  <- set_label(x = data$age, label = "Age group")
data$educ  <- set_label(x = data$educ, label = "Educational level")

descriptive <- data %>% select(distributive, sex, age, educ, income, selfemploy, platform, unnemployment, tecnoestres, pos_tecnoestres, skills, deservingness, gdp_2024, welfare_regime)

descriptive$distributive <- factor(descriptive$distributive,
            levels = c(1, 2, 3, 4, 5),
            labels = c("Strongly disagree", "Disagree",
                       "Neither agree nor disagree", "Agree", "Strongly agree"))

descriptive$sex <- factor(descriptive$sex,
            levels = c(1, 2),
            labels = c("Man", "Woman"))

descriptive$age <- factor(descriptive$age,
            levels = c(1, 2, 3, 4, 5),
            labels = c("18-24", "25-34", "35-44", "45-54", "55-64"))

descriptive$educ <- factor(descriptive$educ,
            levels = c(1, 2, 3),
            labels = c("Low", "Medium", "High"))

descriptive$selfemploy <- factor(descriptive$selfemploy,
            levels= c(1,2),
            labels=c( "No",
                      "Yes"))

descriptive$unnemployment <- factor(descriptive$unnemployment,
            levels = c(1, 2, 3, 4),
            labels = c("Not at all concerned", "Not so concerned",
                       "Somewhat concerned", "Very concerned"))

descriptive$platform <- factor(descriptive$platform,
            levels = c(1, 2),
            labels = c("No", "Yes"))

descriptive$skills <- factor(descriptive$skills,
            levels = c(1, 2, 3, 4),
            labels = c("Very unlikely", "Unlikely",
                       "Likely", "Very likely"))

descriptive$deservingness <- factor(descriptive$deservingness,
            levels = c(1, 2, 3, 4, 5),
            labels = c("Strongly disagree", "Disagree",
                       "Neither agree nor disagree", "Agree", "Strongly agree"))

descriptive$sex <- as_factor(descriptive$sex)
descriptive$age <- as_factor(descriptive$age)
descriptive$educ <- as_factor(descriptive$educ)
descriptive$selfemploy <- as_factor(descriptive$selfemploy)
descriptive$unnemployment <- as_factor(descriptive$unnemployment)
descriptive$platform <- as_factor(descriptive$platform)
descriptive$skills <- as_factor(descriptive$skills)
descriptive$deservingness <- as_factor(descriptive$deservingness)

descriptive$distributive  <- set_label(x = descriptive$distributive, label = "Redistributive justice")
descriptive$tecnoestres  <- set_label(x = descriptive$tecnoestres, label = "Technological displacement	")
descriptive$pos_tecnoestres  <- set_label(x = descriptive$pos_tecnoestres, label = "Perceived technological benefit")
descriptive$skills  <- set_label(x = descriptive$skills, label = "Lose job due skills")
descriptive$unnemployment  <- set_label(x = descriptive$unnemployment, label = "Lose job")

descriptive$deservingness  <- set_label(x = descriptive$deservingness, label = "Deservingness of others")
descriptive$gdp_2024  <- set_label(x = descriptive$gdp_2024, label = "GDP per-capita")

descriptive$selfemploy  <- set_label(x = descriptive$selfemploy, label = "Self-empleoyment worker")
descriptive$platform  <- set_label(x = descriptive$platform, label = "Platform worker")

descriptive$sex  <- set_label(x = descriptive$sex, label = "Sex")
descriptive$age  <- set_label(x = descriptive$age, label = "Age group")
descriptive$educ  <- set_label(x = descriptive$educ, label = "Educational level")
descriptive$welfare_regime  <- set_label(x = descriptive$welfare_regime, label = "Welfare regime")

descriptive$distributive  <- set_label(x = descriptive$distributive, label = "Redistributive justice")


df<-dfSummary(descriptive,
               plain.ascii = FALSE,
               style = "multiline",
               tmp.img.dir = "/tmp",
               graph.magnif = 0.75,
               headings = F,  # encabezado
               varnumbers = F, # num variable
               labels.col = T, # etiquetas
               na.col = F,    # missing
               graph.col = F, # plot
               valid.col = T, # n valido
               col.widths = c(30,10,10,10))

df$Variable <- NULL # delete variable column

print(df)
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

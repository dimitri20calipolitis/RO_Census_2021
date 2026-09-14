# Load the library
library(tidyverse)
library(tidytext)
library(readxl)
library(scales)
library(paletteer)
library(treemapify)
library(ggpmisc)
library(gridExtra)
library(openxlsx)
library(rstatix)
library(DMwR)
library(skedastic)
library(lmtest)
library(car)
library(tseries)
source("RO_Census_functions.R")

# Judete unde raman outliers de valori mari indiferent daca se aplica max sau mean:
# Arges, Bacau, Caras-Severin, Calarasi, Cluj, Constanta, Harghita, Ialomita, Iasi,
# Maramures, Olt, Sibiu, Timis, Valcea

# Judete unde max corecteaza mai bine modelele:
# Botosani, Giurgiu, Satu Mare

# Load data
RO_Census_df <- read_excel("./Ro_Census_2021.xlsx", sheet = 1)
View(RO_Census_df)

RO_Census_rank <- RO_Census_df %>% 
  left_join(RO_Census_df %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

RSquared_vals_df <- read_excel("./RO_Census_Indicators_lm_popsize.xlsx", sheet = 1)

# Load Census with predicts
RO_Census_Alba <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Alba")
RO_Census_Arad <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Arad")
RO_Census_Arges <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Arges")
RO_Census_Bacau <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Bacau")
RO_Census_Bihor <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Bihor")
RO_Census_BistNsd <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Bistrita-Nasaud")
RO_Census_Botosani <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Botosani")
RO_Census_Braila <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Braila")
RO_Census_Brasov <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Brasov")
RO_Census_Buzau <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Buzau")
RO_Census_CarSev <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Caras-Severin")
RO_Census_Calarasi <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Calarasi")
RO_Census_Cluj <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Cluj")
RO_Census_Constanta <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Constanta")
RO_Census_Covasna <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Covasna")
RO_Census_Dambovita <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Dambovita")
RO_Census_Dolj <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Dolj")
RO_Census_Galati <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Galati")
RO_Census_Giurgiu <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Giurgiu")
RO_Census_Gorj <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Gorj")
RO_Census_Harghita <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Harghita")
RO_Census_Hunedoara <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Hunedoara")
RO_Census_Ialomita <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Ialomita")
RO_Census_Iasi <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Iasi")
RO_Census_Ilfov <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Ilfov")
RO_Census_Maramures <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Maramures")
RO_Census_Mehedinti <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Mehedinti")
RO_Census_Mures <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Mures")
RO_Census_Neamt <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Neamt")
RO_Census_Olt <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Olt")
RO_Census_Prahova <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Prahova")
RO_Census_SatuMare <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Satu Mare")
RO_Census_Salaj <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Salaj")
RO_Census_Sibiu <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Sibiu")
RO_Census_Suceava <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Suceava")
RO_Census_Teleorman <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Teleorman")
RO_Census_Timis <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Timis")
RO_Census_Tulcea <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Tulcea")
RO_Census_Vaslui <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Vaslui")
RO_Census_Valcea <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Valcea")
RO_Census_Vrancea <- read_excel("./RO_Census_Predict_popsize.xlsx", sheet = "Vrancea")

# Load Census with outliers
Alba_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Alba")
Arad_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Arad")
Arges_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Arges")
Bacau_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Bacau")
Bihor_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Bihor")
BistNsd_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Bistrita-Nasaud")
Botosani_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Botosani")
Braila_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Braila")
Brasov_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Brasov")
Buzau_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Buzau")
CarSev_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Caras-Severin")
Calarasi_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Calarasi")
Cluj_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Cluj")
Constanta_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Constanta")
Covasna_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Covasna")
Dambovita_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Dambovita")
Dolj_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Dolj")
Galati_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Galati")
Giurgiu_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Giurgiu")
Gorj_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Gorj")
Harghita_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Harghita")
Hunedoara_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Hunedoara")
Ialomita_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Ialomita")
Iasi_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Iasi")
Ilfov_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Ilfov")
Maramures_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Maramures")
Mehedinti_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Mehedinti")
Mures_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Mures")
Neamt_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Neamt")
Olt_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Olt")
Prahova_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Prahova")
SatuMare_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Satu Mare")
Salaj_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Salaj")
Sibiu_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Sibiu")
Suceava_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Suceava")
Teleorman_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Teleorman")
Timis_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Timis")
Tulcea_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Tulcea")
Vaslui_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Vaslui")
Valcea_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Valcea")
Vrancea_Outliers <- read_excel("./RO_Census_Outliers_popsize.xlsx", sheet = "Vrancea")

# I. Analyze the R-Squared values from initial models

# 1. Verify the maximum R-Squared value from each Judet
RSquared_vals_df %>% inner_join(
RSquared_vals_df %>% dplyr::select(-Function_Name) %>% group_by(Judet_Name) %>% 
  summarize(R_Squared = max(R_Squared))
) %>% dplyr::select(Function_Name) %>% group_by(Function_Name) %>% count() %>% 
  rename('Counts' = 'n')
View(RSquared_vals_df %>% inner_join(
  RSquared_vals_df %>% dplyr::select(-Function_Name) %>% group_by(Judet_Name) %>% 
    summarize(R_Squared = max(R_Squared))
))

# 2. Visualize all the R-Squared values by each Judet
RSquared_vals_df %>%
  ggplot(aes(x = reorder_within(Function_Name, R_Squared, Judet_Name), 
             y = R_Squared, fill = Function_Name)) + 
  geom_col(aes(fill = Function_Name), show.legend = FALSE) + 
  facet_wrap(~Judet_Name, scale = "free") + scale_x_reordered() +
  scale_fill_manual(values = c("#088BBEFF", "#F8A02EFF", "#ED3F39FF", "#1B7837FF")) +
  labs(x = "Function", y = "R-Squared") + coord_flip() +
  ggtitle("R-Squared values by each Funcion and Judet") +
  theme(plot.title = element_text(hjust = 0.5))

# 3. Filter on values
RSquared_vals_df %>% filter(R_Squared >= 0.90) %>% dplyr::select(Function_Name) %>% 
  group_by(Function_Name) %>% count() %>% rename('Counts' = 'n')
View(RSquared_vals_df %>% filter(R_Squared >= 0.90))

RSquared_vals_df %>% filter(R_Squared >= 0.95) %>% dplyr::select(Function_Name) %>% 
  group_by(Function_Name) %>% count() %>% rename('Counts' = 'n')
View(RSquared_vals_df %>% filter(R_Squared >= 0.95))

# II. Detect outliers

# 1. Alba
# RO_Census_Alba_New <- RO_Census_df %>%
#   inner_join(
#   RO_Census_Alba %>%
#     filter(Pop_Freq < max(Pop_Freq)) %>%
#     dplyr::select(Judet_Name, Unit_Type, Unit_Name),
#   by = c("Judet_Name", "Unit_Type", "Unit_Name")) %>%
#   left_join((RO_Census_df %>%
#                inner_join(
#                  RO_Census_Alba %>%
#                    filter(Pop_Freq < max(Pop_Freq)) %>%
#                    dplyr::select(Judet_Name, Unit_Type, Unit_Name),
#                  by = c("Judet_Name", "Unit_Type", "Unit_Name"))) %>%
#               group_by(Judet_Name) %>%
#               summarize(Total_Pop = sum(Population))) %>%
#   arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>%
#   mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
#   ungroup()

# RO_Census_Alba_New <- RO_Census_df %>% filter(Judet_Name == "ALBA") %>%
#   inner_join(
#     RO_Census_Alba %>% mutate(LOF = lofactor(Pop_Freq, k = 5)) %>%
#       filter(round(LOF, 2) < 1.5) %>% dplyr::select(Judet_Name, Unit_Type, Unit_Name),
#     by = c("Judet_Name", "Unit_Type", "Unit_Name")) %>%
#   left_join((RO_Census_df %>% filter(Judet_Name == "ALBA") %>%
#                inner_join(
#                  RO_Census_Alba %>% mutate(LOF = lofactor(Pop_Freq, k = 5)) %>%
#                    filter(round(LOF, 2) < 1.5) %>%
#                    dplyr::select(Judet_Name, Unit_Type, Unit_Name),
#                  by = c("Judet_Name", "Unit_Type", "Unit_Name"))) %>%
#               group_by(Judet_Name) %>%
#               summarize(Total_Pop = sum(Population))) %>%
#   arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>%
#   mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
#   ungroup()

AB_out <- get_outliers(Alba_Outliers, RO_Census_Alba)

RO_Census_Alba_New <- RO_Census_df %>% inner_join(
  RO_Census_Alba %>% anti_join(
    bind_rows(
      (AB_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (AB_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (AB_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (AB_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
      ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
    ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Alba_New <- RO_Census_Alba_New %>% 
  left_join(RO_Census_Alba_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
AB_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Alba_New)
summary(AB_pw_lm)
AB_pred_pw_lm <- exp(predict(AB_pw_lm, newdata = RO_Census_Alba_New, 
                             interval = "prediction", level = 0.95))
AB_pw_whtest <- white(AB_pw_lm, interactions = TRUE)
AB_pw_bptest <- bptest(AB_pw_lm, studentize = TRUE)
AB_pw_DWtest <- dwtest(AB_pw_lm)
AB_pw_bgtest <- bgtest(AB_pw_lm, order = 1)
AB_pw_jbtest <- jarque.bera.test(resid(AB_pw_lm))
AB_pw_shtest <- shapiro.test(resid(AB_pw_lm))

# Zipf_Mandelbrot Law
AB_ZM_prm <- get_ZM_Param("RO_Census_Alba_New", "Population", "Rank")
AB_ZM_m <- AB_ZM_prm$m
AB_ZM_alpha <- AB_ZM_prm$alpha_ZM
AB_ZM_cst <- AB_ZM_prm$constant_ZM
AB_ZM_lm <- lm(log(RO_Census_Alba_New$Population) ~ log(RO_Census_Alba_New$Rank + AB_ZM_m))
summary(AB_ZM_lm)
AB_pred_ZM_lm <- exp(predict(AB_ZM_lm, newdata = RO_Census_Alba_New, 
                             interval = "prediction", level = 0.95))
AB_ZM_whtest <- white(AB_ZM_lm, interactions = TRUE)
AB_ZM_bptest <- bptest(AB_ZM_lm, studentize = TRUE)
AB_ZM_DWtest <- dwtest(AB_ZM_lm)
AB_ZM_bgtest <- bgtest(AB_ZM_lm, order = 1)
AB_ZM_jbtest <- jarque.bera.test(resid(AB_ZM_lm))
AB_ZM_shtest <- shapiro.test(resid(AB_ZM_lm))

# Exponential Law
AB_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Alba_New)
summary(AB_exp_lm)
AB_pred_exp_lm <- exp(predict(AB_exp_lm, newdata = RO_Census_Alba_New, 
                              interval = "prediction", level = 0.95))
AB_exp_whtest <- white(AB_exp_lm, interactions = TRUE)
AB_exp_bptest <- bptest(AB_exp_lm, studentize = TRUE)
AB_exp_DWtest <- dwtest(AB_exp_lm)
AB_exp_bgtest <- bgtest(AB_exp_lm, order = 1)
AB_exp_jbtest <- jarque.bera.test(resid(AB_exp_lm))
AB_exp_shtest <- shapiro.test(resid(AB_exp_lm))

# Lavalette Function
RO_Census_Alba_New <- RO_Census_Alba_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Alba_New) - Rank + 1))
)
AB_lav_lm <- lm(log(RO_Census_Alba_New$Population) ~ RO_Census_Alba_New$Lav_exp)
summary(AB_lav_lm)
AB_lav_kst <- exp(signif(AB_lav_lm$coef[[1]], 4))
AB_lav_chi <- signif(AB_lav_lm$coef[[2]], 4)
AB_pred_lav_lm <- exp(predict(AB_lav_lm, newdata = RO_Census_Alba_New, 
                              interval = "prediction", level = 0.95))
AB_lav_whtest <- white(AB_lav_lm, interactions = TRUE)
AB_lav_bptest <- bptest(AB_lav_lm, studentize = TRUE)
AB_lav_DWtest <- dwtest(AB_lav_lm)
AB_lav_bgtest <- bgtest(AB_lav_lm, order = 1)
AB_lav_jbtest <- jarque.bera.test(resid(AB_lav_lm))
AB_lav_shtest <- shapiro.test(resid(AB_lav_lm))

# Data Distributions
RO_Census_Alba_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Alba_New$Rank, 
                              Pop_Freq = AB_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.15,
            label = paste0("y", "==", signif(exp(AB_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(AB_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 57, y = 0.15,
            label = paste0("R^2 ==", signif(summary(AB_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Alba_New$Rank, 
                              Pop_Freq = AB_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.13,
            label = paste0("y", "==", signif(exp(AB_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(AB_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 57, y = 0.13,
            label = paste0("R^2 ==", signif(summary(AB_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Alba_New$Rank, 
                              Pop_Freq = AB_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.11,
            label = paste0("y", "==", signif(AB_ZM_cst, 4), "%.%", "(",
                           signif(AB_ZM_m, 4), "+ x)^", signif(AB_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 57, y = 0.11,
            label = paste0("R^2 ==", signif(summary(AB_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Alba_New$Rank, 
                              Pop_Freq = AB_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.09,
            label = paste0("y", "==", signif(AB_lav_kst, 4), "%.%", "x^",
                           -signif(AB_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 57, y = 0.09,
            label = paste0("R^2 ==", signif(summary(AB_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Alba judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Alba_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Alba judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Alba_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Alba judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Alba_New %>%
     ggplot(aes(x = log(Rank + AB_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Alba judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Alba_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Alba judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 2. Arad
AR_out <- get_outliers(Arad_Outliers, RO_Census_Arad)

RO_Census_Arad_New <- RO_Census_df %>% inner_join(
  RO_Census_Arad %>% anti_join(
    bind_rows(
      (AR_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (AR_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (AR_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (AR_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Arad_New <- RO_Census_Arad_New %>% 
  left_join(RO_Census_Arad_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
AR_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Arad_New)
summary(AR_pw_lm)
AR_pred_pw_lm <- exp(predict(AR_pw_lm, newdata = RO_Census_Arad_New, 
                             interval = "prediction", level = 0.95))
AR_pw_whtest <- white(AR_pw_lm, interactions = TRUE)
AR_pw_bptest <- bptest(AR_pw_lm, studentize = TRUE)
AR_pw_DWtest <- dwtest(AR_pw_lm)
AR_pw_bgtest <- bgtest(AR_pw_lm, order = 1)
AR_pw_jbtest <- jarque.bera.test(resid(AR_pw_lm))
AR_pw_shtest <- shapiro.test(resid(AR_pw_lm))

# Zipf_Mandelbrot Law
AR_ZM_prm <- get_ZM_Param("RO_Census_Arad_New", "Population", "Rank")
AR_ZM_m <- AR_ZM_prm$m
AR_ZM_alpha <- AR_ZM_prm$alpha_ZM
AR_ZM_cst <- AR_ZM_prm$constant_ZM
AR_ZM_lm <- lm(log(RO_Census_Arad_New$Population) ~ log(RO_Census_Arad_New$Rank + AR_ZM_m))
summary(AR_ZM_lm)
AR_pred_ZM_lm <- exp(predict(AR_ZM_lm, newdata = RO_Census_Arad_New, 
                             interval = "prediction", level = 0.95))
AR_ZM_whtest <- white(AR_ZM_lm, interactions = TRUE)
AR_ZM_bptest <- bptest(AR_ZM_lm, studentize = TRUE)
AR_ZM_DWtest <- dwtest(AR_ZM_lm)
AR_ZM_bgtest <- bgtest(AR_ZM_lm, order = 1)
AR_ZM_jbtest <- jarque.bera.test(resid(AR_ZM_lm))
AR_ZM_shtest <- shapiro.test(resid(AR_ZM_lm))

# Exponential Law
AR_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Arad_New)
summary(AR_exp_lm)
AR_pred_exp_lm <- exp(predict(AR_exp_lm, newdata = RO_Census_Arad_New, 
                              interval = "prediction", level = 0.95))
AR_exp_whtest <- white(AR_exp_lm, interactions = TRUE)
AR_exp_bptest <- bptest(AR_exp_lm, studentize = TRUE)
AR_exp_DWtest <- dwtest(AR_exp_lm)
AR_exp_bgtest <- bgtest(AR_exp_lm, order = 1)
AR_exp_jbtest <- jarque.bera.test(resid(AR_exp_lm))
AR_exp_shtest <- shapiro.test(resid(AR_exp_lm))

# Lavalette Function
RO_Census_Arad_New <- RO_Census_Arad_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Arad_New) - Rank + 1))
)
AR_lav_lm <- lm(log(RO_Census_Arad_New$Population) ~ RO_Census_Arad_New$Lav_exp)
summary(AR_lav_lm)
AR_lav_kst <- exp(signif(AR_lav_lm$coef[[1]], 4))
AR_lav_chi <- signif(AR_lav_lm$coef[[2]], 4)
AR_pred_lav_lm <- exp(predict(AR_lav_lm, newdata = RO_Census_Arad_New, 
                              interval = "prediction", level = 0.95))
AR_lav_whtest <- white(AR_lav_lm, interactions = TRUE)
AR_lav_bptest <- bptest(AR_lav_lm, studentize = TRUE)
AR_lav_DWtest <- dwtest(AR_lav_lm)
AR_lav_bgtest <- bgtest(AR_lav_lm, order = 1)
AR_lav_jbtest <- jarque.bera.test(resid(AR_lav_lm))
AR_lav_shtest <- shapiro.test(resid(AR_lav_lm))

# Data Distributions
RO_Census_Arad_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Arad_New$Rank, 
                              Pop_Freq = AR_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 30, y = 0.08,
            label = paste0("y", "==", signif(exp(AR_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(AR_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.08,
            label = paste0("R^2 ==", signif(summary(AR_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arad_New$Rank, 
                              Pop_Freq = AR_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 30, y = 0.07,
            label = paste0("y", "==", signif(exp(AR_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(AR_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.07,
            label = paste0("R^2 ==", signif(summary(AR_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arad_New$Rank, 
                              Pop_Freq = AR_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 30, y = 0.06,
            label = paste0("y", "==", signif(AR_ZM_cst, 4), "%.%", "(",
                           signif(AR_ZM_m, 4), "+ x)^", signif(AR_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.06,
            label = paste0("R^2 ==", signif(summary(AR_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arad_New$Rank, 
                              Pop_Freq = AR_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 30, y = 0.05,
            label = paste0("y", "==", signif(AR_lav_kst, 4), "%.%", "x^",
                           -signif(AR_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.05,
            label = paste0("R^2 ==", signif(summary(AR_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Arad judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Arad_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Arad judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arad_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Arad judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arad_New %>%
     ggplot(aes(x = log(Rank + AR_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Arad judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arad_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Arad judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 3. Arges
AG_out <- get_outliers(Arges_Outliers, RO_Census_Arges)

RO_Census_Arges_New <- RO_Census_df %>% inner_join(
  RO_Census_Arges %>% anti_join(
    bind_rows(
      (AG_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (AG_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (AG_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (AG_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Arges_New <- RO_Census_Arges_New %>% 
  left_join(RO_Census_Arges_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
AG_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Arges_New)
summary(AG_pw_lm)
AG_pred_pw_lm <- exp(predict(AG_pw_lm, newdata = RO_Census_Arges_New, 
                             interval = "prediction", level = 0.95))
AG_pw_whtest <- white(AG_pw_lm, interactions = TRUE)
AG_pw_bptest <- bptest(AG_pw_lm, studentize = TRUE)
AG_pw_DWtest <- dwtest(AG_pw_lm)
AG_pw_bgtest <- bgtest(AG_pw_lm, order = 1)
AG_pw_jbtest <- jarque.bera.test(resid(AG_pw_lm))
AG_pw_shtest <- shapiro.test(resid(AG_pw_lm))

# Zipf_Mandelbrot Law
AG_ZM_prm <- get_ZM_Param("RO_Census_Arges_New", "Population", "Rank")
AG_ZM_m <- AG_ZM_prm$m
AG_ZM_alpha <- AG_ZM_prm$alpha_ZM
AG_ZM_cst <- AG_ZM_prm$constant_ZM
AG_ZM_lm <- lm(log(RO_Census_Arges_New$Population) ~ log(RO_Census_Arges_New$Rank + AG_ZM_m))
summary(AG_ZM_lm)
AG_pred_ZM_lm <- exp(predict(AG_ZM_lm, newdata = RO_Census_Arges_New, 
                             interval = "prediction", level = 0.95))
AG_ZM_whtest <- white(AG_ZM_lm, interactions = TRUE)
AG_ZM_bptest <- bptest(AG_ZM_lm, studentize = TRUE)
AG_ZM_DWtest <- dwtest(AG_ZM_lm)
AG_ZM_bgtest <- bgtest(AG_ZM_lm, order = 1)
AG_ZM_jbtest <- jarque.bera.test(resid(AG_ZM_lm))
AG_ZM_shtest <- shapiro.test(resid(AG_ZM_lm))

# Exponential Law
AG_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Arges_New)
summary(AG_exp_lm)
AG_pred_exp_lm <- exp(predict(AG_exp_lm, newdata = RO_Census_Arges_New, 
                              interval = "prediction", level = 0.95))
AG_exp_whtest <- white(AG_exp_lm, interactions = TRUE)
AG_exp_bptest <- bptest(AG_exp_lm, studentize = TRUE)
AG_exp_DWtest <- dwtest(AG_exp_lm)
AG_exp_bgtest <- bgtest(AG_exp_lm, order = 1)
AG_exp_jbtest <- jarque.bera.test(resid(AG_exp_lm))
AG_exp_shtest <- shapiro.test(resid(AG_exp_lm))

# Lavalette Function
RO_Census_Arges_New <- RO_Census_Arges_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Arges_New) - Rank + 1))
)
AG_lav_lm <- lm(log(RO_Census_Arges_New$Population) ~ RO_Census_Arges_New$Lav_exp)
summary(AG_lav_lm)
AG_lav_kst <- exp(signif(AG_lav_lm$coef[[1]], 4))
AG_lav_chi <- signif(AG_lav_lm$coef[[2]], 4)
AG_pred_lav_lm <- exp(predict(AG_lav_lm, newdata = RO_Census_Arges_New, 
                              interval = "prediction", level = 0.95))
AG_lav_whtest <- white(AG_lav_lm, interactions = TRUE)
AG_lav_bptest <- bptest(AG_lav_lm, studentize = TRUE)
AG_lav_DWtest <- dwtest(AG_lav_lm)
AG_lav_bgtest <- bgtest(AG_lav_lm, order = 1)
AG_lav_jbtest <- jarque.bera.test(resid(AG_lav_lm))
AG_lav_shtest <- shapiro.test(resid(AG_lav_lm))

# Data Distributions
RO_Census_Arges_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Arges_New$Rank, 
                              Pop_Freq = AG_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 30, y = 0.08,
            label = paste0("y", "==", signif(exp(AG_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(AG_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.08,
            label = paste0("R^2 ==", signif(summary(AG_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arges_New$Rank, 
                              Pop_Freq = AG_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 30, y = 0.07,
            label = paste0("y", "==", signif(exp(AG_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(AG_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.07,
            label = paste0("R^2 ==", signif(summary(AG_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arges_New$Rank, 
                              Pop_Freq = AG_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 30, y = 0.06,
            label = paste0("y", "==", signif(AG_ZM_cst, 4), "%.%", "(",
                           signif(AG_ZM_m, 4), "+ x)^", signif(AG_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.06,
            label = paste0("R^2 ==", signif(summary(AG_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arges_New$Rank, 
                              Pop_Freq = AG_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 30, y = 0.05,
            label = paste0("y", "==", signif(AG_lav_kst, 4), "%.%", "x^",
                           -signif(AG_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.05,
            label = paste0("R^2 ==", signif(summary(AG_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Arges judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Arges_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Arges judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arges_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Arges judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arges_New %>%
     ggplot(aes(x = log(Rank + AG_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Arges judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arges_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Arges judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 4. Bacau
BC_out <- get_outliers(Bacau_Outliers, RO_Census_Bacau)

RO_Census_Bacau_New <- RO_Census_df %>% inner_join(
  RO_Census_Bacau %>% anti_join(
    bind_rows(
      (BC_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (BC_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BC_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BC_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Bacau_New <- RO_Census_Bacau_New %>% 
  left_join(RO_Census_Bacau_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BC_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Bacau_New)
summary(BC_pw_lm)
BC_pred_pw_lm <- exp(predict(BC_pw_lm, newdata = RO_Census_Bacau_New, 
                             interval = "prediction", level = 0.95))
BC_pw_whtest <- white(BC_pw_lm, interactions = TRUE)
BC_pw_bptest <- bptest(BC_pw_lm, studentize = TRUE)
BC_pw_DWtest <- dwtest(BC_pw_lm)
BC_pw_bgtest <- bgtest(BC_pw_lm, order = 1)
BC_pw_jbtest <- jarque.bera.test(resid(BC_pw_lm))
BC_pw_shtest <- shapiro.test(resid(BC_pw_lm))

# Zipf_Mandelbrot Law
BC_ZM_prm <- get_ZM_Param("RO_Census_Bacau_New", "Population", "Rank")
BC_ZM_m <- BC_ZM_prm$m
BC_ZM_alpha <- BC_ZM_prm$alpha_ZM
BC_ZM_cst <- BC_ZM_prm$constant_ZM
BC_ZM_lm <- lm(log(RO_Census_Bacau_New$Population) ~ log(RO_Census_Bacau_New$Rank + BC_ZM_m))
summary(BC_ZM_lm)
BC_pred_ZM_lm <- exp(predict(BC_ZM_lm, newdata = RO_Census_Bacau_New, 
                             interval = "prediction", level = 0.95))
BC_ZM_whtest <- white(BC_ZM_lm, interactions = TRUE)
BC_ZM_bptest <- bptest(BC_ZM_lm, studentize = TRUE)
BC_ZM_DWtest <- dwtest(BC_ZM_lm)
BC_ZM_bgtest <- bgtest(BC_ZM_lm, order = 1)
BC_ZM_jbtest <- jarque.bera.test(resid(BC_ZM_lm))
BC_ZM_shtest <- shapiro.test(resid(BC_ZM_lm))

# Exponential Law
BC_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Bacau_New)
summary(BC_exp_lm)
BC_pred_exp_lm <- exp(predict(BC_exp_lm, newdata = RO_Census_Bacau_New, 
                              interval = "prediction", level = 0.95))
BC_exp_whtest <- white(BC_exp_lm, interactions = TRUE)
BC_exp_bptest <- bptest(BC_exp_lm, studentize = TRUE)
BC_exp_DWtest <- dwtest(BC_exp_lm)
BC_exp_bgtest <- bgtest(BC_exp_lm, order = 1)
BC_exp_jbtest <- jarque.bera.test(resid(BC_exp_lm))
BC_exp_shtest <- shapiro.test(resid(BC_exp_lm))

# Lavalette Function
RO_Census_Bacau_New <- RO_Census_Bacau_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Bacau_New) - Rank + 1))
)
BC_lav_lm <- lm(log(RO_Census_Bacau_New$Population) ~ RO_Census_Bacau_New$Lav_exp)
summary(BC_lav_lm)
BC_lav_kst <- exp(signif(BC_lav_lm$coef[[1]], 4))
BC_lav_chi <- signif(BC_lav_lm$coef[[2]], 4)
BC_pred_lav_lm <- exp(predict(BC_lav_lm, newdata = RO_Census_Bacau_New, 
                              interval = "prediction", level = 0.95))
BC_lav_whtest <- white(BC_lav_lm, interactions = TRUE)
BC_lav_bptest <- bptest(BC_lav_lm, studentize = TRUE)
BC_lav_DWtest <- dwtest(BC_lav_lm)
BC_lav_bgtest <- bgtest(BC_lav_lm, order = 1)
BC_lav_jbtest <- jarque.bera.test(resid(BC_lav_lm))
BC_lav_shtest <- shapiro.test(resid(BC_lav_lm))

# Data Distributions
RO_Census_Bacau_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Bacau_New$Rank, 
                              Pop_Freq = BC_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.05,
            label = paste0("y", "==", signif(exp(BC_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BC_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.05,
            label = paste0("R^2 ==", signif(summary(BC_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bacau_New$Rank, 
                              Pop_Freq = BC_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.04,
            label = paste0("y", "==", signif(exp(BC_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BC_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.04,
            label = paste0("R^2 ==", signif(summary(BC_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bacau_New$Rank, 
                              Pop_Freq = BC_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.03,
            label = paste0("y", "==", signif(BC_ZM_cst, 4), "%.%", "(",
                           signif(BC_ZM_m, 4), "+ x)^", signif(BC_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.03,
            label = paste0("R^2 ==", signif(summary(BC_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bacau_New$Rank, 
                              Pop_Freq = BC_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.02,
            label = paste0("y", "==", signif(BC_lav_kst, 4), "%.%", "x^",
                           -signif(BC_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.02,
            label = paste0("R^2 ==", signif(summary(BC_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Bacau judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Bacau_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Bacau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bacau_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Bacau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bacau_New %>%
     ggplot(aes(x = log(Rank + BC_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Bacau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bacau_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Bacau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 5. Bihor
BH_out <- get_outliers(Bihor_Outliers, RO_Census_Bihor)

RO_Census_Bihor_New <- RO_Census_df %>% inner_join(
  RO_Census_Bihor %>% anti_join(
    bind_rows(
      (BH_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (BH_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BH_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BH_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Bihor_New <- RO_Census_Bihor_New %>% 
  left_join(RO_Census_Bihor_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BH_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Bihor_New)
summary(BH_pw_lm)
BH_pred_pw_lm <- exp(predict(BH_pw_lm, newdata = RO_Census_Bihor_New, 
                             interval = "prediction", level = 0.95))
BH_pw_whtest <- white(BH_pw_lm, interactions = TRUE)
BH_pw_bptest <- bptest(BH_pw_lm, studentize = TRUE)
BH_pw_DWtest <- dwtest(BH_pw_lm)
BH_pw_bgtest <- bgtest(BH_pw_lm, order = 1)
BH_pw_jbtest <- jarque.bera.test(resid(BH_pw_lm))
BH_pw_shtest <- shapiro.test(resid(BH_pw_lm))

# Zipf_Mandelbrot Law
BH_ZM_prm <- get_ZM_Param("RO_Census_Bihor_New", "Population", "Rank")
BH_ZM_m <- BH_ZM_prm$m
BH_ZM_alpha <- BH_ZM_prm$alpha_ZM
BH_ZM_cst <- BH_ZM_prm$constant_ZM
BH_ZM_lm <- lm(log(RO_Census_Bihor_New$Population) ~ log(RO_Census_Bihor_New$Rank + BH_ZM_m))
summary(BH_ZM_lm)
BH_pred_ZM_lm <- exp(predict(BH_ZM_lm, newdata = RO_Census_Bihor_New, 
                             interval = "prediction", level = 0.95))
BH_ZM_whtest <- white(BH_ZM_lm, interactions = TRUE)
BH_ZM_bptest <- bptest(BH_ZM_lm, studentize = TRUE)
BH_ZM_DWtest <- dwtest(BH_ZM_lm)
BH_ZM_bgtest <- bgtest(BH_ZM_lm, order = 1)
BH_ZM_jbtest <- jarque.bera.test(resid(BH_ZM_lm))
BH_ZM_shtest <- shapiro.test(resid(BH_ZM_lm))

# Exponential Law
BH_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Bihor_New)
summary(BH_exp_lm)
BH_pred_exp_lm <- exp(predict(BH_exp_lm, newdata = RO_Census_Bihor_New, 
                              interval = "prediction", level = 0.95))
BH_exp_whtest <- white(BH_exp_lm, interactions = TRUE)
BH_exp_bptest <- bptest(BH_exp_lm, studentize = TRUE)
BH_exp_DWtest <- dwtest(BH_exp_lm)
BH_exp_bgtest <- bgtest(BH_exp_lm, order = 1)
BH_exp_jbtest <- jarque.bera.test(resid(BH_exp_lm))
BH_exp_shtest <- shapiro.test(resid(BH_exp_lm))

# Lavalette Function
RO_Census_Bihor_New <- RO_Census_Bihor_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Bihor_New) - Rank + 1))
)
BH_lav_lm <- lm(log(RO_Census_Bihor_New$Population) ~ RO_Census_Bihor_New$Lav_exp)
summary(BH_lav_lm)
BH_lav_kst <- exp(signif(BH_lav_lm$coef[[1]], 4))
BH_lav_chi <- signif(BH_lav_lm$coef[[2]], 4)
BH_pred_lav_lm <- exp(predict(BH_lav_lm, newdata = RO_Census_Bihor_New, 
                              interval = "prediction", level = 0.95))
BH_lav_whtest <- white(BH_lav_lm, interactions = TRUE)
BH_lav_bptest <- bptest(BH_lav_lm, studentize = TRUE)
BH_lav_DWtest <- dwtest(BH_lav_lm)
BH_lav_bgtest <- bgtest(BH_lav_lm, order = 1)
BH_lav_jbtest <- jarque.bera.test(resid(BH_lav_lm))
BH_lav_shtest <- shapiro.test(resid(BH_lav_lm))

# Data Distributions
RO_Census_Bihor_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Bihor_New$Rank, 
                              Pop_Freq = BH_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.06,
            label = paste0("y", "==", signif(exp(BH_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BH_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.06,
            label = paste0("R^2 ==", signif(summary(BH_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bihor_New$Rank, 
                              Pop_Freq = BH_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.05,
            label = paste0("y", "==", signif(exp(BH_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BH_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.05,
            label = paste0("R^2 ==", signif(summary(BH_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bihor_New$Rank, 
                              Pop_Freq = BH_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.04,
            label = paste0("y", "==", signif(BH_ZM_cst, 4), "%.%", "(",
                           signif(BH_ZM_m, 4), "+ x)^", signif(BH_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.04,
            label = paste0("R^2 ==", signif(summary(BH_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bihor_New$Rank, 
                              Pop_Freq = BH_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.03,
            label = paste0("y", "==", signif(BH_lav_kst, 4), "%.%", "x^",
                           -signif(BH_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.03,
            label = paste0("R^2 ==", signif(summary(BH_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Bihor judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Bihor_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Bihor judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bihor_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Bihor judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bihor_New %>%
     ggplot(aes(x = log(Rank + BH_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Bihor judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bihor_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Bihor judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 6. Bistrita-Nasaud
BN_out <- get_outliers(BistNsd_Outliers, RO_Census_BistNsd)

RO_Census_BistNsd_New <- RO_Census_df %>% inner_join(
  RO_Census_BistNsd %>% anti_join(
    bind_rows(
      (BN_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (BN_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BN_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BN_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_BistNsd_New <- RO_Census_BistNsd_New %>% 
  left_join(RO_Census_BistNsd_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BN_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_BistNsd_New)
summary(BN_pw_lm)
BN_pred_pw_lm <- exp(predict(BN_pw_lm, newdata = RO_Census_BistNsd_New, 
                             interval = "prediction", level = 0.95))
BN_pw_whtest <- white(BN_pw_lm, interactions = TRUE)
BN_pw_bptest <- bptest(BN_pw_lm, studentize = TRUE)
BN_pw_DWtest <- dwtest(BN_pw_lm)
BN_pw_bgtest <- bgtest(BN_pw_lm, order = 1)
BN_pw_jbtest <- jarque.bera.test(resid(BN_pw_lm))
BN_pw_shtest <- shapiro.test(resid(BN_pw_lm))

# Zipf_Mandelbrot Law
BN_ZM_prm <- get_ZM_Param("RO_Census_BistNsd_New", "Population", "Rank")
BN_ZM_m <- BN_ZM_prm$m
BN_ZM_alpha <- BN_ZM_prm$alpha_ZM
BN_ZM_cst <- BN_ZM_prm$constant_ZM
BN_ZM_lm <- lm(log(RO_Census_BistNsd_New$Population) ~ log(RO_Census_BistNsd_New$Rank + BN_ZM_m))
summary(BN_ZM_lm)
BN_pred_ZM_lm <- exp(predict(BN_ZM_lm, newdata = RO_Census_BistNsd_New, 
                             interval = "prediction", level = 0.95))
BN_ZM_whtest <- white(BN_ZM_lm, interactions = TRUE)
BN_ZM_bptest <- bptest(BN_ZM_lm, studentize = TRUE)
BN_ZM_DWtest <- dwtest(BN_ZM_lm)
BN_ZM_bgtest <- bgtest(BN_ZM_lm, order = 1)
BN_ZM_jbtest <- jarque.bera.test(resid(BN_ZM_lm))
BN_ZM_shtest <- shapiro.test(resid(BN_ZM_lm))

# Exponential Law
BN_exp_lm <- lm(log(Population) ~ Rank, RO_Census_BistNsd_New)
summary(BN_exp_lm)
BN_pred_exp_lm <- exp(predict(BN_exp_lm, newdata = RO_Census_BistNsd_New, 
                              interval = "prediction", level = 0.95))
BN_exp_whtest <- white(BN_exp_lm, interactions = TRUE)
BN_exp_bptest <- bptest(BN_exp_lm, studentize = TRUE)
BN_exp_DWtest <- dwtest(BN_exp_lm)
BN_exp_bgtest <- bgtest(BN_exp_lm, order = 1)
BN_exp_jbtest <- jarque.bera.test(resid(BN_exp_lm))
BN_exp_shtest <- shapiro.test(resid(BN_exp_lm))

# Lavalette Function
RO_Census_BistNsd_New <- RO_Census_BistNsd_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_BistNsd_New) - Rank + 1))
)
BN_lav_lm <- lm(log(RO_Census_BistNsd_New$Population) ~ RO_Census_BistNsd_New$Lav_exp)
summary(BN_lav_lm)
BN_lav_kst <- exp(signif(BN_lav_lm$coef[[1]], 4))
BN_lav_chi <- signif(BN_lav_lm$coef[[2]], 4)
BN_pred_lav_lm <- exp(predict(BN_lav_lm, newdata = RO_Census_BistNsd_New, 
                              interval = "prediction", level = 0.95))
BN_lav_whtest <- white(BN_lav_lm, interactions = TRUE)
BN_lav_bptest <- bptest(BN_lav_lm, studentize = TRUE)
BN_lav_DWtest <- dwtest(BN_lav_lm)
BN_lav_bgtest <- bgtest(BN_lav_lm, order = 1)
BN_lav_jbtest <- jarque.bera.test(resid(BN_lav_lm))
BN_lav_shtest <- shapiro.test(resid(BN_lav_lm))

# Data Distributions
RO_Census_BistNsd_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_BistNsd_New$Rank, 
                              Pop_Freq = BN_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(exp(BN_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BN_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.08,
            label = paste0("R^2 ==", signif(summary(BN_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_BistNsd_New$Rank, 
                              Pop_Freq = BN_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(BN_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BN_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.07,
            label = paste0("R^2 ==", signif(summary(BN_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_BistNsd_New$Rank, 
                              Pop_Freq = BN_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(BN_ZM_cst, 4), "%.%", "(",
                           signif(BN_ZM_m, 4), "+ x)^", signif(BN_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.06,
            label = paste0("R^2 ==", signif(summary(BN_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_BistNsd_New$Rank, 
                              Pop_Freq = BN_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(BN_lav_kst, 4), "%.%", "x^",
                           -signif(BN_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(BN_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Bistrita-Nasaud judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_BistNsd_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Bistrita-Nasaud judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_BistNsd_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Bistrita-Nasaud judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_BistNsd_New %>%
     ggplot(aes(x = log(Rank + BN_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Bistrita-Nasaud judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_BistNsd_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Bistrita-Nasaud judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 7. Botosani
BT_out <- get_outliers(Botosani_Outliers, RO_Census_Botosani)

RO_Census_Botosani_New <- RO_Census_df %>% inner_join(
  RO_Census_Botosani %>% anti_join(
    bind_rows(
      (BT_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (BT_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BT_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BT_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n == max(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Botosani_New <- RO_Census_Botosani_New %>% 
  left_join(RO_Census_Botosani_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BT_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Botosani_New)
summary(BT_pw_lm)
BT_pred_pw_lm <- exp(predict(BT_pw_lm, newdata = RO_Census_Botosani_New, 
                             interval = "prediction", level = 0.95))
BT_pw_whtest <- white(BT_pw_lm, interactions = TRUE)
BT_pw_bptest <- bptest(BT_pw_lm, studentize = TRUE)
BT_pw_DWtest <- dwtest(BT_pw_lm)
BT_pw_bgtest <- bgtest(BT_pw_lm, order = 1)
BT_pw_jbtest <- jarque.bera.test(resid(BT_pw_lm))
BT_pw_shtest <- shapiro.test(resid(BT_pw_lm))

# Zipf_Mandelbrot Law
BT_ZM_prm <- get_ZM_Param("RO_Census_Botosani_New", "Population", "Rank")
BT_ZM_m <- BT_ZM_prm$m
BT_ZM_alpha <- BT_ZM_prm$alpha_ZM
BT_ZM_cst <- BT_ZM_prm$constant_ZM
BT_ZM_lm <- lm(log(RO_Census_Botosani_New$Population) ~ log(RO_Census_Botosani_New$Rank + BT_ZM_m))
summary(BT_ZM_lm)
BT_pred_ZM_lm <- exp(predict(BT_ZM_lm, newdata = RO_Census_Botosani_New, 
                             interval = "prediction", level = 0.95))
BT_ZM_whtest <- white(BT_ZM_lm, interactions = TRUE)
BT_ZM_bptest <- bptest(BT_ZM_lm, studentize = TRUE)
BT_ZM_DWtest <- dwtest(BT_ZM_lm)
BT_ZM_bgtest <- bgtest(BT_ZM_lm, order = 1)
BT_ZM_jbtest <- jarque.bera.test(resid(BT_ZM_lm))
BT_ZM_shtest <- shapiro.test(resid(BT_ZM_lm))

# Exponential Law
BT_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Botosani_New)
summary(BT_exp_lm)
BT_pred_exp_lm <- exp(predict(BT_exp_lm, newdata = RO_Census_Botosani_New, 
                              interval = "prediction", level = 0.95))
BT_exp_whtest <- white(BT_exp_lm, interactions = TRUE)
BT_exp_bptest <- bptest(BT_exp_lm, studentize = TRUE)
BT_exp_DWtest <- dwtest(BT_exp_lm)
BT_exp_bgtest <- bgtest(BT_exp_lm, order = 1)
BT_exp_jbtest <- jarque.bera.test(resid(BT_exp_lm))
BT_exp_shtest <- shapiro.test(resid(BT_exp_lm))

# Lavalette Function
RO_Census_Botosani_New <- RO_Census_Botosani_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Botosani_New) - Rank + 1))
)
BT_lav_lm <- lm(log(RO_Census_Botosani_New$Population) ~ RO_Census_Botosani_New$Lav_exp)
summary(BT_lav_lm)
BT_lav_kst <- exp(signif(BT_lav_lm$coef[[1]], 4))
BT_lav_chi <- signif(BT_lav_lm$coef[[2]], 4)
BT_pred_lav_lm <- exp(predict(BT_lav_lm, newdata = RO_Census_Botosani_New, 
                              interval = "prediction", level = 0.95))
BT_lav_whtest <- white(BT_lav_lm, interactions = TRUE)
BT_lav_bptest <- bptest(BT_lav_lm, studentize = TRUE)
BT_lav_DWtest <- dwtest(BT_lav_lm)
BT_lav_bgtest <- bgtest(BT_lav_lm, order = 1)
BT_lav_jbtest <- jarque.bera.test(resid(BT_lav_lm))
BT_lav_shtest <- shapiro.test(resid(BT_lav_lm))

# Data Distributions
RO_Census_Botosani_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Botosani_New$Rank, 
                              Pop_Freq = BT_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(exp(BT_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BT_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(BT_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Botosani_New$Rank, 
                              Pop_Freq = BT_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.045,
            label = paste0("y", "==", signif(exp(BT_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BT_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.045,
            label = paste0("R^2 ==", signif(summary(BT_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Botosani_New$Rank, 
                              Pop_Freq = BT_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.04,
            label = paste0("y", "==", signif(BT_ZM_cst, 4), "%.%", "(",
                           signif(BT_ZM_m, 4), "+ x)^", signif(BT_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.04,
            label = paste0("R^2 ==", signif(summary(BT_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Botosani_New$Rank, 
                              Pop_Freq = BT_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.035,
            label = paste0("y", "==", signif(BT_lav_kst, 4), "%.%", "x^",
                           -signif(BT_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.035,
            label = paste0("R^2 ==", signif(summary(BT_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Botosani judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Botosani_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Botosani judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Botosani_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Botosani judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Botosani_New %>%
     ggplot(aes(x = log(Rank + BT_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Botosani judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Botosani_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Botosani judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 8. Braila

# RO_Census_Braila_New <- RO_Census_df %>% filter(Judet_Name == "BRAILA") %>%
#   inner_join(
#     RO_Census_Braila %>% mutate(LOF = lofactor(Pop_Freq, k = 3)) %>%
#       filter(round(LOF, 2) < 1.5) %>% dplyr::select(Judet_Name, Unit_Type, Unit_Name),
#     by = c("Judet_Name", "Unit_Type", "Unit_Name")) %>%
#   left_join((RO_Census_df %>% filter(Judet_Name == "BRAILA") %>%
#                inner_join(
#                  RO_Census_Braila %>% mutate(LOF = lofactor(Pop_Freq, k = 3)) %>%
#                    filter(round(LOF, 2) < 1.5) %>%
#                    dplyr::select(Judet_Name, Unit_Type, Unit_Name),
#                  by = c("Judet_Name", "Unit_Type", "Unit_Name"))) %>%
#               group_by(Judet_Name) %>%
#               summarize(Total_Pop = sum(Population))) %>%
#   arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>%
#   mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
#   ungroup()

BR_out <- get_outliers(Braila_Outliers, RO_Census_Braila)

RO_Census_Braila_New <- RO_Census_df %>% inner_join(
  RO_Census_Braila %>% anti_join(
    bind_rows(
      (BR_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (BR_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BR_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BR_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Braila_New <- RO_Census_Braila_New %>% 
  left_join(RO_Census_Braila_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BR_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Braila_New)
summary(BR_pw_lm)
BR_pred_pw_lm <- exp(predict(BR_pw_lm, newdata = RO_Census_Braila_New, 
                             interval = "prediction", level = 0.95))
BR_pw_whtest <- white(BR_pw_lm, interactions = TRUE)
BR_pw_bptest <- bptest(BR_pw_lm, studentize = TRUE)
BR_pw_DWtest <- dwtest(BR_pw_lm)
BR_pw_bgtest <- bgtest(BR_pw_lm, order = 1)
BR_pw_jbtest <- jarque.bera.test(resid(BR_pw_lm))
BR_pw_shtest <- shapiro.test(resid(BR_pw_lm))

# Zipf_Mandelbrot Law
BR_ZM_prm <- get_ZM_Param("RO_Census_Braila_New", "Population", "Rank")
BR_ZM_m <- BR_ZM_prm$m
BR_ZM_alpha <- BR_ZM_prm$alpha_ZM
BR_ZM_cst <- BR_ZM_prm$constant_ZM
BR_ZM_lm <- lm(log(RO_Census_Braila_New$Population) ~ log(RO_Census_Braila_New$Rank + BR_ZM_m))
summary(BR_ZM_lm)
BR_pred_ZM_lm <- exp(predict(BR_ZM_lm, newdata = RO_Census_Braila_New, 
                             interval = "prediction", level = 0.95))
BR_ZM_whtest <- white(BR_ZM_lm, interactions = TRUE)
BR_ZM_bptest <- bptest(BR_ZM_lm, studentize = TRUE)
BR_ZM_DWtest <- dwtest(BR_ZM_lm)
BR_ZM_bgtest <- bgtest(BR_ZM_lm, order = 1)
BR_ZM_jbtest <- jarque.bera.test(resid(BR_ZM_lm))
BR_ZM_shtest <- shapiro.test(resid(BR_ZM_lm))

# Exponential Law
BR_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Braila_New)
summary(BR_exp_lm)
BR_pred_exp_lm <- exp(predict(BR_exp_lm, newdata = RO_Census_Braila_New, 
                              interval = "prediction", level = 0.95))
BR_exp_whtest <- white(BR_exp_lm, interactions = TRUE)
BR_exp_bptest <- bptest(BR_exp_lm, studentize = TRUE)
BR_exp_DWtest <- dwtest(BR_exp_lm)
BR_exp_bgtest <- bgtest(BR_exp_lm, order = 1)
BR_exp_jbtest <- jarque.bera.test(resid(BR_exp_lm))
BR_exp_shtest <- shapiro.test(resid(BR_exp_lm))

# Lavalette Function
RO_Census_Braila_New <- RO_Census_Braila_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Braila_New) - Rank + 1))
)
BR_lav_lm <- lm(log(RO_Census_Braila_New$Population) ~ RO_Census_Braila_New$Lav_exp)
summary(BR_lav_lm)
BR_lav_kst <- exp(signif(BR_lav_lm$coef[[1]], 4))
BR_lav_chi <- signif(BR_lav_lm$coef[[2]], 4)
BR_pred_lav_lm <- exp(predict(BR_lav_lm, newdata = RO_Census_Braila_New, 
                              interval = "prediction", level = 0.95))
BR_lav_whtest <- white(BR_lav_lm, interactions = TRUE)
BR_lav_bptest <- bptest(BR_lav_lm, studentize = TRUE)
BR_lav_DWtest <- dwtest(BR_lav_lm)
BR_lav_bgtest <- bgtest(BR_lav_lm, order = 1)
BR_lav_jbtest <- jarque.bera.test(resid(BR_lav_lm))
BR_lav_shtest <- shapiro.test(resid(BR_lav_lm))

# Data Distributions
RO_Census_Braila_New %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila_New$Rank, 
                              Population = BR_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 9000,
            label = paste0("y", "==", signif(exp(BR_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BR_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 30, y = 9000,
            label = paste0("R^2 ==", signif(summary(BR_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila_New$Rank, 
                              Population = BR_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 8000,
            label = paste0("y", "==", signif(exp(BR_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BR_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 30, y = 8000,
            label = paste0("R^2 ==", signif(summary(BR_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila_New$Rank, 
                              Population = BR_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 7000,
            label = paste0("y", "==", signif(BR_ZM_cst, 4), "%.%", "(",
                           signif(BR_ZM_m, 4), "+ x)^", signif(BR_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 30, y = 7000,
            label = paste0("R^2 ==", signif(summary(BR_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila_New$Rank, 
                              Population = BR_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 6000,
            label = paste0("y", "==", signif(BR_lav_kst, 4), "%.%", "x^",
                           -signif(BR_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 30, y = 6000,
            label = paste0("R^2 ==", signif(summary(BR_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_y_continuous(labels = scales::label_number(
    big.mark = ",", decimal.mark = ".")) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population", color = "Legend") +
  #ggtitle("Data Distribution on Braila judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Braila_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Braila judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Braila_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Braila judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Braila_New %>%
     ggplot(aes(x = log(Rank + BR_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Braila judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Braila_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Braila judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 9. Brasov
BV_out <- get_outliers(Brasov_Outliers, RO_Census_Brasov)

RO_Census_Brasov_New <- RO_Census_df %>% inner_join(
  RO_Census_Brasov %>% anti_join(
    bind_rows(
      (BV_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (BV_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BV_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BV_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Brasov_New <- RO_Census_Brasov_New %>% 
  left_join(RO_Census_Brasov_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BV_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Brasov_New)
summary(BV_pw_lm)
BV_pred_pw_lm <- exp(predict(BV_pw_lm, newdata = RO_Census_Brasov_New, 
                             interval = "prediction", level = 0.95))
BV_pw_whtest <- white(BV_pw_lm, interactions = TRUE)
BV_pw_bptest <- bptest(BV_pw_lm, studentize = TRUE)
BV_pw_DWtest <- dwtest(BV_pw_lm)
BV_pw_bgtest <- bgtest(BV_pw_lm, order = 1)
BV_pw_jbtest <- jarque.bera.test(resid(BV_pw_lm))
BV_pw_shtest <- shapiro.test(resid(BV_pw_lm))

# Zipf_Mandelbrot Law
BV_ZM_prm <- get_ZM_Param("RO_Census_Brasov_New", "Population", "Rank")
BV_ZM_m <- BV_ZM_prm$m
BV_ZM_alpha <- BV_ZM_prm$alpha_ZM
BV_ZM_cst <- BV_ZM_prm$constant_ZM
BV_ZM_lm <- lm(log(RO_Census_Brasov_New$Population) ~ log(RO_Census_Brasov_New$Rank + BV_ZM_m))
summary(BV_ZM_lm)
BV_pred_ZM_lm <- exp(predict(BV_ZM_lm, newdata = RO_Census_Brasov_New, 
                             interval = "prediction", level = 0.95))
BV_ZM_whtest <- white(BV_ZM_lm, interactions = TRUE)
BV_ZM_bptest <- bptest(BV_ZM_lm, studentize = TRUE)
BV_ZM_DWtest <- dwtest(BV_ZM_lm)
BV_ZM_bgtest <- bgtest(BV_ZM_lm, order = 1)
BV_ZM_jbtest <- jarque.bera.test(resid(BV_ZM_lm))
BV_ZM_shtest <- shapiro.test(resid(BV_ZM_lm))

# Exponential Law
BV_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Brasov_New)
summary(BV_exp_lm)
BV_pred_exp_lm <- exp(predict(BV_exp_lm, newdata = RO_Census_Brasov_New, 
                              interval = "prediction", level = 0.95))
BV_exp_whtest <- white(BV_exp_lm, interactions = TRUE)
BV_exp_bptest <- bptest(BV_exp_lm, studentize = TRUE)
BV_exp_DWtest <- dwtest(BV_exp_lm)
BV_exp_bgtest <- bgtest(BV_exp_lm, order = 1)
BV_exp_jbtest <- jarque.bera.test(resid(BV_exp_lm))
BV_exp_shtest <- shapiro.test(resid(BV_exp_lm))

# Lavalette Function
RO_Census_Brasov_New <- RO_Census_Brasov_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Brasov_New) - Rank + 1))
)
BV_lav_lm <- lm(log(RO_Census_Brasov_New$Population) ~ RO_Census_Brasov_New$Lav_exp)
summary(BV_lav_lm)
BV_lav_kst <- exp(signif(BV_lav_lm$coef[[1]], 4))
BV_lav_chi <- signif(BV_lav_lm$coef[[2]], 4)
BV_pred_lav_lm <- exp(predict(BV_lav_lm, newdata = RO_Census_Brasov_New, 
                              interval = "prediction", level = 0.95))
BV_lav_whtest <- white(BV_lav_lm, interactions = TRUE)
BV_lav_bptest <- bptest(BV_lav_lm, studentize = TRUE)
BV_lav_DWtest <- dwtest(BV_lav_lm)
BV_lav_bgtest <- bgtest(BV_lav_lm, order = 1)
BV_lav_jbtest <- jarque.bera.test(resid(BV_lav_lm))
BV_lav_shtest <- shapiro.test(resid(BV_lav_lm))

# Data Distributions
RO_Census_Brasov_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Brasov_New$Rank, 
                              Pop_Freq = BV_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.15,
            label = paste0("y", "==", signif(exp(BV_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BV_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.15,
            label = paste0("R^2 ==", signif(summary(BV_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Brasov_New$Rank, 
                              Pop_Freq = BV_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.13,
            label = paste0("y", "==", signif(exp(BV_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BV_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.13,
            label = paste0("R^2 ==", signif(summary(BV_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Brasov_New$Rank, 
                              Pop_Freq = BV_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.11,
            label = paste0("y", "==", signif(BV_ZM_cst, 4), "%.%", "(",
                           signif(BV_ZM_m, 4), "+ x)^", signif(BV_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.11,
            label = paste0("R^2 ==", signif(summary(BV_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Brasov_New$Rank, 
                              Pop_Freq = BV_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.09,
            label = paste0("y", "==", signif(BV_lav_kst, 4), "%.%", "x^",
                           -signif(BV_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.09,
            label = paste0("R^2 ==", signif(summary(BV_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Brasov judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Brasov_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Brasov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Brasov_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Brasov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Brasov_New %>%
     ggplot(aes(x = log(Rank + BV_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Brasov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Brasov_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Brasov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 10. Buzau
BZ_out <- get_outliers(Buzau_Outliers, RO_Census_Buzau)

RO_Census_Buzau_New <- RO_Census_df %>% inner_join(
  RO_Census_Buzau %>% anti_join(
    bind_rows(
      (BZ_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (BZ_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BZ_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (BZ_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Buzau_New <- RO_Census_Buzau_New %>% 
  left_join(RO_Census_Buzau_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BZ_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Buzau_New)
summary(BZ_pw_lm)
BZ_pred_pw_lm <- exp(predict(BZ_pw_lm, newdata = RO_Census_Buzau_New, 
                             interval = "prediction", level = 0.95))
BZ_pw_whtest <- white(BZ_pw_lm, interactions = TRUE)
BZ_pw_bptest <- bptest(BZ_pw_lm, studentize = TRUE)
BZ_pw_DWtest <- dwtest(BZ_pw_lm)
BZ_pw_bgtest <- bgtest(BZ_pw_lm, order = 1)
BZ_pw_jbtest <- jarque.bera.test(resid(BZ_pw_lm))
BZ_pw_shtest <- shapiro.test(resid(BZ_pw_lm))

# Zipf_Mandelbrot Law
BZ_ZM_prm <- get_ZM_Param("RO_Census_Buzau_New", "Population", "Rank")
BZ_ZM_m <- BZ_ZM_prm$m
BZ_ZM_alpha <- BZ_ZM_prm$alpha_ZM
BZ_ZM_cst <- BZ_ZM_prm$constant_ZM
BZ_ZM_lm <- lm(log(RO_Census_Buzau_New$Population) ~ log(RO_Census_Buzau_New$Rank + BZ_ZM_m))
summary(BZ_ZM_lm)
BZ_pred_ZM_lm <- exp(predict(BZ_ZM_lm, newdata = RO_Census_Buzau_New, 
                             interval = "prediction", level = 0.95))
BZ_ZM_whtest <- white(BZ_ZM_lm, interactions = TRUE)
BZ_ZM_bptest <- bptest(BZ_ZM_lm, studentize = TRUE)
BZ_ZM_DWtest <- dwtest(BZ_ZM_lm)
BZ_ZM_bgtest <- bgtest(BZ_ZM_lm, order = 1)
BZ_ZM_jbtest <- jarque.bera.test(resid(BZ_ZM_lm))
BZ_ZM_shtest <- shapiro.test(resid(BZ_ZM_lm))

# Exponential Law
BZ_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Buzau_New)
summary(BZ_exp_lm)
BZ_pred_exp_lm <- exp(predict(BZ_exp_lm, newdata = RO_Census_Buzau_New, 
                              interval = "prediction", level = 0.95))
BZ_exp_whtest <- white(BZ_exp_lm, interactions = TRUE)
BZ_exp_bptest <- bptest(BZ_exp_lm, studentize = TRUE)
BZ_exp_DWtest <- dwtest(BZ_exp_lm)
BZ_exp_bgtest <- bgtest(BZ_exp_lm, order = 1)
BZ_exp_jbtest <- jarque.bera.test(resid(BZ_exp_lm))
BZ_exp_shtest <- shapiro.test(resid(BZ_exp_lm))

# Lavalette Function
RO_Census_Buzau_New <- RO_Census_Buzau_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Buzau_New) - Rank + 1))
)
BZ_lav_lm <- lm(log(RO_Census_Buzau_New$Population) ~ RO_Census_Buzau_New$Lav_exp)
summary(BZ_lav_lm)
BZ_lav_kst <- exp(signif(BZ_lav_lm$coef[[1]], 4))
BZ_lav_chi <- signif(BZ_lav_lm$coef[[2]], 4)
BZ_pred_lav_lm <- exp(predict(BZ_lav_lm, newdata = RO_Census_Buzau_New, 
                              interval = "prediction", level = 0.95))
BZ_lav_whtest <- white(BZ_lav_lm, interactions = TRUE)
BZ_lav_bptest <- bptest(BZ_lav_lm, studentize = TRUE)
BZ_lav_DWtest <- dwtest(BZ_lav_lm)
BZ_lav_bgtest <- bgtest(BZ_lav_lm, order = 1)
BZ_lav_jbtest <- jarque.bera.test(resid(BZ_lav_lm))
BZ_lav_shtest <- shapiro.test(resid(BZ_lav_lm))

# Data Distributions
RO_Census_Buzau_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Buzau_New$Rank, 
                              Pop_Freq = BZ_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.08,
            label = paste0("y", "==", signif(exp(BZ_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BZ_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.08,
            label = paste0("R^2 ==", signif(summary(BZ_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Buzau_New$Rank, 
                              Pop_Freq = BZ_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.07,
            label = paste0("y", "==", signif(exp(BZ_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BZ_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.07,
            label = paste0("R^2 ==", signif(summary(BZ_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Buzau_New$Rank, 
                              Pop_Freq = BZ_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.06,
            label = paste0("y", "==", signif(BZ_ZM_cst, 4), "%.%", "(",
                           signif(BZ_ZM_m, 4), "+ x)^", signif(BZ_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.06,
            label = paste0("R^2 ==", signif(summary(BZ_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Buzau_New$Rank, 
                              Pop_Freq = BZ_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.05,
            label = paste0("y", "==", signif(BZ_lav_kst, 4), "%.%", "x^",
                           -signif(BZ_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.05,
            label = paste0("R^2 ==", signif(summary(BZ_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Buzau judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Buzau_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Buzau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Buzau_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Buzau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Buzau_New %>%
     ggplot(aes(x = log(Rank + BZ_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Buzau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Buzau_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Buzau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 11. Caras-Severin
CS_out <- get_outliers(CarSev_Outliers, RO_Census_CarSev)

RO_Census_CarSev_New <- RO_Census_df %>% inner_join(
  RO_Census_CarSev %>% anti_join(
    bind_rows(
      (CS_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (CS_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (CS_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (CS_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_CarSev_New <- RO_Census_CarSev_New %>% 
  left_join(RO_Census_CarSev_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
CS_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_CarSev_New)
summary(CS_pw_lm)
CS_pred_pw_lm <- exp(predict(CS_pw_lm, newdata = RO_Census_CarSev_New, 
                             interval = "prediction", level = 0.95))
CS_pw_whtest <- white(CS_pw_lm, interactions = TRUE)
CS_pw_bptest <- bptest(CS_pw_lm, studentize = TRUE)
CS_pw_DWtest <- dwtest(CS_pw_lm)
CS_pw_bgtest <- bgtest(CS_pw_lm, order = 1)
CS_pw_jbtest <- jarque.bera.test(resid(CS_pw_lm))
CS_pw_shtest <- shapiro.test(resid(CS_pw_lm))

# Zipf_Mandelbrot Law
CS_ZM_prm <- get_ZM_Param("RO_Census_CarSev_New", "Population", "Rank")
CS_ZM_m <- CS_ZM_prm$m
CS_ZM_alpha <- CS_ZM_prm$alpha_ZM
CS_ZM_cst <- CS_ZM_prm$constant_ZM
CS_ZM_lm <- lm(log(RO_Census_CarSev_New$Population) ~ log(RO_Census_CarSev_New$Rank + CS_ZM_m))
summary(CS_ZM_lm)
CS_pred_ZM_lm <- exp(predict(CS_ZM_lm, newdata = RO_Census_CarSev_New, 
                             interval = "prediction", level = 0.95))
CS_ZM_whtest <- white(CS_ZM_lm, interactions = TRUE)
CS_ZM_bptest <- bptest(CS_ZM_lm, studentize = TRUE)
CS_ZM_DWtest <- dwtest(CS_ZM_lm)
CS_ZM_bgtest <- bgtest(CS_ZM_lm, order = 1)
CS_ZM_jbtest <- jarque.bera.test(resid(CS_ZM_lm))
CS_ZM_shtest <- shapiro.test(resid(CS_ZM_lm))

# Exponential Law
CS_exp_lm <- lm(log(Population) ~ Rank, RO_Census_CarSev_New)
summary(CS_exp_lm)
CS_pred_exp_lm <- exp(predict(CS_exp_lm, newdata = RO_Census_CarSev_New, 
                              interval = "prediction", level = 0.95))
CS_exp_whtest <- white(CS_exp_lm, interactions = TRUE)
CS_exp_bptest <- bptest(CS_exp_lm, studentize = TRUE)
CS_exp_DWtest <- dwtest(CS_exp_lm)
CS_exp_bgtest <- bgtest(CS_exp_lm, order = 1)
CS_exp_jbtest <- jarque.bera.test(resid(CS_exp_lm))
CS_exp_shtest <- shapiro.test(resid(CS_exp_lm))

# Lavalette Function
RO_Census_CarSev_New <- RO_Census_CarSev_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_CarSev_New) - Rank + 1))
)
CS_lav_lm <- lm(log(RO_Census_CarSev_New$Population) ~ RO_Census_CarSev_New$Lav_exp)
summary(CS_lav_lm)
CS_lav_kst <- exp(signif(CS_lav_lm$coef[[1]], 4))
CS_lav_chi <- signif(CS_lav_lm$coef[[2]], 4)
CS_pred_lav_lm <- exp(predict(CS_lav_lm, newdata = RO_Census_CarSev_New, 
                              interval = "prediction", level = 0.95))
CS_lav_whtest <- white(CS_lav_lm, interactions = TRUE)
CS_lav_bptest <- bptest(CS_lav_lm, studentize = TRUE)
CS_lav_DWtest <- dwtest(CS_lav_lm)
CS_lav_bgtest <- bgtest(CS_lav_lm, order = 1)
CS_lav_jbtest <- jarque.bera.test(resid(CS_lav_lm))
CS_lav_shtest <- shapiro.test(resid(CS_lav_lm))

# Data Distributions
RO_Census_CarSev_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_CarSev_New$Rank, 
                              Pop_Freq = CS_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.08,
            label = paste0("y", "==", signif(exp(CS_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(CS_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 45, y = 0.08,
            label = paste0("R^2 ==", signif(summary(CS_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_CarSev_New$Rank, 
                              Pop_Freq = CS_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.07,
            label = paste0("y", "==", signif(exp(CS_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(CS_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 45, y = 0.07,
            label = paste0("R^2 ==", signif(summary(CS_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_CarSev_New$Rank, 
                              Pop_Freq = CS_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.06,
            label = paste0("y", "==", signif(CS_ZM_cst, 4), "%.%", "(",
                           signif(CS_ZM_m, 4), "+ x)^", signif(CS_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 45, y = 0.06,
            label = paste0("R^2 ==", signif(summary(CS_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_CarSev_New$Rank, 
                              Pop_Freq = CS_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.05,
            label = paste0("y", "==", signif(CS_lav_kst, 4), "%.%", "x^",
                           -signif(CS_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 45, y = 0.05,
            label = paste0("R^2 ==", signif(summary(CS_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Caras-Severin judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_CarSev_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Caras-Severin judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_CarSev_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Caras-Severin judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_CarSev_New %>%
     ggplot(aes(x = log(Rank + CS_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Caras-Severin judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_CarSev_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Caras-Severin judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 12. Calarasi
CL_out <- get_outliers(Calarasi_Outliers, RO_Census_Calarasi)

RO_Census_Calarasi_New <- RO_Census_df %>% inner_join(
  RO_Census_Calarasi %>% anti_join(
    bind_rows(
      (CL_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (CL_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (CL_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (CL_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Calarasi_New <- RO_Census_Calarasi_New %>% 
  left_join(RO_Census_Calarasi_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
CL_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Calarasi_New)
summary(CL_pw_lm)
CL_pred_pw_lm <- exp(predict(CL_pw_lm, newdata = RO_Census_Calarasi_New, 
                             interval = "prediction", level = 0.95))
CL_pw_whtest <- white(CL_pw_lm, interactions = TRUE)
CL_pw_bptest <- bptest(CL_pw_lm, studentize = TRUE)
CL_pw_DWtest <- dwtest(CL_pw_lm)
CL_pw_bgtest <- bgtest(CL_pw_lm, order = 1)
CL_pw_jbtest <- jarque.bera.test(resid(CL_pw_lm))
CL_pw_shtest <- shapiro.test(resid(CL_pw_lm))

# Zipf_Mandelbrot Law
CL_ZM_prm <- get_ZM_Param("RO_Census_Calarasi_New", "Population", "Rank")
CL_ZM_m <- CL_ZM_prm$m
CL_ZM_alpha <- CL_ZM_prm$alpha_ZM
CL_ZM_cst <- CL_ZM_prm$constant_ZM
CL_ZM_lm <- lm(log(RO_Census_Calarasi_New$Population) ~ log(RO_Census_Calarasi_New$Rank + CL_ZM_m))
summary(CL_ZM_lm)
CL_pred_ZM_lm <- exp(predict(CL_ZM_lm, newdata = RO_Census_Calarasi_New, 
                             interval = "prediction", level = 0.95))
CL_ZM_whtest <- white(CL_ZM_lm, interactions = TRUE)
CL_ZM_bptest <- bptest(CL_ZM_lm, studentize = TRUE)
CL_ZM_DWtest <- dwtest(CL_ZM_lm)
CL_ZM_bgtest <- bgtest(CL_ZM_lm, order = 1)
CL_ZM_jbtest <- jarque.bera.test(resid(CL_ZM_lm))
CL_ZM_shtest <- shapiro.test(resid(CL_ZM_lm))

# Exponential Law
CL_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Calarasi_New)
summary(CL_exp_lm)
CL_pred_exp_lm <- exp(predict(CL_exp_lm, newdata = RO_Census_Calarasi_New, 
                              interval = "prediction", level = 0.95))
CL_exp_whtest <- white(CL_exp_lm, interactions = TRUE)
CL_exp_bptest <- bptest(CL_exp_lm, studentize = TRUE)
CL_exp_DWtest <- dwtest(CL_exp_lm)
CL_exp_bgtest <- bgtest(CL_exp_lm, order = 1)
CL_exp_jbtest <- jarque.bera.test(resid(CL_exp_lm))
CL_exp_shtest <- shapiro.test(resid(CL_exp_lm))

# Lavalette Function
RO_Census_Calarasi_New <- RO_Census_Calarasi_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Calarasi_New) - Rank + 1))
)
CL_lav_lm <- lm(log(RO_Census_Calarasi_New$Population) ~ RO_Census_Calarasi_New$Lav_exp)
summary(CL_lav_lm)
CL_lav_kst <- exp(signif(CL_lav_lm$coef[[1]], 4))
CL_lav_chi <- signif(CL_lav_lm$coef[[2]], 4)
CL_pred_lav_lm <- exp(predict(CL_lav_lm, newdata = RO_Census_Calarasi_New, 
                              interval = "prediction", level = 0.95))
CL_lav_whtest <- white(CL_lav_lm, interactions = TRUE)
CL_lav_bptest <- bptest(CL_lav_lm, studentize = TRUE)
CL_lav_DWtest <- dwtest(CL_lav_lm)
CL_lav_bgtest <- bgtest(CL_lav_lm, order = 1)
CL_lav_jbtest <- jarque.bera.test(resid(CL_lav_lm))
CL_lav_shtest <- shapiro.test(resid(CL_lav_lm))

# Data Distributions
RO_Census_Calarasi_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Calarasi_New$Rank, 
                              Pop_Freq = CL_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(exp(CL_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(CL_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 30, y = 0.08,
            label = paste0("R^2 ==", signif(summary(CL_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Calarasi_New$Rank, 
                              Pop_Freq = CL_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(CL_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(CL_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 30, y = 0.07,
            label = paste0("R^2 ==", signif(summary(CL_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Calarasi_New$Rank, 
                              Pop_Freq = CL_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(CL_ZM_cst, 4), "%.%", "(",
                           signif(CL_ZM_m, 4), "+ x)^", signif(CL_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 30, y = 0.06,
            label = paste0("R^2 ==", signif(summary(CL_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Calarasi_New$Rank, 
                              Pop_Freq = CL_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(CL_lav_kst, 4), "%.%", "x^",
                           -signif(CL_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 30, y = 0.05,
            label = paste0("R^2 ==", signif(summary(CL_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Calarasi judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Calarasi_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Calarasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Calarasi_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Calarasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Calarasi_New %>%
     ggplot(aes(x = log(Rank + CL_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Calarasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Calarasi_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Calarasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 13. Cluj

# RO_Census_Cluj_New <- RO_Census_df %>% filter(Judet_Name == "CLUJ") %>%
#   inner_join(
#     RO_Census_Cluj %>% mutate(LOF = lofactor(Pop_Freq, k = 7)) %>%
#       filter(round(LOF, 2) < 1.5) %>% dplyr::select(Judet_Name, Unit_Type, Unit_Name),
#     by = c("Judet_Name", "Unit_Type", "Unit_Name")) %>%
#   left_join((RO_Census_df %>% filter(Judet_Name == "CLUJ") %>%
#                inner_join(
#                  RO_Census_Cluj %>% mutate(LOF = lofactor(Pop_Freq, k = 7)) %>%
#                    filter(round(LOF, 2) < 1.5) %>%
#                    dplyr::select(Judet_Name, Unit_Type, Unit_Name),
#                  by = c("Judet_Name", "Unit_Type", "Unit_Name"))) %>%
#               group_by(Judet_Name) %>%
#               summarize(Total_Pop = sum(Population))) %>%
#   arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>%
#   mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
#   ungroup()

CJ_out <- get_outliers(Cluj_Outliers, RO_Census_Cluj)

RO_Census_Cluj_New <- RO_Census_df %>% inner_join(
  RO_Census_Cluj %>% anti_join(
    bind_rows(
      (CJ_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (CJ_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (CJ_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (CJ_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Cluj_New <- RO_Census_Cluj_New %>% 
  left_join(RO_Census_Cluj_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
CJ_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Cluj_New)
summary(CJ_pw_lm)
CJ_pred_pw_lm <- exp(predict(CJ_pw_lm, newdata = RO_Census_Cluj_New, 
                             interval = "prediction", level = 0.95))
CJ_pw_whtest <- white(CJ_pw_lm, interactions = TRUE)
CJ_pw_bptest <- bptest(CJ_pw_lm, studentize = TRUE)
CJ_pw_DWtest <- dwtest(CJ_pw_lm)
CJ_pw_bgtest <- bgtest(CJ_pw_lm, order = 1)
CJ_pw_jbtest <- jarque.bera.test(resid(CJ_pw_lm))
CJ_pw_shtest <- shapiro.test(resid(CJ_pw_lm))

# Zipf_Mandelbrot Law
CJ_ZM_prm <- get_ZM_Param("RO_Census_Cluj_New", "Population", "Rank")
CJ_ZM_m <- CJ_ZM_prm$m
CJ_ZM_alpha <- CJ_ZM_prm$alpha_ZM
CJ_ZM_cst <- CJ_ZM_prm$constant_ZM
CJ_ZM_lm <- lm(log(RO_Census_Cluj_New$Population) ~ log(RO_Census_Cluj_New$Rank + CJ_ZM_m))
summary(CJ_ZM_lm)
CJ_pred_ZM_lm <- exp(predict(CJ_ZM_lm, newdata = RO_Census_Cluj_New, 
                             interval = "prediction", level = 0.95))
CJ_ZM_whtest <- white(CJ_ZM_lm, interactions = TRUE)
CJ_ZM_bptest <- bptest(CJ_ZM_lm, studentize = TRUE)
CJ_ZM_DWtest <- dwtest(CJ_ZM_lm)
CJ_ZM_bgtest <- bgtest(CJ_ZM_lm, order = 1)
CJ_ZM_jbtest <- jarque.bera.test(resid(CJ_ZM_lm))
CJ_ZM_shtest <- shapiro.test(resid(CJ_ZM_lm))

# Exponential Law
CJ_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Cluj_New)
summary(CJ_exp_lm)
CJ_pred_exp_lm <- exp(predict(CJ_exp_lm, newdata = RO_Census_Cluj_New, 
                              interval = "prediction", level = 0.95))
CJ_exp_whtest <- white(CJ_exp_lm, interactions = TRUE)
CJ_exp_bptest <- bptest(CJ_exp_lm, studentize = TRUE)
CJ_exp_DWtest <- dwtest(CJ_exp_lm)
CJ_exp_bgtest <- bgtest(CJ_exp_lm, order = 1)
CJ_exp_jbtest <- jarque.bera.test(resid(CJ_exp_lm))
CJ_exp_shtest <- shapiro.test(resid(CJ_exp_lm))

# Lavalette Function
RO_Census_Cluj_New <- RO_Census_Cluj_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Cluj_New) - Rank + 1))
)
CJ_lav_lm <- lm(log(RO_Census_Cluj_New$Population) ~ RO_Census_Cluj_New$Lav_exp)
summary(CJ_lav_lm)
CJ_lav_kst <- exp(signif(CJ_lav_lm$coef[[1]], 4))
CJ_lav_chi <- signif(CJ_lav_lm$coef[[2]], 4)
CJ_pred_lav_lm <- exp(predict(CJ_lav_lm, newdata = RO_Census_Cluj_New, 
                              interval = "prediction", level = 0.95))
CJ_lav_whtest <- white(CJ_lav_lm, interactions = TRUE)
CJ_lav_bptest <- bptest(CJ_lav_lm, studentize = TRUE)
CJ_lav_DWtest <- dwtest(CJ_lav_lm)
CJ_lav_bgtest <- bgtest(CJ_lav_lm, order = 1)
CJ_lav_jbtest <- jarque.bera.test(resid(CJ_lav_lm))
CJ_lav_shtest <- shapiro.test(resid(CJ_lav_lm))

# Data Distributions
RO_Census_Cluj_New %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj_New$Rank, 
                              Population = CJ_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 30, y = 45000,
            label = paste0("y", "==", signif(exp(CJ_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(CJ_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 60, y = 45000,
            label = paste0("R^2 ==", signif(summary(CJ_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj_New$Rank, 
                              Population = CJ_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 30, y = 40000,
            label = paste0("y", "==", signif(exp(CJ_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(CJ_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 60, y = 40000,
            label = paste0("R^2 ==", signif(summary(CJ_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj_New$Rank, 
                              Population = CJ_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 30, y = 35000,
            label = paste0("y", "==", signif(CJ_ZM_cst, 4), "%.%", "(",
                           signif(CJ_ZM_m, 4), "+ x)^", signif(CJ_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 60, y = 35000,
            label = paste0("R^2 ==", signif(summary(CJ_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj_New$Rank, 
                              Population = CJ_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 30, y = 30000,
            label = paste0("y", "==", signif(CJ_lav_kst, 4), "%.%", "x^",
                           -signif(CJ_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 60, y = 30000,
            label = paste0("R^2 ==", signif(summary(CJ_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_y_continuous(labels = scales::label_number(
    big.mark = ",", decimal.mark = ".")) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population", color = "Legend") +
  #ggtitle("Data Distribution on Cluj judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Cluj_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Cluj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Cluj_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Cluj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Cluj_New %>%
     ggplot(aes(x = log(Rank + CJ_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Cluj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Cluj_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Cluj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 14. Constanta
CT_out <- get_outliers(Constanta_Outliers, RO_Census_Constanta)

RO_Census_Constanta_New <- RO_Census_df %>% inner_join(
  RO_Census_Constanta %>% anti_join(
    bind_rows(
      (CT_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (CT_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (CT_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (CT_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Constanta_New <- RO_Census_Constanta_New %>% 
  left_join(RO_Census_Constanta_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
CT_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Constanta_New)
summary(CT_pw_lm)
CT_pred_pw_lm <- exp(predict(CT_pw_lm, newdata = RO_Census_Constanta_New, 
                             interval = "prediction", level = 0.95))
CT_pw_whtest <- white(CT_pw_lm, interactions = TRUE)
CT_pw_bptest <- bptest(CT_pw_lm, studentize = TRUE)
CT_pw_DWtest <- dwtest(CT_pw_lm)
CT_pw_bgtest <- bgtest(CT_pw_lm, order = 1)
CT_pw_jbtest <- jarque.bera.test(resid(CT_pw_lm))
CT_pw_shtest <- shapiro.test(resid(CT_pw_lm))

# Zipf_Mandelbrot Law
CT_ZM_prm <- get_ZM_Param("RO_Census_Constanta_New", "Population", "Rank")
CT_ZM_m <- CT_ZM_prm$m
CT_ZM_alpha <- CT_ZM_prm$alpha_ZM
CT_ZM_cst <- CT_ZM_prm$constant_ZM
CT_ZM_lm <- lm(log(RO_Census_Constanta_New$Population) ~ log(RO_Census_Constanta_New$Rank + CT_ZM_m))
summary(CT_ZM_lm)
CT_pred_ZM_lm <- exp(predict(CT_ZM_lm, newdata = RO_Census_Constanta_New, 
                             interval = "prediction", level = 0.95))
CT_ZM_whtest <- white(CT_ZM_lm, interactions = TRUE)
CT_ZM_bptest <- bptest(CT_ZM_lm, studentize = TRUE)
CT_ZM_DWtest <- dwtest(CT_ZM_lm)
CT_ZM_bgtest <- bgtest(CT_ZM_lm, order = 1)
CT_ZM_jbtest <- jarque.bera.test(resid(CT_ZM_lm))
CT_ZM_shtest <- shapiro.test(resid(CT_ZM_lm))

# Exponential Law
CT_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Constanta_New)
summary(CT_exp_lm)
CT_pred_exp_lm <- exp(predict(CT_exp_lm, newdata = RO_Census_Constanta_New, 
                              interval = "prediction", level = 0.95))
CT_exp_whtest <- white(CT_exp_lm, interactions = TRUE)
CT_exp_bptest <- bptest(CT_exp_lm, studentize = TRUE)
CT_exp_DWtest <- dwtest(CT_exp_lm)
CT_exp_bgtest <- bgtest(CT_exp_lm, order = 1)
CT_exp_jbtest <- jarque.bera.test(resid(CT_exp_lm))
CT_exp_shtest <- shapiro.test(resid(CT_exp_lm))

# Lavalette Function
RO_Census_Constanta_New <- RO_Census_Constanta_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Constanta_New) - Rank + 1))
)
CT_lav_lm <- lm(log(RO_Census_Constanta_New$Population) ~ RO_Census_Constanta_New$Lav_exp)
summary(CT_lav_lm)
CT_lav_kst <- exp(signif(CT_lav_lm$coef[[1]], 4))
CT_lav_chi <- signif(CT_lav_lm$coef[[2]], 4)
CT_pred_lav_lm <- exp(predict(CT_lav_lm, newdata = RO_Census_Constanta_New, 
                              interval = "prediction", level = 0.95))
CT_lav_whtest <- white(CT_lav_lm, interactions = TRUE)
CT_lav_bptest <- bptest(CT_lav_lm, studentize = TRUE)
CT_lav_DWtest <- dwtest(CT_lav_lm)
CT_lav_bgtest <- bgtest(CT_lav_lm, order = 1)
CT_lav_jbtest <- jarque.bera.test(resid(CT_lav_lm))
CT_lav_shtest <- shapiro.test(resid(CT_lav_lm))

# Data Distributions
RO_Census_Constanta_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Constanta_New$Rank, 
                              Pop_Freq = CT_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.11,
            label = paste0("y", "==", signif(exp(CT_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(CT_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.11,
            label = paste0("R^2 ==", signif(summary(CT_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Constanta_New$Rank, 
                              Pop_Freq = CT_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.10,
            label = paste0("y", "==", signif(exp(CT_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(CT_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.10,
            label = paste0("R^2 ==", signif(summary(CT_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Constanta_New$Rank, 
                              Pop_Freq = CT_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.09,
            label = paste0("y", "==", signif(CT_ZM_cst, 4), "%.%", "(",
                           signif(CT_ZM_m, 4), "+ x)^", signif(CT_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.09,
            label = paste0("R^2 ==", signif(summary(CT_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Constanta_New$Rank, 
                              Pop_Freq = CT_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(CT_lav_kst, 4), "%.%", "x^",
                           -signif(CT_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.08,
            label = paste0("R^2 ==", signif(summary(CT_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Constanta judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Constanta_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Constanta judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Constanta_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Constanta judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Constanta_New %>%
     ggplot(aes(x = log(Rank + CT_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Constanta judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Constanta_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Constanta judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 15. Covasna
CV_out <- get_outliers(Covasna_Outliers, RO_Census_Covasna)

RO_Census_Covasna_New <- RO_Census_df %>% inner_join(
  RO_Census_Covasna %>% anti_join(
    bind_rows(
      (CV_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (CV_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (CV_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (CV_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Covasna_New <- RO_Census_Covasna_New %>% 
  left_join(RO_Census_Covasna_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
CV_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Covasna_New)
summary(CV_pw_lm)
CV_pred_pw_lm <- exp(predict(CV_pw_lm, newdata = RO_Census_Covasna_New, 
                             interval = "prediction", level = 0.95))
CV_pw_whtest <- white(CV_pw_lm, interactions = TRUE)
CV_pw_bptest <- bptest(CV_pw_lm, studentize = TRUE)
CV_pw_DWtest <- dwtest(CV_pw_lm)
CV_pw_bgtest <- bgtest(CV_pw_lm, order = 1)
CV_pw_jbtest <- jarque.bera.test(resid(CV_pw_lm))
CV_pw_shtest <- shapiro.test(resid(CV_pw_lm))

# Zipf_Mandelbrot Law
CV_ZM_prm <- get_ZM_Param("RO_Census_Covasna_New", "Population", "Rank")
CV_ZM_m <- CV_ZM_prm$m
CV_ZM_alpha <- CV_ZM_prm$alpha_ZM
CV_ZM_cst <- CV_ZM_prm$constant_ZM
CV_ZM_lm <- lm(log(RO_Census_Covasna_New$Population) ~ log(RO_Census_Covasna_New$Rank + CV_ZM_m))
summary(CV_ZM_lm)
CV_pred_ZM_lm <- exp(predict(CV_ZM_lm, newdata = RO_Census_Covasna_New, 
                             interval = "prediction", level = 0.95))
CV_ZM_whtest <- white(CV_ZM_lm, interactions = TRUE)
CV_ZM_bptest <- bptest(CV_ZM_lm, studentize = TRUE)
CV_ZM_DWtest <- dwtest(CV_ZM_lm)
CV_ZM_bgtest <- bgtest(CV_ZM_lm, order = 1)
CV_ZM_jbtest <- jarque.bera.test(resid(CV_ZM_lm))
CV_ZM_shtest <- shapiro.test(resid(CV_ZM_lm))

# Exponential Law
CV_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Covasna_New)
summary(CV_exp_lm)
CV_pred_exp_lm <- exp(predict(CV_exp_lm, newdata = RO_Census_Covasna_New, 
                              interval = "prediction", level = 0.95))
CV_exp_whtest <- white(CV_exp_lm, interactions = TRUE)
CV_exp_bptest <- bptest(CV_exp_lm, studentize = TRUE)
CV_exp_DWtest <- dwtest(CV_exp_lm)
CV_exp_bgtest <- bgtest(CV_exp_lm, order = 1)
CV_exp_jbtest <- jarque.bera.test(resid(CV_exp_lm))
CV_exp_shtest <- shapiro.test(resid(CV_exp_lm))

# Lavalette Function
RO_Census_Covasna_New <- RO_Census_Covasna_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Covasna_New) - Rank + 1))
)
CV_lav_lm <- lm(log(RO_Census_Covasna_New$Population) ~ RO_Census_Covasna_New$Lav_exp)
summary(CV_lav_lm)
CV_lav_kst <- exp(signif(CV_lav_lm$coef[[1]], 4))
CV_lav_chi <- signif(CV_lav_lm$coef[[2]], 4)
CV_pred_lav_lm <- exp(predict(CV_lav_lm, newdata = RO_Census_Covasna_New, 
                              interval = "prediction", level = 0.95))
CV_lav_whtest <- white(CV_lav_lm, interactions = TRUE)
CV_lav_bptest <- bptest(CV_lav_lm, studentize = TRUE)
CV_lav_DWtest <- dwtest(CV_lav_lm)
CV_lav_bgtest <- bgtest(CV_lav_lm, order = 1)
CV_lav_jbtest <- jarque.bera.test(resid(CV_lav_lm))
CV_lav_shtest <- shapiro.test(resid(CV_lav_lm))

# Data Distributions
RO_Census_Covasna_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Covasna_New$Rank, 
                              Pop_Freq = CV_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(exp(CV_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(CV_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 30, y = 0.08,
            label = paste0("R^2 ==", signif(summary(CV_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Covasna_New$Rank, 
                              Pop_Freq = CV_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(CV_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(CV_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 30, y = 0.07,
            label = paste0("R^2 ==", signif(summary(CV_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Covasna_New$Rank, 
                              Pop_Freq = CV_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(CV_ZM_cst, 4), "%.%", "(",
                           signif(CV_ZM_m, 4), "+ x)^", signif(CV_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 30, y = 0.06,
            label = paste0("R^2 ==", signif(summary(CV_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Covasna_New$Rank, 
                              Pop_Freq = CV_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(CV_lav_kst, 4), "%.%", "x^",
                           -signif(CV_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 30, y = 0.05,
            label = paste0("R^2 ==", signif(summary(CV_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Covasna judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Covasna_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Covasna judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Covasna_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Covasna judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Covasna_New %>%
     ggplot(aes(x = log(Rank + CV_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Covasna judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Covasna_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Covasna judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 16. Dambovita
DB_out <- get_outliers(Dambovita_Outliers, RO_Census_Dambovita)

RO_Census_Dambovita_New <- RO_Census_df %>% inner_join(
  RO_Census_Dambovita %>% anti_join(
    bind_rows(
      (DB_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (DB_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (DB_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (DB_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Dambovita_New <- RO_Census_Dambovita_New %>% 
  left_join(RO_Census_Dambovita_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
DB_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Dambovita_New)
summary(DB_pw_lm)
DB_pred_pw_lm <- exp(predict(DB_pw_lm, newdata = RO_Census_Dambovita_New, 
                             interval = "prediction", level = 0.95))
DB_pw_whtest <- white(DB_pw_lm, interactions = TRUE)
DB_pw_bptest <- bptest(DB_pw_lm, studentize = TRUE)
DB_pw_DWtest <- dwtest(DB_pw_lm)
DB_pw_bgtest <- bgtest(DB_pw_lm, order = 1)
DB_pw_jbtest <- jarque.bera.test(resid(DB_pw_lm))
DB_pw_shtest <- shapiro.test(resid(DB_pw_lm))

# Zipf_Mandelbrot Law
DB_ZM_prm <- get_ZM_Param("RO_Census_Dambovita_New", "Population", "Rank")
DB_ZM_m <- DB_ZM_prm$m
DB_ZM_alpha <- DB_ZM_prm$alpha_ZM
DB_ZM_cst <- DB_ZM_prm$constant_ZM
DB_ZM_lm <- lm(log(RO_Census_Dambovita_New$Population) ~ log(RO_Census_Dambovita_New$Rank + DB_ZM_m))
summary(DB_ZM_lm)
DB_pred_ZM_lm <- exp(predict(DB_ZM_lm, newdata = RO_Census_Dambovita_New, 
                             interval = "prediction", level = 0.95))
DB_ZM_whtest <- white(DB_ZM_lm, interactions = TRUE)
DB_ZM_bptest <- bptest(DB_ZM_lm, studentize = TRUE)
DB_ZM_DWtest <- dwtest(DB_ZM_lm)
DB_ZM_bgtest <- bgtest(DB_ZM_lm, order = 1)
DB_ZM_jbtest <- jarque.bera.test(resid(DB_ZM_lm))
DB_ZM_shtest <- shapiro.test(resid(DB_ZM_lm))

# Exponential Law
DB_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Dambovita_New)
summary(DB_exp_lm)
DB_pred_exp_lm <- exp(predict(DB_exp_lm, newdata = RO_Census_Dambovita_New, 
                              interval = "prediction", level = 0.95))
DB_exp_whtest <- white(DB_exp_lm, interactions = TRUE)
DB_exp_bptest <- bptest(DB_exp_lm, studentize = TRUE)
DB_exp_DWtest <- dwtest(DB_exp_lm)
DB_exp_bgtest <- bgtest(DB_exp_lm, order = 1)
DB_exp_jbtest <- jarque.bera.test(resid(DB_exp_lm))
DB_exp_shtest <- shapiro.test(resid(DB_exp_lm))

# Lavalette Function
RO_Census_Dambovita_New <- RO_Census_Dambovita_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Dambovita_New) - Rank + 1))
)
DB_lav_lm <- lm(log(RO_Census_Dambovita_New$Population) ~ RO_Census_Dambovita_New$Lav_exp)
summary(DB_lav_lm)
DB_lav_kst <- exp(signif(DB_lav_lm$coef[[1]], 4))
DB_lav_chi <- signif(DB_lav_lm$coef[[2]], 4)
DB_pred_lav_lm <- exp(predict(DB_lav_lm, newdata = RO_Census_Dambovita_New, 
                              interval = "prediction", level = 0.95))
DB_lav_whtest <- white(DB_lav_lm, interactions = TRUE)
DB_lav_bptest <- bptest(DB_lav_lm, studentize = TRUE)
DB_lav_DWtest <- dwtest(DB_lav_lm)
DB_lav_bgtest <- bgtest(DB_lav_lm, order = 1)
DB_lav_jbtest <- jarque.bera.test(resid(DB_lav_lm))
DB_lav_shtest <- shapiro.test(resid(DB_lav_lm))

# Data Distributions
RO_Census_Dambovita_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Dambovita_New$Rank, 
                              Pop_Freq = DB_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(exp(DB_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(DB_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(DB_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dambovita_New$Rank, 
                              Pop_Freq = DB_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.045,
            label = paste0("y", "==", signif(exp(DB_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(DB_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.045,
            label = paste0("R^2 ==", signif(summary(DB_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dambovita_New$Rank, 
                              Pop_Freq = DB_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.04,
            label = paste0("y", "==", signif(DB_ZM_cst, 4), "%.%", "(",
                           signif(DB_ZM_m, 4), "+ x)^", signif(DB_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.04,
            label = paste0("R^2 ==", signif(summary(DB_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dambovita_New$Rank, 
                              Pop_Freq = DB_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.035,
            label = paste0("y", "==", signif(DB_lav_kst, 4), "%.%", "x^",
                           -signif(DB_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.035,
            label = paste0("R^2 ==", signif(summary(DB_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Dambovita judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Dambovita_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Dambovita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dambovita_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Dambovita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dambovita_New %>%
     ggplot(aes(x = log(Rank + DB_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Dambovita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dambovita_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Dambovita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 17. Dolj
DJ_out <- get_outliers(Dolj_Outliers, RO_Census_Dolj)

RO_Census_Dolj_New <- RO_Census_df %>% inner_join(
  RO_Census_Dolj %>% anti_join(
    bind_rows(
      (DJ_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (DJ_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (DJ_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (DJ_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Dolj_New <- RO_Census_Dolj_New %>% 
  left_join(RO_Census_Dolj_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
DJ_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Dolj_New)
summary(DJ_pw_lm)
DJ_pred_pw_lm <- exp(predict(DJ_pw_lm, newdata = RO_Census_Dolj_New, 
                             interval = "prediction", level = 0.95))
DJ_pw_whtest <- white(DJ_pw_lm, interactions = TRUE)
DJ_pw_bptest <- bptest(DJ_pw_lm, studentize = TRUE)
DJ_pw_DWtest <- dwtest(DJ_pw_lm)
DJ_pw_bgtest <- bgtest(DJ_pw_lm, order = 1)
DJ_pw_jbtest <- jarque.bera.test(resid(DJ_pw_lm))
DJ_pw_shtest <- shapiro.test(resid(DJ_pw_lm))

# Zipf_Mandelbrot Law
DJ_ZM_prm <- get_ZM_Param("RO_Census_Dolj_New", "Population", "Rank")
DJ_ZM_m <- DJ_ZM_prm$m
DJ_ZM_alpha <- DJ_ZM_prm$alpha_ZM
DJ_ZM_cst <- DJ_ZM_prm$constant_ZM
DJ_ZM_lm <- lm(log(RO_Census_Dolj_New$Population) ~ log(RO_Census_Dolj_New$Rank + DJ_ZM_m))
summary(DJ_ZM_lm)
DJ_pred_ZM_lm <- exp(predict(DJ_ZM_lm, newdata = RO_Census_Dolj_New, 
                             interval = "prediction", level = 0.95))
DJ_ZM_whtest <- white(DJ_ZM_lm, interactions = TRUE)
DJ_ZM_bptest <- bptest(DJ_ZM_lm, studentize = TRUE)
DJ_ZM_DWtest <- dwtest(DJ_ZM_lm)
DJ_ZM_bgtest <- bgtest(DJ_ZM_lm, order = 1)
DJ_ZM_jbtest <- jarque.bera.test(resid(DJ_ZM_lm))
DJ_ZM_shtest <- shapiro.test(resid(DJ_ZM_lm))

# Exponential Law
DJ_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Dolj_New)
summary(DJ_exp_lm)
DJ_pred_exp_lm <- exp(predict(DJ_exp_lm, newdata = RO_Census_Dolj_New, 
                              interval = "prediction", level = 0.95))
DJ_exp_whtest <- white(DJ_exp_lm, interactions = TRUE)
DJ_exp_bptest <- bptest(DJ_exp_lm, studentize = TRUE)
DJ_exp_DWtest <- dwtest(DJ_exp_lm)
DJ_exp_bgtest <- bgtest(DJ_exp_lm, order = 1)
DJ_exp_jbtest <- jarque.bera.test(resid(DJ_exp_lm))
DJ_exp_shtest <- shapiro.test(resid(DJ_exp_lm))

# Lavalette Function
RO_Census_Dolj_New <- RO_Census_Dolj_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Dolj_New) - Rank + 1))
)
DJ_lav_lm <- lm(log(RO_Census_Dolj_New$Population) ~ RO_Census_Dolj_New$Lav_exp)
summary(DJ_lav_lm)
DJ_lav_kst <- exp(signif(DJ_lav_lm$coef[[1]], 4))
DJ_lav_chi <- signif(DJ_lav_lm$coef[[2]], 4)
DJ_pred_lav_lm <- exp(predict(DJ_lav_lm, newdata = RO_Census_Dolj_New, 
                              interval = "prediction", level = 0.95))
DJ_lav_whtest <- white(DJ_lav_lm, interactions = TRUE)
DJ_lav_bptest <- bptest(DJ_lav_lm, studentize = TRUE)
DJ_lav_DWtest <- dwtest(DJ_lav_lm)
DJ_lav_bgtest <- bgtest(DJ_lav_lm, order = 1)
DJ_lav_jbtest <- jarque.bera.test(resid(DJ_lav_lm))
DJ_lav_shtest <- shapiro.test(resid(DJ_lav_lm))

# Data Distributions
RO_Census_Dolj_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Dolj_New$Rank, 
                              Pop_Freq = DJ_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 30, y = 0.07,
            label = paste0("y", "==", signif(exp(DJ_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(DJ_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.07,
            label = paste0("R^2 ==", signif(summary(DJ_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dolj_New$Rank, 
                              Pop_Freq = DJ_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 30, y = 0.06,
            label = paste0("y", "==", signif(exp(DJ_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(DJ_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.06,
            label = paste0("R^2 ==", signif(summary(DJ_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dolj_New$Rank, 
                              Pop_Freq = DJ_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 30, y = 0.05,
            label = paste0("y", "==", signif(DJ_ZM_cst, 4), "%.%", "(",
                           signif(DJ_ZM_m, 4), "+ x)^", signif(DJ_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.05,
            label = paste0("R^2 ==", signif(summary(DJ_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dolj_New$Rank, 
                              Pop_Freq = DJ_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 30, y = 0.04,
            label = paste0("y", "==", signif(DJ_lav_kst, 4), "%.%", "x^",
                           -signif(DJ_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.04,
            label = paste0("R^2 ==", signif(summary(DJ_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Dolj judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Dolj_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Dolj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dolj_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Dolj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dolj_New %>%
     ggplot(aes(x = log(Rank + DJ_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Dolj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dolj_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Dolj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 18. Galati
GL_out <- get_outliers(Galati_Outliers, RO_Census_Galati)

RO_Census_Galati_New <- RO_Census_df %>% inner_join(
  RO_Census_Galati %>% anti_join(
    bind_rows(
      (GL_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (GL_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (GL_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (GL_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Galati_New <- RO_Census_Galati_New %>% 
  left_join(RO_Census_Galati_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
GL_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Galati_New)
summary(GL_pw_lm)
GL_pred_pw_lm <- exp(predict(GL_pw_lm, newdata = RO_Census_Galati_New, 
                             interval = "prediction", level = 0.95))
GL_pw_whtest <- white(GL_pw_lm, interactions = TRUE)
GL_pw_bptest <- bptest(GL_pw_lm, studentize = TRUE)
GL_pw_DWtest <- dwtest(GL_pw_lm)
GL_pw_bgtest <- bgtest(GL_pw_lm, order = 1)
GL_pw_jbtest <- jarque.bera.test(resid(GL_pw_lm))
GL_pw_shtest <- shapiro.test(resid(GL_pw_lm))

# Zipf_Mandelbrot Law
GL_ZM_prm <- get_ZM_Param("RO_Census_Galati_New", "Population", "Rank")
GL_ZM_m <- GL_ZM_prm$m
GL_ZM_alpha <- GL_ZM_prm$alpha_ZM
GL_ZM_cst <- GL_ZM_prm$constant_ZM
GL_ZM_lm <- lm(log(RO_Census_Galati_New$Population) ~ log(RO_Census_Galati_New$Rank + GL_ZM_m))
summary(GL_ZM_lm)
GL_pred_ZM_lm <- exp(predict(GL_ZM_lm, newdata = RO_Census_Galati_New, 
                             interval = "prediction", level = 0.95))
GL_ZM_whtest <- white(GL_ZM_lm, interactions = TRUE)
GL_ZM_bptest <- bptest(GL_ZM_lm, studentize = TRUE)
GL_ZM_DWtest <- dwtest(GL_ZM_lm)
GL_ZM_bgtest <- bgtest(GL_ZM_lm, order = 1)
GL_ZM_jbtest <- jarque.bera.test(resid(GL_ZM_lm))
GL_ZM_shtest <- shapiro.test(resid(GL_ZM_lm))

# Exponential Law
GL_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Galati_New)
summary(GL_exp_lm)
GL_pred_exp_lm <- exp(predict(GL_exp_lm, newdata = RO_Census_Galati_New, 
                              interval = "prediction", level = 0.95))
GL_exp_whtest <- white(GL_exp_lm, interactions = TRUE)
GL_exp_bptest <- bptest(GL_exp_lm, studentize = TRUE)
GL_exp_DWtest <- dwtest(GL_exp_lm)
GL_exp_bgtest <- bgtest(GL_exp_lm, order = 1)
GL_exp_jbtest <- jarque.bera.test(resid(GL_exp_lm))
GL_exp_shtest <- shapiro.test(resid(GL_exp_lm))

# Lavalette Function
RO_Census_Galati_New <- RO_Census_Galati_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Galati_New) - Rank + 1))
)
GL_lav_lm <- lm(log(RO_Census_Galati_New$Population) ~ RO_Census_Galati_New$Lav_exp)
summary(GL_lav_lm)
GL_lav_kst <- exp(signif(GL_lav_lm$coef[[1]], 4))
GL_lav_chi <- signif(GL_lav_lm$coef[[2]], 4)
GL_pred_lav_lm <- exp(predict(GL_lav_lm, newdata = RO_Census_Galati_New, 
                              interval = "prediction", level = 0.95))
GL_lav_whtest <- white(GL_lav_lm, interactions = TRUE)
GL_lav_bptest <- bptest(GL_lav_lm, studentize = TRUE)
GL_lav_DWtest <- dwtest(GL_lav_lm)
GL_lav_bgtest <- bgtest(GL_lav_lm, order = 1)
GL_lav_jbtest <- jarque.bera.test(resid(GL_lav_lm))
GL_lav_shtest <- shapiro.test(resid(GL_lav_lm))

# Data Distributions
RO_Census_Galati_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Galati_New$Rank, 
                              Pop_Freq = GL_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(exp(GL_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(GL_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.08,
            label = paste0("R^2 ==", signif(summary(GL_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Galati_New$Rank, 
                              Pop_Freq = GL_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(GL_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(GL_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.07,
            label = paste0("R^2 ==", signif(summary(GL_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Galati_New$Rank, 
                              Pop_Freq = GL_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(GL_ZM_cst, 4), "%.%", "(",
                           signif(GL_ZM_m, 4), "+ x)^", signif(GL_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.06,
            label = paste0("R^2 ==", signif(summary(GL_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Galati_New$Rank, 
                              Pop_Freq = GL_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(GL_lav_kst, 4), "%.%", "x^",
                           -signif(GL_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(GL_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Galati judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Galati_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Galati judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Galati_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Galati judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Galati_New %>%
     ggplot(aes(x = log(Rank + GL_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Galati judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Galati_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Galati judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 19. Giurgiu
GR_out <- get_outliers(Giurgiu_Outliers, RO_Census_Giurgiu)

RO_Census_Giurgiu_New <- RO_Census_df %>% inner_join(
  RO_Census_Giurgiu %>% anti_join(
    bind_rows(
      (GR_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (GR_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (GR_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (GR_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n == max(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Giurgiu_New <- RO_Census_Giurgiu_New %>% 
  left_join(RO_Census_Giurgiu_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
GR_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Giurgiu_New)
summary(GR_pw_lm)
GR_pred_pw_lm <- exp(predict(GR_pw_lm, newdata = RO_Census_Giurgiu_New, 
                             interval = "prediction", level = 0.95))
GR_pw_whtest <- white(GR_pw_lm, interactions = TRUE)
GR_pw_bptest <- bptest(GR_pw_lm, studentize = TRUE)
GR_pw_DWtest <- dwtest(GR_pw_lm)
GR_pw_bgtest <- bgtest(GR_pw_lm, order = 1)
GR_pw_jbtest <- jarque.bera.test(resid(GR_pw_lm))
GR_pw_shtest <- shapiro.test(resid(GR_pw_lm))

# Zipf_Mandelbrot Law
GR_ZM_prm <- get_ZM_Param("RO_Census_Giurgiu_New", "Population", "Rank")
GR_ZM_m <- GR_ZM_prm$m
GR_ZM_alpha <- GR_ZM_prm$alpha_ZM
GR_ZM_cst <- GR_ZM_prm$constant_ZM
GR_ZM_lm <- lm(log(RO_Census_Giurgiu_New$Population) ~ log(RO_Census_Giurgiu_New$Rank + GR_ZM_m))
summary(GR_ZM_lm)
GR_pred_ZM_lm <- exp(predict(GR_ZM_lm, newdata = RO_Census_Giurgiu_New, 
                             interval = "prediction", level = 0.95))
GR_ZM_whtest <- white(GR_ZM_lm, interactions = TRUE)
GR_ZM_bptest <- bptest(GR_ZM_lm, studentize = TRUE)
GR_ZM_DWtest <- dwtest(GR_ZM_lm)
GR_ZM_bgtest <- bgtest(GR_ZM_lm, order = 1)
GR_ZM_jbtest <- jarque.bera.test(resid(GR_ZM_lm))
GR_ZM_shtest <- shapiro.test(resid(GR_ZM_lm))

# Exponential Law
GR_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Giurgiu_New)
summary(GR_exp_lm)
GR_pred_exp_lm <- exp(predict(GR_exp_lm, newdata = RO_Census_Giurgiu_New, 
                              interval = "prediction", level = 0.95))
GR_exp_whtest <- white(GR_exp_lm, interactions = TRUE)
GR_exp_bptest <- bptest(GR_exp_lm, studentize = TRUE)
GR_exp_DWtest <- dwtest(GR_exp_lm)
GR_exp_bgtest <- bgtest(GR_exp_lm, order = 1)
GR_exp_jbtest <- jarque.bera.test(resid(GR_exp_lm))
GR_exp_shtest <- shapiro.test(resid(GR_exp_lm))

# Lavalette Function
RO_Census_Giurgiu_New <- RO_Census_Giurgiu_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Giurgiu_New) - Rank + 1))
)
GR_lav_lm <- lm(log(RO_Census_Giurgiu_New$Population) ~ RO_Census_Giurgiu_New$Lav_exp)
summary(GR_lav_lm)
GR_lav_kst <- exp(signif(GR_lav_lm$coef[[1]], 4))
GR_lav_chi <- signif(GR_lav_lm$coef[[2]], 4)
GR_pred_lav_lm <- exp(predict(GR_lav_lm, newdata = RO_Census_Giurgiu_New, 
                              interval = "prediction", level = 0.95))
GR_lav_whtest <- white(GR_lav_lm, interactions = TRUE)
GR_lav_bptest <- bptest(GR_lav_lm, studentize = TRUE)
GR_lav_DWtest <- dwtest(GR_lav_lm)
GR_lav_bgtest <- bgtest(GR_lav_lm, order = 1)
GR_lav_jbtest <- jarque.bera.test(resid(GR_lav_lm))
GR_lav_shtest <- shapiro.test(resid(GR_lav_lm))

# Data Distributions
RO_Census_Giurgiu_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Giurgiu_New$Rank, 
                              Pop_Freq = GR_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(GR_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(GR_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 30, y = 0.07,
            label = paste0("R^2 ==", signif(summary(GR_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Giurgiu_New$Rank, 
                              Pop_Freq = GR_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(exp(GR_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(GR_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 30, y = 0.06,
            label = paste0("R^2 ==", signif(summary(GR_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Giurgiu_New$Rank, 
                              Pop_Freq = GR_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(GR_ZM_cst, 4), "%.%", "(",
                           signif(GR_ZM_m, 4), "+ x)^", signif(GR_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 30, y = 0.05,
            label = paste0("R^2 ==", signif(summary(GR_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Giurgiu_New$Rank, 
                              Pop_Freq = GR_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.04,
            label = paste0("y", "==", signif(GR_lav_kst, 4), "%.%", "x^",
                           -signif(GR_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 30, y = 0.04,
            label = paste0("R^2 ==", signif(summary(GR_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Giurgiu judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Giurgiu_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Giurgiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Giurgiu_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Giurgiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Giurgiu_New %>%
     ggplot(aes(x = log(Rank + GR_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Giurgiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Giurgiu_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Giurgiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 20. Gorj
GJ_out <- get_outliers(Gorj_Outliers, RO_Census_Gorj)

RO_Census_Gorj_New <- RO_Census_df %>% inner_join(
  RO_Census_Gorj %>% anti_join(
    bind_rows(
      (GJ_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (GJ_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (GJ_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (GJ_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Gorj_New <- RO_Census_Gorj_New %>% 
  left_join(RO_Census_Gorj_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
GJ_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Gorj_New)
summary(GJ_pw_lm)
GJ_pred_pw_lm <- exp(predict(GJ_pw_lm, newdata = RO_Census_Gorj_New, 
                             interval = "prediction", level = 0.95))
GJ_pw_whtest <- white(GJ_pw_lm, interactions = TRUE)
GJ_pw_bptest <- bptest(GJ_pw_lm, studentize = TRUE)
GJ_pw_DWtest <- dwtest(GJ_pw_lm)
GJ_pw_bgtest <- bgtest(GJ_pw_lm, order = 1)
GJ_pw_jbtest <- jarque.bera.test(resid(GJ_pw_lm))
GJ_pw_shtest <- shapiro.test(resid(GJ_pw_lm))

# Zipf_Mandelbrot Law
GJ_ZM_prm <- get_ZM_Param("RO_Census_Gorj_New", "Population", "Rank")
GJ_ZM_m <- GJ_ZM_prm$m
GJ_ZM_alpha <- GJ_ZM_prm$alpha_ZM
GJ_ZM_cst <- GJ_ZM_prm$constant_ZM
GJ_ZM_lm <- lm(log(RO_Census_Gorj_New$Population) ~ log(RO_Census_Gorj_New$Rank + GJ_ZM_m))
summary(GJ_ZM_lm)
GJ_pred_ZM_lm <- exp(predict(GJ_ZM_lm, newdata = RO_Census_Gorj_New, 
                             interval = "prediction", level = 0.95))
GJ_ZM_whtest <- white(GJ_ZM_lm, interactions = TRUE)
GJ_ZM_bptest <- bptest(GJ_ZM_lm, studentize = TRUE)
GJ_ZM_DWtest <- dwtest(GJ_ZM_lm)
GJ_ZM_bgtest <- bgtest(GJ_ZM_lm, order = 1)
GJ_ZM_jbtest <- jarque.bera.test(resid(GJ_ZM_lm))
GJ_ZM_shtest <- shapiro.test(resid(GJ_ZM_lm))

# Exponential Law
GJ_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Gorj_New)
summary(GJ_exp_lm)
GJ_pred_exp_lm <- exp(predict(GJ_exp_lm, newdata = RO_Census_Gorj_New, 
                              interval = "prediction", level = 0.95))
GJ_exp_whtest <- white(GJ_exp_lm, interactions = TRUE)
GJ_exp_bptest <- bptest(GJ_exp_lm, studentize = TRUE)
GJ_exp_DWtest <- dwtest(GJ_exp_lm)
GJ_exp_bgtest <- bgtest(GJ_exp_lm, order = 1)
GJ_exp_jbtest <- jarque.bera.test(resid(GJ_exp_lm))
GJ_exp_shtest <- shapiro.test(resid(GJ_exp_lm))

# Lavalette Function
RO_Census_Gorj_New <- RO_Census_Gorj_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Gorj_New) - Rank + 1))
)
GJ_lav_lm <- lm(log(RO_Census_Gorj_New$Population) ~ RO_Census_Gorj_New$Lav_exp)
summary(GJ_lav_lm)
GJ_lav_kst <- exp(signif(GJ_lav_lm$coef[[1]], 4))
GJ_lav_chi <- signif(GJ_lav_lm$coef[[2]], 4)
GJ_pred_lav_lm <- exp(predict(GJ_lav_lm, newdata = RO_Census_Gorj_New, 
                              interval = "prediction", level = 0.95))
GJ_lav_whtest <- white(GJ_lav_lm, interactions = TRUE)
GJ_lav_bptest <- bptest(GJ_lav_lm, studentize = TRUE)
GJ_lav_DWtest <- dwtest(GJ_lav_lm)
GJ_lav_bgtest <- bgtest(GJ_lav_lm, order = 1)
GJ_lav_jbtest <- jarque.bera.test(resid(GJ_lav_lm))
GJ_lav_shtest <- shapiro.test(resid(GJ_lav_lm))

# Data Distributions
RO_Census_Gorj_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Gorj_New$Rank, 
                              Pop_Freq = GJ_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(GJ_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(GJ_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.07,
            label = paste0("R^2 ==", signif(summary(GJ_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Gorj_New$Rank, 
                              Pop_Freq = GJ_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(exp(GJ_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(GJ_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.06,
            label = paste0("R^2 ==", signif(summary(GJ_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Gorj_New$Rank, 
                              Pop_Freq = GJ_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(GJ_ZM_cst, 4), "%.%", "(",
                           signif(GJ_ZM_m, 4), "+ x)^", signif(GJ_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(GJ_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Gorj_New$Rank, 
                              Pop_Freq = GJ_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.04,
            label = paste0("y", "==", signif(GJ_lav_kst, 4), "%.%", "x^",
                           -signif(GJ_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.04,
            label = paste0("R^2 ==", signif(summary(GJ_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Gorj judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Gorj_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Gorj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Gorj_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Gorj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Gorj_New %>%
     ggplot(aes(x = log(Rank + GJ_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Gorj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Gorj_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Gorj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 21. Harghita
HR_out <- get_outliers(Harghita_Outliers, RO_Census_Harghita)

RO_Census_Harghita_New <- RO_Census_df %>% inner_join(
  RO_Census_Harghita %>% anti_join(
    bind_rows(
      (HR_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (HR_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (HR_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (HR_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Harghita_New <- RO_Census_Harghita_New %>% 
  left_join(RO_Census_Harghita_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
HR_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Harghita_New)
summary(HR_pw_lm)
HR_pred_pw_lm <- exp(predict(HR_pw_lm, newdata = RO_Census_Harghita_New, 
                             interval = "prediction", level = 0.95))
HR_pw_whtest <- white(HR_pw_lm, interactions = TRUE)
HR_pw_bptest <- bptest(HR_pw_lm, studentize = TRUE)
HR_pw_DWtest <- dwtest(HR_pw_lm)
HR_pw_bgtest <- bgtest(HR_pw_lm, order = 1)
HR_pw_jbtest <- jarque.bera.test(resid(HR_pw_lm))
HR_pw_shtest <- shapiro.test(resid(HR_pw_lm))

# Zipf_Mandelbrot Law
HR_ZM_prm <- get_ZM_Param("RO_Census_Harghita_New", "Population", "Rank")
HR_ZM_m <- HR_ZM_prm$m
HR_ZM_alpha <- HR_ZM_prm$alpha_ZM
HR_ZM_cst <- HR_ZM_prm$constant_ZM
HR_ZM_lm <- lm(log(RO_Census_Harghita_New$Population) ~ log(RO_Census_Harghita_New$Rank + HR_ZM_m))
summary(HR_ZM_lm)
HR_pred_ZM_lm <- exp(predict(HR_ZM_lm, newdata = RO_Census_Harghita_New, 
                             interval = "prediction", level = 0.95))
HR_ZM_whtest <- white(HR_ZM_lm, interactions = TRUE)
HR_ZM_bptest <- bptest(HR_ZM_lm, studentize = TRUE)
HR_ZM_DWtest <- dwtest(HR_ZM_lm)
HR_ZM_bgtest <- bgtest(HR_ZM_lm, order = 1)
HR_ZM_jbtest <- jarque.bera.test(resid(HR_ZM_lm))
HR_ZM_shtest <- shapiro.test(resid(HR_ZM_lm))

# Exponential Law
HR_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Harghita_New)
summary(HR_exp_lm)
HR_pred_exp_lm <- exp(predict(HR_exp_lm, newdata = RO_Census_Harghita_New, 
                              interval = "prediction", level = 0.95))
HR_exp_whtest <- white(HR_exp_lm, interactions = TRUE)
HR_exp_bptest <- bptest(HR_exp_lm, studentize = TRUE)
HR_exp_DWtest <- dwtest(HR_exp_lm)
HR_exp_bgtest <- bgtest(HR_exp_lm, order = 1)
HR_exp_jbtest <- jarque.bera.test(resid(HR_exp_lm))
HR_exp_shtest <- shapiro.test(resid(HR_exp_lm))

# Lavalette Function
RO_Census_Harghita_New <- RO_Census_Harghita_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Harghita_New) - Rank + 1))
)
HR_lav_lm <- lm(log(RO_Census_Harghita_New$Population) ~ RO_Census_Harghita_New$Lav_exp)
summary(HR_lav_lm)
HR_lav_kst <- exp(signif(HR_lav_lm$coef[[1]], 4))
HR_lav_chi <- signif(HR_lav_lm$coef[[2]], 4)
HR_pred_lav_lm <- exp(predict(HR_lav_lm, newdata = RO_Census_Harghita_New, 
                              interval = "prediction", level = 0.95))
HR_lav_whtest <- white(HR_lav_lm, interactions = TRUE)
HR_lav_bptest <- bptest(HR_lav_lm, studentize = TRUE)
HR_lav_DWtest <- dwtest(HR_lav_lm)
HR_lav_bgtest <- bgtest(HR_lav_lm, order = 1)
HR_lav_jbtest <- jarque.bera.test(resid(HR_lav_lm))
HR_lav_shtest <- shapiro.test(resid(HR_lav_lm))

# Data Distributions
RO_Census_Harghita_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Harghita_New$Rank, 
                              Pop_Freq = HR_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(exp(HR_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(HR_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.06,
            label = paste0("R^2 ==", signif(summary(HR_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Harghita_New$Rank, 
                              Pop_Freq = HR_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.055,
            label = paste0("y", "==", signif(exp(HR_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(HR_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.055,
            label = paste0("R^2 ==", signif(summary(HR_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Harghita_New$Rank, 
                              Pop_Freq = HR_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(HR_ZM_cst, 4), "%.%", "(",
                           signif(HR_ZM_m, 4), "+ x)^", signif(HR_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(HR_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Harghita_New$Rank, 
                              Pop_Freq = HR_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.045,
            label = paste0("y", "==", signif(HR_lav_kst, 4), "%.%", "x^",
                           -signif(HR_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.045,
            label = paste0("R^2 ==", signif(summary(HR_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Harghita judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Harghita_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Harghita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Harghita_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Harghita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Harghita_New %>%
     ggplot(aes(x = log(Rank + HR_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Harghita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Harghita_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Harghita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 22. Hunedoara
HD_out <- get_outliers(Hunedoara_Outliers, RO_Census_Hunedoara)

RO_Census_Hunedoara_New <- RO_Census_df %>% inner_join(
  RO_Census_Hunedoara %>% anti_join(
    bind_rows(
      (HD_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (HD_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (HD_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (HD_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Hunedoara_New <- RO_Census_Hunedoara_New %>% 
  left_join(RO_Census_Hunedoara_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
HD_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Hunedoara_New)
summary(HD_pw_lm)
HD_pred_pw_lm <- exp(predict(HD_pw_lm, newdata = RO_Census_Hunedoara_New, 
                             interval = "prediction", level = 0.95))
HD_pw_whtest <- white(HD_pw_lm, interactions = TRUE)
HD_pw_bptest <- bptest(HD_pw_lm, studentize = TRUE)
HD_pw_DWtest <- dwtest(HD_pw_lm)
HD_pw_bgtest <- bgtest(HD_pw_lm, order = 1)
HD_pw_jbtest <- jarque.bera.test(resid(HD_pw_lm))
HD_pw_shtest <- shapiro.test(resid(HD_pw_lm))

# Zipf_Mandelbrot Law
HD_ZM_prm <- get_ZM_Param("RO_Census_Hunedoara_New", "Population", "Rank")
HD_ZM_m <- HD_ZM_prm$m
HD_ZM_alpha <- HD_ZM_prm$alpha_ZM
HD_ZM_cst <- HD_ZM_prm$constant_ZM
HD_ZM_lm <- lm(log(RO_Census_Hunedoara_New$Population) ~ log(RO_Census_Hunedoara_New$Rank + HD_ZM_m))
summary(HD_ZM_lm)
HD_pred_ZM_lm <- exp(predict(HD_ZM_lm, newdata = RO_Census_Hunedoara_New, 
                             interval = "prediction", level = 0.95))
HD_ZM_whtest <- white(HD_ZM_lm, interactions = TRUE)
HD_ZM_bptest <- bptest(HD_ZM_lm, studentize = TRUE)
HD_ZM_DWtest <- dwtest(HD_ZM_lm)
HD_ZM_bgtest <- bgtest(HD_ZM_lm, order = 1)
HD_ZM_jbtest <- jarque.bera.test(resid(HD_ZM_lm))
HD_ZM_shtest <- shapiro.test(resid(HD_ZM_lm))

# Exponential Law
HD_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Hunedoara_New)
summary(HD_exp_lm)
HD_pred_exp_lm <- exp(predict(HD_exp_lm, newdata = RO_Census_Hunedoara_New, 
                              interval = "prediction", level = 0.95))
HD_exp_whtest <- white(HD_exp_lm, interactions = TRUE)
HD_exp_bptest <- bptest(HD_exp_lm, studentize = TRUE)
HD_exp_DWtest <- dwtest(HD_exp_lm)
HD_exp_bgtest <- bgtest(HD_exp_lm, order = 1)
HD_exp_jbtest <- jarque.bera.test(resid(HD_exp_lm))
HD_exp_shtest <- shapiro.test(resid(HD_exp_lm))

# Lavalette Function
RO_Census_Hunedoara_New <- RO_Census_Hunedoara_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Hunedoara_New) - Rank + 1))
)
HD_lav_lm <- lm(log(RO_Census_Hunedoara_New$Population) ~ RO_Census_Hunedoara_New$Lav_exp)
summary(HD_lav_lm)
HD_lav_kst <- exp(signif(HD_lav_lm$coef[[1]], 4))
HD_lav_chi <- signif(HD_lav_lm$coef[[2]], 4)
HD_pred_lav_lm <- exp(predict(HD_lav_lm, newdata = RO_Census_Hunedoara_New, 
                              interval = "prediction", level = 0.95))
HD_lav_whtest <- white(HD_lav_lm, interactions = TRUE)
HD_lav_bptest <- bptest(HD_lav_lm, studentize = TRUE)
HD_lav_DWtest <- dwtest(HD_lav_lm)
HD_lav_bgtest <- bgtest(HD_lav_lm, order = 1)
HD_lav_jbtest <- jarque.bera.test(resid(HD_lav_lm))
HD_lav_shtest <- shapiro.test(resid(HD_lav_lm))

# Data Distributions
RO_Census_Hunedoara_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Hunedoara_New$Rank, 
                              Pop_Freq = HD_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.20,
            label = paste0("y", "==", signif(exp(HD_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(HD_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.20,
            label = paste0("R^2 ==", signif(summary(HD_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Hunedoara_New$Rank, 
                              Pop_Freq = HD_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.17,
            label = paste0("y", "==", signif(exp(HD_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(HD_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.17,
            label = paste0("R^2 ==", signif(summary(HD_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Hunedoara_New$Rank, 
                              Pop_Freq = HD_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.14,
            label = paste0("y", "==", signif(HD_ZM_cst, 4), "%.%", "(",
                           signif(HD_ZM_m, 4), "+ x)^", signif(HD_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.14,
            label = paste0("R^2 ==", signif(summary(HD_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Hunedoara_New$Rank, 
                              Pop_Freq = HD_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.11,
            label = paste0("y", "==", signif(HD_lav_kst, 4), "%.%", "x^",
                           -signif(HD_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.11,
            label = paste0("R^2 ==", signif(summary(HD_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Hunedoara judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Hunedoara_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Hunedoara judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Hunedoara_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Hunedoara judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Hunedoara_New %>%
     ggplot(aes(x = log(Rank + HD_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Hunedoara judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Hunedoara_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Hunedoara judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 23. Ialomita
IL_out <- get_outliers(Ialomita_Outliers, RO_Census_Ialomita)

RO_Census_Ialomita_New <- RO_Census_df %>% inner_join(
  RO_Census_Ialomita %>% anti_join(
    bind_rows(
      (IL_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (IL_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (IL_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (IL_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Ialomita_New <- RO_Census_Ialomita_New %>% 
  left_join(RO_Census_Ialomita_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
IL_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Ialomita_New)
summary(IL_pw_lm)
IL_pred_pw_lm <- exp(predict(IL_pw_lm, newdata = RO_Census_Ialomita_New, 
                             interval = "prediction", level = 0.95))
IL_pw_whtest <- white(IL_pw_lm, interactions = TRUE)
IL_pw_bptest <- bptest(IL_pw_lm, studentize = TRUE)
IL_pw_DWtest <- dwtest(IL_pw_lm)
IL_pw_bgtest <- bgtest(IL_pw_lm, order = 1)
IL_pw_jbtest <- jarque.bera.test(resid(IL_pw_lm))
IL_pw_shtest <- shapiro.test(resid(IL_pw_lm))

# Zipf_Mandelbrot Law
IL_ZM_prm <- get_ZM_Param("RO_Census_Ialomita_New", "Population", "Rank")
IL_ZM_m <- IL_ZM_prm$m
IL_ZM_alpha <- IL_ZM_prm$alpha_ZM
IL_ZM_cst <- IL_ZM_prm$constant_ZM
IL_ZM_lm <- lm(log(RO_Census_Ialomita_New$Population) ~ log(RO_Census_Ialomita_New$Rank + IL_ZM_m))
summary(IL_ZM_lm)
IL_pred_ZM_lm <- exp(predict(IL_ZM_lm, newdata = RO_Census_Ialomita_New, 
                             interval = "prediction", level = 0.95))
IL_ZM_whtest <- white(IL_ZM_lm, interactions = TRUE)
IL_ZM_bptest <- bptest(IL_ZM_lm, studentize = TRUE)
IL_ZM_DWtest <- dwtest(IL_ZM_lm)
IL_ZM_bgtest <- bgtest(IL_ZM_lm, order = 1)
IL_ZM_jbtest <- jarque.bera.test(resid(IL_ZM_lm))
IL_ZM_shtest <- shapiro.test(resid(IL_ZM_lm))

# Exponential Law
IL_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Ialomita_New)
summary(IL_exp_lm)
IL_pred_exp_lm <- exp(predict(IL_exp_lm, newdata = RO_Census_Ialomita_New, 
                              interval = "prediction", level = 0.95))
IL_exp_whtest <- white(IL_exp_lm, interactions = TRUE)
IL_exp_bptest <- bptest(IL_exp_lm, studentize = TRUE)
IL_exp_DWtest <- dwtest(IL_exp_lm)
IL_exp_bgtest <- bgtest(IL_exp_lm, order = 1)
IL_exp_jbtest <- jarque.bera.test(resid(IL_exp_lm))
IL_exp_shtest <- shapiro.test(resid(IL_exp_lm))

# Lavalette Function
RO_Census_Ialomita_New <- RO_Census_Ialomita_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Ialomita_New) - Rank + 1))
)
IL_lav_lm <- lm(log(RO_Census_Ialomita_New$Population) ~ RO_Census_Ialomita_New$Lav_exp)
summary(IL_lav_lm)
IL_lav_kst <- exp(signif(IL_lav_lm$coef[[1]], 4))
IL_lav_chi <- signif(IL_lav_lm$coef[[2]], 4)
IL_pred_lav_lm <- exp(predict(IL_lav_lm, newdata = RO_Census_Ialomita_New, 
                              interval = "prediction", level = 0.95))
IL_lav_whtest <- white(IL_lav_lm, interactions = TRUE)
IL_lav_bptest <- bptest(IL_lav_lm, studentize = TRUE)
IL_lav_DWtest <- dwtest(IL_lav_lm)
IL_lav_bgtest <- bgtest(IL_lav_lm, order = 1)
IL_lav_jbtest <- jarque.bera.test(resid(IL_lav_lm))
IL_lav_shtest <- shapiro.test(resid(IL_lav_lm))

# Data Distributions
RO_Census_Ialomita_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Ialomita_New$Rank, 
                              Pop_Freq = IL_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(exp(IL_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(IL_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.06,
            label = paste0("R^2 ==", signif(summary(IL_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ialomita_New$Rank, 
                              Pop_Freq = IL_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.055,
            label = paste0("y", "==", signif(exp(IL_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(IL_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.055,
            label = paste0("R^2 ==", signif(summary(IL_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ialomita_New$Rank, 
                              Pop_Freq = IL_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(IL_ZM_cst, 4), "%.%", "(",
                           signif(IL_ZM_m, 4), "+ x)^", signif(IL_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(IL_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ialomita_New$Rank, 
                              Pop_Freq = IL_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.045,
            label = paste0("y", "==", signif(IL_lav_kst, 4), "%.%", "x^",
                           -signif(IL_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.045,
            label = paste0("R^2 ==", signif(summary(IL_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Ialomita judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Ialomita_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Ialomita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ialomita_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Ialomita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ialomita_New %>%
     ggplot(aes(x = log(Rank + IL_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Ialomita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ialomita_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Ialomita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 24. Iasi
IS_out <- get_outliers(Iasi_Outliers, RO_Census_Iasi)

RO_Census_Iasi_New <- RO_Census_df %>% inner_join(
  RO_Census_Iasi %>% anti_join(
    bind_rows(
      (IS_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (IS_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (IS_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (IS_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Iasi_New <- RO_Census_Iasi_New %>% 
  left_join(RO_Census_Iasi_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
IS_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Iasi_New)
summary(IS_pw_lm)
IS_pred_pw_lm <- exp(predict(IS_pw_lm, newdata = RO_Census_Iasi_New, 
                             interval = "prediction", level = 0.95))
IS_pw_whtest <- white(IS_pw_lm, interactions = TRUE)
IS_pw_bptest <- bptest(IS_pw_lm, studentize = TRUE)
IS_pw_DWtest <- dwtest(IS_pw_lm)
IS_pw_bgtest <- bgtest(IS_pw_lm, order = 1)
IS_pw_jbtest <- jarque.bera.test(resid(IS_pw_lm))
IS_pw_shtest <- shapiro.test(resid(IS_pw_lm))

# Zipf_Mandelbrot Law
IS_ZM_prm <- get_ZM_Param("RO_Census_Iasi_New", "Population", "Rank")
IS_ZM_m <- IS_ZM_prm$m
IS_ZM_alpha <- IS_ZM_prm$alpha_ZM
IS_ZM_cst <- IS_ZM_prm$constant_ZM
IS_ZM_lm <- lm(log(RO_Census_Iasi_New$Population) ~ log(RO_Census_Iasi_New$Rank + IS_ZM_m))
summary(IS_ZM_lm)
IS_pred_ZM_lm <- exp(predict(IS_ZM_lm, newdata = RO_Census_Iasi_New, 
                             interval = "prediction", level = 0.95))
IS_ZM_whtest <- white(IS_ZM_lm, interactions = TRUE)
IS_ZM_bptest <- bptest(IS_ZM_lm, studentize = TRUE)
IS_ZM_DWtest <- dwtest(IS_ZM_lm)
IS_ZM_bgtest <- bgtest(IS_ZM_lm, order = 1)
IS_ZM_jbtest <- jarque.bera.test(resid(IS_ZM_lm))
IS_ZM_shtest <- shapiro.test(resid(IS_ZM_lm))

# Exponential Law
IS_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Iasi_New)
summary(IS_exp_lm)
IS_pred_exp_lm <- exp(predict(IS_exp_lm, newdata = RO_Census_Iasi_New, 
                              interval = "prediction", level = 0.95))
IS_exp_whtest <- white(IS_exp_lm, interactions = TRUE)
IS_exp_bptest <- bptest(IS_exp_lm, studentize = TRUE)
IS_exp_DWtest <- dwtest(IS_exp_lm)
IS_exp_bgtest <- bgtest(IS_exp_lm, order = 1)
IS_exp_jbtest <- jarque.bera.test(resid(IS_exp_lm))
IS_exp_shtest <- shapiro.test(resid(IS_exp_lm))

# Lavalette Function
RO_Census_Iasi_New <- RO_Census_Iasi_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Iasi_New) - Rank + 1))
)
IS_lav_lm <- lm(log(RO_Census_Iasi_New$Population) ~ RO_Census_Iasi_New$Lav_exp)
summary(IS_lav_lm)
IS_lav_kst <- exp(signif(IS_lav_lm$coef[[1]], 4))
IS_lav_chi <- signif(IS_lav_lm$coef[[2]], 4)
IS_pred_lav_lm <- exp(predict(IS_lav_lm, newdata = RO_Census_Iasi_New, 
                              interval = "prediction", level = 0.95))
IS_lav_whtest <- white(IS_lav_lm, interactions = TRUE)
IS_lav_bptest <- bptest(IS_lav_lm, studentize = TRUE)
IS_lav_DWtest <- dwtest(IS_lav_lm)
IS_lav_bgtest <- bgtest(IS_lav_lm, order = 1)
IS_lav_jbtest <- jarque.bera.test(resid(IS_lav_lm))
IS_lav_shtest <- shapiro.test(resid(IS_lav_lm))

# Data Distributions
RO_Census_Iasi_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Iasi_New$Rank, 
                              Pop_Freq = IS_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.06,
            label = paste0("y", "==", signif(exp(IS_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(IS_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.06,
            label = paste0("R^2 ==", signif(summary(IS_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Iasi_New$Rank, 
                              Pop_Freq = IS_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.055,
            label = paste0("y", "==", signif(exp(IS_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(IS_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.055,
            label = paste0("R^2 ==", signif(summary(IS_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Iasi_New$Rank, 
                              Pop_Freq = IS_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.05,
            label = paste0("y", "==", signif(IS_ZM_cst, 4), "%.%", "(",
                           signif(IS_ZM_m, 4), "+ x)^", signif(IS_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.05,
            label = paste0("R^2 ==", signif(summary(IS_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Iasi_New$Rank, 
                              Pop_Freq = IS_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.045,
            label = paste0("y", "==", signif(IS_lav_kst, 4), "%.%", "x^",
                           -signif(IS_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.045,
            label = paste0("R^2 ==", signif(summary(IS_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Iasi judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Iasi_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Iasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Iasi_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Iasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Iasi_New %>%
     ggplot(aes(x = log(Rank + IS_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Iasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Iasi_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Iasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 25. Ilfov
IF_out <- get_outliers(Ilfov_Outliers, RO_Census_Ilfov)

RO_Census_Ilfov_New <- RO_Census_df %>% inner_join(
  RO_Census_Ilfov %>% anti_join(
    bind_rows(
      (IF_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (IF_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (IF_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (IF_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Ilfov_New <- RO_Census_Ilfov_New %>% 
  left_join(RO_Census_Ilfov_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
IF_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Ilfov_New)
summary(IF_pw_lm)
IF_pred_pw_lm <- exp(predict(IF_pw_lm, newdata = RO_Census_Ilfov_New, 
                             interval = "prediction", level = 0.95))
IF_pw_whtest <- white(IF_pw_lm, interactions = TRUE)
IF_pw_bptest <- bptest(IF_pw_lm, studentize = TRUE)
IF_pw_DWtest <- dwtest(IF_pw_lm)
IF_pw_bgtest <- bgtest(IF_pw_lm, order = 1)
IF_pw_jbtest <- jarque.bera.test(resid(IF_pw_lm))
IF_pw_shtest <- shapiro.test(resid(IF_pw_lm))

# Zipf_Mandelbrot Law
IF_ZM_prm <- get_ZM_Param("RO_Census_Ilfov_New", "Population", "Rank")
IF_ZM_m <- IF_ZM_prm$m
IF_ZM_alpha <- IF_ZM_prm$alpha_ZM
IF_ZM_cst <- IF_ZM_prm$constant_ZM
IF_ZM_lm <- lm(log(RO_Census_Ilfov_New$Population) ~ log(RO_Census_Ilfov_New$Rank + IF_ZM_m))
summary(IF_ZM_lm)
IF_pred_ZM_lm <- exp(predict(IF_ZM_lm, newdata = RO_Census_Ilfov_New, 
                             interval = "prediction", level = 0.95))
IF_ZM_whtest <- white(IF_ZM_lm, interactions = TRUE)
IF_ZM_bptest <- bptest(IF_ZM_lm, studentize = TRUE)
IF_ZM_DWtest <- dwtest(IF_ZM_lm)
IF_ZM_bgtest <- bgtest(IF_ZM_lm, order = 1)
IF_ZM_jbtest <- jarque.bera.test(resid(IF_ZM_lm))
IF_ZM_shtest <- shapiro.test(resid(IF_ZM_lm))

# Exponential Law
IF_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Ilfov_New)
summary(IF_exp_lm)
IF_pred_exp_lm <- exp(predict(IF_exp_lm, newdata = RO_Census_Ilfov_New, 
                              interval = "prediction", level = 0.95))
IF_exp_whtest <- white(IF_exp_lm, interactions = TRUE)
IF_exp_bptest <- bptest(IF_exp_lm, studentize = TRUE)
IF_exp_DWtest <- dwtest(IF_exp_lm)
IF_exp_bgtest <- bgtest(IF_exp_lm, order = 1)
IF_exp_jbtest <- jarque.bera.test(resid(IF_exp_lm))
IF_exp_shtest <- shapiro.test(resid(IF_exp_lm))

# Lavalette Function
RO_Census_Ilfov_New <- RO_Census_Ilfov_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Ilfov_New) - Rank + 1))
)
IF_lav_lm <- lm(log(RO_Census_Ilfov_New$Population) ~ RO_Census_Ilfov_New$Lav_exp)
summary(IF_lav_lm)
IF_lav_kst <- exp(signif(IF_lav_lm$coef[[1]], 4))
IF_lav_chi <- signif(IF_lav_lm$coef[[2]], 4)
IF_pred_lav_lm <- exp(predict(IF_lav_lm, newdata = RO_Census_Ilfov_New, 
                              interval = "prediction", level = 0.95))
IF_lav_whtest <- white(IF_lav_lm, interactions = TRUE)
IF_lav_bptest <- bptest(IF_lav_lm, studentize = TRUE)
IF_lav_DWtest <- dwtest(IF_lav_lm)
IF_lav_bgtest <- bgtest(IF_lav_lm, order = 1)
IF_lav_jbtest <- jarque.bera.test(resid(IF_lav_lm))
IF_lav_shtest <- shapiro.test(resid(IF_lav_lm))

# Data Distributions
RO_Census_Ilfov_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Ilfov_New$Rank, 
                              Pop_Freq = IF_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(exp(IF_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(IF_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 30, y = 0.08,
            label = paste0("R^2 ==", signif(summary(IF_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ilfov_New$Rank, 
                              Pop_Freq = IF_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(IF_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(IF_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 30, y = 0.07,
            label = paste0("R^2 ==", signif(summary(IF_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ilfov_New$Rank, 
                              Pop_Freq = IF_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(IF_ZM_cst, 4), "%.%", "(",
                           signif(IF_ZM_m, 4), "+ x)^", signif(IF_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 30, y = 0.06,
            label = paste0("R^2 ==", signif(summary(IF_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ilfov_New$Rank, 
                              Pop_Freq = IF_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(IF_lav_kst, 4), "%.%", "x^",
                           -signif(IF_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 30, y = 0.05,
            label = paste0("R^2 ==", signif(summary(IF_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Ilfov judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Ilfov_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Ilfov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ilfov_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Ilfov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ilfov_New %>%
     ggplot(aes(x = log(Rank + IF_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Ilfov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ilfov_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Ilfov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 26. Maramures
MM_out <- get_outliers(Maramures_Outliers, RO_Census_Maramures)

RO_Census_Maramures_New <- RO_Census_df %>% inner_join(
  RO_Census_Maramures %>% anti_join(
    bind_rows(
      (MM_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (MM_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (MM_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (MM_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Maramures_New <- RO_Census_Maramures_New %>% 
  left_join(RO_Census_Maramures_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
MM_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Maramures_New)
summary(MM_pw_lm)
MM_pred_pw_lm <- exp(predict(MM_pw_lm, newdata = RO_Census_Maramures_New, 
                             interval = "prediction", level = 0.95))
MM_pw_whtest <- white(MM_pw_lm, interactions = TRUE)
MM_pw_bptest <- bptest(MM_pw_lm, studentize = TRUE)
MM_pw_DWtest <- dwtest(MM_pw_lm)
MM_pw_bgtest <- bgtest(MM_pw_lm, order = 1)
MM_pw_jbtest <- jarque.bera.test(resid(MM_pw_lm))
MM_pw_shtest <- shapiro.test(resid(MM_pw_lm))

# Zipf_Mandelbrot Law
MM_ZM_prm <- get_ZM_Param("RO_Census_Maramures_New", "Population", "Rank")
MM_ZM_m <- MM_ZM_prm$m
MM_ZM_alpha <- MM_ZM_prm$alpha_ZM
MM_ZM_cst <- MM_ZM_prm$constant_ZM
MM_ZM_lm <- lm(log(RO_Census_Maramures_New$Population) ~ log(RO_Census_Maramures_New$Rank + MM_ZM_m))
summary(MM_ZM_lm)
MM_pred_ZM_lm <- exp(predict(MM_ZM_lm, newdata = RO_Census_Maramures_New, 
                             interval = "prediction", level = 0.95))
MM_ZM_whtest <- white(MM_ZM_lm, interactions = TRUE)
MM_ZM_bptest <- bptest(MM_ZM_lm, studentize = TRUE)
MM_ZM_DWtest <- dwtest(MM_ZM_lm)
MM_ZM_bgtest <- bgtest(MM_ZM_lm, order = 1)
MM_ZM_jbtest <- jarque.bera.test(resid(MM_ZM_lm))
MM_ZM_shtest <- shapiro.test(resid(MM_ZM_lm))

# Exponential Law
MM_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Maramures_New)
summary(MM_exp_lm)
MM_pred_exp_lm <- exp(predict(MM_exp_lm, newdata = RO_Census_Maramures_New, 
                              interval = "prediction", level = 0.95))
MM_exp_whtest <- white(MM_exp_lm, interactions = TRUE)
MM_exp_bptest <- bptest(MM_exp_lm, studentize = TRUE)
MM_exp_DWtest <- dwtest(MM_exp_lm)
MM_exp_bgtest <- bgtest(MM_exp_lm, order = 1)
MM_exp_jbtest <- jarque.bera.test(resid(MM_exp_lm))
MM_exp_shtest <- shapiro.test(resid(MM_exp_lm))

# Lavalette Function
RO_Census_Maramures_New <- RO_Census_Maramures_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Maramures_New) - Rank + 1))
)
MM_lav_lm <- lm(log(RO_Census_Maramures_New$Population) ~ RO_Census_Maramures_New$Lav_exp)
summary(MM_lav_lm)
MM_lav_kst <- exp(signif(MM_lav_lm$coef[[1]], 4))
MM_lav_chi <- signif(MM_lav_lm$coef[[2]], 4)
MM_pred_lav_lm <- exp(predict(MM_lav_lm, newdata = RO_Census_Maramures_New, 
                              interval = "prediction", level = 0.95))
MM_lav_whtest <- white(MM_lav_lm, interactions = TRUE)
MM_lav_bptest <- bptest(MM_lav_lm, studentize = TRUE)
MM_lav_DWtest <- dwtest(MM_lav_lm)
MM_lav_bgtest <- bgtest(MM_lav_lm, order = 1)
MM_lav_jbtest <- jarque.bera.test(resid(MM_lav_lm))
MM_lav_shtest <- shapiro.test(resid(MM_lav_lm))

# Data Distributions
RO_Census_Maramures_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Maramures_New$Rank, 
                              Pop_Freq = MM_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(exp(MM_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(MM_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.08,
            label = paste0("R^2 ==", signif(summary(MM_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Maramures_New$Rank, 
                              Pop_Freq = MM_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(MM_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(MM_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.07,
            label = paste0("R^2 ==", signif(summary(MM_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Maramures_New$Rank, 
                              Pop_Freq = MM_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(MM_ZM_cst, 4), "%.%", "(",
                           signif(MM_ZM_m, 4), "+ x)^", signif(MM_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.06,
            label = paste0("R^2 ==", signif(summary(MM_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Maramures_New$Rank, 
                              Pop_Freq = MM_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(MM_lav_kst, 4), "%.%", "x^",
                           -signif(MM_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(MM_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Maramures judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Maramures_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Maramures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Maramures_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Maramures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Maramures_New %>%
     ggplot(aes(x = log(Rank + MM_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Maramures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Maramures_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Maramures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 27. Mehedinti
MH_out <- get_outliers(Mehedinti_Outliers, RO_Census_Mehedinti)

RO_Census_Mehedinti_New <- RO_Census_df %>% inner_join(
  RO_Census_Mehedinti %>% anti_join(
    bind_rows(
      (MH_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (MH_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (MH_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (MH_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Mehedinti_New <- RO_Census_Mehedinti_New %>% 
  left_join(RO_Census_Mehedinti_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
MH_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Mehedinti_New)
summary(MH_pw_lm)
MH_pred_pw_lm <- exp(predict(MH_pw_lm, newdata = RO_Census_Mehedinti_New, 
                             interval = "prediction", level = 0.95))
MH_pw_whtest <- white(MH_pw_lm, interactions = TRUE)
MH_pw_bptest <- bptest(MH_pw_lm, studentize = TRUE)
MH_pw_DWtest <- dwtest(MH_pw_lm)
MH_pw_bgtest <- bgtest(MH_pw_lm, order = 1)
MH_pw_jbtest <- jarque.bera.test(resid(MH_pw_lm))
MH_pw_shtest <- shapiro.test(resid(MH_pw_lm))

# Zipf_Mandelbrot Law
MH_ZM_prm <- get_ZM_Param("RO_Census_Mehedinti_New", "Population", "Rank")
MH_ZM_m <- MH_ZM_prm$m
MH_ZM_alpha <- MH_ZM_prm$alpha_ZM
MH_ZM_cst <- MH_ZM_prm$constant_ZM
MH_ZM_lm <- lm(log(RO_Census_Mehedinti_New$Population) ~ log(RO_Census_Mehedinti_New$Rank + MH_ZM_m))
summary(MH_ZM_lm)
MH_pred_ZM_lm <- exp(predict(MH_ZM_lm, newdata = RO_Census_Mehedinti_New, 
                             interval = "prediction", level = 0.95))
MH_ZM_whtest <- white(MH_ZM_lm, interactions = TRUE)
MH_ZM_bptest <- bptest(MH_ZM_lm, studentize = TRUE)
MH_ZM_DWtest <- dwtest(MH_ZM_lm)
MH_ZM_bgtest <- bgtest(MH_ZM_lm, order = 1)
MH_ZM_jbtest <- jarque.bera.test(resid(MH_ZM_lm))
MH_ZM_shtest <- shapiro.test(resid(MH_ZM_lm))

# Exponential Law
MH_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Mehedinti_New)
summary(MH_exp_lm)
MH_pred_exp_lm <- exp(predict(MH_exp_lm, newdata = RO_Census_Mehedinti_New, 
                              interval = "prediction", level = 0.95))
MH_exp_whtest <- white(MH_exp_lm, interactions = TRUE)
MH_exp_bptest <- bptest(MH_exp_lm, studentize = TRUE)
MH_exp_DWtest <- dwtest(MH_exp_lm)
MH_exp_bgtest <- bgtest(MH_exp_lm, order = 1)
MH_exp_jbtest <- jarque.bera.test(resid(MH_exp_lm))
MH_exp_shtest <- shapiro.test(resid(MH_exp_lm))

# Lavalette Function
RO_Census_Mehedinti_New <- RO_Census_Mehedinti_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Mehedinti_New) - Rank + 1))
)
MH_lav_lm <- lm(log(RO_Census_Mehedinti_New$Population) ~ RO_Census_Mehedinti_New$Lav_exp)
summary(MH_lav_lm)
MH_lav_kst <- exp(signif(MH_lav_lm$coef[[1]], 4))
MH_lav_chi <- signif(MH_lav_lm$coef[[2]], 4)
MH_pred_lav_lm <- exp(predict(MH_lav_lm, newdata = RO_Census_Mehedinti_New, 
                              interval = "prediction", level = 0.95))
MH_lav_whtest <- white(MH_lav_lm, interactions = TRUE)
MH_lav_bptest <- bptest(MH_lav_lm, studentize = TRUE)
MH_lav_DWtest <- dwtest(MH_lav_lm)
MH_lav_bgtest <- bgtest(MH_lav_lm, order = 1)
MH_lav_jbtest <- jarque.bera.test(resid(MH_lav_lm))
MH_lav_shtest <- shapiro.test(resid(MH_lav_lm))

# Data Distributions
RO_Census_Mehedinti_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Mehedinti_New$Rank, 
                              Pop_Freq = MH_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(exp(MH_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(MH_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.08,
            label = paste0("R^2 ==", signif(summary(MH_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mehedinti_New$Rank, 
                              Pop_Freq = MH_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(MH_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(MH_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.07,
            label = paste0("R^2 ==", signif(summary(MH_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mehedinti_New$Rank, 
                              Pop_Freq = MH_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(MH_ZM_cst, 4), "%.%", "(",
                           signif(MH_ZM_m, 4), "+ x)^", signif(MH_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.06,
            label = paste0("R^2 ==", signif(summary(MH_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mehedinti_New$Rank, 
                              Pop_Freq = MH_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(MH_lav_kst, 4), "%.%", "x^",
                           -signif(MH_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(MH_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Mehedinti judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Mehedinti_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Mehedinti judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mehedinti_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Mehedinti judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mehedinti_New %>%
     ggplot(aes(x = log(Rank + MH_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Mehedinti judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mehedinti_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Mehedinti judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 28. Mures
MS_out <- get_outliers(Mures_Outliers, RO_Census_Mures)

RO_Census_Mures_New <- RO_Census_df %>% inner_join(
  RO_Census_Mures %>% anti_join(
    bind_rows(
      (MS_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (MS_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (MS_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (MS_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Mures_New <- RO_Census_Mures_New %>% 
  left_join(RO_Census_Mures_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
MS_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Mures_New)
summary(MS_pw_lm)
MS_pred_pw_lm <- exp(predict(MS_pw_lm, newdata = RO_Census_Mures_New, 
                             interval = "prediction", level = 0.95))
MS_pw_whtest <- white(MS_pw_lm, interactions = TRUE)
MS_pw_bptest <- bptest(MS_pw_lm, studentize = TRUE)
MS_pw_DWtest <- dwtest(MS_pw_lm)
MS_pw_bgtest <- bgtest(MS_pw_lm, order = 1)
MS_pw_jbtest <- jarque.bera.test(resid(MS_pw_lm))
MS_pw_shtest <- shapiro.test(resid(MS_pw_lm))

# Zipf_Mandelbrot Law
MS_ZM_prm <- get_ZM_Param("RO_Census_Mures_New", "Population", "Rank")
MS_ZM_m <- MS_ZM_prm$m
MS_ZM_alpha <- MS_ZM_prm$alpha_ZM
MS_ZM_cst <- MS_ZM_prm$constant_ZM
MS_ZM_lm <- lm(log(RO_Census_Mures_New$Population) ~ log(RO_Census_Mures_New$Rank + MS_ZM_m))
summary(MS_ZM_lm)
MS_pred_ZM_lm <- exp(predict(MS_ZM_lm, newdata = RO_Census_Mures_New, 
                             interval = "prediction", level = 0.95))
MS_ZM_whtest <- white(MS_ZM_lm, interactions = TRUE)
MS_ZM_bptest <- bptest(MS_ZM_lm, studentize = TRUE)
MS_ZM_DWtest <- dwtest(MS_ZM_lm)
MS_ZM_bgtest <- bgtest(MS_ZM_lm, order = 1)
MS_ZM_jbtest <- jarque.bera.test(resid(MS_ZM_lm))
MS_ZM_shtest <- shapiro.test(resid(MS_ZM_lm))

# Exponential Law
MS_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Mures_New)
summary(MS_exp_lm)
MS_pred_exp_lm <- exp(predict(MS_exp_lm, newdata = RO_Census_Mures_New, 
                              interval = "prediction", level = 0.95))
MS_exp_whtest <- white(MS_exp_lm, interactions = TRUE)
MS_exp_bptest <- bptest(MS_exp_lm, studentize = TRUE)
MS_exp_DWtest <- dwtest(MS_exp_lm)
MS_exp_bgtest <- bgtest(MS_exp_lm, order = 1)
MS_exp_jbtest <- jarque.bera.test(resid(MS_exp_lm))
MS_exp_shtest <- shapiro.test(resid(MS_exp_lm))

# Lavalette Function
RO_Census_Mures_New <- RO_Census_Mures_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Mures_New) - Rank + 1))
)
MS_lav_lm <- lm(log(RO_Census_Mures_New$Population) ~ RO_Census_Mures_New$Lav_exp)
summary(MS_lav_lm)
MS_lav_kst <- exp(signif(MS_lav_lm$coef[[1]], 4))
MS_lav_chi <- signif(MS_lav_lm$coef[[2]], 4)
MS_pred_lav_lm <- exp(predict(MS_lav_lm, newdata = RO_Census_Mures_New, 
                              interval = "prediction", level = 0.95))
MS_lav_whtest <- white(MS_lav_lm, interactions = TRUE)
MS_lav_bptest <- bptest(MS_lav_lm, studentize = TRUE)
MS_lav_DWtest <- dwtest(MS_lav_lm)
MS_lav_bgtest <- bgtest(MS_lav_lm, order = 1)
MS_lav_jbtest <- jarque.bera.test(resid(MS_lav_lm))
MS_lav_shtest <- shapiro.test(resid(MS_lav_lm))

# Data Distributions
RO_Census_Mures_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Mures_New$Rank, 
                              Pop_Freq = MS_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.07,
            label = paste0("y", "==", signif(exp(MS_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(MS_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.07,
            label = paste0("R^2 ==", signif(summary(MS_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mures_New$Rank, 
                              Pop_Freq = MS_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.06,
            label = paste0("y", "==", signif(exp(MS_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(MS_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.06,
            label = paste0("R^2 ==", signif(summary(MS_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mures_New$Rank, 
                              Pop_Freq = MS_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.05,
            label = paste0("y", "==", signif(MS_ZM_cst, 4), "%.%", "(",
                           signif(MS_ZM_m, 4), "+ x)^", signif(MS_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.05,
            label = paste0("R^2 ==", signif(summary(MS_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mures_New$Rank, 
                              Pop_Freq = MS_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.04,
            label = paste0("y", "==", signif(MS_lav_kst, 4), "%.%", "x^",
                           -signif(MS_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.04,
            label = paste0("R^2 ==", signif(summary(MS_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Mures judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Mures_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Mures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mures_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Mures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mures_New %>%
     ggplot(aes(x = log(Rank + MS_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Mures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mures_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Mures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 29. Neamt
NT_out <- get_outliers(Neamt_Outliers, RO_Census_Neamt)

RO_Census_Neamt_New <- RO_Census_df %>% inner_join(
  RO_Census_Neamt %>% anti_join(
    bind_rows(
      (NT_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (NT_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (NT_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (NT_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Neamt_New <- RO_Census_Neamt_New %>% 
  left_join(RO_Census_Neamt_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
NT_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Neamt_New)
summary(NT_pw_lm)
NT_pred_pw_lm <- exp(predict(NT_pw_lm, newdata = RO_Census_Neamt_New, 
                             interval = "prediction", level = 0.95))
NT_pw_whtest <- white(NT_pw_lm, interactions = TRUE)
NT_pw_bptest <- bptest(NT_pw_lm, studentize = TRUE)
NT_pw_DWtest <- dwtest(NT_pw_lm)
NT_pw_bgtest <- bgtest(NT_pw_lm, order = 1)
NT_pw_jbtest <- jarque.bera.test(resid(NT_pw_lm))
NT_pw_shtest <- shapiro.test(resid(NT_pw_lm))

# Zipf_Mandelbrot Law
NT_ZM_prm <- get_ZM_Param("RO_Census_Neamt_New", "Population", "Rank")
NT_ZM_m <- NT_ZM_prm$m
NT_ZM_alpha <- NT_ZM_prm$alpha_ZM
NT_ZM_cst <- NT_ZM_prm$constant_ZM
NT_ZM_lm <- lm(log(RO_Census_Neamt_New$Population) ~ log(RO_Census_Neamt_New$Rank + NT_ZM_m))
summary(NT_ZM_lm)
NT_pred_ZM_lm <- exp(predict(NT_ZM_lm, newdata = RO_Census_Neamt_New, 
                             interval = "prediction", level = 0.95))
NT_ZM_whtest <- white(NT_ZM_lm, interactions = TRUE)
NT_ZM_bptest <- bptest(NT_ZM_lm, studentize = TRUE)
NT_ZM_DWtest <- dwtest(NT_ZM_lm)
NT_ZM_bgtest <- bgtest(NT_ZM_lm, order = 1)
NT_ZM_jbtest <- jarque.bera.test(resid(NT_ZM_lm))
NT_ZM_shtest <- shapiro.test(resid(NT_ZM_lm))

# Exponential Law
NT_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Neamt_New)
summary(NT_exp_lm)
NT_pred_exp_lm <- exp(predict(NT_exp_lm, newdata = RO_Census_Neamt_New, 
                              interval = "prediction", level = 0.95))
NT_exp_whtest <- white(NT_exp_lm, interactions = TRUE)
NT_exp_bptest <- bptest(NT_exp_lm, studentize = TRUE)
NT_exp_DWtest <- dwtest(NT_exp_lm)
NT_exp_bgtest <- bgtest(NT_exp_lm, order = 1)
NT_exp_jbtest <- jarque.bera.test(resid(NT_exp_lm))
NT_exp_shtest <- shapiro.test(resid(NT_exp_lm))

# Lavalette Function
RO_Census_Neamt_New <- RO_Census_Neamt_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Neamt_New) - Rank + 1))
)
NT_lav_lm <- lm(log(RO_Census_Neamt_New$Population) ~ RO_Census_Neamt_New$Lav_exp)
summary(NT_lav_lm)
NT_lav_kst <- exp(signif(NT_lav_lm$coef[[1]], 4))
NT_lav_chi <- signif(NT_lav_lm$coef[[2]], 4)
NT_pred_lav_lm <- exp(predict(NT_lav_lm, newdata = RO_Census_Neamt_New, 
                              interval = "prediction", level = 0.95))
NT_lav_whtest <- white(NT_lav_lm, interactions = TRUE)
NT_lav_bptest <- bptest(NT_lav_lm, studentize = TRUE)
NT_lav_DWtest <- dwtest(NT_lav_lm)
NT_lav_bgtest <- bgtest(NT_lav_lm, order = 1)
NT_lav_jbtest <- jarque.bera.test(resid(NT_lav_lm))
NT_lav_shtest <- shapiro.test(resid(NT_lav_lm))

# Data Distributions
RO_Census_Neamt_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Neamt_New$Rank, 
                              Pop_Freq = NT_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(exp(NT_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(NT_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(NT_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Neamt_New$Rank, 
                              Pop_Freq = NT_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.045,
            label = paste0("y", "==", signif(exp(NT_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(NT_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.045,
            label = paste0("R^2 ==", signif(summary(NT_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Neamt_New$Rank, 
                              Pop_Freq = NT_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.04,
            label = paste0("y", "==", signif(NT_ZM_cst, 4), "%.%", "(",
                           signif(NT_ZM_m, 4), "+ x)^", signif(NT_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.04,
            label = paste0("R^2 ==", signif(summary(NT_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Neamt_New$Rank, 
                              Pop_Freq = NT_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.035,
            label = paste0("y", "==", signif(NT_lav_kst, 4), "%.%", "x^",
                           -signif(NT_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.035,
            label = paste0("R^2 ==", signif(summary(NT_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Neamt judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Neamt_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Neamt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Neamt_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Neamt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Neamt_New %>%
     ggplot(aes(x = log(Rank + NT_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Neamt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Neamt_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Neamt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 30. Olt
OT_out <- get_outliers(Olt_Outliers, RO_Census_Olt)

RO_Census_Olt_New <- RO_Census_df %>% inner_join(
  RO_Census_Olt %>% anti_join(
    bind_rows(
      (OT_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (OT_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (OT_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (OT_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Olt_New <- RO_Census_Olt_New %>% 
  left_join(RO_Census_Olt_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
OT_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Olt_New)
summary(OT_pw_lm)
OT_pred_pw_lm <- exp(predict(OT_pw_lm, newdata = RO_Census_Olt_New, 
                             interval = "prediction", level = 0.95))
OT_pw_whtest <- white(OT_pw_lm, interactions = TRUE)
OT_pw_bptest <- bptest(OT_pw_lm, studentize = TRUE)
OT_pw_DWtest <- dwtest(OT_pw_lm)
OT_pw_bgtest <- bgtest(OT_pw_lm, order = 1)
OT_pw_jbtest <- jarque.bera.test(resid(OT_pw_lm))
OT_pw_shtest <- shapiro.test(resid(OT_pw_lm))

# Zipf_Mandelbrot Law
OT_ZM_prm <- get_ZM_Param("RO_Census_Olt_New", "Population", "Rank")
OT_ZM_m <- OT_ZM_prm$m
OT_ZM_alpha <- OT_ZM_prm$alpha_ZM
OT_ZM_cst <- OT_ZM_prm$constant_ZM
OT_ZM_lm <- lm(log(RO_Census_Olt_New$Population) ~ log(RO_Census_Olt_New$Rank + OT_ZM_m))
summary(OT_ZM_lm)
OT_pred_ZM_lm <- exp(predict(OT_ZM_lm, newdata = RO_Census_Olt_New, 
                             interval = "prediction", level = 0.95))
OT_ZM_whtest <- white(OT_ZM_lm, interactions = TRUE)
OT_ZM_bptest <- bptest(OT_ZM_lm, studentize = TRUE)
OT_ZM_DWtest <- dwtest(OT_ZM_lm)
OT_ZM_bgtest <- bgtest(OT_ZM_lm, order = 1)
OT_ZM_jbtest <- jarque.bera.test(resid(OT_ZM_lm))
OT_ZM_shtest <- shapiro.test(resid(OT_ZM_lm))

# Exponential Law
OT_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Olt_New)
summary(OT_exp_lm)
OT_pred_exp_lm <- exp(predict(OT_exp_lm, newdata = RO_Census_Olt_New, 
                              interval = "prediction", level = 0.95))
OT_exp_whtest <- white(OT_exp_lm, interactions = TRUE)
OT_exp_bptest <- bptest(OT_exp_lm, studentize = TRUE)
OT_exp_DWtest <- dwtest(OT_exp_lm)
OT_exp_bgtest <- bgtest(OT_exp_lm, order = 1)
OT_exp_jbtest <- jarque.bera.test(resid(OT_exp_lm))
OT_exp_shtest <- shapiro.test(resid(OT_exp_lm))

# Lavalette Function
RO_Census_Olt_New <- RO_Census_Olt_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Olt_New) - Rank + 1))
)
OT_lav_lm <- lm(log(RO_Census_Olt_New$Population) ~ RO_Census_Olt_New$Lav_exp)
summary(OT_lav_lm)
OT_lav_kst <- exp(signif(OT_lav_lm$coef[[1]], 4))
OT_lav_chi <- signif(OT_lav_lm$coef[[2]], 4)
OT_pred_lav_lm <- exp(predict(OT_lav_lm, newdata = RO_Census_Olt_New, 
                              interval = "prediction", level = 0.95))
OT_lav_whtest <- white(OT_lav_lm, interactions = TRUE)
OT_lav_bptest <- bptest(OT_lav_lm, studentize = TRUE)
OT_lav_DWtest <- dwtest(OT_lav_lm)
OT_lav_bgtest <- bgtest(OT_lav_lm, order = 1)
OT_lav_jbtest <- jarque.bera.test(resid(OT_lav_lm))
OT_lav_shtest <- shapiro.test(resid(OT_lav_lm))

# Data Distributions
RO_Census_Olt_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Olt_New$Rank, 
                              Pop_Freq = OT_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 30, y = 0.03,
            label = paste0("y", "==", signif(exp(OT_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(OT_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 60, y = 0.03,
            label = paste0("R^2 ==", signif(summary(OT_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Olt_New$Rank, 
                              Pop_Freq = OT_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 30, y = 0.025,
            label = paste0("y", "==", signif(exp(OT_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(OT_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 60, y = 0.025,
            label = paste0("R^2 ==", signif(summary(OT_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Olt_New$Rank, 
                              Pop_Freq = OT_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 30, y = 0.02,
            label = paste0("y", "==", signif(OT_ZM_cst, 4), "%.%", "(",
                           signif(OT_ZM_m, 4), "+ x)^", signif(OT_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 60, y = 0.02,
            label = paste0("R^2 ==", signif(summary(OT_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Olt_New$Rank, 
                              Pop_Freq = OT_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 30, y = 0.015,
            label = paste0("y", "==", signif(OT_lav_kst, 4), "%.%", "x^",
                           -signif(OT_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 60, y = 0.015,
            label = paste0("R^2 ==", signif(summary(OT_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Olt judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Olt_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Olt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Olt_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Olt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Olt_New %>%
     ggplot(aes(x = log(Rank + OT_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Olt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Olt_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Olt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 31. Prahova
PH_out <- get_outliers(Prahova_Outliers, RO_Census_Prahova)

RO_Census_Prahova_New <- RO_Census_df %>% inner_join(
  RO_Census_Prahova %>% anti_join(
    bind_rows(
      (PH_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (PH_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (PH_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (PH_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Prahova_New <- RO_Census_Prahova_New %>% 
  left_join(RO_Census_Prahova_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
PH_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Prahova_New)
summary(PH_pw_lm)
PH_pred_pw_lm <- exp(predict(PH_pw_lm, newdata = RO_Census_Prahova_New, 
                             interval = "prediction", level = 0.95))
PH_pw_whtest <- white(PH_pw_lm, interactions = TRUE)
PH_pw_bptest <- bptest(PH_pw_lm, studentize = TRUE)
PH_pw_DWtest <- dwtest(PH_pw_lm)
PH_pw_bgtest <- bgtest(PH_pw_lm, order = 1)
PH_pw_jbtest <- jarque.bera.test(resid(PH_pw_lm))
PH_pw_shtest <- shapiro.test(resid(PH_pw_lm))

# Zipf_Mandelbrot Law
PH_ZM_prm <- get_ZM_Param("RO_Census_Prahova_New", "Population", "Rank")
PH_ZM_m <- PH_ZM_prm$m
PH_ZM_alpha <- PH_ZM_prm$alpha_ZM
PH_ZM_cst <- PH_ZM_prm$constant_ZM
PH_ZM_lm <- lm(log(RO_Census_Prahova_New$Population) ~ log(RO_Census_Prahova_New$Rank + PH_ZM_m))
summary(PH_ZM_lm)
PH_pred_ZM_lm <- exp(predict(PH_ZM_lm, newdata = RO_Census_Prahova_New, 
                             interval = "prediction", level = 0.95))
PH_ZM_whtest <- white(PH_ZM_lm, interactions = TRUE)
PH_ZM_bptest <- bptest(PH_ZM_lm, studentize = TRUE)
PH_ZM_DWtest <- dwtest(PH_ZM_lm)
PH_ZM_bgtest <- bgtest(PH_ZM_lm, order = 1)
PH_ZM_jbtest <- jarque.bera.test(resid(PH_ZM_lm))
PH_ZM_shtest <- shapiro.test(resid(PH_ZM_lm))

# Exponential Law
PH_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Prahova_New)
summary(PH_exp_lm)
PH_pred_exp_lm <- exp(predict(PH_exp_lm, newdata = RO_Census_Prahova_New, 
                              interval = "prediction", level = 0.95))
PH_exp_whtest <- white(PH_exp_lm, interactions = TRUE)
PH_exp_bptest <- bptest(PH_exp_lm, studentize = TRUE)
PH_exp_DWtest <- dwtest(PH_exp_lm)
PH_exp_bgtest <- bgtest(PH_exp_lm, order = 1)
PH_exp_jbtest <- jarque.bera.test(resid(PH_exp_lm))
PH_exp_shtest <- shapiro.test(resid(PH_exp_lm))

# Lavalette Function
RO_Census_Prahova_New <- RO_Census_Prahova_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Prahova_New) - Rank + 1))
)
PH_lav_lm <- lm(log(RO_Census_Prahova_New$Population) ~ RO_Census_Prahova_New$Lav_exp)
summary(PH_lav_lm)
PH_lav_kst <- exp(signif(PH_lav_lm$coef[[1]], 4))
PH_lav_chi <- signif(PH_lav_lm$coef[[2]], 4)
PH_pred_lav_lm <- exp(predict(PH_lav_lm, newdata = RO_Census_Prahova_New, 
                              interval = "prediction", level = 0.95))
PH_lav_whtest <- white(PH_lav_lm, interactions = TRUE)
PH_lav_bptest <- bptest(PH_lav_lm, studentize = TRUE)
PH_lav_DWtest <- dwtest(PH_lav_lm)
PH_lav_bgtest <- bgtest(PH_lav_lm, order = 1)
PH_lav_jbtest <- jarque.bera.test(resid(PH_lav_lm))
PH_lav_shtest <- shapiro.test(resid(PH_lav_lm))

# Data Distributions
RO_Census_Prahova_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Prahova_New$Rank, 
                              Pop_Freq = PH_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.08,
            label = paste0("y", "==", signif(exp(PH_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(PH_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.08,
            label = paste0("R^2 ==", signif(summary(PH_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Prahova_New$Rank, 
                              Pop_Freq = PH_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.07,
            label = paste0("y", "==", signif(exp(PH_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(PH_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.07,
            label = paste0("R^2 ==", signif(summary(PH_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Prahova_New$Rank, 
                              Pop_Freq = PH_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.06,
            label = paste0("y", "==", signif(PH_ZM_cst, 4), "%.%", "(",
                           signif(PH_ZM_m, 4), "+ x)^", signif(PH_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.06,
            label = paste0("R^2 ==", signif(summary(PH_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Prahova_New$Rank, 
                              Pop_Freq = PH_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.05,
            label = paste0("y", "==", signif(PH_lav_kst, 4), "%.%", "x^",
                           -signif(PH_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.05,
            label = paste0("R^2 ==", signif(summary(PH_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Prahova judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Prahova_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Prahova judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Prahova_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Prahova judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Prahova_New %>%
     ggplot(aes(x = log(Rank + PH_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Prahova judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Prahova_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Prahova judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 32. Satu Mare
SM_out <- get_outliers(SatuMare_Outliers, RO_Census_SatuMare)

RO_Census_SatuMare_New <- RO_Census_df %>% inner_join(
  RO_Census_SatuMare %>% anti_join(
    bind_rows(
      (SM_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (SM_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (SM_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (SM_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n == max(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_SatuMare_New <- RO_Census_SatuMare_New %>% 
  left_join(RO_Census_SatuMare_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
SM_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_SatuMare_New)
summary(SM_pw_lm)
SM_pred_pw_lm <- exp(predict(SM_pw_lm, newdata = RO_Census_SatuMare_New, 
                             interval = "prediction", level = 0.95))
SM_pw_whtest <- white(SM_pw_lm, interactions = TRUE)
SM_pw_bptest <- bptest(SM_pw_lm, studentize = TRUE)
SM_pw_DWtest <- dwtest(SM_pw_lm)
SM_pw_bgtest <- bgtest(SM_pw_lm, order = 1)
SM_pw_jbtest <- jarque.bera.test(resid(SM_pw_lm))
SM_pw_shtest <- shapiro.test(resid(SM_pw_lm))

# Zipf_Mandelbrot Law
SM_ZM_prm <- get_ZM_Param("RO_Census_SatuMare_New", "Population", "Rank")
SM_ZM_m <- SM_ZM_prm$m
SM_ZM_alpha <- SM_ZM_prm$alpha_ZM
SM_ZM_cst <- SM_ZM_prm$constant_ZM
SM_ZM_lm <- lm(log(RO_Census_SatuMare_New$Population) ~ log(RO_Census_SatuMare_New$Rank + SM_ZM_m))
summary(SM_ZM_lm)
SM_pred_ZM_lm <- exp(predict(SM_ZM_lm, newdata = RO_Census_SatuMare_New, 
                             interval = "prediction", level = 0.95))
SM_ZM_whtest <- white(SM_ZM_lm, interactions = TRUE)
SM_ZM_bptest <- bptest(SM_ZM_lm, studentize = TRUE)
SM_ZM_DWtest <- dwtest(SM_ZM_lm)
SM_ZM_bgtest <- bgtest(SM_ZM_lm, order = 1)
SM_ZM_jbtest <- jarque.bera.test(resid(SM_ZM_lm))
SM_ZM_shtest <- shapiro.test(resid(SM_ZM_lm))

# Exponential Law
SM_exp_lm <- lm(log(Population) ~ Rank, RO_Census_SatuMare_New)
summary(SM_exp_lm)
SM_pred_exp_lm <- exp(predict(SM_exp_lm, newdata = RO_Census_SatuMare_New, 
                              interval = "prediction", level = 0.95))
SM_exp_whtest <- white(SM_exp_lm, interactions = TRUE)
SM_exp_bptest <- bptest(SM_exp_lm, studentize = TRUE)
SM_exp_DWtest <- dwtest(SM_exp_lm)
SM_exp_bgtest <- bgtest(SM_exp_lm, order = 1)
SM_exp_jbtest <- jarque.bera.test(resid(SM_exp_lm))
SM_exp_shtest <- shapiro.test(resid(SM_exp_lm))

# Lavalette Function
RO_Census_SatuMare_New <- RO_Census_SatuMare_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_SatuMare_New) - Rank + 1))
)
SM_lav_lm <- lm(log(RO_Census_SatuMare_New$Population) ~ RO_Census_SatuMare_New$Lav_exp)
summary(SM_lav_lm)
SM_lav_kst <- exp(signif(SM_lav_lm$coef[[1]], 4))
SM_lav_chi <- signif(SM_lav_lm$coef[[2]], 4)
SM_pred_lav_lm <- exp(predict(SM_lav_lm, newdata = RO_Census_SatuMare_New, 
                              interval = "prediction", level = 0.95))
SM_lav_whtest <- white(SM_lav_lm, interactions = TRUE)
SM_lav_bptest <- bptest(SM_lav_lm, studentize = TRUE)
SM_lav_DWtest <- dwtest(SM_lav_lm)
SM_lav_bgtest <- bgtest(SM_lav_lm, order = 1)
SM_lav_jbtest <- jarque.bera.test(resid(SM_lav_lm))
SM_lav_shtest <- shapiro.test(resid(SM_lav_lm))

# Data Distributions
RO_Census_SatuMare_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_SatuMare_New$Rank, 
                              Pop_Freq = SM_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(SM_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(SM_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.07,
            label = paste0("R^2 ==", signif(summary(SM_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_SatuMare_New$Rank, 
                              Pop_Freq = SM_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(exp(SM_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(SM_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.06,
            label = paste0("R^2 ==", signif(summary(SM_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_SatuMare_New$Rank, 
                              Pop_Freq = SM_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(SM_ZM_cst, 4), "%.%", "(",
                           signif(SM_ZM_m, 4), "+ x)^", signif(SM_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(SM_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_SatuMare_New$Rank, 
                              Pop_Freq = SM_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.04,
            label = paste0("y", "==", signif(SM_lav_kst, 4), "%.%", "x^",
                           -signif(SM_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.04,
            label = paste0("R^2 ==", signif(summary(SM_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Satu Mare judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_SatuMare_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Satu Mare judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_SatuMare_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Satu Mare judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_SatuMare_New %>%
     ggplot(aes(x = log(Rank + SM_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Satu Mare judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_SatuMare_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Satu Mare judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 33. Salaj
SJ_out <- get_outliers(Salaj_Outliers, RO_Census_Salaj)

RO_Census_Salaj_New <- RO_Census_df %>% inner_join(
  RO_Census_Salaj %>% anti_join(
    bind_rows(
      (SJ_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (SJ_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (SJ_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (SJ_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Salaj_New <- RO_Census_Salaj_New %>% 
  left_join(RO_Census_Salaj_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
SJ_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Salaj_New)
summary(SJ_pw_lm)
SJ_pred_pw_lm <- exp(predict(SJ_pw_lm, newdata = RO_Census_Salaj_New, 
                             interval = "prediction", level = 0.95))
SJ_pw_whtest <- white(SJ_pw_lm, interactions = TRUE)
SJ_pw_bptest <- bptest(SJ_pw_lm, studentize = TRUE)
SJ_pw_DWtest <- dwtest(SJ_pw_lm)
SJ_pw_bgtest <- bgtest(SJ_pw_lm, order = 1)
SJ_pw_jbtest <- jarque.bera.test(resid(SJ_pw_lm))
SJ_pw_shtest <- shapiro.test(resid(SJ_pw_lm))

# Zipf_Mandelbrot Law
SJ_ZM_prm <- get_ZM_Param("RO_Census_Salaj_New", "Population", "Rank")
SJ_ZM_m <- SJ_ZM_prm$m
SJ_ZM_alpha <- SJ_ZM_prm$alpha_ZM
SJ_ZM_cst <- SJ_ZM_prm$constant_ZM
SJ_ZM_lm <- lm(log(RO_Census_Salaj_New$Population) ~ log(RO_Census_Salaj_New$Rank + SJ_ZM_m))
summary(SJ_ZM_lm)
SJ_pred_ZM_lm <- exp(predict(SJ_ZM_lm, newdata = RO_Census_Salaj_New, 
                             interval = "prediction", level = 0.95))
SJ_ZM_whtest <- white(SJ_ZM_lm, interactions = TRUE)
SJ_ZM_bptest <- bptest(SJ_ZM_lm, studentize = TRUE)
SJ_ZM_DWtest <- dwtest(SJ_ZM_lm)
SJ_ZM_bgtest <- bgtest(SJ_ZM_lm, order = 1)
SJ_ZM_jbtest <- jarque.bera.test(resid(SJ_ZM_lm))
SJ_ZM_shtest <- shapiro.test(resid(SJ_ZM_lm))

# Exponential Law
SJ_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Salaj_New)
summary(SJ_exp_lm)
SJ_pred_exp_lm <- exp(predict(SJ_exp_lm, newdata = RO_Census_Salaj_New, 
                              interval = "prediction", level = 0.95))
SJ_exp_whtest <- white(SJ_exp_lm, interactions = TRUE)
SJ_exp_bptest <- bptest(SJ_exp_lm, studentize = TRUE)
SJ_exp_DWtest <- dwtest(SJ_exp_lm)
SJ_exp_bgtest <- bgtest(SJ_exp_lm, order = 1)
SJ_exp_jbtest <- jarque.bera.test(resid(SJ_exp_lm))
SJ_exp_shtest <- shapiro.test(resid(SJ_exp_lm))

# Lavalette Function
RO_Census_Salaj_New <- RO_Census_Salaj_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Salaj_New) - Rank + 1))
)
SJ_lav_lm <- lm(log(RO_Census_Salaj_New$Population) ~ RO_Census_Salaj_New$Lav_exp)
summary(SJ_lav_lm)
SJ_lav_kst <- exp(signif(SJ_lav_lm$coef[[1]], 4))
SJ_lav_chi <- signif(SJ_lav_lm$coef[[2]], 4)
SJ_pred_lav_lm <- exp(predict(SJ_lav_lm, newdata = RO_Census_Salaj_New, 
                              interval = "prediction", level = 0.95))
SJ_lav_whtest <- white(SJ_lav_lm, interactions = TRUE)
SJ_lav_bptest <- bptest(SJ_lav_lm, studentize = TRUE)
SJ_lav_DWtest <- dwtest(SJ_lav_lm)
SJ_lav_bgtest <- bgtest(SJ_lav_lm, order = 1)
SJ_lav_jbtest <- jarque.bera.test(resid(SJ_lav_lm))
SJ_lav_shtest <- shapiro.test(resid(SJ_lav_lm))

# Data Distributions
RO_Census_Salaj_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Salaj_New$Rank, 
                              Pop_Freq = SJ_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(exp(SJ_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(SJ_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.08,
            label = paste0("R^2 ==", signif(summary(SJ_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Salaj_New$Rank, 
                              Pop_Freq = SJ_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(SJ_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(SJ_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.07,
            label = paste0("R^2 ==", signif(summary(SJ_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Salaj_New$Rank, 
                              Pop_Freq = SJ_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(SJ_ZM_cst, 4), "%.%", "(",
                           signif(SJ_ZM_m, 4), "+ x)^", signif(SJ_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.06,
            label = paste0("R^2 ==", signif(summary(SJ_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Salaj_New$Rank, 
                              Pop_Freq = SJ_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(SJ_lav_kst, 4), "%.%", "x^",
                           -signif(SJ_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(SJ_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Salaj judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Salaj_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Salaj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Salaj_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Salaj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Salaj_New %>%
     ggplot(aes(x = log(Rank + SJ_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Salaj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Salaj_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Salaj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 34. Sibiu
SB_out <- get_outliers(Sibiu_Outliers, RO_Census_Sibiu)

RO_Census_Sibiu_New <- RO_Census_df %>% inner_join(
  RO_Census_Sibiu %>% anti_join(
    bind_rows(
      (SB_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (SB_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (SB_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (SB_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Sibiu_New <- RO_Census_Sibiu_New %>% 
  left_join(RO_Census_Sibiu_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
SB_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Sibiu_New)
summary(SB_pw_lm)
SB_pred_pw_lm <- exp(predict(SB_pw_lm, newdata = RO_Census_Sibiu_New, 
                             interval = "prediction", level = 0.95))
SB_pw_whtest <- white(SB_pw_lm, interactions = TRUE)
SB_pw_bptest <- bptest(SB_pw_lm, studentize = TRUE)
SB_pw_DWtest <- dwtest(SB_pw_lm)
SB_pw_bgtest <- bgtest(SB_pw_lm, order = 1)
SB_pw_jbtest <- jarque.bera.test(resid(SB_pw_lm))
SB_pw_shtest <- shapiro.test(resid(SB_pw_lm))

# Zipf_Mandelbrot Law
SB_ZM_prm <- get_ZM_Param("RO_Census_Sibiu_New", "Population", "Rank")
SB_ZM_m <- SB_ZM_prm$m
SB_ZM_alpha <- SB_ZM_prm$alpha_ZM
SB_ZM_cst <- SB_ZM_prm$constant_ZM
SB_ZM_lm <- lm(log(RO_Census_Sibiu_New$Population) ~ log(RO_Census_Sibiu_New$Rank + SB_ZM_m))
summary(SB_ZM_lm)
SB_pred_ZM_lm <- exp(predict(SB_ZM_lm, newdata = RO_Census_Sibiu_New, 
                             interval = "prediction", level = 0.95))
SB_ZM_whtest <- white(SB_ZM_lm, interactions = TRUE)
SB_ZM_bptest <- bptest(SB_ZM_lm, studentize = TRUE)
SB_ZM_DWtest <- dwtest(SB_ZM_lm)
SB_ZM_bgtest <- bgtest(SB_ZM_lm, order = 1)
SB_ZM_jbtest <- jarque.bera.test(resid(SB_ZM_lm))
SB_ZM_shtest <- shapiro.test(resid(SB_ZM_lm))

# Exponential Law
SB_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Sibiu_New)
summary(SB_exp_lm)
SB_pred_exp_lm <- exp(predict(SB_exp_lm, newdata = RO_Census_Sibiu_New, 
                              interval = "prediction", level = 0.95))
SB_exp_whtest <- white(SB_exp_lm, interactions = TRUE)
SB_exp_bptest <- bptest(SB_exp_lm, studentize = TRUE)
SB_exp_DWtest <- dwtest(SB_exp_lm)
SB_exp_bgtest <- bgtest(SB_exp_lm, order = 1)
SB_exp_jbtest <- jarque.bera.test(resid(SB_exp_lm))
SB_exp_shtest <- shapiro.test(resid(SB_exp_lm))

# Lavalette Function
RO_Census_Sibiu_New <- RO_Census_Sibiu_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Sibiu_New) - Rank + 1))
)
SB_lav_lm <- lm(log(RO_Census_Sibiu_New$Population) ~ RO_Census_Sibiu_New$Lav_exp)
summary(SB_lav_lm)
SB_lav_kst <- exp(signif(SB_lav_lm$coef[[1]], 4))
SB_lav_chi <- signif(SB_lav_lm$coef[[2]], 4)
SB_pred_lav_lm <- exp(predict(SB_lav_lm, newdata = RO_Census_Sibiu_New, 
                              interval = "prediction", level = 0.95))
SB_lav_whtest <- white(SB_lav_lm, interactions = TRUE)
SB_lav_bptest <- bptest(SB_lav_lm, studentize = TRUE)
SB_lav_DWtest <- dwtest(SB_lav_lm)
SB_lav_bgtest <- bgtest(SB_lav_lm, order = 1)
SB_lav_jbtest <- jarque.bera.test(resid(SB_lav_lm))
SB_lav_shtest <- shapiro.test(resid(SB_lav_lm))

# Data Distributions
RO_Census_Sibiu_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Sibiu_New$Rank, 
                              Pop_Freq = SB_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(exp(SB_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(SB_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.08,
            label = paste0("R^2 ==", signif(summary(SB_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Sibiu_New$Rank, 
                              Pop_Freq = SB_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(SB_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(SB_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.07,
            label = paste0("R^2 ==", signif(summary(SB_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Sibiu_New$Rank, 
                              Pop_Freq = SB_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(SB_ZM_cst, 4), "%.%", "(",
                           signif(SB_ZM_m, 4), "+ x)^", signif(SB_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.06,
            label = paste0("R^2 ==", signif(summary(SB_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Sibiu_New$Rank, 
                              Pop_Freq = SB_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(SB_lav_kst, 4), "%.%", "x^",
                           -signif(SB_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(SB_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Sibiu judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Sibiu_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Sibiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Sibiu_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Sibiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Sibiu_New %>%
     ggplot(aes(x = log(Rank + SB_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Sibiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Sibiu_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Sibiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 35. Suceava
SV_out <- get_outliers(Suceava_Outliers, RO_Census_Suceava)

RO_Census_Suceava_New <- RO_Census_df %>% inner_join(
  RO_Census_Suceava %>% anti_join(
    bind_rows(
      (SV_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (SV_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (SV_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (SV_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Suceava_New <- RO_Census_Suceava_New %>% 
  left_join(RO_Census_Suceava_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
SV_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Suceava_New)
summary(SV_pw_lm)
SV_pred_pw_lm <- exp(predict(SV_pw_lm, newdata = RO_Census_Suceava_New, 
                             interval = "prediction", level = 0.95))
SV_pw_whtest <- white(SV_pw_lm, interactions = TRUE)
SV_pw_bptest <- bptest(SV_pw_lm, studentize = TRUE)
SV_pw_DWtest <- dwtest(SV_pw_lm)
SV_pw_bgtest <- bgtest(SV_pw_lm, order = 1)
SV_pw_jbtest <- jarque.bera.test(resid(SV_pw_lm))
SV_pw_shtest <- shapiro.test(resid(SV_pw_lm))

# Zipf_Mandelbrot Law
SV_ZM_prm <- get_ZM_Param("RO_Census_Suceava_New", "Population", "Rank")
SV_ZM_m <- SV_ZM_prm$m
SV_ZM_alpha <- SV_ZM_prm$alpha_ZM
SV_ZM_cst <- SV_ZM_prm$constant_ZM
SV_ZM_lm <- lm(log(RO_Census_Suceava_New$Population) ~ log(RO_Census_Suceava_New$Rank + SV_ZM_m))
summary(SV_ZM_lm)
SV_pred_ZM_lm <- exp(predict(SV_ZM_lm, newdata = RO_Census_Suceava_New, 
                             interval = "prediction", level = 0.95))
SV_ZM_whtest <- white(SV_ZM_lm, interactions = TRUE)
SV_ZM_bptest <- bptest(SV_ZM_lm, studentize = TRUE)
SV_ZM_DWtest <- dwtest(SV_ZM_lm)
SV_ZM_bgtest <- bgtest(SV_ZM_lm, order = 1)
SV_ZM_jbtest <- jarque.bera.test(resid(SV_ZM_lm))
SV_ZM_shtest <- shapiro.test(resid(SV_ZM_lm))

# Exponential Law
SV_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Suceava_New)
summary(SV_exp_lm)
SV_pred_exp_lm <- exp(predict(SV_exp_lm, newdata = RO_Census_Suceava_New, 
                              interval = "prediction", level = 0.95))
SV_exp_whtest <- white(SV_exp_lm, interactions = TRUE)
SV_exp_bptest <- bptest(SV_exp_lm, studentize = TRUE)
SV_exp_DWtest <- dwtest(SV_exp_lm)
SV_exp_bgtest <- bgtest(SV_exp_lm, order = 1)
SV_exp_jbtest <- jarque.bera.test(resid(SV_exp_lm))
SV_exp_shtest <- shapiro.test(resid(SV_exp_lm))

# Lavalette Function
RO_Census_Suceava_New <- RO_Census_Suceava_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Suceava_New) - Rank + 1))
)
SV_lav_lm <- lm(log(RO_Census_Suceava_New$Population) ~ RO_Census_Suceava_New$Lav_exp)
summary(SV_lav_lm)
SV_lav_kst <- exp(signif(SV_lav_lm$coef[[1]], 4))
SV_lav_chi <- signif(SV_lav_lm$coef[[2]], 4)
SV_pred_lav_lm <- exp(predict(SV_lav_lm, newdata = RO_Census_Suceava_New, 
                              interval = "prediction", level = 0.95))
SV_lav_whtest <- white(SV_lav_lm, interactions = TRUE)
SV_lav_bptest <- bptest(SV_lav_lm, studentize = TRUE)
SV_lav_DWtest <- dwtest(SV_lav_lm)
SV_lav_bgtest <- bgtest(SV_lav_lm, order = 1)
SV_lav_jbtest <- jarque.bera.test(resid(SV_lav_lm))
SV_lav_shtest <- shapiro.test(resid(SV_lav_lm))

# Data Distributions
RO_Census_Suceava_New %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava_New$Rank, 
                              Population = SV_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 45, y = 30000,
            label = paste0("y", "==", signif(exp(SV_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(SV_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 75, y = 30000,
            label = paste0("R^2 ==", signif(summary(SV_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava_New$Rank, 
                              Population = SV_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 45, y = 25000,
            label = paste0("y", "==", signif(exp(SV_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(SV_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 75, y = 25000,
            label = paste0("R^2 ==", signif(summary(SV_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava_New$Rank, 
                              Population = SV_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 45, y = 20000,
            label = paste0("y", "==", signif(SV_ZM_cst, 4), "%.%", "(",
                           signif(SV_ZM_m, 4), "+ x)^", signif(SV_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 75, y = 20000,
            label = paste0("R^2 ==", signif(summary(SV_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava_New$Rank, 
                              Population = SV_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 45, y = 15000,
            label = paste0("y", "==", signif(SV_lav_kst, 4), "%.%", "x^",
                           -signif(SV_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 75, y = 15000,
            label = paste0("R^2 ==", signif(summary(SV_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_y_continuous(labels = scales::label_number(
    big.mark = ",", decimal.mark = ".")) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population", color = "Legend") +
  #ggtitle("Data Distribution on Suceava judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Suceava_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Suceava judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Suceava_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Suceava judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Suceava_New %>%
     ggplot(aes(x = log(Rank + SV_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Suceava judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Suceava_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Suceava judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 36. Teleorman
TR_out <- get_outliers(Teleorman_Outliers, RO_Census_Teleorman)

RO_Census_Teleorman_New <- RO_Census_df %>% inner_join(
  RO_Census_Teleorman %>% anti_join(
    bind_rows(
      (TR_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (TR_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (TR_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (TR_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Teleorman_New <- RO_Census_Teleorman_New %>% 
  left_join(RO_Census_Teleorman_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
TR_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Teleorman_New)
summary(TR_pw_lm)
TR_pred_pw_lm <- exp(predict(TR_pw_lm, newdata = RO_Census_Teleorman_New, 
                             interval = "prediction", level = 0.95))
TR_pw_whtest <- white(TR_pw_lm, interactions = TRUE)
TR_pw_bptest <- bptest(TR_pw_lm, studentize = TRUE)
TR_pw_DWtest <- dwtest(TR_pw_lm)
TR_pw_bgtest <- bgtest(TR_pw_lm, order = 1)
TR_pw_jbtest <- jarque.bera.test(resid(TR_pw_lm))
TR_pw_shtest <- shapiro.test(resid(TR_pw_lm))

# Zipf_Mandelbrot Law
TR_ZM_prm <- get_ZM_Param("RO_Census_Teleorman_New", "Population", "Rank")
TR_ZM_m <- TR_ZM_prm$m
TR_ZM_alpha <- TR_ZM_prm$alpha_ZM
TR_ZM_cst <- TR_ZM_prm$constant_ZM
TR_ZM_lm <- lm(log(RO_Census_Teleorman_New$Population) ~ log(RO_Census_Teleorman_New$Rank + TR_ZM_m))
summary(TR_ZM_lm)
TR_pred_ZM_lm <- exp(predict(TR_ZM_lm, newdata = RO_Census_Teleorman_New, 
                             interval = "prediction", level = 0.95))
TR_ZM_whtest <- white(TR_ZM_lm, interactions = TRUE)
TR_ZM_bptest <- bptest(TR_ZM_lm, studentize = TRUE)
TR_ZM_DWtest <- dwtest(TR_ZM_lm)
TR_ZM_bgtest <- bgtest(TR_ZM_lm, order = 1)
TR_ZM_jbtest <- jarque.bera.test(resid(TR_ZM_lm))
TR_ZM_shtest <- shapiro.test(resid(TR_ZM_lm))

# Exponential Law
TR_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Teleorman_New)
summary(TR_exp_lm)
TR_pred_exp_lm <- exp(predict(TR_exp_lm, newdata = RO_Census_Teleorman_New, 
                              interval = "prediction", level = 0.95))
TR_exp_whtest <- white(TR_exp_lm, interactions = TRUE)
TR_exp_bptest <- bptest(TR_exp_lm, studentize = TRUE)
TR_exp_DWtest <- dwtest(TR_exp_lm)
TR_exp_bgtest <- bgtest(TR_exp_lm, order = 1)
TR_exp_jbtest <- jarque.bera.test(resid(TR_exp_lm))
TR_exp_shtest <- shapiro.test(resid(TR_exp_lm))

# Lavalette Function
RO_Census_Teleorman_New <- RO_Census_Teleorman_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Teleorman_New) - Rank + 1))
)
TR_lav_lm <- lm(log(RO_Census_Teleorman_New$Population) ~ RO_Census_Teleorman_New$Lav_exp)
summary(TR_lav_lm)
TR_lav_kst <- exp(signif(TR_lav_lm$coef[[1]], 4))
TR_lav_chi <- signif(TR_lav_lm$coef[[2]], 4)
TR_pred_lav_lm <- exp(predict(TR_lav_lm, newdata = RO_Census_Teleorman_New, 
                              interval = "prediction", level = 0.95))
TR_lav_whtest <- white(TR_lav_lm, interactions = TRUE)
TR_lav_bptest <- bptest(TR_lav_lm, studentize = TRUE)
TR_lav_DWtest <- dwtest(TR_lav_lm)
TR_lav_bgtest <- bgtest(TR_lav_lm, order = 1)
TR_lav_jbtest <- jarque.bera.test(resid(TR_lav_lm))
TR_lav_shtest <- shapiro.test(resid(TR_lav_lm))

# Data Distributions
RO_Census_Teleorman_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Teleorman_New$Rank, 
                              Pop_Freq = TR_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.05,
            label = paste0("y", "==", signif(exp(TR_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(TR_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.05,
            label = paste0("R^2 ==", signif(summary(TR_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Teleorman_New$Rank, 
                              Pop_Freq = TR_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.045,
            label = paste0("y", "==", signif(exp(TR_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(TR_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.045,
            label = paste0("R^2 ==", signif(summary(TR_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Teleorman_New$Rank, 
                              Pop_Freq = TR_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.04,
            label = paste0("y", "==", signif(TR_ZM_cst, 4), "%.%", "(",
                           signif(TR_ZM_m, 4), "+ x)^", signif(TR_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.04,
            label = paste0("R^2 ==", signif(summary(TR_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Teleorman_New$Rank, 
                              Pop_Freq = TR_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.035,
            label = paste0("y", "==", signif(TR_lav_kst, 4), "%.%", "x^",
                           -signif(TR_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.035,
            label = paste0("R^2 ==", signif(summary(TR_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Teleorman judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Teleorman_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Teleorman judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Teleorman_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Teleorman judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Teleorman_New %>%
     ggplot(aes(x = log(Rank + TR_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Teleorman judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Teleorman_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Teleorman judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 37. Timis
TM_out <- get_outliers(Timis_Outliers, RO_Census_Timis)

RO_Census_Timis_New <- RO_Census_df %>% inner_join(
  RO_Census_Timis %>% anti_join(
    bind_rows(
      (TM_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (TM_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (TM_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (TM_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Timis_New <- RO_Census_Timis_New %>% 
  left_join(RO_Census_Timis_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
TM_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Timis_New)
summary(TM_pw_lm)
TM_pred_pw_lm <- exp(predict(TM_pw_lm, newdata = RO_Census_Timis_New, 
                             interval = "prediction", level = 0.95))
TM_pw_whtest <- white(TM_pw_lm, interactions = TRUE)
TM_pw_bptest <- bptest(TM_pw_lm, studentize = TRUE)
TM_pw_DWtest <- dwtest(TM_pw_lm)
TM_pw_bgtest <- bgtest(TM_pw_lm, order = 1)
TM_pw_jbtest <- jarque.bera.test(resid(TM_pw_lm))
TM_pw_shtest <- shapiro.test(resid(TM_pw_lm))

# Zipf_Mandelbrot Law
TM_ZM_prm <- get_ZM_Param("RO_Census_Timis_New", "Population", "Rank")
TM_ZM_m <- TM_ZM_prm$m
TM_ZM_alpha <- TM_ZM_prm$alpha_ZM
TM_ZM_cst <- TM_ZM_prm$constant_ZM
TM_ZM_lm <- lm(log(RO_Census_Timis_New$Population) ~ log(RO_Census_Timis_New$Rank + TM_ZM_m))
summary(TM_ZM_lm)
TM_pred_ZM_lm <- exp(predict(TM_ZM_lm, newdata = RO_Census_Timis_New, 
                             interval = "prediction", level = 0.95))
TM_ZM_whtest <- white(TM_ZM_lm, interactions = TRUE)
TM_ZM_bptest <- bptest(TM_ZM_lm, studentize = TRUE)
TM_ZM_DWtest <- dwtest(TM_ZM_lm)
TM_ZM_bgtest <- bgtest(TM_ZM_lm, order = 1)
TM_ZM_jbtest <- jarque.bera.test(resid(TM_ZM_lm))
TM_ZM_shtest <- shapiro.test(resid(TM_ZM_lm))

# Exponential Law
TM_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Timis_New)
summary(TM_exp_lm)
TM_pred_exp_lm <- exp(predict(TM_exp_lm, newdata = RO_Census_Timis_New, 
                              interval = "prediction", level = 0.95))
TM_exp_whtest <- white(TM_exp_lm, interactions = TRUE)
TM_exp_bptest <- bptest(TM_exp_lm, studentize = TRUE)
TM_exp_DWtest <- dwtest(TM_exp_lm)
TM_exp_bgtest <- bgtest(TM_exp_lm, order = 1)
TM_exp_jbtest <- jarque.bera.test(resid(TM_exp_lm))
TM_exp_shtest <- shapiro.test(resid(TM_exp_lm))

# Lavalette Function
RO_Census_Timis_New <- RO_Census_Timis_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Timis_New) - Rank + 1))
)
TM_lav_lm <- lm(log(RO_Census_Timis_New$Population) ~ RO_Census_Timis_New$Lav_exp)
summary(TM_lav_lm)
TM_lav_kst <- exp(signif(TM_lav_lm$coef[[1]], 4))
TM_lav_chi <- signif(TM_lav_lm$coef[[2]], 4)
TM_pred_lav_lm <- exp(predict(TM_lav_lm, newdata = RO_Census_Timis_New, 
                              interval = "prediction", level = 0.95))
TM_lav_whtest <- white(TM_lav_lm, interactions = TRUE)
TM_lav_bptest <- bptest(TM_lav_lm, studentize = TRUE)
TM_lav_DWtest <- dwtest(TM_lav_lm)
TM_lav_bgtest <- bgtest(TM_lav_lm, order = 1)
TM_lav_jbtest <- jarque.bera.test(resid(TM_lav_lm))
TM_lav_shtest <- shapiro.test(resid(TM_lav_lm))

# Data Distributions
RO_Census_Timis_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Timis_New$Rank, 
                              Pop_Freq = TM_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.08,
            label = paste0("y", "==", signif(exp(TM_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(TM_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.08,
            label = paste0("R^2 ==", signif(summary(TM_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Timis_New$Rank, 
                              Pop_Freq = TM_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.07,
            label = paste0("y", "==", signif(exp(TM_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(TM_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.07,
            label = paste0("R^2 ==", signif(summary(TM_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Timis_New$Rank, 
                              Pop_Freq = TM_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.06,
            label = paste0("y", "==", signif(TM_ZM_cst, 4), "%.%", "(",
                           signif(TM_ZM_m, 4), "+ x)^", signif(TM_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.06,
            label = paste0("R^2 ==", signif(summary(TM_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Timis_New$Rank, 
                              Pop_Freq = TM_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.05,
            label = paste0("y", "==", signif(TM_lav_kst, 4), "%.%", "x^",
                           -signif(TM_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.05,
            label = paste0("R^2 ==", signif(summary(TM_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Timis judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Timis_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Timis judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Timis_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Timis judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Timis_New %>%
     ggplot(aes(x = log(Rank + TM_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Timis judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Timis_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Timis judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 38. Tulcea
TL_out <- get_outliers(Tulcea_Outliers, RO_Census_Tulcea)

RO_Census_Tulcea_New <- RO_Census_df %>% inner_join(
  RO_Census_Tulcea %>% anti_join(
    bind_rows(
      (TL_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (TL_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (TL_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (TL_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Tulcea_New <- RO_Census_Tulcea_New %>% 
  left_join(RO_Census_Tulcea_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
TL_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Tulcea_New)
summary(TL_pw_lm)
TL_pred_pw_lm <- exp(predict(TL_pw_lm, newdata = RO_Census_Tulcea_New, 
                             interval = "prediction", level = 0.95))
TL_pw_whtest <- white(TL_pw_lm, interactions = TRUE)
TL_pw_bptest <- bptest(TL_pw_lm, studentize = TRUE)
TL_pw_DWtest <- dwtest(TL_pw_lm)
TL_pw_bgtest <- bgtest(TL_pw_lm, order = 1)
TL_pw_jbtest <- jarque.bera.test(resid(TL_pw_lm))
TL_pw_shtest <- shapiro.test(resid(TL_pw_lm))

# Zipf_Mandelbrot Law
TL_ZM_prm <- get_ZM_Param("RO_Census_Tulcea_New", "Population", "Rank")
TL_ZM_m <- TL_ZM_prm$m
TL_ZM_alpha <- TL_ZM_prm$alpha_ZM
TL_ZM_cst <- TL_ZM_prm$constant_ZM
TL_ZM_lm <- lm(log(RO_Census_Tulcea_New$Population) ~ log(RO_Census_Tulcea_New$Rank + TL_ZM_m))
summary(TL_ZM_lm)
TL_pred_ZM_lm <- exp(predict(TL_ZM_lm, newdata = RO_Census_Tulcea_New, 
                             interval = "prediction", level = 0.95))
TL_ZM_whtest <- white(TL_ZM_lm, interactions = TRUE)
TL_ZM_bptest <- bptest(TL_ZM_lm, studentize = TRUE)
TL_ZM_DWtest <- dwtest(TL_ZM_lm)
TL_ZM_bgtest <- bgtest(TL_ZM_lm, order = 1)
TL_ZM_jbtest <- jarque.bera.test(resid(TL_ZM_lm))
TL_ZM_shtest <- shapiro.test(resid(TL_ZM_lm))

# Exponential Law
TL_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Tulcea_New)
summary(TL_exp_lm)
TL_pred_exp_lm <- exp(predict(TL_exp_lm, newdata = RO_Census_Tulcea_New, 
                              interval = "prediction", level = 0.95))
TL_exp_whtest <- white(TL_exp_lm, interactions = TRUE)
TL_exp_bptest <- bptest(TL_exp_lm, studentize = TRUE)
TL_exp_DWtest <- dwtest(TL_exp_lm)
TL_exp_bgtest <- bgtest(TL_exp_lm, order = 1)
TL_exp_jbtest <- jarque.bera.test(resid(TL_exp_lm))
TL_exp_shtest <- shapiro.test(resid(TL_exp_lm))

# Lavalette Function
RO_Census_Tulcea_New <- RO_Census_Tulcea_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Tulcea_New) - Rank + 1))
)
TL_lav_lm <- lm(log(RO_Census_Tulcea_New$Population) ~ RO_Census_Tulcea_New$Lav_exp)
summary(TL_lav_lm)
TL_lav_kst <- exp(signif(TL_lav_lm$coef[[1]], 4))
TL_lav_chi <- signif(TL_lav_lm$coef[[2]], 4)
TL_pred_lav_lm <- exp(predict(TL_lav_lm, newdata = RO_Census_Tulcea_New, 
                              interval = "prediction", level = 0.95))
TL_lav_whtest <- white(TL_lav_lm, interactions = TRUE)
TL_lav_bptest <- bptest(TL_lav_lm, studentize = TRUE)
TL_lav_DWtest <- dwtest(TL_lav_lm)
TL_lav_bgtest <- bgtest(TL_lav_lm, order = 1)
TL_lav_jbtest <- jarque.bera.test(resid(TL_lav_lm))
TL_lav_shtest <- shapiro.test(resid(TL_lav_lm))

# Data Distributions
RO_Census_Tulcea_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Tulcea_New$Rank, 
                              Pop_Freq = TL_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(exp(TL_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(TL_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 30, y = 0.08,
            label = paste0("R^2 ==", signif(summary(TL_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Tulcea_New$Rank, 
                              Pop_Freq = TL_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(TL_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(TL_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 30, y = 0.07,
            label = paste0("R^2 ==", signif(summary(TL_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Tulcea_New$Rank, 
                              Pop_Freq = TL_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(TL_ZM_cst, 4), "%.%", "(",
                           signif(TL_ZM_m, 4), "+ x)^", signif(TL_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 30, y = 0.06,
            label = paste0("R^2 ==", signif(summary(TL_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Tulcea_New$Rank, 
                              Pop_Freq = TL_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(TL_lav_kst, 4), "%.%", "x^",
                           -signif(TL_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 30, y = 0.05,
            label = paste0("R^2 ==", signif(summary(TL_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Tulcea judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Tulcea_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Tulcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Tulcea_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Tulcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Tulcea_New %>%
     ggplot(aes(x = log(Rank + TL_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Tulcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Tulcea_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Tulcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 39. Vaslui
VS_out <- get_outliers(Vaslui_Outliers, RO_Census_Vaslui)

RO_Census_Vaslui_New <- RO_Census_df %>% inner_join(
  RO_Census_Vaslui %>% anti_join(
    bind_rows(
      (VS_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (VS_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (VS_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (VS_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Vaslui_New <- RO_Census_Vaslui_New %>% 
  left_join(RO_Census_Vaslui_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
VS_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Vaslui_New)
summary(VS_pw_lm)
VS_pred_pw_lm <- exp(predict(VS_pw_lm, newdata = RO_Census_Vaslui_New, 
                             interval = "prediction", level = 0.95))
VS_pw_whtest <- white(VS_pw_lm, interactions = TRUE)
VS_pw_bptest <- bptest(VS_pw_lm, studentize = TRUE)
VS_pw_DWtest <- dwtest(VS_pw_lm)
VS_pw_bgtest <- bgtest(VS_pw_lm, order = 1)
VS_pw_jbtest <- jarque.bera.test(resid(VS_pw_lm))
VS_pw_shtest <- shapiro.test(resid(VS_pw_lm))

# Zipf_Mandelbrot Law
VS_ZM_prm <- get_ZM_Param("RO_Census_Vaslui_New", "Population", "Rank")
VS_ZM_m <- VS_ZM_prm$m
VS_ZM_alpha <- VS_ZM_prm$alpha_ZM
VS_ZM_cst <- VS_ZM_prm$constant_ZM
VS_ZM_lm <- lm(log(RO_Census_Vaslui_New$Population) ~ log(RO_Census_Vaslui_New$Rank + VS_ZM_m))
summary(VS_ZM_lm)
VS_pred_ZM_lm <- exp(predict(VS_ZM_lm, newdata = RO_Census_Vaslui_New, 
                             interval = "prediction", level = 0.95))
VS_ZM_whtest <- white(VS_ZM_lm, interactions = TRUE)
VS_ZM_bptest <- bptest(VS_ZM_lm, studentize = TRUE)
VS_ZM_DWtest <- dwtest(VS_ZM_lm)
VS_ZM_bgtest <- bgtest(VS_ZM_lm, order = 1)
VS_ZM_jbtest <- jarque.bera.test(resid(VS_ZM_lm))
VS_ZM_shtest <- shapiro.test(resid(VS_ZM_lm))

# Exponential Law
VS_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Vaslui_New)
summary(VS_exp_lm)
VS_pred_exp_lm <- exp(predict(VS_exp_lm, newdata = RO_Census_Vaslui_New, 
                              interval = "prediction", level = 0.95))
VS_exp_whtest <- white(VS_exp_lm, interactions = TRUE)
VS_exp_bptest <- bptest(VS_exp_lm, studentize = TRUE)
VS_exp_DWtest <- dwtest(VS_exp_lm)
VS_exp_bgtest <- bgtest(VS_exp_lm, order = 1)
VS_exp_jbtest <- jarque.bera.test(resid(VS_exp_lm))
VS_exp_shtest <- shapiro.test(resid(VS_exp_lm))

# Lavalette Function
RO_Census_Vaslui_New <- RO_Census_Vaslui_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Vaslui_New) - Rank + 1))
)
VS_lav_lm <- lm(log(RO_Census_Vaslui_New$Population) ~ RO_Census_Vaslui_New$Lav_exp)
summary(VS_lav_lm)
VS_lav_kst <- exp(signif(VS_lav_lm$coef[[1]], 4))
VS_lav_chi <- signif(VS_lav_lm$coef[[2]], 4)
VS_pred_lav_lm <- exp(predict(VS_lav_lm, newdata = RO_Census_Vaslui_New, 
                              interval = "prediction", level = 0.95))
VS_lav_whtest <- white(VS_lav_lm, interactions = TRUE)
VS_lav_bptest <- bptest(VS_lav_lm, studentize = TRUE)
VS_lav_DWtest <- dwtest(VS_lav_lm)
VS_lav_bgtest <- bgtest(VS_lav_lm, order = 1)
VS_lav_jbtest <- jarque.bera.test(resid(VS_lav_lm))
VS_lav_shtest <- shapiro.test(resid(VS_lav_lm))

# Data Distributions
RO_Census_Vaslui_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Vaslui_New$Rank, 
                              Pop_Freq = VS_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(exp(VS_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(VS_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(VS_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vaslui_New$Rank, 
                              Pop_Freq = VS_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.045,
            label = paste0("y", "==", signif(exp(VS_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(VS_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.045,
            label = paste0("R^2 ==", signif(summary(VS_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vaslui_New$Rank, 
                              Pop_Freq = VS_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.04,
            label = paste0("y", "==", signif(VS_ZM_cst, 4), "%.%", "(",
                           signif(VS_ZM_m, 4), "+ x)^", signif(VS_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.04,
            label = paste0("R^2 ==", signif(summary(VS_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vaslui_New$Rank, 
                              Pop_Freq = VS_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.035,
            label = paste0("y", "==", signif(VS_lav_kst, 4), "%.%", "x^",
                           -signif(VS_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.035,
            label = paste0("R^2 ==", signif(summary(VS_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Vaslui judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Vaslui_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Vaslui judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vaslui_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Vaslui judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vaslui_New %>%
     ggplot(aes(x = log(Rank + VS_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Vaslui judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vaslui_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Vaslui judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 40. Valcea
VL_out <- get_outliers(Valcea_Outliers, RO_Census_Valcea)

RO_Census_Valcea_New <- RO_Census_df %>% inner_join(
  RO_Census_Valcea %>% anti_join(
    bind_rows(
      (VL_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (VL_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (VL_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (VL_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Valcea_New <- RO_Census_Valcea_New %>% 
  left_join(RO_Census_Valcea_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
VL_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Valcea_New)
summary(VL_pw_lm)
VL_pred_pw_lm <- exp(predict(VL_pw_lm, newdata = RO_Census_Valcea_New, 
                             interval = "prediction", level = 0.95))
VL_pw_whtest <- white(VL_pw_lm, interactions = TRUE)
VL_pw_bptest <- bptest(VL_pw_lm, studentize = TRUE)
VL_pw_DWtest <- dwtest(VL_pw_lm)
VL_pw_bgtest <- bgtest(VL_pw_lm, order = 1)
VL_pw_jbtest <- jarque.bera.test(resid(VL_pw_lm))
VL_pw_shtest <- shapiro.test(resid(VL_pw_lm))

# Zipf_Mandelbrot Law
VL_ZM_prm <- get_ZM_Param("RO_Census_Valcea_New", "Population", "Rank")
VL_ZM_m <- VL_ZM_prm$m
VL_ZM_alpha <- VL_ZM_prm$alpha_ZM
VL_ZM_cst <- VL_ZM_prm$constant_ZM
VL_ZM_lm <- lm(log(RO_Census_Valcea_New$Population) ~ log(RO_Census_Valcea_New$Rank + VL_ZM_m))
summary(VL_ZM_lm)
VL_pred_ZM_lm <- exp(predict(VL_ZM_lm, newdata = RO_Census_Valcea_New, 
                             interval = "prediction", level = 0.95))
VL_ZM_whtest <- white(VL_ZM_lm, interactions = TRUE)
VL_ZM_bptest <- bptest(VL_ZM_lm, studentize = TRUE)
VL_ZM_DWtest <- dwtest(VL_ZM_lm)
VL_ZM_bgtest <- bgtest(VL_ZM_lm, order = 1)
VL_ZM_jbtest <- jarque.bera.test(resid(VL_ZM_lm))
VL_ZM_shtest <- shapiro.test(resid(VL_ZM_lm))

# Exponential Law
VL_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Valcea_New)
summary(VL_exp_lm)
VL_pred_exp_lm <- exp(predict(VL_exp_lm, newdata = RO_Census_Valcea_New, 
                              interval = "prediction", level = 0.95))
VL_exp_whtest <- white(VL_exp_lm, interactions = TRUE)
VL_exp_bptest <- bptest(VL_exp_lm, studentize = TRUE)
VL_exp_DWtest <- dwtest(VL_exp_lm)
VL_exp_bgtest <- bgtest(VL_exp_lm, order = 1)
VL_exp_jbtest <- jarque.bera.test(resid(VL_exp_lm))
VL_exp_shtest <- shapiro.test(resid(VL_exp_lm))

# Lavalette Function
RO_Census_Valcea_New <- RO_Census_Valcea_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Valcea_New) - Rank + 1))
)
VL_lav_lm <- lm(log(RO_Census_Valcea_New$Population) ~ RO_Census_Valcea_New$Lav_exp)
summary(VL_lav_lm)
VL_lav_kst <- exp(signif(VL_lav_lm$coef[[1]], 4))
VL_lav_chi <- signif(VL_lav_lm$coef[[2]], 4)
VL_pred_lav_lm <- exp(predict(VL_lav_lm, newdata = RO_Census_Valcea_New, 
                              interval = "prediction", level = 0.95))
VL_lav_whtest <- white(VL_lav_lm, interactions = TRUE)
VL_lav_bptest <- bptest(VL_lav_lm, studentize = TRUE)
VL_lav_DWtest <- dwtest(VL_lav_lm)
VL_lav_bgtest <- bgtest(VL_lav_lm, order = 1)
VL_lav_jbtest <- jarque.bera.test(resid(VL_lav_lm))
VL_lav_shtest <- shapiro.test(resid(VL_lav_lm))

# Data Distributions
RO_Census_Valcea_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Valcea_New$Rank, 
                              Pop_Freq = VL_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(exp(VL_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(VL_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(VL_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Valcea_New$Rank, 
                              Pop_Freq = VL_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.045,
            label = paste0("y", "==", signif(exp(VL_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(VL_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.045,
            label = paste0("R^2 ==", signif(summary(VL_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Valcea_New$Rank, 
                              Pop_Freq = VL_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.04,
            label = paste0("y", "==", signif(VL_ZM_cst, 4), "%.%", "(",
                           signif(VL_ZM_m, 4), "+ x)^", signif(VL_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.04,
            label = paste0("R^2 ==", signif(summary(VL_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Valcea_New$Rank, 
                              Pop_Freq = VL_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.035,
            label = paste0("y", "==", signif(VL_lav_kst, 4), "%.%", "x^",
                           -signif(VL_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.035,
            label = paste0("R^2 ==", signif(summary(VL_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Valcea judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Valcea_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Valcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Valcea_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Valcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Valcea_New %>%
     ggplot(aes(x = log(Rank + VL_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Valcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Valcea_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Valcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 41. Vrancea
VN_out <- get_outliers(Vrancea_Outliers, RO_Census_Vrancea)

RO_Census_Vrancea_New <- RO_Census_df %>% inner_join(
  RO_Census_Vrancea %>% anti_join(
    bind_rows(
      (VN_out$PW %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>% 
         dplyr::select(Rank) %>% distinct()),
      (VN_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (VN_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct()),
      (VN_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         dplyr::select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Vrancea_New <- RO_Census_Vrancea_New %>% 
  left_join(RO_Census_Vrancea_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
VN_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Vrancea_New)
summary(VN_pw_lm)
VN_pred_pw_lm <- exp(predict(VN_pw_lm, newdata = RO_Census_Vrancea_New, 
                             interval = "prediction", level = 0.95))
VN_pw_whtest <- white(VN_pw_lm, interactions = TRUE)
VN_pw_bptest <- bptest(VN_pw_lm, studentize = TRUE)
VN_pw_DWtest <- dwtest(VN_pw_lm)
VN_pw_bgtest <- bgtest(VN_pw_lm, order = 1)
VN_pw_jbtest <- jarque.bera.test(resid(VN_pw_lm))
VN_pw_shtest <- shapiro.test(resid(VN_pw_lm))

# Zipf_Mandelbrot Law
VN_ZM_prm <- get_ZM_Param("RO_Census_Vrancea_New", "Population", "Rank")
VN_ZM_m <- VN_ZM_prm$m
VN_ZM_alpha <- VN_ZM_prm$alpha_ZM
VN_ZM_cst <- VN_ZM_prm$constant_ZM
VN_ZM_lm <- lm(log(RO_Census_Vrancea_New$Population) ~ log(RO_Census_Vrancea_New$Rank + VN_ZM_m))
summary(VN_ZM_lm)
VN_pred_ZM_lm <- exp(predict(VN_ZM_lm, newdata = RO_Census_Vrancea_New, 
                             interval = "prediction", level = 0.95))
VN_ZM_whtest <- white(VN_ZM_lm, interactions = TRUE)
VN_ZM_bptest <- bptest(VN_ZM_lm, studentize = TRUE)
VN_ZM_DWtest <- dwtest(VN_ZM_lm)
VN_ZM_bgtest <- bgtest(VN_ZM_lm, order = 1)
VN_ZM_jbtest <- jarque.bera.test(resid(VN_ZM_lm))
VN_ZM_shtest <- shapiro.test(resid(VN_ZM_lm))

# Exponential Law
VN_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Vrancea_New)
summary(VN_exp_lm)
VN_pred_exp_lm <- exp(predict(VN_exp_lm, newdata = RO_Census_Vrancea_New, 
                              interval = "prediction", level = 0.95))
VN_exp_whtest <- white(VN_exp_lm, interactions = TRUE)
VN_exp_bptest <- bptest(VN_exp_lm, studentize = TRUE)
VN_exp_DWtest <- dwtest(VN_exp_lm)
VN_exp_bgtest <- bgtest(VN_exp_lm, order = 1)
VN_exp_jbtest <- jarque.bera.test(resid(VN_exp_lm))
VN_exp_shtest <- shapiro.test(resid(VN_exp_lm))

# Lavalette Function
RO_Census_Vrancea_New <- RO_Census_Vrancea_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Vrancea_New) - Rank + 1))
)
VN_lav_lm <- lm(log(RO_Census_Vrancea_New$Population) ~ RO_Census_Vrancea_New$Lav_exp)
summary(VN_lav_lm)
VN_lav_kst <- exp(signif(VN_lav_lm$coef[[1]], 4))
VN_lav_chi <- signif(VN_lav_lm$coef[[2]], 4)
VN_pred_lav_lm <- exp(predict(VN_lav_lm, newdata = RO_Census_Vrancea_New, 
                              interval = "prediction", level = 0.95))
VN_lav_whtest <- white(VN_lav_lm, interactions = TRUE)
VN_lav_bptest <- bptest(VN_lav_lm, studentize = TRUE)
VN_lav_DWtest <- dwtest(VN_lav_lm)
VN_lav_bgtest <- bgtest(VN_lav_lm, order = 1)
VN_lav_jbtest <- jarque.bera.test(resid(VN_lav_lm))
VN_lav_shtest <- shapiro.test(resid(VN_lav_lm))

# Data Distributions
RO_Census_Vrancea_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Vrancea_New$Rank, 
                              Pop_Freq = VN_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(VN_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(VN_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.07,
            label = paste0("R^2 ==", signif(summary(VN_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vrancea_New$Rank, 
                              Pop_Freq = VN_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(exp(VN_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(VN_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.06,
            label = paste0("R^2 ==", signif(summary(VN_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vrancea_New$Rank, 
                              Pop_Freq = VN_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(VN_ZM_cst, 4), "%.%", "(",
                           signif(VN_ZM_m, 4), "+ x)^", signif(VN_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.05,
            label = paste0("R^2 ==", signif(summary(VN_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vrancea_New$Rank, 
                              Pop_Freq = VN_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.04,
            label = paste0("y", "==", signif(VN_lav_kst, 4), "%.%", "x^",
                           -signif(VN_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.04,
            label = paste0("R^2 ==", signif(summary(VN_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Vrancea judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Vrancea_New %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Vrancea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vrancea_New %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Vrancea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vrancea_New %>%
     ggplot(aes(x = log(Rank + VN_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Vrancea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vrancea_New %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Vrancea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# III. Create tables with metadata from regressions

# 1. R-Squared
RSquared_t <- bind_rows(
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Power",
    R_Squared = c(
      signif(summary(AB_pw_lm)$r.squared, 4), signif(summary(AR_pw_lm)$r.squared, 4),
      signif(summary(AG_pw_lm)$r.squared, 4), signif(summary(BC_pw_lm)$r.squared, 4),
      signif(summary(BH_pw_lm)$r.squared, 4), signif(summary(BN_pw_lm)$r.squared, 4),
      signif(summary(BT_pw_lm)$r.squared, 4), signif(summary(BV_pw_lm)$r.squared, 4),
      signif(summary(BR_pw_lm)$r.squared, 4), signif(summary(BZ_pw_lm)$r.squared, 4),
      signif(summary(CS_pw_lm)$r.squared, 4), signif(summary(CL_pw_lm)$r.squared, 4),
      signif(summary(CJ_pw_lm)$r.squared, 4), signif(summary(CT_pw_lm)$r.squared, 4),
      signif(summary(CV_pw_lm)$r.squared, 4), signif(summary(DB_pw_lm)$r.squared, 4),
      signif(summary(DJ_pw_lm)$r.squared, 4), signif(summary(GL_pw_lm)$r.squared, 4),
      signif(summary(GR_pw_lm)$r.squared, 4), signif(summary(GJ_pw_lm)$r.squared, 4),
      signif(summary(HR_pw_lm)$r.squared, 4), signif(summary(HD_pw_lm)$r.squared, 4),
      signif(summary(IL_pw_lm)$r.squared, 4), signif(summary(IS_pw_lm)$r.squared, 4),
      signif(summary(IF_pw_lm)$r.squared, 4), signif(summary(MM_pw_lm)$r.squared, 4),
      signif(summary(MH_pw_lm)$r.squared, 4), signif(summary(MS_pw_lm)$r.squared, 4),
      signif(summary(NT_pw_lm)$r.squared, 4), signif(summary(OT_pw_lm)$r.squared, 4),
      signif(summary(PH_pw_lm)$r.squared, 4), signif(summary(SM_pw_lm)$r.squared, 4),
      signif(summary(SJ_pw_lm)$r.squared, 4), signif(summary(SB_pw_lm)$r.squared, 4),
      signif(summary(SV_pw_lm)$r.squared, 4), signif(summary(TR_pw_lm)$r.squared, 4),
      signif(summary(TM_pw_lm)$r.squared, 4), signif(summary(TL_pw_lm)$r.squared, 4),
      signif(summary(VS_pw_lm)$r.squared, 4), signif(summary(VL_pw_lm)$r.squared, 4),
      signif(summary(VN_pw_lm)$r.squared, 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Exponential",
    R_Squared = c(
      signif(summary(AB_exp_lm)$r.squared, 4), signif(summary(AR_exp_lm)$r.squared, 4),
      signif(summary(AG_exp_lm)$r.squared, 4), signif(summary(BC_exp_lm)$r.squared, 4),
      signif(summary(BH_exp_lm)$r.squared, 4), signif(summary(BN_exp_lm)$r.squared, 4),
      signif(summary(BT_exp_lm)$r.squared, 4), signif(summary(BV_exp_lm)$r.squared, 4),
      signif(summary(BR_exp_lm)$r.squared, 4), signif(summary(BZ_exp_lm)$r.squared, 4),
      signif(summary(CS_exp_lm)$r.squared, 4), signif(summary(CL_exp_lm)$r.squared, 4),
      signif(summary(CJ_exp_lm)$r.squared, 4), signif(summary(CT_exp_lm)$r.squared, 4),
      signif(summary(CV_exp_lm)$r.squared, 4), signif(summary(DB_exp_lm)$r.squared, 4),
      signif(summary(DJ_exp_lm)$r.squared, 4), signif(summary(GL_exp_lm)$r.squared, 4),
      signif(summary(GR_exp_lm)$r.squared, 4), signif(summary(GJ_exp_lm)$r.squared, 4),
      signif(summary(HR_exp_lm)$r.squared, 4), signif(summary(HD_exp_lm)$r.squared, 4),
      signif(summary(IL_exp_lm)$r.squared, 4), signif(summary(IS_exp_lm)$r.squared, 4),
      signif(summary(IF_exp_lm)$r.squared, 4), signif(summary(MM_exp_lm)$r.squared, 4),
      signif(summary(MH_exp_lm)$r.squared, 4), signif(summary(MS_exp_lm)$r.squared, 4),
      signif(summary(NT_exp_lm)$r.squared, 4), signif(summary(OT_exp_lm)$r.squared, 4),
      signif(summary(PH_exp_lm)$r.squared, 4), signif(summary(SM_exp_lm)$r.squared, 4),
      signif(summary(SJ_exp_lm)$r.squared, 4), signif(summary(SB_exp_lm)$r.squared, 4),
      signif(summary(SV_exp_lm)$r.squared, 4), signif(summary(TR_exp_lm)$r.squared, 4),
      signif(summary(TM_exp_lm)$r.squared, 4), signif(summary(TL_exp_lm)$r.squared, 4),
      signif(summary(VS_exp_lm)$r.squared, 4), signif(summary(VL_exp_lm)$r.squared, 4),
      signif(summary(VN_exp_lm)$r.squared, 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Zipf-Mandelbrot",
    R_Squared = c(
      signif(summary(AB_ZM_lm)$r.squared, 4), signif(summary(AR_ZM_lm)$r.squared, 4),
      signif(summary(AG_ZM_lm)$r.squared, 4), signif(summary(BC_ZM_lm)$r.squared, 4),
      signif(summary(BH_ZM_lm)$r.squared, 4), signif(summary(BN_ZM_lm)$r.squared, 4),
      signif(summary(BT_ZM_lm)$r.squared, 4), signif(summary(BV_ZM_lm)$r.squared, 4),
      signif(summary(BR_ZM_lm)$r.squared, 4), signif(summary(BZ_ZM_lm)$r.squared, 4),
      signif(summary(CS_ZM_lm)$r.squared, 4), signif(summary(CL_ZM_lm)$r.squared, 4),
      signif(summary(CJ_ZM_lm)$r.squared, 4), signif(summary(CT_ZM_lm)$r.squared, 4),
      signif(summary(CV_ZM_lm)$r.squared, 4), signif(summary(DB_ZM_lm)$r.squared, 4),
      signif(summary(DJ_ZM_lm)$r.squared, 4), signif(summary(GL_ZM_lm)$r.squared, 4),
      signif(summary(GR_ZM_lm)$r.squared, 4), signif(summary(GJ_ZM_lm)$r.squared, 4),
      signif(summary(HR_ZM_lm)$r.squared, 4), signif(summary(HD_ZM_lm)$r.squared, 4),
      signif(summary(IL_ZM_lm)$r.squared, 4), signif(summary(IS_ZM_lm)$r.squared, 4),
      signif(summary(IF_ZM_lm)$r.squared, 4), signif(summary(MM_ZM_lm)$r.squared, 4),
      signif(summary(MH_ZM_lm)$r.squared, 4), signif(summary(MS_ZM_lm)$r.squared, 4),
      signif(summary(NT_ZM_lm)$r.squared, 4), signif(summary(OT_ZM_lm)$r.squared, 4),
      signif(summary(PH_ZM_lm)$r.squared, 4), signif(summary(SM_ZM_lm)$r.squared, 4),
      signif(summary(SJ_ZM_lm)$r.squared, 4), signif(summary(SB_ZM_lm)$r.squared, 4),
      signif(summary(SV_ZM_lm)$r.squared, 4), signif(summary(TR_ZM_lm)$r.squared, 4),
      signif(summary(TM_ZM_lm)$r.squared, 4), signif(summary(TL_ZM_lm)$r.squared, 4),
      signif(summary(VS_ZM_lm)$r.squared, 4), signif(summary(VL_ZM_lm)$r.squared, 4),
      signif(summary(VN_ZM_lm)$r.squared, 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Lavalette",
    R_Squared = c(
      signif(summary(AB_lav_lm)$r.squared, 4), signif(summary(AR_lav_lm)$r.squared, 4),
      signif(summary(AG_lav_lm)$r.squared, 4), signif(summary(BC_lav_lm)$r.squared, 4),
      signif(summary(BH_lav_lm)$r.squared, 4), signif(summary(BN_lav_lm)$r.squared, 4),
      signif(summary(BT_lav_lm)$r.squared, 4), signif(summary(BV_lav_lm)$r.squared, 4),
      signif(summary(BR_lav_lm)$r.squared, 4), signif(summary(BZ_lav_lm)$r.squared, 4),
      signif(summary(CS_lav_lm)$r.squared, 4), signif(summary(CL_lav_lm)$r.squared, 4),
      signif(summary(CJ_lav_lm)$r.squared, 4), signif(summary(CT_lav_lm)$r.squared, 4),
      signif(summary(CV_lav_lm)$r.squared, 4), signif(summary(DB_lav_lm)$r.squared, 4),
      signif(summary(DJ_lav_lm)$r.squared, 4), signif(summary(GL_lav_lm)$r.squared, 4),
      signif(summary(GR_lav_lm)$r.squared, 4), signif(summary(GJ_lav_lm)$r.squared, 4),
      signif(summary(HR_lav_lm)$r.squared, 4), signif(summary(HD_lav_lm)$r.squared, 4),
      signif(summary(IL_lav_lm)$r.squared, 4), signif(summary(IS_lav_lm)$r.squared, 4),
      signif(summary(IF_lav_lm)$r.squared, 4), signif(summary(MM_lav_lm)$r.squared, 4),
      signif(summary(MH_lav_lm)$r.squared, 4), signif(summary(MS_lav_lm)$r.squared, 4),
      signif(summary(NT_lav_lm)$r.squared, 4), signif(summary(OT_lav_lm)$r.squared, 4),
      signif(summary(PH_lav_lm)$r.squared, 4), signif(summary(SM_lav_lm)$r.squared, 4),
      signif(summary(SJ_lav_lm)$r.squared, 4), signif(summary(SB_lav_lm)$r.squared, 4),
      signif(summary(SV_lav_lm)$r.squared, 4), signif(summary(TR_lav_lm)$r.squared, 4),
      signif(summary(TM_lav_lm)$r.squared, 4), signif(summary(TL_lav_lm)$r.squared, 4),
      signif(summary(VS_lav_lm)$r.squared, 4), signif(summary(VL_lav_lm)$r.squared, 4),
      signif(summary(VN_lav_lm)$r.squared, 4))
  ))
) %>% arrange(Judet_Name, Function_Name)

# 2. Adjusted R-Squared
Adj_RSquared_t <- bind_rows(
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Power",
    Adj_R_Squared = c(
      signif(summary(AB_pw_lm)$adj.r.squared, 4), signif(summary(AR_pw_lm)$adj.r.squared, 4),
      signif(summary(AG_pw_lm)$adj.r.squared, 4), signif(summary(BC_pw_lm)$adj.r.squared, 4),
      signif(summary(BH_pw_lm)$adj.r.squared, 4), signif(summary(BN_pw_lm)$adj.r.squared, 4),
      signif(summary(BT_pw_lm)$adj.r.squared, 4), signif(summary(BV_pw_lm)$adj.r.squared, 4),
      signif(summary(BR_pw_lm)$adj.r.squared, 4), signif(summary(BZ_pw_lm)$adj.r.squared, 4),
      signif(summary(CS_pw_lm)$adj.r.squared, 4), signif(summary(CL_pw_lm)$adj.r.squared, 4),
      signif(summary(CJ_pw_lm)$adj.r.squared, 4), signif(summary(CT_pw_lm)$adj.r.squared, 4),
      signif(summary(CV_pw_lm)$adj.r.squared, 4), signif(summary(DB_pw_lm)$adj.r.squared, 4),
      signif(summary(DJ_pw_lm)$adj.r.squared, 4), signif(summary(GL_pw_lm)$adj.r.squared, 4),
      signif(summary(GR_pw_lm)$adj.r.squared, 4), signif(summary(GJ_pw_lm)$adj.r.squared, 4),
      signif(summary(HR_pw_lm)$adj.r.squared, 4), signif(summary(HD_pw_lm)$adj.r.squared, 4),
      signif(summary(IL_pw_lm)$adj.r.squared, 4), signif(summary(IS_pw_lm)$adj.r.squared, 4),
      signif(summary(IF_pw_lm)$adj.r.squared, 4), signif(summary(MM_pw_lm)$adj.r.squared, 4),
      signif(summary(MH_pw_lm)$adj.r.squared, 4), signif(summary(MS_pw_lm)$adj.r.squared, 4),
      signif(summary(NT_pw_lm)$adj.r.squared, 4), signif(summary(OT_pw_lm)$adj.r.squared, 4),
      signif(summary(PH_pw_lm)$adj.r.squared, 4), signif(summary(SM_pw_lm)$adj.r.squared, 4),
      signif(summary(SJ_pw_lm)$adj.r.squared, 4), signif(summary(SB_pw_lm)$adj.r.squared, 4),
      signif(summary(SV_pw_lm)$adj.r.squared, 4), signif(summary(TR_pw_lm)$adj.r.squared, 4),
      signif(summary(TM_pw_lm)$adj.r.squared, 4), signif(summary(TL_pw_lm)$adj.r.squared, 4),
      signif(summary(VS_pw_lm)$adj.r.squared, 4), signif(summary(VL_pw_lm)$adj.r.squared, 4),
      signif(summary(VN_pw_lm)$adj.r.squared, 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Exponential",
    Adj_R_Squared = c(
      signif(summary(AB_exp_lm)$adj.r.squared, 4), signif(summary(AR_exp_lm)$adj.r.squared, 4),
      signif(summary(AG_exp_lm)$adj.r.squared, 4), signif(summary(BC_exp_lm)$adj.r.squared, 4),
      signif(summary(BH_exp_lm)$adj.r.squared, 4), signif(summary(BN_exp_lm)$adj.r.squared, 4),
      signif(summary(BT_exp_lm)$adj.r.squared, 4), signif(summary(BV_exp_lm)$adj.r.squared, 4),
      signif(summary(BR_exp_lm)$adj.r.squared, 4), signif(summary(BZ_exp_lm)$adj.r.squared, 4),
      signif(summary(CS_exp_lm)$adj.r.squared, 4), signif(summary(CL_exp_lm)$adj.r.squared, 4),
      signif(summary(CJ_exp_lm)$adj.r.squared, 4), signif(summary(CT_exp_lm)$adj.r.squared, 4),
      signif(summary(CV_exp_lm)$adj.r.squared, 4), signif(summary(DB_exp_lm)$adj.r.squared, 4),
      signif(summary(DJ_exp_lm)$adj.r.squared, 4), signif(summary(GL_exp_lm)$adj.r.squared, 4),
      signif(summary(GR_exp_lm)$adj.r.squared, 4), signif(summary(GJ_exp_lm)$adj.r.squared, 4),
      signif(summary(HR_exp_lm)$adj.r.squared, 4), signif(summary(HD_exp_lm)$adj.r.squared, 4),
      signif(summary(IL_exp_lm)$adj.r.squared, 4), signif(summary(IS_exp_lm)$adj.r.squared, 4),
      signif(summary(IF_exp_lm)$adj.r.squared, 4), signif(summary(MM_exp_lm)$adj.r.squared, 4),
      signif(summary(MH_exp_lm)$adj.r.squared, 4), signif(summary(MS_exp_lm)$adj.r.squared, 4),
      signif(summary(NT_exp_lm)$adj.r.squared, 4), signif(summary(OT_exp_lm)$adj.r.squared, 4),
      signif(summary(PH_exp_lm)$adj.r.squared, 4), signif(summary(SM_exp_lm)$adj.r.squared, 4),
      signif(summary(SJ_exp_lm)$adj.r.squared, 4), signif(summary(SB_exp_lm)$adj.r.squared, 4),
      signif(summary(SV_exp_lm)$adj.r.squared, 4), signif(summary(TR_exp_lm)$adj.r.squared, 4),
      signif(summary(TM_exp_lm)$adj.r.squared, 4), signif(summary(TL_exp_lm)$adj.r.squared, 4),
      signif(summary(VS_exp_lm)$adj.r.squared, 4), signif(summary(VL_exp_lm)$adj.r.squared, 4),
      signif(summary(VN_exp_lm)$adj.r.squared, 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Zipf-Mandelbrot",
    Adj_R_Squared = c(
      signif(summary(AB_ZM_lm)$adj.r.squared, 4), signif(summary(AR_ZM_lm)$adj.r.squared, 4),
      signif(summary(AG_ZM_lm)$adj.r.squared, 4), signif(summary(BC_ZM_lm)$adj.r.squared, 4),
      signif(summary(BH_ZM_lm)$adj.r.squared, 4), signif(summary(BN_ZM_lm)$adj.r.squared, 4),
      signif(summary(BT_ZM_lm)$adj.r.squared, 4), signif(summary(BV_ZM_lm)$adj.r.squared, 4),
      signif(summary(BR_ZM_lm)$adj.r.squared, 4), signif(summary(BZ_ZM_lm)$adj.r.squared, 4),
      signif(summary(CS_ZM_lm)$adj.r.squared, 4), signif(summary(CL_ZM_lm)$adj.r.squared, 4),
      signif(summary(CJ_ZM_lm)$adj.r.squared, 4), signif(summary(CT_ZM_lm)$adj.r.squared, 4),
      signif(summary(CV_ZM_lm)$adj.r.squared, 4), signif(summary(DB_ZM_lm)$adj.r.squared, 4),
      signif(summary(DJ_ZM_lm)$adj.r.squared, 4), signif(summary(GL_ZM_lm)$adj.r.squared, 4),
      signif(summary(GR_ZM_lm)$adj.r.squared, 4), signif(summary(GJ_ZM_lm)$adj.r.squared, 4),
      signif(summary(HR_ZM_lm)$adj.r.squared, 4), signif(summary(HD_ZM_lm)$adj.r.squared, 4),
      signif(summary(IL_ZM_lm)$adj.r.squared, 4), signif(summary(IS_ZM_lm)$adj.r.squared, 4),
      signif(summary(IF_ZM_lm)$adj.r.squared, 4), signif(summary(MM_ZM_lm)$adj.r.squared, 4),
      signif(summary(MH_ZM_lm)$adj.r.squared, 4), signif(summary(MS_ZM_lm)$adj.r.squared, 4),
      signif(summary(NT_ZM_lm)$adj.r.squared, 4), signif(summary(OT_ZM_lm)$adj.r.squared, 4),
      signif(summary(PH_ZM_lm)$adj.r.squared, 4), signif(summary(SM_ZM_lm)$adj.r.squared, 4),
      signif(summary(SJ_ZM_lm)$adj.r.squared, 4), signif(summary(SB_ZM_lm)$adj.r.squared, 4),
      signif(summary(SV_ZM_lm)$adj.r.squared, 4), signif(summary(TR_ZM_lm)$adj.r.squared, 4),
      signif(summary(TM_ZM_lm)$adj.r.squared, 4), signif(summary(TL_ZM_lm)$adj.r.squared, 4),
      signif(summary(VS_ZM_lm)$adj.r.squared, 4), signif(summary(VL_ZM_lm)$adj.r.squared, 4),
      signif(summary(VN_ZM_lm)$adj.r.squared, 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Lavalette",
    Adj_R_Squared = c(
      signif(summary(AB_lav_lm)$adj.r.squared, 4), signif(summary(AR_lav_lm)$adj.r.squared, 4),
      signif(summary(AG_lav_lm)$adj.r.squared, 4), signif(summary(BC_lav_lm)$adj.r.squared, 4),
      signif(summary(BH_lav_lm)$adj.r.squared, 4), signif(summary(BN_lav_lm)$adj.r.squared, 4),
      signif(summary(BT_lav_lm)$adj.r.squared, 4), signif(summary(BV_lav_lm)$adj.r.squared, 4),
      signif(summary(BR_lav_lm)$adj.r.squared, 4), signif(summary(BZ_lav_lm)$adj.r.squared, 4),
      signif(summary(CS_lav_lm)$adj.r.squared, 4), signif(summary(CL_lav_lm)$adj.r.squared, 4),
      signif(summary(CJ_lav_lm)$adj.r.squared, 4), signif(summary(CT_lav_lm)$adj.r.squared, 4),
      signif(summary(CV_lav_lm)$adj.r.squared, 4), signif(summary(DB_lav_lm)$adj.r.squared, 4),
      signif(summary(DJ_lav_lm)$adj.r.squared, 4), signif(summary(GL_lav_lm)$adj.r.squared, 4),
      signif(summary(GR_lav_lm)$adj.r.squared, 4), signif(summary(GJ_lav_lm)$adj.r.squared, 4),
      signif(summary(HR_lav_lm)$adj.r.squared, 4), signif(summary(HD_lav_lm)$adj.r.squared, 4),
      signif(summary(IL_lav_lm)$adj.r.squared, 4), signif(summary(IS_lav_lm)$adj.r.squared, 4),
      signif(summary(IF_lav_lm)$adj.r.squared, 4), signif(summary(MM_lav_lm)$adj.r.squared, 4),
      signif(summary(MH_lav_lm)$adj.r.squared, 4), signif(summary(MS_lav_lm)$adj.r.squared, 4),
      signif(summary(NT_lav_lm)$adj.r.squared, 4), signif(summary(OT_lav_lm)$adj.r.squared, 4),
      signif(summary(PH_lav_lm)$adj.r.squared, 4), signif(summary(SM_lav_lm)$adj.r.squared, 4),
      signif(summary(SJ_lav_lm)$adj.r.squared, 4), signif(summary(SB_lav_lm)$adj.r.squared, 4),
      signif(summary(SV_lav_lm)$adj.r.squared, 4), signif(summary(TR_lav_lm)$adj.r.squared, 4),
      signif(summary(TM_lav_lm)$adj.r.squared, 4), signif(summary(TL_lav_lm)$adj.r.squared, 4),
      signif(summary(VS_lav_lm)$adj.r.squared, 4), signif(summary(VL_lav_lm)$adj.r.squared, 4),
      signif(summary(VN_lav_lm)$adj.r.squared, 4))
  ))
) %>% arrange(Judet_Name, Function_Name)

# 3. F-Statistic
F_Stat_t <- bind_rows(
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Power",
    F_Stat = c(
      signif(summary(AB_pw_lm)$fstatistic[[1]], 4), signif(summary(AR_pw_lm)$fstatistic[[1]], 4),
      signif(summary(AG_pw_lm)$fstatistic[[1]], 4), signif(summary(BC_pw_lm)$fstatistic[[1]], 4),
      signif(summary(BH_pw_lm)$fstatistic[[1]], 4), signif(summary(BN_pw_lm)$fstatistic[[1]], 4),
      signif(summary(BT_pw_lm)$fstatistic[[1]], 4), signif(summary(BV_pw_lm)$fstatistic[[1]], 4),
      signif(summary(BR_pw_lm)$fstatistic[[1]], 4), signif(summary(BZ_pw_lm)$fstatistic[[1]], 4),
      signif(summary(CS_pw_lm)$fstatistic[[1]], 4), signif(summary(CL_pw_lm)$fstatistic[[1]], 4),
      signif(summary(CJ_pw_lm)$fstatistic[[1]], 4), signif(summary(CT_pw_lm)$fstatistic[[1]], 4),
      signif(summary(CV_pw_lm)$fstatistic[[1]], 4), signif(summary(DB_pw_lm)$fstatistic[[1]], 4),
      signif(summary(DJ_pw_lm)$fstatistic[[1]], 4), signif(summary(GL_pw_lm)$fstatistic[[1]], 4),
      signif(summary(GR_pw_lm)$fstatistic[[1]], 4), signif(summary(GJ_pw_lm)$fstatistic[[1]], 4),
      signif(summary(HR_pw_lm)$fstatistic[[1]], 4), signif(summary(HD_pw_lm)$fstatistic[[1]], 4),
      signif(summary(IL_pw_lm)$fstatistic[[1]], 4), signif(summary(IS_pw_lm)$fstatistic[[1]], 4),
      signif(summary(IF_pw_lm)$fstatistic[[1]], 4), signif(summary(MM_pw_lm)$fstatistic[[1]], 4),
      signif(summary(MH_pw_lm)$fstatistic[[1]], 4), signif(summary(MS_pw_lm)$fstatistic[[1]], 4),
      signif(summary(NT_pw_lm)$fstatistic[[1]], 4), signif(summary(OT_pw_lm)$fstatistic[[1]], 4),
      signif(summary(PH_pw_lm)$fstatistic[[1]], 4), signif(summary(SM_pw_lm)$fstatistic[[1]], 4),
      signif(summary(SJ_pw_lm)$fstatistic[[1]], 4), signif(summary(SB_pw_lm)$fstatistic[[1]], 4),
      signif(summary(SV_pw_lm)$fstatistic[[1]], 4), signif(summary(TR_pw_lm)$fstatistic[[1]], 4),
      signif(summary(TM_pw_lm)$fstatistic[[1]], 4), signif(summary(TL_pw_lm)$fstatistic[[1]], 4),
      signif(summary(VS_pw_lm)$fstatistic[[1]], 4), signif(summary(VL_pw_lm)$fstatistic[[1]], 4),
      signif(summary(VN_pw_lm)$fstatistic[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Exponential",
    F_Stat = c(
      signif(summary(AB_exp_lm)$fstatistic[[1]], 4), signif(summary(AR_exp_lm)$fstatistic[[1]], 4),
      signif(summary(AG_exp_lm)$fstatistic[[1]], 4), signif(summary(BC_exp_lm)$fstatistic[[1]], 4),
      signif(summary(BH_exp_lm)$fstatistic[[1]], 4), signif(summary(BN_exp_lm)$fstatistic[[1]], 4),
      signif(summary(BT_exp_lm)$fstatistic[[1]], 4), signif(summary(BV_exp_lm)$fstatistic[[1]], 4),
      signif(summary(BR_exp_lm)$fstatistic[[1]], 4), signif(summary(BZ_exp_lm)$fstatistic[[1]], 4),
      signif(summary(CS_exp_lm)$fstatistic[[1]], 4), signif(summary(CL_exp_lm)$fstatistic[[1]], 4),
      signif(summary(CJ_exp_lm)$fstatistic[[1]], 4), signif(summary(CT_exp_lm)$fstatistic[[1]], 4),
      signif(summary(CV_exp_lm)$fstatistic[[1]], 4), signif(summary(DB_exp_lm)$fstatistic[[1]], 4),
      signif(summary(DJ_exp_lm)$fstatistic[[1]], 4), signif(summary(GL_exp_lm)$fstatistic[[1]], 4),
      signif(summary(GR_exp_lm)$fstatistic[[1]], 4), signif(summary(GJ_exp_lm)$fstatistic[[1]], 4),
      signif(summary(HR_exp_lm)$fstatistic[[1]], 4), signif(summary(HD_exp_lm)$fstatistic[[1]], 4),
      signif(summary(IL_exp_lm)$fstatistic[[1]], 4), signif(summary(IS_exp_lm)$fstatistic[[1]], 4),
      signif(summary(IF_exp_lm)$fstatistic[[1]], 4), signif(summary(MM_exp_lm)$fstatistic[[1]], 4),
      signif(summary(MH_exp_lm)$fstatistic[[1]], 4), signif(summary(MS_exp_lm)$fstatistic[[1]], 4),
      signif(summary(NT_exp_lm)$fstatistic[[1]], 4), signif(summary(OT_exp_lm)$fstatistic[[1]], 4),
      signif(summary(PH_exp_lm)$fstatistic[[1]], 4), signif(summary(SM_exp_lm)$fstatistic[[1]], 4),
      signif(summary(SJ_exp_lm)$fstatistic[[1]], 4), signif(summary(SB_exp_lm)$fstatistic[[1]], 4),
      signif(summary(SV_exp_lm)$fstatistic[[1]], 4), signif(summary(TR_exp_lm)$fstatistic[[1]], 4),
      signif(summary(TM_exp_lm)$fstatistic[[1]], 4), signif(summary(TL_exp_lm)$fstatistic[[1]], 4),
      signif(summary(VS_exp_lm)$fstatistic[[1]], 4), signif(summary(VL_exp_lm)$fstatistic[[1]], 4),
      signif(summary(VN_exp_lm)$fstatistic[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Zipf-Mandelbrot",
    F_Stat = c(
      signif(summary(AB_ZM_lm)$fstatistic[[1]], 4), signif(summary(AR_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(AG_ZM_lm)$fstatistic[[1]], 4), signif(summary(BC_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(BH_ZM_lm)$fstatistic[[1]], 4), signif(summary(BN_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(BT_ZM_lm)$fstatistic[[1]], 4), signif(summary(BV_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(BR_ZM_lm)$fstatistic[[1]], 4), signif(summary(BZ_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(CS_ZM_lm)$fstatistic[[1]], 4), signif(summary(CL_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(CJ_ZM_lm)$fstatistic[[1]], 4), signif(summary(CT_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(CV_ZM_lm)$fstatistic[[1]], 4), signif(summary(DB_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(DJ_ZM_lm)$fstatistic[[1]], 4), signif(summary(GL_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(GR_ZM_lm)$fstatistic[[1]], 4), signif(summary(GJ_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(HR_ZM_lm)$fstatistic[[1]], 4), signif(summary(HD_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(IL_ZM_lm)$fstatistic[[1]], 4), signif(summary(IS_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(IF_ZM_lm)$fstatistic[[1]], 4), signif(summary(MM_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(MH_ZM_lm)$fstatistic[[1]], 4), signif(summary(MS_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(NT_ZM_lm)$fstatistic[[1]], 4), signif(summary(OT_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(PH_ZM_lm)$fstatistic[[1]], 4), signif(summary(SM_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(SJ_ZM_lm)$fstatistic[[1]], 4), signif(summary(SB_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(SV_ZM_lm)$fstatistic[[1]], 4), signif(summary(TR_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(TM_ZM_lm)$fstatistic[[1]], 4), signif(summary(TL_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(VS_ZM_lm)$fstatistic[[1]], 4), signif(summary(VL_ZM_lm)$fstatistic[[1]], 4),
      signif(summary(VN_ZM_lm)$fstatistic[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Lavalette",
    F_Stat = c(
      signif(summary(AB_lav_lm)$fstatistic[[1]], 4), signif(summary(AR_lav_lm)$fstatistic[[1]], 4),
      signif(summary(AG_lav_lm)$fstatistic[[1]], 4), signif(summary(BC_lav_lm)$fstatistic[[1]], 4),
      signif(summary(BH_lav_lm)$fstatistic[[1]], 4), signif(summary(BN_lav_lm)$fstatistic[[1]], 4),
      signif(summary(BT_lav_lm)$fstatistic[[1]], 4), signif(summary(BV_lav_lm)$fstatistic[[1]], 4),
      signif(summary(BR_lav_lm)$fstatistic[[1]], 4), signif(summary(BZ_lav_lm)$fstatistic[[1]], 4),
      signif(summary(CS_lav_lm)$fstatistic[[1]], 4), signif(summary(CL_lav_lm)$fstatistic[[1]], 4),
      signif(summary(CJ_lav_lm)$fstatistic[[1]], 4), signif(summary(CT_lav_lm)$fstatistic[[1]], 4),
      signif(summary(CV_lav_lm)$fstatistic[[1]], 4), signif(summary(DB_lav_lm)$fstatistic[[1]], 4),
      signif(summary(DJ_lav_lm)$fstatistic[[1]], 4), signif(summary(GL_lav_lm)$fstatistic[[1]], 4),
      signif(summary(GR_lav_lm)$fstatistic[[1]], 4), signif(summary(GJ_lav_lm)$fstatistic[[1]], 4),
      signif(summary(HR_lav_lm)$fstatistic[[1]], 4), signif(summary(HD_lav_lm)$fstatistic[[1]], 4),
      signif(summary(IL_lav_lm)$fstatistic[[1]], 4), signif(summary(IS_lav_lm)$fstatistic[[1]], 4),
      signif(summary(IF_lav_lm)$fstatistic[[1]], 4), signif(summary(MM_lav_lm)$fstatistic[[1]], 4),
      signif(summary(MH_lav_lm)$fstatistic[[1]], 4), signif(summary(MS_lav_lm)$fstatistic[[1]], 4),
      signif(summary(NT_lav_lm)$fstatistic[[1]], 4), signif(summary(OT_lav_lm)$fstatistic[[1]], 4),
      signif(summary(PH_lav_lm)$fstatistic[[1]], 4), signif(summary(SM_lav_lm)$fstatistic[[1]], 4),
      signif(summary(SJ_lav_lm)$fstatistic[[1]], 4), signif(summary(SB_lav_lm)$fstatistic[[1]], 4),
      signif(summary(SV_lav_lm)$fstatistic[[1]], 4), signif(summary(TR_lav_lm)$fstatistic[[1]], 4),
      signif(summary(TM_lav_lm)$fstatistic[[1]], 4), signif(summary(TL_lav_lm)$fstatistic[[1]], 4),
      signif(summary(VS_lav_lm)$fstatistic[[1]], 4), signif(summary(VL_lav_lm)$fstatistic[[1]], 4),
      signif(summary(VN_lav_lm)$fstatistic[[1]], 4))
  ))
) %>% arrange(Judet_Name, Function_Name)

# 4. AIC
AIC_t <- bind_rows(
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Power",
    AIC = c(
      signif(AIC(AB_pw_lm), 4), signif(AIC(AR_pw_lm), 4), 
      signif(AIC(AG_pw_lm), 4), signif(AIC(BC_pw_lm), 4),
      signif(AIC(BH_pw_lm), 4), signif(AIC(BN_pw_lm), 4),
      signif(AIC(BT_pw_lm), 4), signif(AIC(BV_pw_lm), 4),
      signif(AIC(BR_pw_lm), 4), signif(AIC(BZ_pw_lm), 4),
      signif(AIC(CS_pw_lm), 4), signif(AIC(CL_pw_lm), 4),
      signif(AIC(CJ_pw_lm), 4), signif(AIC(CT_pw_lm), 4),
      signif(AIC(CV_pw_lm), 4), signif(AIC(DB_pw_lm), 4),
      signif(AIC(DJ_pw_lm), 4), signif(AIC(GL_pw_lm), 4),
      signif(AIC(GR_pw_lm), 4), signif(AIC(GJ_pw_lm), 4),
      signif(AIC(HR_pw_lm), 4), signif(AIC(HD_pw_lm), 4),
      signif(AIC(IL_pw_lm), 4), signif(AIC(IS_pw_lm), 4),
      signif(AIC(IF_pw_lm), 4), signif(AIC(MM_pw_lm), 4),
      signif(AIC(MH_pw_lm), 4), signif(AIC(MS_pw_lm), 4),
      signif(AIC(NT_pw_lm), 4), signif(AIC(OT_pw_lm), 4),
      signif(AIC(PH_pw_lm), 4), signif(AIC(SM_pw_lm), 4),
      signif(AIC(SJ_pw_lm), 4), signif(AIC(SB_pw_lm), 4),
      signif(AIC(SV_pw_lm), 4), signif(AIC(TR_pw_lm), 4),
      signif(AIC(TM_pw_lm), 4), signif(AIC(TL_pw_lm), 4),
      signif(AIC(VS_pw_lm), 4), signif(AIC(VL_pw_lm), 4),
      signif(AIC(VN_pw_lm), 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Exponential",
    AIC = c(
      signif(AIC(AB_exp_lm), 4), signif(AIC(AR_exp_lm), 4),
      signif(AIC(AG_exp_lm), 4), signif(AIC(BC_exp_lm), 4),
      signif(AIC(BH_exp_lm), 4), signif(AIC(BN_exp_lm), 4),
      signif(AIC(BT_exp_lm), 4), signif(AIC(BV_exp_lm), 4),
      signif(AIC(BR_exp_lm), 4), signif(AIC(BZ_exp_lm), 4),
      signif(AIC(CS_exp_lm), 4), signif(AIC(CL_exp_lm), 4),
      signif(AIC(CJ_exp_lm), 4), signif(AIC(CT_exp_lm), 4),
      signif(AIC(CV_exp_lm), 4), signif(AIC(DB_exp_lm), 4),
      signif(AIC(DJ_exp_lm), 4), signif(AIC(GL_exp_lm), 4),
      signif(AIC(GR_exp_lm), 4), signif(AIC(GJ_exp_lm), 4),
      signif(AIC(HR_exp_lm), 4), signif(AIC(HD_exp_lm), 4),
      signif(AIC(IL_exp_lm), 4), signif(AIC(IS_exp_lm), 4),
      signif(AIC(IF_exp_lm), 4), signif(AIC(MM_exp_lm), 4),
      signif(AIC(MH_exp_lm), 4), signif(AIC(MS_exp_lm), 4),
      signif(AIC(NT_exp_lm), 4), signif(AIC(OT_exp_lm), 4),
      signif(AIC(PH_exp_lm), 4), signif(AIC(SM_exp_lm), 4),
      signif(AIC(SJ_exp_lm), 4), signif(AIC(SB_exp_lm), 4),
      signif(AIC(SV_exp_lm), 4), signif(AIC(TR_exp_lm), 4),
      signif(AIC(TM_exp_lm), 4), signif(AIC(TL_exp_lm), 4),
      signif(AIC(VS_exp_lm), 4), signif(AIC(VL_exp_lm), 4),
      signif(AIC(VN_exp_lm), 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Zipf-Mandelbrot",
    AIC = c(
      signif(AIC(AB_ZM_lm), 4), signif(AIC(AR_ZM_lm), 4),
      signif(AIC(AG_ZM_lm), 4), signif(AIC(BC_ZM_lm), 4),
      signif(AIC(BH_ZM_lm), 4), signif(AIC(BN_ZM_lm), 4),
      signif(AIC(BT_ZM_lm), 4), signif(AIC(BV_ZM_lm), 4),
      signif(AIC(BR_ZM_lm), 4), signif(AIC(BZ_ZM_lm), 4),
      signif(AIC(CS_ZM_lm), 4), signif(AIC(CL_ZM_lm), 4),
      signif(AIC(CJ_ZM_lm), 4), signif(AIC(CT_ZM_lm), 4),
      signif(AIC(CV_ZM_lm), 4), signif(AIC(DB_ZM_lm), 4),
      signif(AIC(DJ_ZM_lm), 4), signif(AIC(GL_ZM_lm), 4),
      signif(AIC(GR_ZM_lm), 4), signif(AIC(GJ_ZM_lm), 4),
      signif(AIC(HR_ZM_lm), 4), signif(AIC(HD_ZM_lm), 4),
      signif(AIC(IL_ZM_lm), 4), signif(AIC(IS_ZM_lm), 4),
      signif(AIC(IF_ZM_lm), 4), signif(AIC(MM_ZM_lm), 4),
      signif(AIC(MH_ZM_lm), 4), signif(AIC(MS_ZM_lm), 4),
      signif(AIC(NT_ZM_lm), 4), signif(AIC(OT_ZM_lm), 4),
      signif(AIC(PH_ZM_lm), 4), signif(AIC(SM_ZM_lm), 4),
      signif(AIC(SJ_ZM_lm), 4), signif(AIC(SB_ZM_lm), 4),
      signif(AIC(SV_ZM_lm), 4), signif(AIC(TR_ZM_lm), 4),
      signif(AIC(TM_ZM_lm), 4), signif(AIC(TL_ZM_lm), 4),
      signif(AIC(VS_ZM_lm), 4), signif(AIC(VL_ZM_lm), 4),
      signif(AIC(VN_ZM_lm), 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Lavalette",
    AIC = c(
      signif(AIC(AB_lav_lm), 4), signif(AIC(AR_lav_lm), 4),
      signif(AIC(AG_lav_lm), 4), signif(AIC(BC_lav_lm), 4),
      signif(AIC(BH_lav_lm), 4), signif(AIC(BN_lav_lm), 4),
      signif(AIC(BT_lav_lm), 4), signif(AIC(BV_lav_lm), 4),
      signif(AIC(BR_lav_lm), 4), signif(AIC(BZ_lav_lm), 4),
      signif(AIC(CS_lav_lm), 4), signif(AIC(CL_lav_lm), 4),
      signif(AIC(CJ_lav_lm), 4), signif(AIC(CT_lav_lm), 4),
      signif(AIC(CV_lav_lm), 4), signif(AIC(DB_lav_lm), 4),
      signif(AIC(DJ_lav_lm), 4), signif(AIC(GL_lav_lm), 4),
      signif(AIC(GR_lav_lm), 4), signif(AIC(GJ_lav_lm), 4),
      signif(AIC(HR_lav_lm), 4), signif(AIC(HD_lav_lm), 4),
      signif(AIC(IL_lav_lm), 4), signif(AIC(IS_lav_lm), 4),
      signif(AIC(IF_lav_lm), 4), signif(AIC(MM_lav_lm), 4),
      signif(AIC(MH_lav_lm), 4), signif(AIC(MS_lav_lm), 4),
      signif(AIC(NT_lav_lm), 4), signif(AIC(OT_lav_lm), 4),
      signif(AIC(PH_lav_lm), 4), signif(AIC(SM_lav_lm), 4),
      signif(AIC(SJ_lav_lm), 4), signif(AIC(SB_lav_lm), 4),
      signif(AIC(SV_lav_lm), 4), signif(AIC(TR_lav_lm), 4),
      signif(AIC(TM_lav_lm), 4), signif(AIC(TL_lav_lm), 4),
      signif(AIC(VS_lav_lm), 4), signif(AIC(VL_lav_lm), 4),
      signif(AIC(VN_lav_lm), 4))
  ))
) %>% arrange(Judet_Name, Function_Name)

# 7. White Test
White_test_t <- bind_rows(
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Power",
    White_Test = c(
      signif(AB_pw_whtest$statistic, 4), signif(AR_pw_whtest$statistic, 4), 
      signif(AG_pw_whtest$statistic, 4), signif(BC_pw_whtest$statistic, 4),
      signif(BH_pw_whtest$statistic, 4), signif(BN_pw_whtest$statistic, 4),
      signif(BT_pw_whtest$statistic, 4), signif(BV_pw_whtest$statistic, 4),
      signif(BR_pw_whtest$statistic, 4), signif(BZ_pw_whtest$statistic, 4),
      signif(CS_pw_whtest$statistic, 4), signif(CL_pw_whtest$statistic, 4),
      signif(CJ_pw_whtest$statistic, 4), signif(CT_pw_whtest$statistic, 4),
      signif(CV_pw_whtest$statistic, 4), signif(DB_pw_whtest$statistic, 4),
      signif(DJ_pw_whtest$statistic, 4), signif(GL_pw_whtest$statistic, 4),
      signif(GR_pw_whtest$statistic, 4), signif(GJ_pw_whtest$statistic, 4),
      signif(HR_pw_whtest$statistic, 4), signif(HD_pw_whtest$statistic, 4),
      signif(IL_pw_whtest$statistic, 4), signif(IS_pw_whtest$statistic, 4),
      signif(IF_pw_whtest$statistic, 4), signif(MM_pw_whtest$statistic, 4),
      signif(MH_pw_whtest$statistic, 4), signif(MS_pw_whtest$statistic, 4),
      signif(NT_pw_whtest$statistic, 4), signif(OT_pw_whtest$statistic, 4),
      signif(PH_pw_whtest$statistic, 4), signif(SM_pw_whtest$statistic, 4),
      signif(SJ_pw_whtest$statistic, 4), signif(SB_pw_whtest$statistic, 4),
      signif(SV_pw_whtest$statistic, 4), signif(TR_pw_whtest$statistic, 4),
      signif(TM_pw_whtest$statistic, 4), signif(TL_pw_whtest$statistic, 4),
      signif(VS_pw_whtest$statistic, 4), signif(VL_pw_whtest$statistic, 4),
      signif(VN_pw_whtest$statistic, 4)),
    White_PValue = c(
      signif(AB_pw_whtest$p.value, 4), signif(AR_pw_whtest$p.value, 4), 
      signif(AG_pw_whtest$p.value, 4), signif(BC_pw_whtest$p.value, 4),
      signif(BH_pw_whtest$p.value, 4), signif(BN_pw_whtest$p.value, 4),
      signif(BT_pw_whtest$p.value, 4), signif(BV_pw_whtest$p.value, 4),
      signif(BR_pw_whtest$p.value, 4), signif(BZ_pw_whtest$p.value, 4),
      signif(CS_pw_whtest$p.value, 4), signif(CL_pw_whtest$p.value, 4),
      signif(CJ_pw_whtest$p.value, 4), signif(CT_pw_whtest$p.value, 4),
      signif(CV_pw_whtest$p.value, 4), signif(DB_pw_whtest$p.value, 4),
      signif(DJ_pw_whtest$p.value, 4), signif(GL_pw_whtest$p.value, 4),
      signif(GR_pw_whtest$p.value, 4), signif(GJ_pw_whtest$p.value, 4),
      signif(HR_pw_whtest$p.value, 4), signif(HD_pw_whtest$p.value, 4),
      signif(IL_pw_whtest$p.value, 4), signif(IS_pw_whtest$p.value, 4),
      signif(IF_pw_whtest$p.value, 4), signif(MM_pw_whtest$p.value, 4),
      signif(MH_pw_whtest$p.value, 4), signif(MS_pw_whtest$p.value, 4),
      signif(NT_pw_whtest$p.value, 4), signif(OT_pw_whtest$p.value, 4),
      signif(PH_pw_whtest$p.value, 4), signif(SM_pw_whtest$p.value, 4),
      signif(SJ_pw_whtest$p.value, 4), signif(SB_pw_whtest$p.value, 4),
      signif(SV_pw_whtest$p.value, 4), signif(TR_pw_whtest$p.value, 4),
      signif(TM_pw_whtest$p.value, 4), signif(TL_pw_whtest$p.value, 4),
      signif(VS_pw_whtest$p.value, 4), signif(VL_pw_whtest$p.value, 4),
      signif(VN_pw_whtest$p.value, 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Exponential",
    White_Test = c(
      signif(AB_exp_whtest$statistic, 4), signif(AR_exp_whtest$statistic, 4), 
      signif(AG_exp_whtest$statistic, 4), signif(BC_exp_whtest$statistic, 4),
      signif(BH_exp_whtest$statistic, 4), signif(BN_exp_whtest$statistic, 4),
      signif(BT_exp_whtest$statistic, 4), signif(BV_exp_whtest$statistic, 4),
      signif(BR_exp_whtest$statistic, 4), signif(BZ_exp_whtest$statistic, 4),
      signif(CS_exp_whtest$statistic, 4), signif(CL_exp_whtest$statistic, 4),
      signif(CJ_exp_whtest$statistic, 4), signif(CT_exp_whtest$statistic, 4),
      signif(CV_exp_whtest$statistic, 4), signif(DB_exp_whtest$statistic, 4),
      signif(DJ_exp_whtest$statistic, 4), signif(GL_exp_whtest$statistic, 4),
      signif(GR_exp_whtest$statistic, 4), signif(GJ_exp_whtest$statistic, 4),
      signif(HR_exp_whtest$statistic, 4), signif(HD_exp_whtest$statistic, 4),
      signif(IL_exp_whtest$statistic, 4), signif(IS_exp_whtest$statistic, 4),
      signif(IF_exp_whtest$statistic, 4), signif(MM_exp_whtest$statistic, 4),
      signif(MH_exp_whtest$statistic, 4), signif(MS_exp_whtest$statistic, 4),
      signif(NT_exp_whtest$statistic, 4), signif(OT_exp_whtest$statistic, 4),
      signif(PH_exp_whtest$statistic, 4), signif(SM_exp_whtest$statistic, 4),
      signif(SJ_exp_whtest$statistic, 4), signif(SB_exp_whtest$statistic, 4),
      signif(SV_exp_whtest$statistic, 4), signif(TR_exp_whtest$statistic, 4),
      signif(TM_exp_whtest$statistic, 4), signif(TL_exp_whtest$statistic, 4),
      signif(VS_exp_whtest$statistic, 4), signif(VL_exp_whtest$statistic, 4),
      signif(VN_exp_whtest$statistic, 4)),
    White_PValue = c(
      signif(AB_exp_whtest$p.value, 4), signif(AR_exp_whtest$p.value, 4), 
      signif(AG_exp_whtest$p.value, 4), signif(BC_exp_whtest$p.value, 4),
      signif(BH_exp_whtest$p.value, 4), signif(BN_exp_whtest$p.value, 4),
      signif(BT_exp_whtest$p.value, 4), signif(BV_exp_whtest$p.value, 4),
      signif(BR_exp_whtest$p.value, 4), signif(BZ_exp_whtest$p.value, 4),
      signif(CS_exp_whtest$p.value, 4), signif(CL_exp_whtest$p.value, 4),
      signif(CJ_exp_whtest$p.value, 4), signif(CT_exp_whtest$p.value, 4),
      signif(CV_exp_whtest$p.value, 4), signif(DB_exp_whtest$p.value, 4),
      signif(DJ_exp_whtest$p.value, 4), signif(GL_exp_whtest$p.value, 4),
      signif(GR_exp_whtest$p.value, 4), signif(GJ_exp_whtest$p.value, 4),
      signif(HR_exp_whtest$p.value, 4), signif(HD_exp_whtest$p.value, 4),
      signif(IL_exp_whtest$p.value, 4), signif(IS_exp_whtest$p.value, 4),
      signif(IF_exp_whtest$p.value, 4), signif(MM_exp_whtest$p.value, 4),
      signif(MH_exp_whtest$p.value, 4), signif(MS_exp_whtest$p.value, 4),
      signif(NT_exp_whtest$p.value, 4), signif(OT_exp_whtest$p.value, 4),
      signif(PH_exp_whtest$p.value, 4), signif(SM_exp_whtest$p.value, 4),
      signif(SJ_exp_whtest$p.value, 4), signif(SB_exp_whtest$p.value, 4),
      signif(SV_exp_whtest$p.value, 4), signif(TR_exp_whtest$p.value, 4),
      signif(TM_exp_whtest$p.value, 4), signif(TL_exp_whtest$p.value, 4),
      signif(VS_exp_whtest$p.value, 4), signif(VL_exp_whtest$p.value, 4),
      signif(VN_exp_whtest$p.value, 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Zipf-Mandelbrot",
    White_Test = c(
      signif(AB_ZM_whtest$statistic, 4), signif(AR_ZM_whtest$statistic, 4), 
      signif(AG_ZM_whtest$statistic, 4), signif(BC_ZM_whtest$statistic, 4),
      signif(BH_ZM_whtest$statistic, 4), signif(BN_ZM_whtest$statistic, 4),
      signif(BT_ZM_whtest$statistic, 4), signif(BV_ZM_whtest$statistic, 4),
      signif(BR_ZM_whtest$statistic, 4), signif(BZ_ZM_whtest$statistic, 4),
      signif(CS_ZM_whtest$statistic, 4), signif(CL_ZM_whtest$statistic, 4),
      signif(CJ_ZM_whtest$statistic, 4), signif(CT_ZM_whtest$statistic, 4),
      signif(CV_ZM_whtest$statistic, 4), signif(DB_ZM_whtest$statistic, 4),
      signif(DJ_ZM_whtest$statistic, 4), signif(GL_ZM_whtest$statistic, 4),
      signif(GR_ZM_whtest$statistic, 4), signif(GJ_ZM_whtest$statistic, 4),
      signif(HR_ZM_whtest$statistic, 4), signif(HD_ZM_whtest$statistic, 4),
      signif(IL_ZM_whtest$statistic, 4), signif(IS_ZM_whtest$statistic, 4),
      signif(IF_ZM_whtest$statistic, 4), signif(MM_ZM_whtest$statistic, 4),
      signif(MH_ZM_whtest$statistic, 4), signif(MS_ZM_whtest$statistic, 4),
      signif(NT_ZM_whtest$statistic, 4), signif(OT_ZM_whtest$statistic, 4),
      signif(PH_ZM_whtest$statistic, 4), signif(SM_ZM_whtest$statistic, 4),
      signif(SJ_ZM_whtest$statistic, 4), signif(SB_ZM_whtest$statistic, 4),
      signif(SV_ZM_whtest$statistic, 4), signif(TR_ZM_whtest$statistic, 4),
      signif(TM_ZM_whtest$statistic, 4), signif(TL_ZM_whtest$statistic, 4),
      signif(VS_ZM_whtest$statistic, 4), signif(VL_ZM_whtest$statistic, 4),
      signif(VN_ZM_whtest$statistic, 4)),
    White_PValue = c(
      signif(AB_ZM_whtest$p.value, 4), signif(AR_ZM_whtest$p.value, 4), 
      signif(AG_ZM_whtest$p.value, 4), signif(BC_ZM_whtest$p.value, 4),
      signif(BH_ZM_whtest$p.value, 4), signif(BN_ZM_whtest$p.value, 4),
      signif(BT_ZM_whtest$p.value, 4), signif(BV_ZM_whtest$p.value, 4),
      signif(BR_ZM_whtest$p.value, 4), signif(BZ_ZM_whtest$p.value, 4),
      signif(CS_ZM_whtest$p.value, 4), signif(CL_ZM_whtest$p.value, 4),
      signif(CJ_ZM_whtest$p.value, 4), signif(CT_ZM_whtest$p.value, 4),
      signif(CV_ZM_whtest$p.value, 4), signif(DB_ZM_whtest$p.value, 4),
      signif(DJ_ZM_whtest$p.value, 4), signif(GL_ZM_whtest$p.value, 4),
      signif(GR_ZM_whtest$p.value, 4), signif(GJ_ZM_whtest$p.value, 4),
      signif(HR_ZM_whtest$p.value, 4), signif(HD_ZM_whtest$p.value, 4),
      signif(IL_ZM_whtest$p.value, 4), signif(IS_ZM_whtest$p.value, 4),
      signif(IF_ZM_whtest$p.value, 4), signif(MM_ZM_whtest$p.value, 4),
      signif(MH_ZM_whtest$p.value, 4), signif(MS_ZM_whtest$p.value, 4),
      signif(NT_ZM_whtest$p.value, 4), signif(OT_ZM_whtest$p.value, 4),
      signif(PH_ZM_whtest$p.value, 4), signif(SM_ZM_whtest$p.value, 4),
      signif(SJ_ZM_whtest$p.value, 4), signif(SB_ZM_whtest$p.value, 4),
      signif(SV_ZM_whtest$p.value, 4), signif(TR_ZM_whtest$p.value, 4),
      signif(TM_ZM_whtest$p.value, 4), signif(TL_ZM_whtest$p.value, 4),
      signif(VS_ZM_whtest$p.value, 4), signif(VL_ZM_whtest$p.value, 4),
      signif(VN_ZM_whtest$p.value, 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Lavalette",
    White_Test = c(
      signif(AB_lav_whtest$statistic, 4), signif(AR_lav_whtest$statistic, 4), 
      signif(AG_lav_whtest$statistic, 4), signif(BC_lav_whtest$statistic, 4),
      signif(BH_lav_whtest$statistic, 4), signif(BN_lav_whtest$statistic, 4),
      signif(BT_lav_whtest$statistic, 4), signif(BV_lav_whtest$statistic, 4),
      signif(BR_lav_whtest$statistic, 4), signif(BZ_lav_whtest$statistic, 4),
      signif(CS_lav_whtest$statistic, 4), signif(CL_lav_whtest$statistic, 4),
      signif(CJ_lav_whtest$statistic, 4), signif(CT_lav_whtest$statistic, 4),
      signif(CV_lav_whtest$statistic, 4), signif(DB_lav_whtest$statistic, 4),
      signif(DJ_lav_whtest$statistic, 4), signif(GL_lav_whtest$statistic, 4),
      signif(GR_lav_whtest$statistic, 4), signif(GJ_lav_whtest$statistic, 4),
      signif(HR_lav_whtest$statistic, 4), signif(HD_lav_whtest$statistic, 4),
      signif(IL_lav_whtest$statistic, 4), signif(IS_lav_whtest$statistic, 4),
      signif(IF_lav_whtest$statistic, 4), signif(MM_lav_whtest$statistic, 4),
      signif(MH_lav_whtest$statistic, 4), signif(MS_lav_whtest$statistic, 4),
      signif(NT_lav_whtest$statistic, 4), signif(OT_lav_whtest$statistic, 4),
      signif(PH_lav_whtest$statistic, 4), signif(SM_lav_whtest$statistic, 4),
      signif(SJ_lav_whtest$statistic, 4), signif(SB_lav_whtest$statistic, 4),
      signif(SV_lav_whtest$statistic, 4), signif(TR_lav_whtest$statistic, 4),
      signif(TM_lav_whtest$statistic, 4), signif(TL_lav_whtest$statistic, 4),
      signif(VS_lav_whtest$statistic, 4), signif(VL_lav_whtest$statistic, 4),
      signif(VN_lav_whtest$statistic, 4)),
    White_PValue = c(
      signif(AB_lav_whtest$p.value, 4), signif(AR_lav_whtest$p.value, 4), 
      signif(AG_lav_whtest$p.value, 4), signif(BC_lav_whtest$p.value, 4),
      signif(BH_lav_whtest$p.value, 4), signif(BN_lav_whtest$p.value, 4),
      signif(BT_lav_whtest$p.value, 4), signif(BV_lav_whtest$p.value, 4),
      signif(BR_lav_whtest$p.value, 4), signif(BZ_lav_whtest$p.value, 4),
      signif(CS_lav_whtest$p.value, 4), signif(CL_lav_whtest$p.value, 4),
      signif(CJ_lav_whtest$p.value, 4), signif(CT_lav_whtest$p.value, 4),
      signif(CV_lav_whtest$p.value, 4), signif(DB_lav_whtest$p.value, 4),
      signif(DJ_lav_whtest$p.value, 4), signif(GL_lav_whtest$p.value, 4),
      signif(GR_lav_whtest$p.value, 4), signif(GJ_lav_whtest$p.value, 4),
      signif(HR_lav_whtest$p.value, 4), signif(HD_lav_whtest$p.value, 4),
      signif(IL_lav_whtest$p.value, 4), signif(IS_lav_whtest$p.value, 4),
      signif(IF_lav_whtest$p.value, 4), signif(MM_lav_whtest$p.value, 4),
      signif(MH_lav_whtest$p.value, 4), signif(MS_lav_whtest$p.value, 4),
      signif(NT_lav_whtest$p.value, 4), signif(OT_lav_whtest$p.value, 4),
      signif(PH_lav_whtest$p.value, 4), signif(SM_lav_whtest$p.value, 4),
      signif(SJ_lav_whtest$p.value, 4), signif(SB_lav_whtest$p.value, 4),
      signif(SV_lav_whtest$p.value, 4), signif(TR_lav_whtest$p.value, 4),
      signif(TM_lav_whtest$p.value, 4), signif(TL_lav_whtest$p.value, 4),
      signif(VS_lav_whtest$p.value, 4), signif(VL_lav_whtest$p.value, 4),
      signif(VN_lav_whtest$p.value, 4))
  ))
) %>% mutate(Homoscedasticity = if_else(White_PValue > 0.05, "Y", "N"))

# 8. Breusch-Pagan Test
BP_test_t <- bind_rows(
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Power",
    BP_Test = c(
      signif(AB_pw_bptest$statistic[[1]], 4), signif(AR_pw_bptest$statistic[[1]], 4), 
      signif(AG_pw_bptest$statistic[[1]], 4), signif(BC_pw_bptest$statistic[[1]], 4),
      signif(BH_pw_bptest$statistic[[1]], 4), signif(BN_pw_bptest$statistic[[1]], 4),
      signif(BT_pw_bptest$statistic[[1]], 4), signif(BV_pw_bptest$statistic[[1]], 4),
      signif(BR_pw_bptest$statistic[[1]], 4), signif(BZ_pw_bptest$statistic[[1]], 4),
      signif(CS_pw_bptest$statistic[[1]], 4), signif(CL_pw_bptest$statistic[[1]], 4),
      signif(CJ_pw_bptest$statistic[[1]], 4), signif(CT_pw_bptest$statistic[[1]], 4),
      signif(CV_pw_bptest$statistic[[1]], 4), signif(DB_pw_bptest$statistic[[1]], 4),
      signif(DJ_pw_bptest$statistic[[1]], 4), signif(GL_pw_bptest$statistic[[1]], 4),
      signif(GR_pw_bptest$statistic[[1]], 4), signif(GJ_pw_bptest$statistic[[1]], 4),
      signif(HR_pw_bptest$statistic[[1]], 4), signif(HD_pw_bptest$statistic[[1]], 4),
      signif(IL_pw_bptest$statistic[[1]], 4), signif(IS_pw_bptest$statistic[[1]], 4),
      signif(IF_pw_bptest$statistic[[1]], 4), signif(MM_pw_bptest$statistic[[1]], 4),
      signif(MH_pw_bptest$statistic[[1]], 4), signif(MS_pw_bptest$statistic[[1]], 4),
      signif(NT_pw_bptest$statistic[[1]], 4), signif(OT_pw_bptest$statistic[[1]], 4),
      signif(PH_pw_bptest$statistic[[1]], 4), signif(SM_pw_bptest$statistic[[1]], 4),
      signif(SJ_pw_bptest$statistic[[1]], 4), signif(SB_pw_bptest$statistic[[1]], 4),
      signif(SV_pw_bptest$statistic[[1]], 4), signif(TR_pw_bptest$statistic[[1]], 4),
      signif(TM_pw_bptest$statistic[[1]], 4), signif(TL_pw_bptest$statistic[[1]], 4),
      signif(VS_pw_bptest$statistic[[1]], 4), signif(VL_pw_bptest$statistic[[1]], 4),
      signif(VN_pw_bptest$statistic[[1]], 4)),
    BP_PValue = c(
      signif(AB_pw_bptest$p.value[[1]], 4), signif(AR_pw_bptest$p.value[[1]], 4), 
      signif(AG_pw_bptest$p.value[[1]], 4), signif(BC_pw_bptest$p.value[[1]], 4),
      signif(BH_pw_bptest$p.value[[1]], 4), signif(BN_pw_bptest$p.value[[1]], 4),
      signif(BT_pw_bptest$p.value[[1]], 4), signif(BV_pw_bptest$p.value[[1]], 4),
      signif(BR_pw_bptest$p.value[[1]], 4), signif(BZ_pw_bptest$p.value[[1]], 4),
      signif(CS_pw_bptest$p.value[[1]], 4), signif(CL_pw_bptest$p.value[[1]], 4),
      signif(CJ_pw_bptest$p.value[[1]], 4), signif(CT_pw_bptest$p.value[[1]], 4),
      signif(CV_pw_bptest$p.value[[1]], 4), signif(DB_pw_bptest$p.value[[1]], 4),
      signif(DJ_pw_bptest$p.value[[1]], 4), signif(GL_pw_bptest$p.value[[1]], 4),
      signif(GR_pw_bptest$p.value[[1]], 4), signif(GJ_pw_bptest$p.value[[1]], 4),
      signif(HR_pw_bptest$p.value[[1]], 4), signif(HD_pw_bptest$p.value[[1]], 4),
      signif(IL_pw_bptest$p.value[[1]], 4), signif(IS_pw_bptest$p.value[[1]], 4),
      signif(IF_pw_bptest$p.value[[1]], 4), signif(MM_pw_bptest$p.value[[1]], 4),
      signif(MH_pw_bptest$p.value[[1]], 4), signif(MS_pw_bptest$p.value[[1]], 4),
      signif(NT_pw_bptest$p.value[[1]], 4), signif(OT_pw_bptest$p.value[[1]], 4),
      signif(PH_pw_bptest$p.value[[1]], 4), signif(SM_pw_bptest$p.value[[1]], 4),
      signif(SJ_pw_bptest$p.value[[1]], 4), signif(SB_pw_bptest$p.value[[1]], 4),
      signif(SV_pw_bptest$p.value[[1]], 4), signif(TR_pw_bptest$p.value[[1]], 4),
      signif(TM_pw_bptest$p.value[[1]], 4), signif(TL_pw_bptest$p.value[[1]], 4),
      signif(VS_pw_bptest$p.value[[1]], 4), signif(VL_pw_bptest$p.value[[1]], 4),
      signif(VN_pw_bptest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Exponential",
    BP_Test = c(
      signif(AB_exp_bptest$statistic[[1]], 4), signif(AR_exp_bptest$statistic[[1]], 4), 
      signif(AG_exp_bptest$statistic[[1]], 4), signif(BC_exp_bptest$statistic[[1]], 4),
      signif(BH_exp_bptest$statistic[[1]], 4), signif(BN_exp_bptest$statistic[[1]], 4),
      signif(BT_exp_bptest$statistic[[1]], 4), signif(BV_exp_bptest$statistic[[1]], 4),
      signif(BR_exp_bptest$statistic[[1]], 4), signif(BZ_exp_bptest$statistic[[1]], 4),
      signif(CS_exp_bptest$statistic[[1]], 4), signif(CL_exp_bptest$statistic[[1]], 4),
      signif(CJ_exp_bptest$statistic[[1]], 4), signif(CT_exp_bptest$statistic[[1]], 4),
      signif(CV_exp_bptest$statistic[[1]], 4), signif(DB_exp_bptest$statistic[[1]], 4),
      signif(DJ_exp_bptest$statistic[[1]], 4), signif(GL_exp_bptest$statistic[[1]], 4),
      signif(GR_exp_bptest$statistic[[1]], 4), signif(GJ_exp_bptest$statistic[[1]], 4),
      signif(HR_exp_bptest$statistic[[1]], 4), signif(HD_exp_bptest$statistic[[1]], 4),
      signif(IL_exp_bptest$statistic[[1]], 4), signif(IS_exp_bptest$statistic[[1]], 4),
      signif(IF_exp_bptest$statistic[[1]], 4), signif(MM_exp_bptest$statistic[[1]], 4),
      signif(MH_exp_bptest$statistic[[1]], 4), signif(MS_exp_bptest$statistic[[1]], 4),
      signif(NT_exp_bptest$statistic[[1]], 4), signif(OT_exp_bptest$statistic[[1]], 4),
      signif(PH_exp_bptest$statistic[[1]], 4), signif(SM_exp_bptest$statistic[[1]], 4),
      signif(SJ_exp_bptest$statistic[[1]], 4), signif(SB_exp_bptest$statistic[[1]], 4),
      signif(SV_exp_bptest$statistic[[1]], 4), signif(TR_exp_bptest$statistic[[1]], 4),
      signif(TM_exp_bptest$statistic[[1]], 4), signif(TL_exp_bptest$statistic[[1]], 4),
      signif(VS_exp_bptest$statistic[[1]], 4), signif(VL_exp_bptest$statistic[[1]], 4),
      signif(VN_exp_bptest$statistic[[1]], 4)),
    BP_PValue = c(
      signif(AB_exp_bptest$p.value[[1]], 4), signif(AR_exp_bptest$p.value[[1]], 4), 
      signif(AG_exp_bptest$p.value[[1]], 4), signif(BC_exp_bptest$p.value[[1]], 4),
      signif(BH_exp_bptest$p.value[[1]], 4), signif(BN_exp_bptest$p.value[[1]], 4),
      signif(BT_exp_bptest$p.value[[1]], 4), signif(BV_exp_bptest$p.value[[1]], 4),
      signif(BR_exp_bptest$p.value[[1]], 4), signif(BZ_exp_bptest$p.value[[1]], 4),
      signif(CS_exp_bptest$p.value[[1]], 4), signif(CL_exp_bptest$p.value[[1]], 4),
      signif(CJ_exp_bptest$p.value[[1]], 4), signif(CT_exp_bptest$p.value[[1]], 4),
      signif(CV_exp_bptest$p.value[[1]], 4), signif(DB_exp_bptest$p.value[[1]], 4),
      signif(DJ_exp_bptest$p.value[[1]], 4), signif(GL_exp_bptest$p.value[[1]], 4),
      signif(GR_exp_bptest$p.value[[1]], 4), signif(GJ_exp_bptest$p.value[[1]], 4),
      signif(HR_exp_bptest$p.value[[1]], 4), signif(HD_exp_bptest$p.value[[1]], 4),
      signif(IL_exp_bptest$p.value[[1]], 4), signif(IS_exp_bptest$p.value[[1]], 4),
      signif(IF_exp_bptest$p.value[[1]], 4), signif(MM_exp_bptest$p.value[[1]], 4),
      signif(MH_exp_bptest$p.value[[1]], 4), signif(MS_exp_bptest$p.value[[1]], 4),
      signif(NT_exp_bptest$p.value[[1]], 4), signif(OT_exp_bptest$p.value[[1]], 4),
      signif(PH_exp_bptest$p.value[[1]], 4), signif(SM_exp_bptest$p.value[[1]], 4),
      signif(SJ_exp_bptest$p.value[[1]], 4), signif(SB_exp_bptest$p.value[[1]], 4),
      signif(SV_exp_bptest$p.value[[1]], 4), signif(TR_exp_bptest$p.value[[1]], 4),
      signif(TM_exp_bptest$p.value[[1]], 4), signif(TL_exp_bptest$p.value[[1]], 4),
      signif(VS_exp_bptest$p.value[[1]], 4), signif(VL_exp_bptest$p.value[[1]], 4),
      signif(VN_exp_bptest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Zipf-Mandelbrot",
    BP_Test = c(
      signif(AB_ZM_bptest$statistic[[1]], 4), signif(AR_ZM_bptest$statistic[[1]], 4), 
      signif(AG_ZM_bptest$statistic[[1]], 4), signif(BC_ZM_bptest$statistic[[1]], 4),
      signif(BH_ZM_bptest$statistic[[1]], 4), signif(BN_ZM_bptest$statistic[[1]], 4),
      signif(BT_ZM_bptest$statistic[[1]], 4), signif(BV_ZM_bptest$statistic[[1]], 4),
      signif(BR_ZM_bptest$statistic[[1]], 4), signif(BZ_ZM_bptest$statistic[[1]], 4),
      signif(CS_ZM_bptest$statistic[[1]], 4), signif(CL_ZM_bptest$statistic[[1]], 4),
      signif(CJ_ZM_bptest$statistic[[1]], 4), signif(CT_ZM_bptest$statistic[[1]], 4),
      signif(CV_ZM_bptest$statistic[[1]], 4), signif(DB_ZM_bptest$statistic[[1]], 4),
      signif(DJ_ZM_bptest$statistic[[1]], 4), signif(GL_ZM_bptest$statistic[[1]], 4),
      signif(GR_ZM_bptest$statistic[[1]], 4), signif(GJ_ZM_bptest$statistic[[1]], 4),
      signif(HR_ZM_bptest$statistic[[1]], 4), signif(HD_ZM_bptest$statistic[[1]], 4),
      signif(IL_ZM_bptest$statistic[[1]], 4), signif(IS_ZM_bptest$statistic[[1]], 4),
      signif(IF_ZM_bptest$statistic[[1]], 4), signif(MM_ZM_bptest$statistic[[1]], 4),
      signif(MH_ZM_bptest$statistic[[1]], 4), signif(MS_ZM_bptest$statistic[[1]], 4),
      signif(NT_ZM_bptest$statistic[[1]], 4), signif(OT_ZM_bptest$statistic[[1]], 4),
      signif(PH_ZM_bptest$statistic[[1]], 4), signif(SM_ZM_bptest$statistic[[1]], 4),
      signif(SJ_ZM_bptest$statistic[[1]], 4), signif(SB_ZM_bptest$statistic[[1]], 4),
      signif(SV_ZM_bptest$statistic[[1]], 4), signif(TR_ZM_bptest$statistic[[1]], 4),
      signif(TM_ZM_bptest$statistic[[1]], 4), signif(TL_ZM_bptest$statistic[[1]], 4),
      signif(VS_ZM_bptest$statistic[[1]], 4), signif(VL_ZM_bptest$statistic[[1]], 4),
      signif(VN_ZM_bptest$statistic[[1]], 4)),
    BP_PValue = c(
      signif(AB_ZM_bptest$p.value[[1]], 4), signif(AR_ZM_bptest$p.value[[1]], 4), 
      signif(AG_ZM_bptest$p.value[[1]], 4), signif(BC_ZM_bptest$p.value[[1]], 4),
      signif(BH_ZM_bptest$p.value[[1]], 4), signif(BN_ZM_bptest$p.value[[1]], 4),
      signif(BT_ZM_bptest$p.value[[1]], 4), signif(BV_ZM_bptest$p.value[[1]], 4),
      signif(BR_ZM_bptest$p.value[[1]], 4), signif(BZ_ZM_bptest$p.value[[1]], 4),
      signif(CS_ZM_bptest$p.value[[1]], 4), signif(CL_ZM_bptest$p.value[[1]], 4),
      signif(CJ_ZM_bptest$p.value[[1]], 4), signif(CT_ZM_bptest$p.value[[1]], 4),
      signif(CV_ZM_bptest$p.value[[1]], 4), signif(DB_ZM_bptest$p.value[[1]], 4),
      signif(DJ_ZM_bptest$p.value[[1]], 4), signif(GL_ZM_bptest$p.value[[1]], 4),
      signif(GR_ZM_bptest$p.value[[1]], 4), signif(GJ_ZM_bptest$p.value[[1]], 4),
      signif(HR_ZM_bptest$p.value[[1]], 4), signif(HD_ZM_bptest$p.value[[1]], 4),
      signif(IL_ZM_bptest$p.value[[1]], 4), signif(IS_ZM_bptest$p.value[[1]], 4),
      signif(IF_ZM_bptest$p.value[[1]], 4), signif(MM_ZM_bptest$p.value[[1]], 4),
      signif(MH_ZM_bptest$p.value[[1]], 4), signif(MS_ZM_bptest$p.value[[1]], 4),
      signif(NT_ZM_bptest$p.value[[1]], 4), signif(OT_ZM_bptest$p.value[[1]], 4),
      signif(PH_ZM_bptest$p.value[[1]], 4), signif(SM_ZM_bptest$p.value[[1]], 4),
      signif(SJ_ZM_bptest$p.value[[1]], 4), signif(SB_ZM_bptest$p.value[[1]], 4),
      signif(SV_ZM_bptest$p.value[[1]], 4), signif(TR_ZM_bptest$p.value[[1]], 4),
      signif(TM_ZM_bptest$p.value[[1]], 4), signif(TL_ZM_bptest$p.value[[1]], 4),
      signif(VS_ZM_bptest$p.value[[1]], 4), signif(VL_ZM_bptest$p.value[[1]], 4),
      signif(VN_ZM_bptest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Lavalette",
    BP_Test = c(
      signif(AB_lav_bptest$statistic[[1]], 4), signif(AR_lav_bptest$statistic[[1]], 4), 
      signif(AG_lav_bptest$statistic[[1]], 4), signif(BC_lav_bptest$statistic[[1]], 4),
      signif(BH_lav_bptest$statistic[[1]], 4), signif(BN_lav_bptest$statistic[[1]], 4),
      signif(BT_lav_bptest$statistic[[1]], 4), signif(BV_lav_bptest$statistic[[1]], 4),
      signif(BR_lav_bptest$statistic[[1]], 4), signif(BZ_lav_bptest$statistic[[1]], 4),
      signif(CS_lav_bptest$statistic[[1]], 4), signif(CL_lav_bptest$statistic[[1]], 4),
      signif(CJ_lav_bptest$statistic[[1]], 4), signif(CT_lav_bptest$statistic[[1]], 4),
      signif(CV_lav_bptest$statistic[[1]], 4), signif(DB_lav_bptest$statistic[[1]], 4),
      signif(DJ_lav_bptest$statistic[[1]], 4), signif(GL_lav_bptest$statistic[[1]], 4),
      signif(GR_lav_bptest$statistic[[1]], 4), signif(GJ_lav_bptest$statistic[[1]], 4),
      signif(HR_lav_bptest$statistic[[1]], 4), signif(HD_lav_bptest$statistic[[1]], 4),
      signif(IL_lav_bptest$statistic[[1]], 4), signif(IS_lav_bptest$statistic[[1]], 4),
      signif(IF_lav_bptest$statistic[[1]], 4), signif(MM_lav_bptest$statistic[[1]], 4),
      signif(MH_lav_bptest$statistic[[1]], 4), signif(MS_lav_bptest$statistic[[1]], 4),
      signif(NT_lav_bptest$statistic[[1]], 4), signif(OT_lav_bptest$statistic[[1]], 4),
      signif(PH_lav_bptest$statistic[[1]], 4), signif(SM_lav_bptest$statistic[[1]], 4),
      signif(SJ_lav_bptest$statistic[[1]], 4), signif(SB_lav_bptest$statistic[[1]], 4),
      signif(SV_lav_bptest$statistic[[1]], 4), signif(TR_lav_bptest$statistic[[1]], 4),
      signif(TM_lav_bptest$statistic[[1]], 4), signif(TL_lav_bptest$statistic[[1]], 4),
      signif(VS_lav_bptest$statistic[[1]], 4), signif(VL_lav_bptest$statistic[[1]], 4),
      signif(VN_lav_bptest$statistic[[1]], 4)),
    BP_PValue = c(
      signif(AB_lav_bptest$p.value[[1]], 4), signif(AR_lav_bptest$p.value[[1]], 4), 
      signif(AG_lav_bptest$p.value[[1]], 4), signif(BC_lav_bptest$p.value[[1]], 4),
      signif(BH_lav_bptest$p.value[[1]], 4), signif(BN_lav_bptest$p.value[[1]], 4),
      signif(BT_lav_bptest$p.value[[1]], 4), signif(BV_lav_bptest$p.value[[1]], 4),
      signif(BR_lav_bptest$p.value[[1]], 4), signif(BZ_lav_bptest$p.value[[1]], 4),
      signif(CS_lav_bptest$p.value[[1]], 4), signif(CL_lav_bptest$p.value[[1]], 4),
      signif(CJ_lav_bptest$p.value[[1]], 4), signif(CT_lav_bptest$p.value[[1]], 4),
      signif(CV_lav_bptest$p.value[[1]], 4), signif(DB_lav_bptest$p.value[[1]], 4),
      signif(DJ_lav_bptest$p.value[[1]], 4), signif(GL_lav_bptest$p.value[[1]], 4),
      signif(GR_lav_bptest$p.value[[1]], 4), signif(GJ_lav_bptest$p.value[[1]], 4),
      signif(HR_lav_bptest$p.value[[1]], 4), signif(HD_lav_bptest$p.value[[1]], 4),
      signif(IL_lav_bptest$p.value[[1]], 4), signif(IS_lav_bptest$p.value[[1]], 4),
      signif(IF_lav_bptest$p.value[[1]], 4), signif(MM_lav_bptest$p.value[[1]], 4),
      signif(MH_lav_bptest$p.value[[1]], 4), signif(MS_lav_bptest$p.value[[1]], 4),
      signif(NT_lav_bptest$p.value[[1]], 4), signif(OT_lav_bptest$p.value[[1]], 4),
      signif(PH_lav_bptest$p.value[[1]], 4), signif(SM_lav_bptest$p.value[[1]], 4),
      signif(SJ_lav_bptest$p.value[[1]], 4), signif(SB_lav_bptest$p.value[[1]], 4),
      signif(SV_lav_bptest$p.value[[1]], 4), signif(TR_lav_bptest$p.value[[1]], 4),
      signif(TM_lav_bptest$p.value[[1]], 4), signif(TL_lav_bptest$p.value[[1]], 4),
      signif(VS_lav_bptest$p.value[[1]], 4), signif(VL_lav_bptest$p.value[[1]], 4),
      signif(VN_lav_bptest$p.value[[1]], 4))
  ))
) %>% mutate(Homoscedasticity = if_else(BP_PValue > 0.05, "Y", "N"))

# 9. Durbin-Watson Test
DW_test_t <- bind_rows(
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Power",
    DW_Test = c(
      signif(AB_pw_DWtest$statistic[[1]], 4), signif(AR_pw_DWtest$statistic[[1]], 4), 
      signif(AG_pw_DWtest$statistic[[1]], 4), signif(BC_pw_DWtest$statistic[[1]], 4),
      signif(BH_pw_DWtest$statistic[[1]], 4), signif(BN_pw_DWtest$statistic[[1]], 4),
      signif(BT_pw_DWtest$statistic[[1]], 4), signif(BV_pw_DWtest$statistic[[1]], 4),
      signif(BR_pw_DWtest$statistic[[1]], 4), signif(BZ_pw_DWtest$statistic[[1]], 4),
      signif(CS_pw_DWtest$statistic[[1]], 4), signif(CL_pw_DWtest$statistic[[1]], 4),
      signif(CJ_pw_DWtest$statistic[[1]], 4), signif(CT_pw_DWtest$statistic[[1]], 4),
      signif(CV_pw_DWtest$statistic[[1]], 4), signif(DB_pw_DWtest$statistic[[1]], 4),
      signif(DJ_pw_DWtest$statistic[[1]], 4), signif(GL_pw_DWtest$statistic[[1]], 4),
      signif(GR_pw_DWtest$statistic[[1]], 4), signif(GJ_pw_DWtest$statistic[[1]], 4),
      signif(HR_pw_DWtest$statistic[[1]], 4), signif(HD_pw_DWtest$statistic[[1]], 4),
      signif(IL_pw_DWtest$statistic[[1]], 4), signif(IS_pw_DWtest$statistic[[1]], 4),
      signif(IF_pw_DWtest$statistic[[1]], 4), signif(MM_pw_DWtest$statistic[[1]], 4),
      signif(MH_pw_DWtest$statistic[[1]], 4), signif(MS_pw_DWtest$statistic[[1]], 4),
      signif(NT_pw_DWtest$statistic[[1]], 4), signif(OT_pw_DWtest$statistic[[1]], 4),
      signif(PH_pw_DWtest$statistic[[1]], 4), signif(SM_pw_DWtest$statistic[[1]], 4),
      signif(SJ_pw_DWtest$statistic[[1]], 4), signif(SB_pw_DWtest$statistic[[1]], 4),
      signif(SV_pw_DWtest$statistic[[1]], 4), signif(TR_pw_DWtest$statistic[[1]], 4),
      signif(TM_pw_DWtest$statistic[[1]], 4), signif(TL_pw_DWtest$statistic[[1]], 4),
      signif(VS_pw_DWtest$statistic[[1]], 4), signif(VL_pw_DWtest$statistic[[1]], 4),
      signif(VN_pw_DWtest$statistic[[1]], 4)),
    DW_PValue = c(
      signif(AB_pw_DWtest$p.value[[1]], 4), signif(AR_pw_DWtest$p.value[[1]], 4), 
      signif(AG_pw_DWtest$p.value[[1]], 4), signif(BC_pw_DWtest$p.value[[1]], 4),
      signif(BH_pw_DWtest$p.value[[1]], 4), signif(BN_pw_DWtest$p.value[[1]], 4),
      signif(BT_pw_DWtest$p.value[[1]], 4), signif(BV_pw_DWtest$p.value[[1]], 4),
      signif(BR_pw_DWtest$p.value[[1]], 4), signif(BZ_pw_DWtest$p.value[[1]], 4),
      signif(CS_pw_DWtest$p.value[[1]], 4), signif(CL_pw_DWtest$p.value[[1]], 4),
      signif(CJ_pw_DWtest$p.value[[1]], 4), signif(CT_pw_DWtest$p.value[[1]], 4),
      signif(CV_pw_DWtest$p.value[[1]], 4), signif(DB_pw_DWtest$p.value[[1]], 4),
      signif(DJ_pw_DWtest$p.value[[1]], 4), signif(GL_pw_DWtest$p.value[[1]], 4),
      signif(GR_pw_DWtest$p.value[[1]], 4), signif(GJ_pw_DWtest$p.value[[1]], 4),
      signif(HR_pw_DWtest$p.value[[1]], 4), signif(HD_pw_DWtest$p.value[[1]], 4),
      signif(IL_pw_DWtest$p.value[[1]], 4), signif(IS_pw_DWtest$p.value[[1]], 4),
      signif(IF_pw_DWtest$p.value[[1]], 4), signif(MM_pw_DWtest$p.value[[1]], 4),
      signif(MH_pw_DWtest$p.value[[1]], 4), signif(MS_pw_DWtest$p.value[[1]], 4),
      signif(NT_pw_DWtest$p.value[[1]], 4), signif(OT_pw_DWtest$p.value[[1]], 4),
      signif(PH_pw_DWtest$p.value[[1]], 4), signif(SM_pw_DWtest$p.value[[1]], 4),
      signif(SJ_pw_DWtest$p.value[[1]], 4), signif(SB_pw_DWtest$p.value[[1]], 4),
      signif(SV_pw_DWtest$p.value[[1]], 4), signif(TR_pw_DWtest$p.value[[1]], 4),
      signif(TM_pw_DWtest$p.value[[1]], 4), signif(TL_pw_DWtest$p.value[[1]], 4),
      signif(VS_pw_DWtest$p.value[[1]], 4), signif(VL_pw_DWtest$p.value[[1]], 4),
      signif(VN_pw_DWtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Exponential",
    DW_Test = c(
      signif(AB_exp_DWtest$statistic[[1]], 4), signif(AR_exp_DWtest$statistic[[1]], 4), 
      signif(AG_exp_DWtest$statistic[[1]], 4), signif(BC_exp_DWtest$statistic[[1]], 4),
      signif(BH_exp_DWtest$statistic[[1]], 4), signif(BN_exp_DWtest$statistic[[1]], 4),
      signif(BT_exp_DWtest$statistic[[1]], 4), signif(BV_exp_DWtest$statistic[[1]], 4),
      signif(BR_exp_DWtest$statistic[[1]], 4), signif(BZ_exp_DWtest$statistic[[1]], 4),
      signif(CS_exp_DWtest$statistic[[1]], 4), signif(CL_exp_DWtest$statistic[[1]], 4),
      signif(CJ_exp_DWtest$statistic[[1]], 4), signif(CT_exp_DWtest$statistic[[1]], 4),
      signif(CV_exp_DWtest$statistic[[1]], 4), signif(DB_exp_DWtest$statistic[[1]], 4),
      signif(DJ_exp_DWtest$statistic[[1]], 4), signif(GL_exp_DWtest$statistic[[1]], 4),
      signif(GR_exp_DWtest$statistic[[1]], 4), signif(GJ_exp_DWtest$statistic[[1]], 4),
      signif(HR_exp_DWtest$statistic[[1]], 4), signif(HD_exp_DWtest$statistic[[1]], 4),
      signif(IL_exp_DWtest$statistic[[1]], 4), signif(IS_exp_DWtest$statistic[[1]], 4),
      signif(IF_exp_DWtest$statistic[[1]], 4), signif(MM_exp_DWtest$statistic[[1]], 4),
      signif(MH_exp_DWtest$statistic[[1]], 4), signif(MS_exp_DWtest$statistic[[1]], 4),
      signif(NT_exp_DWtest$statistic[[1]], 4), signif(OT_exp_DWtest$statistic[[1]], 4),
      signif(PH_exp_DWtest$statistic[[1]], 4), signif(SM_exp_DWtest$statistic[[1]], 4),
      signif(SJ_exp_DWtest$statistic[[1]], 4), signif(SB_exp_DWtest$statistic[[1]], 4),
      signif(SV_exp_DWtest$statistic[[1]], 4), signif(TR_exp_DWtest$statistic[[1]], 4),
      signif(TM_exp_DWtest$statistic[[1]], 4), signif(TL_exp_DWtest$statistic[[1]], 4),
      signif(VS_exp_DWtest$statistic[[1]], 4), signif(VL_exp_DWtest$statistic[[1]], 4),
      signif(VN_exp_DWtest$statistic[[1]], 4)),
    DW_PValue = c(
      signif(AB_exp_DWtest$p.value[[1]], 4), signif(AR_exp_DWtest$p.value[[1]], 4), 
      signif(AG_exp_DWtest$p.value[[1]], 4), signif(BC_exp_DWtest$p.value[[1]], 4),
      signif(BH_exp_DWtest$p.value[[1]], 4), signif(BN_exp_DWtest$p.value[[1]], 4),
      signif(BT_exp_DWtest$p.value[[1]], 4), signif(BV_exp_DWtest$p.value[[1]], 4),
      signif(BR_exp_DWtest$p.value[[1]], 4), signif(BZ_exp_DWtest$p.value[[1]], 4),
      signif(CS_exp_DWtest$p.value[[1]], 4), signif(CL_exp_DWtest$p.value[[1]], 4),
      signif(CJ_exp_DWtest$p.value[[1]], 4), signif(CT_exp_DWtest$p.value[[1]], 4),
      signif(CV_exp_DWtest$p.value[[1]], 4), signif(DB_exp_DWtest$p.value[[1]], 4),
      signif(DJ_exp_DWtest$p.value[[1]], 4), signif(GL_exp_DWtest$p.value[[1]], 4),
      signif(GR_exp_DWtest$p.value[[1]], 4), signif(GJ_exp_DWtest$p.value[[1]], 4),
      signif(HR_exp_DWtest$p.value[[1]], 4), signif(HD_exp_DWtest$p.value[[1]], 4),
      signif(IL_exp_DWtest$p.value[[1]], 4), signif(IS_exp_DWtest$p.value[[1]], 4),
      signif(IF_exp_DWtest$p.value[[1]], 4), signif(MM_exp_DWtest$p.value[[1]], 4),
      signif(MH_exp_DWtest$p.value[[1]], 4), signif(MS_exp_DWtest$p.value[[1]], 4),
      signif(NT_exp_DWtest$p.value[[1]], 4), signif(OT_exp_DWtest$p.value[[1]], 4),
      signif(PH_exp_DWtest$p.value[[1]], 4), signif(SM_exp_DWtest$p.value[[1]], 4),
      signif(SJ_exp_DWtest$p.value[[1]], 4), signif(SB_exp_DWtest$p.value[[1]], 4),
      signif(SV_exp_DWtest$p.value[[1]], 4), signif(TR_exp_DWtest$p.value[[1]], 4),
      signif(TM_exp_DWtest$p.value[[1]], 4), signif(TL_exp_DWtest$p.value[[1]], 4),
      signif(VS_exp_DWtest$p.value[[1]], 4), signif(VL_exp_DWtest$p.value[[1]], 4),
      signif(VN_exp_DWtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Zipf-Mandelbrot",
    DW_Test = c(
      signif(AB_ZM_DWtest$statistic[[1]], 4), signif(AR_ZM_DWtest$statistic[[1]], 4), 
      signif(AG_ZM_DWtest$statistic[[1]], 4), signif(BC_ZM_DWtest$statistic[[1]], 4),
      signif(BH_ZM_DWtest$statistic[[1]], 4), signif(BN_ZM_DWtest$statistic[[1]], 4),
      signif(BT_ZM_DWtest$statistic[[1]], 4), signif(BV_ZM_DWtest$statistic[[1]], 4),
      signif(BR_ZM_DWtest$statistic[[1]], 4), signif(BZ_ZM_DWtest$statistic[[1]], 4),
      signif(CS_ZM_DWtest$statistic[[1]], 4), signif(CL_ZM_DWtest$statistic[[1]], 4),
      signif(CJ_ZM_DWtest$statistic[[1]], 4), signif(CT_ZM_DWtest$statistic[[1]], 4),
      signif(CV_ZM_DWtest$statistic[[1]], 4), signif(DB_ZM_DWtest$statistic[[1]], 4),
      signif(DJ_ZM_DWtest$statistic[[1]], 4), signif(GL_ZM_DWtest$statistic[[1]], 4),
      signif(GR_ZM_DWtest$statistic[[1]], 4), signif(GJ_ZM_DWtest$statistic[[1]], 4),
      signif(HR_ZM_DWtest$statistic[[1]], 4), signif(HD_ZM_DWtest$statistic[[1]], 4),
      signif(IL_ZM_DWtest$statistic[[1]], 4), signif(IS_ZM_DWtest$statistic[[1]], 4),
      signif(IF_ZM_DWtest$statistic[[1]], 4), signif(MM_ZM_DWtest$statistic[[1]], 4),
      signif(MH_ZM_DWtest$statistic[[1]], 4), signif(MS_ZM_DWtest$statistic[[1]], 4),
      signif(NT_ZM_DWtest$statistic[[1]], 4), signif(OT_ZM_DWtest$statistic[[1]], 4),
      signif(PH_ZM_DWtest$statistic[[1]], 4), signif(SM_ZM_DWtest$statistic[[1]], 4),
      signif(SJ_ZM_DWtest$statistic[[1]], 4), signif(SB_ZM_DWtest$statistic[[1]], 4),
      signif(SV_ZM_DWtest$statistic[[1]], 4), signif(TR_ZM_DWtest$statistic[[1]], 4),
      signif(TM_ZM_DWtest$statistic[[1]], 4), signif(TL_ZM_DWtest$statistic[[1]], 4),
      signif(VS_ZM_DWtest$statistic[[1]], 4), signif(VL_ZM_DWtest$statistic[[1]], 4),
      signif(VN_ZM_DWtest$statistic[[1]], 4)),
    DW_PValue = c(
      signif(AB_ZM_DWtest$p.value[[1]], 4), signif(AR_ZM_DWtest$p.value[[1]], 4), 
      signif(AG_ZM_DWtest$p.value[[1]], 4), signif(BC_ZM_DWtest$p.value[[1]], 4),
      signif(BH_ZM_DWtest$p.value[[1]], 4), signif(BN_ZM_DWtest$p.value[[1]], 4),
      signif(BT_ZM_DWtest$p.value[[1]], 4), signif(BV_ZM_DWtest$p.value[[1]], 4),
      signif(BR_ZM_DWtest$p.value[[1]], 4), signif(BZ_ZM_DWtest$p.value[[1]], 4),
      signif(CS_ZM_DWtest$p.value[[1]], 4), signif(CL_ZM_DWtest$p.value[[1]], 4),
      signif(CJ_ZM_DWtest$p.value[[1]], 4), signif(CT_ZM_DWtest$p.value[[1]], 4),
      signif(CV_ZM_DWtest$p.value[[1]], 4), signif(DB_ZM_DWtest$p.value[[1]], 4),
      signif(DJ_ZM_DWtest$p.value[[1]], 4), signif(GL_ZM_DWtest$p.value[[1]], 4),
      signif(GR_ZM_DWtest$p.value[[1]], 4), signif(GJ_ZM_DWtest$p.value[[1]], 4),
      signif(HR_ZM_DWtest$p.value[[1]], 4), signif(HD_ZM_DWtest$p.value[[1]], 4),
      signif(IL_ZM_DWtest$p.value[[1]], 4), signif(IS_ZM_DWtest$p.value[[1]], 4),
      signif(IF_ZM_DWtest$p.value[[1]], 4), signif(MM_ZM_DWtest$p.value[[1]], 4),
      signif(MH_ZM_DWtest$p.value[[1]], 4), signif(MS_ZM_DWtest$p.value[[1]], 4),
      signif(NT_ZM_DWtest$p.value[[1]], 4), signif(OT_ZM_DWtest$p.value[[1]], 4),
      signif(PH_ZM_DWtest$p.value[[1]], 4), signif(SM_ZM_DWtest$p.value[[1]], 4),
      signif(SJ_ZM_DWtest$p.value[[1]], 4), signif(SB_ZM_DWtest$p.value[[1]], 4),
      signif(SV_ZM_DWtest$p.value[[1]], 4), signif(TR_ZM_DWtest$p.value[[1]], 4),
      signif(TM_ZM_DWtest$p.value[[1]], 4), signif(TL_ZM_DWtest$p.value[[1]], 4),
      signif(VS_ZM_DWtest$p.value[[1]], 4), signif(VL_ZM_DWtest$p.value[[1]], 4),
      signif(VN_ZM_DWtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Lavalette",
    DW_Test = c(
      signif(AB_lav_DWtest$statistic[[1]], 4), signif(AR_lav_DWtest$statistic[[1]], 4), 
      signif(AG_lav_DWtest$statistic[[1]], 4), signif(BC_lav_DWtest$statistic[[1]], 4),
      signif(BH_lav_DWtest$statistic[[1]], 4), signif(BN_lav_DWtest$statistic[[1]], 4),
      signif(BT_lav_DWtest$statistic[[1]], 4), signif(BV_lav_DWtest$statistic[[1]], 4),
      signif(BR_lav_DWtest$statistic[[1]], 4), signif(BZ_lav_DWtest$statistic[[1]], 4),
      signif(CS_lav_DWtest$statistic[[1]], 4), signif(CL_lav_DWtest$statistic[[1]], 4),
      signif(CJ_lav_DWtest$statistic[[1]], 4), signif(CT_lav_DWtest$statistic[[1]], 4),
      signif(CV_lav_DWtest$statistic[[1]], 4), signif(DB_lav_DWtest$statistic[[1]], 4),
      signif(DJ_lav_DWtest$statistic[[1]], 4), signif(GL_lav_DWtest$statistic[[1]], 4),
      signif(GR_lav_DWtest$statistic[[1]], 4), signif(GJ_lav_DWtest$statistic[[1]], 4),
      signif(HR_lav_DWtest$statistic[[1]], 4), signif(HD_lav_DWtest$statistic[[1]], 4),
      signif(IL_lav_DWtest$statistic[[1]], 4), signif(IS_lav_DWtest$statistic[[1]], 4),
      signif(IF_lav_DWtest$statistic[[1]], 4), signif(MM_lav_DWtest$statistic[[1]], 4),
      signif(MH_lav_DWtest$statistic[[1]], 4), signif(MS_lav_DWtest$statistic[[1]], 4),
      signif(NT_lav_DWtest$statistic[[1]], 4), signif(OT_lav_DWtest$statistic[[1]], 4),
      signif(PH_lav_DWtest$statistic[[1]], 4), signif(SM_lav_DWtest$statistic[[1]], 4),
      signif(SJ_lav_DWtest$statistic[[1]], 4), signif(SB_lav_DWtest$statistic[[1]], 4),
      signif(SV_lav_DWtest$statistic[[1]], 4), signif(TR_lav_DWtest$statistic[[1]], 4),
      signif(TM_lav_DWtest$statistic[[1]], 4), signif(TL_lav_DWtest$statistic[[1]], 4),
      signif(VS_lav_DWtest$statistic[[1]], 4), signif(VL_lav_DWtest$statistic[[1]], 4),
      signif(VN_lav_DWtest$statistic[[1]], 4)),
    DW_PValue = c(
      signif(AB_lav_DWtest$p.value[[1]], 4), signif(AR_lav_DWtest$p.value[[1]], 4), 
      signif(AG_lav_DWtest$p.value[[1]], 4), signif(BC_lav_DWtest$p.value[[1]], 4),
      signif(BH_lav_DWtest$p.value[[1]], 4), signif(BN_lav_DWtest$p.value[[1]], 4),
      signif(BT_lav_DWtest$p.value[[1]], 4), signif(BV_lav_DWtest$p.value[[1]], 4),
      signif(BR_lav_DWtest$p.value[[1]], 4), signif(BZ_lav_DWtest$p.value[[1]], 4),
      signif(CS_lav_DWtest$p.value[[1]], 4), signif(CL_lav_DWtest$p.value[[1]], 4),
      signif(CJ_lav_DWtest$p.value[[1]], 4), signif(CT_lav_DWtest$p.value[[1]], 4),
      signif(CV_lav_DWtest$p.value[[1]], 4), signif(DB_lav_DWtest$p.value[[1]], 4),
      signif(DJ_lav_DWtest$p.value[[1]], 4), signif(GL_lav_DWtest$p.value[[1]], 4),
      signif(GR_lav_DWtest$p.value[[1]], 4), signif(GJ_lav_DWtest$p.value[[1]], 4),
      signif(HR_lav_DWtest$p.value[[1]], 4), signif(HD_lav_DWtest$p.value[[1]], 4),
      signif(IL_lav_DWtest$p.value[[1]], 4), signif(IS_lav_DWtest$p.value[[1]], 4),
      signif(IF_lav_DWtest$p.value[[1]], 4), signif(MM_lav_DWtest$p.value[[1]], 4),
      signif(MH_lav_DWtest$p.value[[1]], 4), signif(MS_lav_DWtest$p.value[[1]], 4),
      signif(NT_lav_DWtest$p.value[[1]], 4), signif(OT_lav_DWtest$p.value[[1]], 4),
      signif(PH_lav_DWtest$p.value[[1]], 4), signif(SM_lav_DWtest$p.value[[1]], 4),
      signif(SJ_lav_DWtest$p.value[[1]], 4), signif(SB_lav_DWtest$p.value[[1]], 4),
      signif(SV_lav_DWtest$p.value[[1]], 4), signif(TR_lav_DWtest$p.value[[1]], 4),
      signif(TM_lav_DWtest$p.value[[1]], 4), signif(TL_lav_DWtest$p.value[[1]], 4),
      signif(VS_lav_DWtest$p.value[[1]], 4), signif(VL_lav_DWtest$p.value[[1]], 4),
      signif(VN_lav_DWtest$p.value[[1]], 4))
  ))
) %>% mutate(Autocorrelation = if_else(DW_PValue < 0.05, "Y", "N"))

# 10. Breusch-Godfrey Test
BG_test_t <- bind_rows(
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Power",
    BG_Test = c(
      signif(AB_pw_bgtest$statistic[[1]], 4), signif(AR_pw_bgtest$statistic[[1]], 4), 
      signif(AG_pw_bgtest$statistic[[1]], 4), signif(BC_pw_bgtest$statistic[[1]], 4),
      signif(BH_pw_bgtest$statistic[[1]], 4), signif(BN_pw_bgtest$statistic[[1]], 4),
      signif(BT_pw_bgtest$statistic[[1]], 4), signif(BV_pw_bgtest$statistic[[1]], 4),
      signif(BR_pw_bgtest$statistic[[1]], 4), signif(BZ_pw_bgtest$statistic[[1]], 4),
      signif(CS_pw_bgtest$statistic[[1]], 4), signif(CL_pw_bgtest$statistic[[1]], 4),
      signif(CJ_pw_bgtest$statistic[[1]], 4), signif(CT_pw_bgtest$statistic[[1]], 4),
      signif(CV_pw_bgtest$statistic[[1]], 4), signif(DB_pw_bgtest$statistic[[1]], 4),
      signif(DJ_pw_bgtest$statistic[[1]], 4), signif(GL_pw_bgtest$statistic[[1]], 4),
      signif(GR_pw_bgtest$statistic[[1]], 4), signif(GJ_pw_bgtest$statistic[[1]], 4),
      signif(HR_pw_bgtest$statistic[[1]], 4), signif(HD_pw_bgtest$statistic[[1]], 4),
      signif(IL_pw_bgtest$statistic[[1]], 4), signif(IS_pw_bgtest$statistic[[1]], 4),
      signif(IF_pw_bgtest$statistic[[1]], 4), signif(MM_pw_bgtest$statistic[[1]], 4),
      signif(MH_pw_bgtest$statistic[[1]], 4), signif(MS_pw_bgtest$statistic[[1]], 4),
      signif(NT_pw_bgtest$statistic[[1]], 4), signif(OT_pw_bgtest$statistic[[1]], 4),
      signif(PH_pw_bgtest$statistic[[1]], 4), signif(SM_pw_bgtest$statistic[[1]], 4),
      signif(SJ_pw_bgtest$statistic[[1]], 4), signif(SB_pw_bgtest$statistic[[1]], 4),
      signif(SV_pw_bgtest$statistic[[1]], 4), signif(TR_pw_bgtest$statistic[[1]], 4),
      signif(TM_pw_bgtest$statistic[[1]], 4), signif(TL_pw_bgtest$statistic[[1]], 4),
      signif(VS_pw_bgtest$statistic[[1]], 4), signif(VL_pw_bgtest$statistic[[1]], 4),
      signif(VN_pw_bgtest$statistic[[1]], 4)),
    BG_PValue = c(
      signif(AB_pw_bgtest$p.value[[1]], 4), signif(AR_pw_bgtest$p.value[[1]], 4), 
      signif(AG_pw_bgtest$p.value[[1]], 4), signif(BC_pw_bgtest$p.value[[1]], 4),
      signif(BH_pw_bgtest$p.value[[1]], 4), signif(BN_pw_bgtest$p.value[[1]], 4),
      signif(BT_pw_bgtest$p.value[[1]], 4), signif(BV_pw_bgtest$p.value[[1]], 4),
      signif(BR_pw_bgtest$p.value[[1]], 4), signif(BZ_pw_bgtest$p.value[[1]], 4),
      signif(CS_pw_bgtest$p.value[[1]], 4), signif(CL_pw_bgtest$p.value[[1]], 4),
      signif(CJ_pw_bgtest$p.value[[1]], 4), signif(CT_pw_bgtest$p.value[[1]], 4),
      signif(CV_pw_bgtest$p.value[[1]], 4), signif(DB_pw_bgtest$p.value[[1]], 4),
      signif(DJ_pw_bgtest$p.value[[1]], 4), signif(GL_pw_bgtest$p.value[[1]], 4),
      signif(GR_pw_bgtest$p.value[[1]], 4), signif(GJ_pw_bgtest$p.value[[1]], 4),
      signif(HR_pw_bgtest$p.value[[1]], 4), signif(HD_pw_bgtest$p.value[[1]], 4),
      signif(IL_pw_bgtest$p.value[[1]], 4), signif(IS_pw_bgtest$p.value[[1]], 4),
      signif(IF_pw_bgtest$p.value[[1]], 4), signif(MM_pw_bgtest$p.value[[1]], 4),
      signif(MH_pw_bgtest$p.value[[1]], 4), signif(MS_pw_bgtest$p.value[[1]], 4),
      signif(NT_pw_bgtest$p.value[[1]], 4), signif(OT_pw_bgtest$p.value[[1]], 4),
      signif(PH_pw_bgtest$p.value[[1]], 4), signif(SM_pw_bgtest$p.value[[1]], 4),
      signif(SJ_pw_bgtest$p.value[[1]], 4), signif(SB_pw_bgtest$p.value[[1]], 4),
      signif(SV_pw_bgtest$p.value[[1]], 4), signif(TR_pw_bgtest$p.value[[1]], 4),
      signif(TM_pw_bgtest$p.value[[1]], 4), signif(TL_pw_bgtest$p.value[[1]], 4),
      signif(VS_pw_bgtest$p.value[[1]], 4), signif(VL_pw_bgtest$p.value[[1]], 4),
      signif(VN_pw_bgtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Exponential",
    BG_Test = c(
      signif(AB_exp_bgtest$statistic[[1]], 4), signif(AR_exp_bgtest$statistic[[1]], 4), 
      signif(AG_exp_bgtest$statistic[[1]], 4), signif(BC_exp_bgtest$statistic[[1]], 4),
      signif(BH_exp_bgtest$statistic[[1]], 4), signif(BN_exp_bgtest$statistic[[1]], 4),
      signif(BT_exp_bgtest$statistic[[1]], 4), signif(BV_exp_bgtest$statistic[[1]], 4),
      signif(BR_exp_bgtest$statistic[[1]], 4), signif(BZ_exp_bgtest$statistic[[1]], 4),
      signif(CS_exp_bgtest$statistic[[1]], 4), signif(CL_exp_bgtest$statistic[[1]], 4),
      signif(CJ_exp_bgtest$statistic[[1]], 4), signif(CT_exp_bgtest$statistic[[1]], 4),
      signif(CV_exp_bgtest$statistic[[1]], 4), signif(DB_exp_bgtest$statistic[[1]], 4),
      signif(DJ_exp_bgtest$statistic[[1]], 4), signif(GL_exp_bgtest$statistic[[1]], 4),
      signif(GR_exp_bgtest$statistic[[1]], 4), signif(GJ_exp_bgtest$statistic[[1]], 4),
      signif(HR_exp_bgtest$statistic[[1]], 4), signif(HD_exp_bgtest$statistic[[1]], 4),
      signif(IL_exp_bgtest$statistic[[1]], 4), signif(IS_exp_bgtest$statistic[[1]], 4),
      signif(IF_exp_bgtest$statistic[[1]], 4), signif(MM_exp_bgtest$statistic[[1]], 4),
      signif(MH_exp_bgtest$statistic[[1]], 4), signif(MS_exp_bgtest$statistic[[1]], 4),
      signif(NT_exp_bgtest$statistic[[1]], 4), signif(OT_exp_bgtest$statistic[[1]], 4),
      signif(PH_exp_bgtest$statistic[[1]], 4), signif(SM_exp_bgtest$statistic[[1]], 4),
      signif(SJ_exp_bgtest$statistic[[1]], 4), signif(SB_exp_bgtest$statistic[[1]], 4),
      signif(SV_exp_bgtest$statistic[[1]], 4), signif(TR_exp_bgtest$statistic[[1]], 4),
      signif(TM_exp_bgtest$statistic[[1]], 4), signif(TL_exp_bgtest$statistic[[1]], 4),
      signif(VS_exp_bgtest$statistic[[1]], 4), signif(VL_exp_bgtest$statistic[[1]], 4),
      signif(VN_exp_bgtest$statistic[[1]], 4)),
    BG_PValue = c(
      signif(AB_exp_bgtest$p.value[[1]], 4), signif(AR_exp_bgtest$p.value[[1]], 4), 
      signif(AG_exp_bgtest$p.value[[1]], 4), signif(BC_exp_bgtest$p.value[[1]], 4),
      signif(BH_exp_bgtest$p.value[[1]], 4), signif(BN_exp_bgtest$p.value[[1]], 4),
      signif(BT_exp_bgtest$p.value[[1]], 4), signif(BV_exp_bgtest$p.value[[1]], 4),
      signif(BR_exp_bgtest$p.value[[1]], 4), signif(BZ_exp_bgtest$p.value[[1]], 4),
      signif(CS_exp_bgtest$p.value[[1]], 4), signif(CL_exp_bgtest$p.value[[1]], 4),
      signif(CJ_exp_bgtest$p.value[[1]], 4), signif(CT_exp_bgtest$p.value[[1]], 4),
      signif(CV_exp_bgtest$p.value[[1]], 4), signif(DB_exp_bgtest$p.value[[1]], 4),
      signif(DJ_exp_bgtest$p.value[[1]], 4), signif(GL_exp_bgtest$p.value[[1]], 4),
      signif(GR_exp_bgtest$p.value[[1]], 4), signif(GJ_exp_bgtest$p.value[[1]], 4),
      signif(HR_exp_bgtest$p.value[[1]], 4), signif(HD_exp_bgtest$p.value[[1]], 4),
      signif(IL_exp_bgtest$p.value[[1]], 4), signif(IS_exp_bgtest$p.value[[1]], 4),
      signif(IF_exp_bgtest$p.value[[1]], 4), signif(MM_exp_bgtest$p.value[[1]], 4),
      signif(MH_exp_bgtest$p.value[[1]], 4), signif(MS_exp_bgtest$p.value[[1]], 4),
      signif(NT_exp_bgtest$p.value[[1]], 4), signif(OT_exp_bgtest$p.value[[1]], 4),
      signif(PH_exp_bgtest$p.value[[1]], 4), signif(SM_exp_bgtest$p.value[[1]], 4),
      signif(SJ_exp_bgtest$p.value[[1]], 4), signif(SB_exp_bgtest$p.value[[1]], 4),
      signif(SV_exp_bgtest$p.value[[1]], 4), signif(TR_exp_bgtest$p.value[[1]], 4),
      signif(TM_exp_bgtest$p.value[[1]], 4), signif(TL_exp_bgtest$p.value[[1]], 4),
      signif(VS_exp_bgtest$p.value[[1]], 4), signif(VL_exp_bgtest$p.value[[1]], 4),
      signif(VN_exp_bgtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Zipf-Mandelbrot",
    BG_Test = c(
      signif(AB_ZM_bgtest$statistic[[1]], 4), signif(AR_ZM_bgtest$statistic[[1]], 4), 
      signif(AG_ZM_bgtest$statistic[[1]], 4), signif(BC_ZM_bgtest$statistic[[1]], 4),
      signif(BH_ZM_bgtest$statistic[[1]], 4), signif(BN_ZM_bgtest$statistic[[1]], 4),
      signif(BT_ZM_bgtest$statistic[[1]], 4), signif(BV_ZM_bgtest$statistic[[1]], 4),
      signif(BR_ZM_bgtest$statistic[[1]], 4), signif(BZ_ZM_bgtest$statistic[[1]], 4),
      signif(CS_ZM_bgtest$statistic[[1]], 4), signif(CL_ZM_bgtest$statistic[[1]], 4),
      signif(CJ_ZM_bgtest$statistic[[1]], 4), signif(CT_ZM_bgtest$statistic[[1]], 4),
      signif(CV_ZM_bgtest$statistic[[1]], 4), signif(DB_ZM_bgtest$statistic[[1]], 4),
      signif(DJ_ZM_bgtest$statistic[[1]], 4), signif(GL_ZM_bgtest$statistic[[1]], 4),
      signif(GR_ZM_bgtest$statistic[[1]], 4), signif(GJ_ZM_bgtest$statistic[[1]], 4),
      signif(HR_ZM_bgtest$statistic[[1]], 4), signif(HD_ZM_bgtest$statistic[[1]], 4),
      signif(IL_ZM_bgtest$statistic[[1]], 4), signif(IS_ZM_bgtest$statistic[[1]], 4),
      signif(IF_ZM_bgtest$statistic[[1]], 4), signif(MM_ZM_bgtest$statistic[[1]], 4),
      signif(MH_ZM_bgtest$statistic[[1]], 4), signif(MS_ZM_bgtest$statistic[[1]], 4),
      signif(NT_ZM_bgtest$statistic[[1]], 4), signif(OT_ZM_bgtest$statistic[[1]], 4),
      signif(PH_ZM_bgtest$statistic[[1]], 4), signif(SM_ZM_bgtest$statistic[[1]], 4),
      signif(SJ_ZM_bgtest$statistic[[1]], 4), signif(SB_ZM_bgtest$statistic[[1]], 4),
      signif(SV_ZM_bgtest$statistic[[1]], 4), signif(TR_ZM_bgtest$statistic[[1]], 4),
      signif(TM_ZM_bgtest$statistic[[1]], 4), signif(TL_ZM_bgtest$statistic[[1]], 4),
      signif(VS_ZM_bgtest$statistic[[1]], 4), signif(VL_ZM_bgtest$statistic[[1]], 4),
      signif(VN_ZM_bgtest$statistic[[1]], 4)),
    BG_PValue = c(
      signif(AB_ZM_bgtest$p.value[[1]], 4), signif(AR_ZM_bgtest$p.value[[1]], 4), 
      signif(AG_ZM_bgtest$p.value[[1]], 4), signif(BC_ZM_bgtest$p.value[[1]], 4),
      signif(BH_ZM_bgtest$p.value[[1]], 4), signif(BN_ZM_bgtest$p.value[[1]], 4),
      signif(BT_ZM_bgtest$p.value[[1]], 4), signif(BV_ZM_bgtest$p.value[[1]], 4),
      signif(BR_ZM_bgtest$p.value[[1]], 4), signif(BZ_ZM_bgtest$p.value[[1]], 4),
      signif(CS_ZM_bgtest$p.value[[1]], 4), signif(CL_ZM_bgtest$p.value[[1]], 4),
      signif(CJ_ZM_bgtest$p.value[[1]], 4), signif(CT_ZM_bgtest$p.value[[1]], 4),
      signif(CV_ZM_bgtest$p.value[[1]], 4), signif(DB_ZM_bgtest$p.value[[1]], 4),
      signif(DJ_ZM_bgtest$p.value[[1]], 4), signif(GL_ZM_bgtest$p.value[[1]], 4),
      signif(GR_ZM_bgtest$p.value[[1]], 4), signif(GJ_ZM_bgtest$p.value[[1]], 4),
      signif(HR_ZM_bgtest$p.value[[1]], 4), signif(HD_ZM_bgtest$p.value[[1]], 4),
      signif(IL_ZM_bgtest$p.value[[1]], 4), signif(IS_ZM_bgtest$p.value[[1]], 4),
      signif(IF_ZM_bgtest$p.value[[1]], 4), signif(MM_ZM_bgtest$p.value[[1]], 4),
      signif(MH_ZM_bgtest$p.value[[1]], 4), signif(MS_ZM_bgtest$p.value[[1]], 4),
      signif(NT_ZM_bgtest$p.value[[1]], 4), signif(OT_ZM_bgtest$p.value[[1]], 4),
      signif(PH_ZM_bgtest$p.value[[1]], 4), signif(SM_ZM_bgtest$p.value[[1]], 4),
      signif(SJ_ZM_bgtest$p.value[[1]], 4), signif(SB_ZM_bgtest$p.value[[1]], 4),
      signif(SV_ZM_bgtest$p.value[[1]], 4), signif(TR_ZM_bgtest$p.value[[1]], 4),
      signif(TM_ZM_bgtest$p.value[[1]], 4), signif(TL_ZM_bgtest$p.value[[1]], 4),
      signif(VS_ZM_bgtest$p.value[[1]], 4), signif(VL_ZM_bgtest$p.value[[1]], 4),
      signif(VN_ZM_bgtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Lavalette",
    BG_Test = c(
      signif(AB_lav_bgtest$statistic[[1]], 4), signif(AR_lav_bgtest$statistic[[1]], 4), 
      signif(AG_lav_bgtest$statistic[[1]], 4), signif(BC_lav_bgtest$statistic[[1]], 4),
      signif(BH_lav_bgtest$statistic[[1]], 4), signif(BN_lav_bgtest$statistic[[1]], 4),
      signif(BT_lav_bgtest$statistic[[1]], 4), signif(BV_lav_bgtest$statistic[[1]], 4),
      signif(BR_lav_bgtest$statistic[[1]], 4), signif(BZ_lav_bgtest$statistic[[1]], 4),
      signif(CS_lav_bgtest$statistic[[1]], 4), signif(CL_lav_bgtest$statistic[[1]], 4),
      signif(CJ_lav_bgtest$statistic[[1]], 4), signif(CT_lav_bgtest$statistic[[1]], 4),
      signif(CV_lav_bgtest$statistic[[1]], 4), signif(DB_lav_bgtest$statistic[[1]], 4),
      signif(DJ_lav_bgtest$statistic[[1]], 4), signif(GL_lav_bgtest$statistic[[1]], 4),
      signif(GR_lav_bgtest$statistic[[1]], 4), signif(GJ_lav_bgtest$statistic[[1]], 4),
      signif(HR_lav_bgtest$statistic[[1]], 4), signif(HD_lav_bgtest$statistic[[1]], 4),
      signif(IL_lav_bgtest$statistic[[1]], 4), signif(IS_lav_bgtest$statistic[[1]], 4),
      signif(IF_lav_bgtest$statistic[[1]], 4), signif(MM_lav_bgtest$statistic[[1]], 4),
      signif(MH_lav_bgtest$statistic[[1]], 4), signif(MS_lav_bgtest$statistic[[1]], 4),
      signif(NT_lav_bgtest$statistic[[1]], 4), signif(OT_lav_bgtest$statistic[[1]], 4),
      signif(PH_lav_bgtest$statistic[[1]], 4), signif(SM_lav_bgtest$statistic[[1]], 4),
      signif(SJ_lav_bgtest$statistic[[1]], 4), signif(SB_lav_bgtest$statistic[[1]], 4),
      signif(SV_lav_bgtest$statistic[[1]], 4), signif(TR_lav_bgtest$statistic[[1]], 4),
      signif(TM_lav_bgtest$statistic[[1]], 4), signif(TL_lav_bgtest$statistic[[1]], 4),
      signif(VS_lav_bgtest$statistic[[1]], 4), signif(VL_lav_bgtest$statistic[[1]], 4),
      signif(VN_lav_bgtest$statistic[[1]], 4)),
    BG_PValue = c(
      signif(AB_lav_bgtest$p.value[[1]], 4), signif(AR_lav_bgtest$p.value[[1]], 4), 
      signif(AG_lav_bgtest$p.value[[1]], 4), signif(BC_lav_bgtest$p.value[[1]], 4),
      signif(BH_lav_bgtest$p.value[[1]], 4), signif(BN_lav_bgtest$p.value[[1]], 4),
      signif(BT_lav_bgtest$p.value[[1]], 4), signif(BV_lav_bgtest$p.value[[1]], 4),
      signif(BR_lav_bgtest$p.value[[1]], 4), signif(BZ_lav_bgtest$p.value[[1]], 4),
      signif(CS_lav_bgtest$p.value[[1]], 4), signif(CL_lav_bgtest$p.value[[1]], 4),
      signif(CJ_lav_bgtest$p.value[[1]], 4), signif(CT_lav_bgtest$p.value[[1]], 4),
      signif(CV_lav_bgtest$p.value[[1]], 4), signif(DB_lav_bgtest$p.value[[1]], 4),
      signif(DJ_lav_bgtest$p.value[[1]], 4), signif(GL_lav_bgtest$p.value[[1]], 4),
      signif(GR_lav_bgtest$p.value[[1]], 4), signif(GJ_lav_bgtest$p.value[[1]], 4),
      signif(HR_lav_bgtest$p.value[[1]], 4), signif(HD_lav_bgtest$p.value[[1]], 4),
      signif(IL_lav_bgtest$p.value[[1]], 4), signif(IS_lav_bgtest$p.value[[1]], 4),
      signif(IF_lav_bgtest$p.value[[1]], 4), signif(MM_lav_bgtest$p.value[[1]], 4),
      signif(MH_lav_bgtest$p.value[[1]], 4), signif(MS_lav_bgtest$p.value[[1]], 4),
      signif(NT_lav_bgtest$p.value[[1]], 4), signif(OT_lav_bgtest$p.value[[1]], 4),
      signif(PH_lav_bgtest$p.value[[1]], 4), signif(SM_lav_bgtest$p.value[[1]], 4),
      signif(SJ_lav_bgtest$p.value[[1]], 4), signif(SB_lav_bgtest$p.value[[1]], 4),
      signif(SV_lav_bgtest$p.value[[1]], 4), signif(TR_lav_bgtest$p.value[[1]], 4),
      signif(TM_lav_bgtest$p.value[[1]], 4), signif(TL_lav_bgtest$p.value[[1]], 4),
      signif(VS_lav_bgtest$p.value[[1]], 4), signif(VL_lav_bgtest$p.value[[1]], 4),
      signif(VN_lav_bgtest$p.value[[1]], 4))
  ))
) %>% mutate(Autocorrelation = if_else(BG_PValue < 0.05, "Y", "N"))

# 11. Jarque-Bera Test
JB_test_t <- bind_rows(
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Power",
    JB_Test = c(
      signif(AB_pw_jbtest$statistic[[1]], 4), signif(AR_pw_jbtest$statistic[[1]], 4), 
      signif(AG_pw_jbtest$statistic[[1]], 4), signif(BC_pw_jbtest$statistic[[1]], 4),
      signif(BH_pw_jbtest$statistic[[1]], 4), signif(BN_pw_jbtest$statistic[[1]], 4),
      signif(BT_pw_jbtest$statistic[[1]], 4), signif(BV_pw_jbtest$statistic[[1]], 4),
      signif(BR_pw_jbtest$statistic[[1]], 4), signif(BZ_pw_jbtest$statistic[[1]], 4),
      signif(CS_pw_jbtest$statistic[[1]], 4), signif(CL_pw_jbtest$statistic[[1]], 4),
      signif(CJ_pw_jbtest$statistic[[1]], 4), signif(CT_pw_jbtest$statistic[[1]], 4),
      signif(CV_pw_jbtest$statistic[[1]], 4), signif(DB_pw_jbtest$statistic[[1]], 4),
      signif(DJ_pw_jbtest$statistic[[1]], 4), signif(GL_pw_jbtest$statistic[[1]], 4),
      signif(GR_pw_jbtest$statistic[[1]], 4), signif(GJ_pw_jbtest$statistic[[1]], 4),
      signif(HR_pw_jbtest$statistic[[1]], 4), signif(HD_pw_jbtest$statistic[[1]], 4),
      signif(IL_pw_jbtest$statistic[[1]], 4), signif(IS_pw_jbtest$statistic[[1]], 4),
      signif(IF_pw_jbtest$statistic[[1]], 4), signif(MM_pw_jbtest$statistic[[1]], 4),
      signif(MH_pw_jbtest$statistic[[1]], 4), signif(MS_pw_jbtest$statistic[[1]], 4),
      signif(NT_pw_jbtest$statistic[[1]], 4), signif(OT_pw_jbtest$statistic[[1]], 4),
      signif(PH_pw_jbtest$statistic[[1]], 4), signif(SM_pw_jbtest$statistic[[1]], 4),
      signif(SJ_pw_jbtest$statistic[[1]], 4), signif(SB_pw_jbtest$statistic[[1]], 4),
      signif(SV_pw_jbtest$statistic[[1]], 4), signif(TR_pw_jbtest$statistic[[1]], 4),
      signif(TM_pw_jbtest$statistic[[1]], 4), signif(TL_pw_jbtest$statistic[[1]], 4),
      signif(VS_pw_jbtest$statistic[[1]], 4), signif(VL_pw_jbtest$statistic[[1]], 4),
      signif(VN_pw_jbtest$statistic[[1]], 4)),
    JB_PValue = c(
      signif(AB_pw_jbtest$p.value[[1]], 4), signif(AR_pw_jbtest$p.value[[1]], 4), 
      signif(AG_pw_jbtest$p.value[[1]], 4), signif(BC_pw_jbtest$p.value[[1]], 4),
      signif(BH_pw_jbtest$p.value[[1]], 4), signif(BN_pw_jbtest$p.value[[1]], 4),
      signif(BT_pw_jbtest$p.value[[1]], 4), signif(BV_pw_jbtest$p.value[[1]], 4),
      signif(BR_pw_jbtest$p.value[[1]], 4), signif(BZ_pw_jbtest$p.value[[1]], 4),
      signif(CS_pw_jbtest$p.value[[1]], 4), signif(CL_pw_jbtest$p.value[[1]], 4),
      signif(CJ_pw_jbtest$p.value[[1]], 4), signif(CT_pw_jbtest$p.value[[1]], 4),
      signif(CV_pw_jbtest$p.value[[1]], 4), signif(DB_pw_jbtest$p.value[[1]], 4),
      signif(DJ_pw_jbtest$p.value[[1]], 4), signif(GL_pw_jbtest$p.value[[1]], 4),
      signif(GR_pw_jbtest$p.value[[1]], 4), signif(GJ_pw_jbtest$p.value[[1]], 4),
      signif(HR_pw_jbtest$p.value[[1]], 4), signif(HD_pw_jbtest$p.value[[1]], 4),
      signif(IL_pw_jbtest$p.value[[1]], 4), signif(IS_pw_jbtest$p.value[[1]], 4),
      signif(IF_pw_jbtest$p.value[[1]], 4), signif(MM_pw_jbtest$p.value[[1]], 4),
      signif(MH_pw_jbtest$p.value[[1]], 4), signif(MS_pw_jbtest$p.value[[1]], 4),
      signif(NT_pw_jbtest$p.value[[1]], 4), signif(OT_pw_jbtest$p.value[[1]], 4),
      signif(PH_pw_jbtest$p.value[[1]], 4), signif(SM_pw_jbtest$p.value[[1]], 4),
      signif(SJ_pw_jbtest$p.value[[1]], 4), signif(SB_pw_jbtest$p.value[[1]], 4),
      signif(SV_pw_jbtest$p.value[[1]], 4), signif(TR_pw_jbtest$p.value[[1]], 4),
      signif(TM_pw_jbtest$p.value[[1]], 4), signif(TL_pw_jbtest$p.value[[1]], 4),
      signif(VS_pw_jbtest$p.value[[1]], 4), signif(VL_pw_jbtest$p.value[[1]], 4),
      signif(VN_pw_jbtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Exponential",
    JB_Test = c(
      signif(AB_exp_jbtest$statistic[[1]], 4), signif(AR_exp_jbtest$statistic[[1]], 4), 
      signif(AG_exp_jbtest$statistic[[1]], 4), signif(BC_exp_jbtest$statistic[[1]], 4),
      signif(BH_exp_jbtest$statistic[[1]], 4), signif(BN_exp_jbtest$statistic[[1]], 4),
      signif(BT_exp_jbtest$statistic[[1]], 4), signif(BV_exp_jbtest$statistic[[1]], 4),
      signif(BR_exp_jbtest$statistic[[1]], 4), signif(BZ_exp_jbtest$statistic[[1]], 4),
      signif(CS_exp_jbtest$statistic[[1]], 4), signif(CL_exp_jbtest$statistic[[1]], 4),
      signif(CJ_exp_jbtest$statistic[[1]], 4), signif(CT_exp_jbtest$statistic[[1]], 4),
      signif(CV_exp_jbtest$statistic[[1]], 4), signif(DB_exp_jbtest$statistic[[1]], 4),
      signif(DJ_exp_jbtest$statistic[[1]], 4), signif(GL_exp_jbtest$statistic[[1]], 4),
      signif(GR_exp_jbtest$statistic[[1]], 4), signif(GJ_exp_jbtest$statistic[[1]], 4),
      signif(HR_exp_jbtest$statistic[[1]], 4), signif(HD_exp_jbtest$statistic[[1]], 4),
      signif(IL_exp_jbtest$statistic[[1]], 4), signif(IS_exp_jbtest$statistic[[1]], 4),
      signif(IF_exp_jbtest$statistic[[1]], 4), signif(MM_exp_jbtest$statistic[[1]], 4),
      signif(MH_exp_jbtest$statistic[[1]], 4), signif(MS_exp_jbtest$statistic[[1]], 4),
      signif(NT_exp_jbtest$statistic[[1]], 4), signif(OT_exp_jbtest$statistic[[1]], 4),
      signif(PH_exp_jbtest$statistic[[1]], 4), signif(SM_exp_jbtest$statistic[[1]], 4),
      signif(SJ_exp_jbtest$statistic[[1]], 4), signif(SB_exp_jbtest$statistic[[1]], 4),
      signif(SV_exp_jbtest$statistic[[1]], 4), signif(TR_exp_jbtest$statistic[[1]], 4),
      signif(TM_exp_jbtest$statistic[[1]], 4), signif(TL_exp_jbtest$statistic[[1]], 4),
      signif(VS_exp_jbtest$statistic[[1]], 4), signif(VL_exp_jbtest$statistic[[1]], 4),
      signif(VN_exp_jbtest$statistic[[1]], 4)),
    JB_PValue = c(
      signif(AB_exp_jbtest$p.value[[1]], 4), signif(AR_exp_jbtest$p.value[[1]], 4), 
      signif(AG_exp_jbtest$p.value[[1]], 4), signif(BC_exp_jbtest$p.value[[1]], 4),
      signif(BH_exp_jbtest$p.value[[1]], 4), signif(BN_exp_jbtest$p.value[[1]], 4),
      signif(BT_exp_jbtest$p.value[[1]], 4), signif(BV_exp_jbtest$p.value[[1]], 4),
      signif(BR_exp_jbtest$p.value[[1]], 4), signif(BZ_exp_jbtest$p.value[[1]], 4),
      signif(CS_exp_jbtest$p.value[[1]], 4), signif(CL_exp_jbtest$p.value[[1]], 4),
      signif(CJ_exp_jbtest$p.value[[1]], 4), signif(CT_exp_jbtest$p.value[[1]], 4),
      signif(CV_exp_jbtest$p.value[[1]], 4), signif(DB_exp_jbtest$p.value[[1]], 4),
      signif(DJ_exp_jbtest$p.value[[1]], 4), signif(GL_exp_jbtest$p.value[[1]], 4),
      signif(GR_exp_jbtest$p.value[[1]], 4), signif(GJ_exp_jbtest$p.value[[1]], 4),
      signif(HR_exp_jbtest$p.value[[1]], 4), signif(HD_exp_jbtest$p.value[[1]], 4),
      signif(IL_exp_jbtest$p.value[[1]], 4), signif(IS_exp_jbtest$p.value[[1]], 4),
      signif(IF_exp_jbtest$p.value[[1]], 4), signif(MM_exp_jbtest$p.value[[1]], 4),
      signif(MH_exp_jbtest$p.value[[1]], 4), signif(MS_exp_jbtest$p.value[[1]], 4),
      signif(NT_exp_jbtest$p.value[[1]], 4), signif(OT_exp_jbtest$p.value[[1]], 4),
      signif(PH_exp_jbtest$p.value[[1]], 4), signif(SM_exp_jbtest$p.value[[1]], 4),
      signif(SJ_exp_jbtest$p.value[[1]], 4), signif(SB_exp_jbtest$p.value[[1]], 4),
      signif(SV_exp_jbtest$p.value[[1]], 4), signif(TR_exp_jbtest$p.value[[1]], 4),
      signif(TM_exp_jbtest$p.value[[1]], 4), signif(TL_exp_jbtest$p.value[[1]], 4),
      signif(VS_exp_jbtest$p.value[[1]], 4), signif(VL_exp_jbtest$p.value[[1]], 4),
      signif(VN_exp_jbtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Zipf-Mandelbrot",
    JB_Test = c(
      signif(AB_ZM_jbtest$statistic[[1]], 4), signif(AR_ZM_jbtest$statistic[[1]], 4), 
      signif(AG_ZM_jbtest$statistic[[1]], 4), signif(BC_ZM_jbtest$statistic[[1]], 4),
      signif(BH_ZM_jbtest$statistic[[1]], 4), signif(BN_ZM_jbtest$statistic[[1]], 4),
      signif(BT_ZM_jbtest$statistic[[1]], 4), signif(BV_ZM_jbtest$statistic[[1]], 4),
      signif(BR_ZM_jbtest$statistic[[1]], 4), signif(BZ_ZM_jbtest$statistic[[1]], 4),
      signif(CS_ZM_jbtest$statistic[[1]], 4), signif(CL_ZM_jbtest$statistic[[1]], 4),
      signif(CJ_ZM_jbtest$statistic[[1]], 4), signif(CT_ZM_jbtest$statistic[[1]], 4),
      signif(CV_ZM_jbtest$statistic[[1]], 4), signif(DB_ZM_jbtest$statistic[[1]], 4),
      signif(DJ_ZM_jbtest$statistic[[1]], 4), signif(GL_ZM_jbtest$statistic[[1]], 4),
      signif(GR_ZM_jbtest$statistic[[1]], 4), signif(GJ_ZM_jbtest$statistic[[1]], 4),
      signif(HR_ZM_jbtest$statistic[[1]], 4), signif(HD_ZM_jbtest$statistic[[1]], 4),
      signif(IL_ZM_jbtest$statistic[[1]], 4), signif(IS_ZM_jbtest$statistic[[1]], 4),
      signif(IF_ZM_jbtest$statistic[[1]], 4), signif(MM_ZM_jbtest$statistic[[1]], 4),
      signif(MH_ZM_jbtest$statistic[[1]], 4), signif(MS_ZM_jbtest$statistic[[1]], 4),
      signif(NT_ZM_jbtest$statistic[[1]], 4), signif(OT_ZM_jbtest$statistic[[1]], 4),
      signif(PH_ZM_jbtest$statistic[[1]], 4), signif(SM_ZM_jbtest$statistic[[1]], 4),
      signif(SJ_ZM_jbtest$statistic[[1]], 4), signif(SB_ZM_jbtest$statistic[[1]], 4),
      signif(SV_ZM_jbtest$statistic[[1]], 4), signif(TR_ZM_jbtest$statistic[[1]], 4),
      signif(TM_ZM_jbtest$statistic[[1]], 4), signif(TL_ZM_jbtest$statistic[[1]], 4),
      signif(VS_ZM_jbtest$statistic[[1]], 4), signif(VL_ZM_jbtest$statistic[[1]], 4),
      signif(VN_ZM_jbtest$statistic[[1]], 4)),
    JB_PValue = c(
      signif(AB_ZM_jbtest$p.value[[1]], 4), signif(AR_ZM_jbtest$p.value[[1]], 4), 
      signif(AG_ZM_jbtest$p.value[[1]], 4), signif(BC_ZM_jbtest$p.value[[1]], 4),
      signif(BH_ZM_jbtest$p.value[[1]], 4), signif(BN_ZM_jbtest$p.value[[1]], 4),
      signif(BT_ZM_jbtest$p.value[[1]], 4), signif(BV_ZM_jbtest$p.value[[1]], 4),
      signif(BR_ZM_jbtest$p.value[[1]], 4), signif(BZ_ZM_jbtest$p.value[[1]], 4),
      signif(CS_ZM_jbtest$p.value[[1]], 4), signif(CL_ZM_jbtest$p.value[[1]], 4),
      signif(CJ_ZM_jbtest$p.value[[1]], 4), signif(CT_ZM_jbtest$p.value[[1]], 4),
      signif(CV_ZM_jbtest$p.value[[1]], 4), signif(DB_ZM_jbtest$p.value[[1]], 4),
      signif(DJ_ZM_jbtest$p.value[[1]], 4), signif(GL_ZM_jbtest$p.value[[1]], 4),
      signif(GR_ZM_jbtest$p.value[[1]], 4), signif(GJ_ZM_jbtest$p.value[[1]], 4),
      signif(HR_ZM_jbtest$p.value[[1]], 4), signif(HD_ZM_jbtest$p.value[[1]], 4),
      signif(IL_ZM_jbtest$p.value[[1]], 4), signif(IS_ZM_jbtest$p.value[[1]], 4),
      signif(IF_ZM_jbtest$p.value[[1]], 4), signif(MM_ZM_jbtest$p.value[[1]], 4),
      signif(MH_ZM_jbtest$p.value[[1]], 4), signif(MS_ZM_jbtest$p.value[[1]], 4),
      signif(NT_ZM_jbtest$p.value[[1]], 4), signif(OT_ZM_jbtest$p.value[[1]], 4),
      signif(PH_ZM_jbtest$p.value[[1]], 4), signif(SM_ZM_jbtest$p.value[[1]], 4),
      signif(SJ_ZM_jbtest$p.value[[1]], 4), signif(SB_ZM_jbtest$p.value[[1]], 4),
      signif(SV_ZM_jbtest$p.value[[1]], 4), signif(TR_ZM_jbtest$p.value[[1]], 4),
      signif(TM_ZM_jbtest$p.value[[1]], 4), signif(TL_ZM_jbtest$p.value[[1]], 4),
      signif(VS_ZM_jbtest$p.value[[1]], 4), signif(VL_ZM_jbtest$p.value[[1]], 4),
      signif(VN_ZM_jbtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Lavalette",
    JB_Test = c(
      signif(AB_lav_jbtest$statistic[[1]], 4), signif(AR_lav_jbtest$statistic[[1]], 4), 
      signif(AG_lav_jbtest$statistic[[1]], 4), signif(BC_lav_jbtest$statistic[[1]], 4),
      signif(BH_lav_jbtest$statistic[[1]], 4), signif(BN_lav_jbtest$statistic[[1]], 4),
      signif(BT_lav_jbtest$statistic[[1]], 4), signif(BV_lav_jbtest$statistic[[1]], 4),
      signif(BR_lav_jbtest$statistic[[1]], 4), signif(BZ_lav_jbtest$statistic[[1]], 4),
      signif(CS_lav_jbtest$statistic[[1]], 4), signif(CL_lav_jbtest$statistic[[1]], 4),
      signif(CJ_lav_jbtest$statistic[[1]], 4), signif(CT_lav_jbtest$statistic[[1]], 4),
      signif(CV_lav_jbtest$statistic[[1]], 4), signif(DB_lav_jbtest$statistic[[1]], 4),
      signif(DJ_lav_jbtest$statistic[[1]], 4), signif(GL_lav_jbtest$statistic[[1]], 4),
      signif(GR_lav_jbtest$statistic[[1]], 4), signif(GJ_lav_jbtest$statistic[[1]], 4),
      signif(HR_lav_jbtest$statistic[[1]], 4), signif(HD_lav_jbtest$statistic[[1]], 4),
      signif(IL_lav_jbtest$statistic[[1]], 4), signif(IS_lav_jbtest$statistic[[1]], 4),
      signif(IF_lav_jbtest$statistic[[1]], 4), signif(MM_lav_jbtest$statistic[[1]], 4),
      signif(MH_lav_jbtest$statistic[[1]], 4), signif(MS_lav_jbtest$statistic[[1]], 4),
      signif(NT_lav_jbtest$statistic[[1]], 4), signif(OT_lav_jbtest$statistic[[1]], 4),
      signif(PH_lav_jbtest$statistic[[1]], 4), signif(SM_lav_jbtest$statistic[[1]], 4),
      signif(SJ_lav_jbtest$statistic[[1]], 4), signif(SB_lav_jbtest$statistic[[1]], 4),
      signif(SV_lav_jbtest$statistic[[1]], 4), signif(TR_lav_jbtest$statistic[[1]], 4),
      signif(TM_lav_jbtest$statistic[[1]], 4), signif(TL_lav_jbtest$statistic[[1]], 4),
      signif(VS_lav_jbtest$statistic[[1]], 4), signif(VL_lav_jbtest$statistic[[1]], 4),
      signif(VN_lav_jbtest$statistic[[1]], 4)),
    JB_PValue = c(
      signif(AB_lav_jbtest$p.value[[1]], 4), signif(AR_lav_jbtest$p.value[[1]], 4), 
      signif(AG_lav_jbtest$p.value[[1]], 4), signif(BC_lav_jbtest$p.value[[1]], 4),
      signif(BH_lav_jbtest$p.value[[1]], 4), signif(BN_lav_jbtest$p.value[[1]], 4),
      signif(BT_lav_jbtest$p.value[[1]], 4), signif(BV_lav_jbtest$p.value[[1]], 4),
      signif(BR_lav_jbtest$p.value[[1]], 4), signif(BZ_lav_jbtest$p.value[[1]], 4),
      signif(CS_lav_jbtest$p.value[[1]], 4), signif(CL_lav_jbtest$p.value[[1]], 4),
      signif(CJ_lav_jbtest$p.value[[1]], 4), signif(CT_lav_jbtest$p.value[[1]], 4),
      signif(CV_lav_jbtest$p.value[[1]], 4), signif(DB_lav_jbtest$p.value[[1]], 4),
      signif(DJ_lav_jbtest$p.value[[1]], 4), signif(GL_lav_jbtest$p.value[[1]], 4),
      signif(GR_lav_jbtest$p.value[[1]], 4), signif(GJ_lav_jbtest$p.value[[1]], 4),
      signif(HR_lav_jbtest$p.value[[1]], 4), signif(HD_lav_jbtest$p.value[[1]], 4),
      signif(IL_lav_jbtest$p.value[[1]], 4), signif(IS_lav_jbtest$p.value[[1]], 4),
      signif(IF_lav_jbtest$p.value[[1]], 4), signif(MM_lav_jbtest$p.value[[1]], 4),
      signif(MH_lav_jbtest$p.value[[1]], 4), signif(MS_lav_jbtest$p.value[[1]], 4),
      signif(NT_lav_jbtest$p.value[[1]], 4), signif(OT_lav_jbtest$p.value[[1]], 4),
      signif(PH_lav_jbtest$p.value[[1]], 4), signif(SM_lav_jbtest$p.value[[1]], 4),
      signif(SJ_lav_jbtest$p.value[[1]], 4), signif(SB_lav_jbtest$p.value[[1]], 4),
      signif(SV_lav_jbtest$p.value[[1]], 4), signif(TR_lav_jbtest$p.value[[1]], 4),
      signif(TM_lav_jbtest$p.value[[1]], 4), signif(TL_lav_jbtest$p.value[[1]], 4),
      signif(VS_lav_jbtest$p.value[[1]], 4), signif(VL_lav_jbtest$p.value[[1]], 4),
      signif(VN_lav_jbtest$p.value[[1]], 4))
  ))
) %>% mutate(Normal_Distribution = if_else(JB_PValue > 0.05, "Y", "N"))

# 12. Shapiro-Wilks Test
SH_test_t <- bind_rows(
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Power",
    SH_Test = c(
      signif(AB_pw_shtest$statistic[[1]], 4), signif(AR_pw_shtest$statistic[[1]], 4), 
      signif(AG_pw_shtest$statistic[[1]], 4), signif(BC_pw_shtest$statistic[[1]], 4),
      signif(BH_pw_shtest$statistic[[1]], 4), signif(BN_pw_shtest$statistic[[1]], 4),
      signif(BT_pw_shtest$statistic[[1]], 4), signif(BV_pw_shtest$statistic[[1]], 4),
      signif(BR_pw_shtest$statistic[[1]], 4), signif(BZ_pw_shtest$statistic[[1]], 4),
      signif(CS_pw_shtest$statistic[[1]], 4), signif(CL_pw_shtest$statistic[[1]], 4),
      signif(CJ_pw_shtest$statistic[[1]], 4), signif(CT_pw_shtest$statistic[[1]], 4),
      signif(CV_pw_shtest$statistic[[1]], 4), signif(DB_pw_shtest$statistic[[1]], 4),
      signif(DJ_pw_shtest$statistic[[1]], 4), signif(GL_pw_shtest$statistic[[1]], 4),
      signif(GR_pw_shtest$statistic[[1]], 4), signif(GJ_pw_shtest$statistic[[1]], 4),
      signif(HR_pw_shtest$statistic[[1]], 4), signif(HD_pw_shtest$statistic[[1]], 4),
      signif(IL_pw_shtest$statistic[[1]], 4), signif(IS_pw_shtest$statistic[[1]], 4),
      signif(IF_pw_shtest$statistic[[1]], 4), signif(MM_pw_shtest$statistic[[1]], 4),
      signif(MH_pw_shtest$statistic[[1]], 4), signif(MS_pw_shtest$statistic[[1]], 4),
      signif(NT_pw_shtest$statistic[[1]], 4), signif(OT_pw_shtest$statistic[[1]], 4),
      signif(PH_pw_shtest$statistic[[1]], 4), signif(SM_pw_shtest$statistic[[1]], 4),
      signif(SJ_pw_shtest$statistic[[1]], 4), signif(SB_pw_shtest$statistic[[1]], 4),
      signif(SV_pw_shtest$statistic[[1]], 4), signif(TR_pw_shtest$statistic[[1]], 4),
      signif(TM_pw_shtest$statistic[[1]], 4), signif(TL_pw_shtest$statistic[[1]], 4),
      signif(VS_pw_shtest$statistic[[1]], 4), signif(VL_pw_shtest$statistic[[1]], 4),
      signif(VN_pw_shtest$statistic[[1]], 4)),
    SH_PValue = c(
      signif(AB_pw_shtest$p.value[[1]], 4), signif(AR_pw_shtest$p.value[[1]], 4), 
      signif(AG_pw_shtest$p.value[[1]], 4), signif(BC_pw_shtest$p.value[[1]], 4),
      signif(BH_pw_shtest$p.value[[1]], 4), signif(BN_pw_shtest$p.value[[1]], 4),
      signif(BT_pw_shtest$p.value[[1]], 4), signif(BV_pw_shtest$p.value[[1]], 4),
      signif(BR_pw_shtest$p.value[[1]], 4), signif(BZ_pw_shtest$p.value[[1]], 4),
      signif(CS_pw_shtest$p.value[[1]], 4), signif(CL_pw_shtest$p.value[[1]], 4),
      signif(CJ_pw_shtest$p.value[[1]], 4), signif(CT_pw_shtest$p.value[[1]], 4),
      signif(CV_pw_shtest$p.value[[1]], 4), signif(DB_pw_shtest$p.value[[1]], 4),
      signif(DJ_pw_shtest$p.value[[1]], 4), signif(GL_pw_shtest$p.value[[1]], 4),
      signif(GR_pw_shtest$p.value[[1]], 4), signif(GJ_pw_shtest$p.value[[1]], 4),
      signif(HR_pw_shtest$p.value[[1]], 4), signif(HD_pw_shtest$p.value[[1]], 4),
      signif(IL_pw_shtest$p.value[[1]], 4), signif(IS_pw_shtest$p.value[[1]], 4),
      signif(IF_pw_shtest$p.value[[1]], 4), signif(MM_pw_shtest$p.value[[1]], 4),
      signif(MH_pw_shtest$p.value[[1]], 4), signif(MS_pw_shtest$p.value[[1]], 4),
      signif(NT_pw_shtest$p.value[[1]], 4), signif(OT_pw_shtest$p.value[[1]], 4),
      signif(PH_pw_shtest$p.value[[1]], 4), signif(SM_pw_shtest$p.value[[1]], 4),
      signif(SJ_pw_shtest$p.value[[1]], 4), signif(SB_pw_shtest$p.value[[1]], 4),
      signif(SV_pw_shtest$p.value[[1]], 4), signif(TR_pw_shtest$p.value[[1]], 4),
      signif(TM_pw_shtest$p.value[[1]], 4), signif(TL_pw_shtest$p.value[[1]], 4),
      signif(VS_pw_shtest$p.value[[1]], 4), signif(VL_pw_shtest$p.value[[1]], 4),
      signif(VN_pw_shtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Exponential",
    SH_Test = c(
      signif(AB_exp_shtest$statistic[[1]], 4), signif(AR_exp_shtest$statistic[[1]], 4), 
      signif(AG_exp_shtest$statistic[[1]], 4), signif(BC_exp_shtest$statistic[[1]], 4),
      signif(BH_exp_shtest$statistic[[1]], 4), signif(BN_exp_shtest$statistic[[1]], 4),
      signif(BT_exp_shtest$statistic[[1]], 4), signif(BV_exp_shtest$statistic[[1]], 4),
      signif(BR_exp_shtest$statistic[[1]], 4), signif(BZ_exp_shtest$statistic[[1]], 4),
      signif(CS_exp_shtest$statistic[[1]], 4), signif(CL_exp_shtest$statistic[[1]], 4),
      signif(CJ_exp_shtest$statistic[[1]], 4), signif(CT_exp_shtest$statistic[[1]], 4),
      signif(CV_exp_shtest$statistic[[1]], 4), signif(DB_exp_shtest$statistic[[1]], 4),
      signif(DJ_exp_shtest$statistic[[1]], 4), signif(GL_exp_shtest$statistic[[1]], 4),
      signif(GR_exp_shtest$statistic[[1]], 4), signif(GJ_exp_shtest$statistic[[1]], 4),
      signif(HR_exp_shtest$statistic[[1]], 4), signif(HD_exp_shtest$statistic[[1]], 4),
      signif(IL_exp_shtest$statistic[[1]], 4), signif(IS_exp_shtest$statistic[[1]], 4),
      signif(IF_exp_shtest$statistic[[1]], 4), signif(MM_exp_shtest$statistic[[1]], 4),
      signif(MH_exp_shtest$statistic[[1]], 4), signif(MS_exp_shtest$statistic[[1]], 4),
      signif(NT_exp_shtest$statistic[[1]], 4), signif(OT_exp_shtest$statistic[[1]], 4),
      signif(PH_exp_shtest$statistic[[1]], 4), signif(SM_exp_shtest$statistic[[1]], 4),
      signif(SJ_exp_shtest$statistic[[1]], 4), signif(SB_exp_shtest$statistic[[1]], 4),
      signif(SV_exp_shtest$statistic[[1]], 4), signif(TR_exp_shtest$statistic[[1]], 4),
      signif(TM_exp_shtest$statistic[[1]], 4), signif(TL_exp_shtest$statistic[[1]], 4),
      signif(VS_exp_shtest$statistic[[1]], 4), signif(VL_exp_shtest$statistic[[1]], 4),
      signif(VN_exp_shtest$statistic[[1]], 4)),
    SH_PValue = c(
      signif(AB_exp_shtest$p.value[[1]], 4), signif(AR_exp_shtest$p.value[[1]], 4), 
      signif(AG_exp_shtest$p.value[[1]], 4), signif(BC_exp_shtest$p.value[[1]], 4),
      signif(BH_exp_shtest$p.value[[1]], 4), signif(BN_exp_shtest$p.value[[1]], 4),
      signif(BT_exp_shtest$p.value[[1]], 4), signif(BV_exp_shtest$p.value[[1]], 4),
      signif(BR_exp_shtest$p.value[[1]], 4), signif(BZ_exp_shtest$p.value[[1]], 4),
      signif(CS_exp_shtest$p.value[[1]], 4), signif(CL_exp_shtest$p.value[[1]], 4),
      signif(CJ_exp_shtest$p.value[[1]], 4), signif(CT_exp_shtest$p.value[[1]], 4),
      signif(CV_exp_shtest$p.value[[1]], 4), signif(DB_exp_shtest$p.value[[1]], 4),
      signif(DJ_exp_shtest$p.value[[1]], 4), signif(GL_exp_shtest$p.value[[1]], 4),
      signif(GR_exp_shtest$p.value[[1]], 4), signif(GJ_exp_shtest$p.value[[1]], 4),
      signif(HR_exp_shtest$p.value[[1]], 4), signif(HD_exp_shtest$p.value[[1]], 4),
      signif(IL_exp_shtest$p.value[[1]], 4), signif(IS_exp_shtest$p.value[[1]], 4),
      signif(IF_exp_shtest$p.value[[1]], 4), signif(MM_exp_shtest$p.value[[1]], 4),
      signif(MH_exp_shtest$p.value[[1]], 4), signif(MS_exp_shtest$p.value[[1]], 4),
      signif(NT_exp_shtest$p.value[[1]], 4), signif(OT_exp_shtest$p.value[[1]], 4),
      signif(PH_exp_shtest$p.value[[1]], 4), signif(SM_exp_shtest$p.value[[1]], 4),
      signif(SJ_exp_shtest$p.value[[1]], 4), signif(SB_exp_shtest$p.value[[1]], 4),
      signif(SV_exp_shtest$p.value[[1]], 4), signif(TR_exp_shtest$p.value[[1]], 4),
      signif(TM_exp_shtest$p.value[[1]], 4), signif(TL_exp_shtest$p.value[[1]], 4),
      signif(VS_exp_shtest$p.value[[1]], 4), signif(VL_exp_shtest$p.value[[1]], 4),
      signif(VN_exp_shtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Zipf-Mandelbrot",
    SH_Test = c(
      signif(AB_ZM_shtest$statistic[[1]], 4), signif(AR_ZM_shtest$statistic[[1]], 4), 
      signif(AG_ZM_shtest$statistic[[1]], 4), signif(BC_ZM_shtest$statistic[[1]], 4),
      signif(BH_ZM_shtest$statistic[[1]], 4), signif(BN_ZM_shtest$statistic[[1]], 4),
      signif(BT_ZM_shtest$statistic[[1]], 4), signif(BV_ZM_shtest$statistic[[1]], 4),
      signif(BR_ZM_shtest$statistic[[1]], 4), signif(BZ_ZM_shtest$statistic[[1]], 4),
      signif(CS_ZM_shtest$statistic[[1]], 4), signif(CL_ZM_shtest$statistic[[1]], 4),
      signif(CJ_ZM_shtest$statistic[[1]], 4), signif(CT_ZM_shtest$statistic[[1]], 4),
      signif(CV_ZM_shtest$statistic[[1]], 4), signif(DB_ZM_shtest$statistic[[1]], 4),
      signif(DJ_ZM_shtest$statistic[[1]], 4), signif(GL_ZM_shtest$statistic[[1]], 4),
      signif(GR_ZM_shtest$statistic[[1]], 4), signif(GJ_ZM_shtest$statistic[[1]], 4),
      signif(HR_ZM_shtest$statistic[[1]], 4), signif(HD_ZM_shtest$statistic[[1]], 4),
      signif(IL_ZM_shtest$statistic[[1]], 4), signif(IS_ZM_shtest$statistic[[1]], 4),
      signif(IF_ZM_shtest$statistic[[1]], 4), signif(MM_ZM_shtest$statistic[[1]], 4),
      signif(MH_ZM_shtest$statistic[[1]], 4), signif(MS_ZM_shtest$statistic[[1]], 4),
      signif(NT_ZM_shtest$statistic[[1]], 4), signif(OT_ZM_shtest$statistic[[1]], 4),
      signif(PH_ZM_shtest$statistic[[1]], 4), signif(SM_ZM_shtest$statistic[[1]], 4),
      signif(SJ_ZM_shtest$statistic[[1]], 4), signif(SB_ZM_shtest$statistic[[1]], 4),
      signif(SV_ZM_shtest$statistic[[1]], 4), signif(TR_ZM_shtest$statistic[[1]], 4),
      signif(TM_ZM_shtest$statistic[[1]], 4), signif(TL_ZM_shtest$statistic[[1]], 4),
      signif(VS_ZM_shtest$statistic[[1]], 4), signif(VL_ZM_shtest$statistic[[1]], 4),
      signif(VN_ZM_shtest$statistic[[1]], 4)),
    SH_PValue = c(
      signif(AB_ZM_shtest$p.value[[1]], 4), signif(AR_ZM_shtest$p.value[[1]], 4), 
      signif(AG_ZM_shtest$p.value[[1]], 4), signif(BC_ZM_shtest$p.value[[1]], 4),
      signif(BH_ZM_shtest$p.value[[1]], 4), signif(BN_ZM_shtest$p.value[[1]], 4),
      signif(BT_ZM_shtest$p.value[[1]], 4), signif(BV_ZM_shtest$p.value[[1]], 4),
      signif(BR_ZM_shtest$p.value[[1]], 4), signif(BZ_ZM_shtest$p.value[[1]], 4),
      signif(CS_ZM_shtest$p.value[[1]], 4), signif(CL_ZM_shtest$p.value[[1]], 4),
      signif(CJ_ZM_shtest$p.value[[1]], 4), signif(CT_ZM_shtest$p.value[[1]], 4),
      signif(CV_ZM_shtest$p.value[[1]], 4), signif(DB_ZM_shtest$p.value[[1]], 4),
      signif(DJ_ZM_shtest$p.value[[1]], 4), signif(GL_ZM_shtest$p.value[[1]], 4),
      signif(GR_ZM_shtest$p.value[[1]], 4), signif(GJ_ZM_shtest$p.value[[1]], 4),
      signif(HR_ZM_shtest$p.value[[1]], 4), signif(HD_ZM_shtest$p.value[[1]], 4),
      signif(IL_ZM_shtest$p.value[[1]], 4), signif(IS_ZM_shtest$p.value[[1]], 4),
      signif(IF_ZM_shtest$p.value[[1]], 4), signif(MM_ZM_shtest$p.value[[1]], 4),
      signif(MH_ZM_shtest$p.value[[1]], 4), signif(MS_ZM_shtest$p.value[[1]], 4),
      signif(NT_ZM_shtest$p.value[[1]], 4), signif(OT_ZM_shtest$p.value[[1]], 4),
      signif(PH_ZM_shtest$p.value[[1]], 4), signif(SM_ZM_shtest$p.value[[1]], 4),
      signif(SJ_ZM_shtest$p.value[[1]], 4), signif(SB_ZM_shtest$p.value[[1]], 4),
      signif(SV_ZM_shtest$p.value[[1]], 4), signif(TR_ZM_shtest$p.value[[1]], 4),
      signif(TM_ZM_shtest$p.value[[1]], 4), signif(TL_ZM_shtest$p.value[[1]], 4),
      signif(VS_ZM_shtest$p.value[[1]], 4), signif(VL_ZM_shtest$p.value[[1]], 4),
      signif(VN_ZM_shtest$p.value[[1]], 4))
  )),
  as.data.frame(list(
    Judet_Name = (RO_Census_df %>% dplyr::select(Judet_Name) %>% distinct() %>% 
                    filter(!is.na(Judet_Name))),
    Function_Name = "Lavalette",
    SH_Test = c(
      signif(AB_lav_shtest$statistic[[1]], 4), signif(AR_lav_shtest$statistic[[1]], 4), 
      signif(AG_lav_shtest$statistic[[1]], 4), signif(BC_lav_shtest$statistic[[1]], 4),
      signif(BH_lav_shtest$statistic[[1]], 4), signif(BN_lav_shtest$statistic[[1]], 4),
      signif(BT_lav_shtest$statistic[[1]], 4), signif(BV_lav_shtest$statistic[[1]], 4),
      signif(BR_lav_shtest$statistic[[1]], 4), signif(BZ_lav_shtest$statistic[[1]], 4),
      signif(CS_lav_shtest$statistic[[1]], 4), signif(CL_lav_shtest$statistic[[1]], 4),
      signif(CJ_lav_shtest$statistic[[1]], 4), signif(CT_lav_shtest$statistic[[1]], 4),
      signif(CV_lav_shtest$statistic[[1]], 4), signif(DB_lav_shtest$statistic[[1]], 4),
      signif(DJ_lav_shtest$statistic[[1]], 4), signif(GL_lav_shtest$statistic[[1]], 4),
      signif(GR_lav_shtest$statistic[[1]], 4), signif(GJ_lav_shtest$statistic[[1]], 4),
      signif(HR_lav_shtest$statistic[[1]], 4), signif(HD_lav_shtest$statistic[[1]], 4),
      signif(IL_lav_shtest$statistic[[1]], 4), signif(IS_lav_shtest$statistic[[1]], 4),
      signif(IF_lav_shtest$statistic[[1]], 4), signif(MM_lav_shtest$statistic[[1]], 4),
      signif(MH_lav_shtest$statistic[[1]], 4), signif(MS_lav_shtest$statistic[[1]], 4),
      signif(NT_lav_shtest$statistic[[1]], 4), signif(OT_lav_shtest$statistic[[1]], 4),
      signif(PH_lav_shtest$statistic[[1]], 4), signif(SM_lav_shtest$statistic[[1]], 4),
      signif(SJ_lav_shtest$statistic[[1]], 4), signif(SB_lav_shtest$statistic[[1]], 4),
      signif(SV_lav_shtest$statistic[[1]], 4), signif(TR_lav_shtest$statistic[[1]], 4),
      signif(TM_lav_shtest$statistic[[1]], 4), signif(TL_lav_shtest$statistic[[1]], 4),
      signif(VS_lav_shtest$statistic[[1]], 4), signif(VL_lav_shtest$statistic[[1]], 4),
      signif(VN_lav_shtest$statistic[[1]], 4)),
    SH_PValue = c(
      signif(AB_lav_shtest$p.value[[1]], 4), signif(AR_lav_shtest$p.value[[1]], 4), 
      signif(AG_lav_shtest$p.value[[1]], 4), signif(BC_lav_shtest$p.value[[1]], 4),
      signif(BH_lav_shtest$p.value[[1]], 4), signif(BN_lav_shtest$p.value[[1]], 4),
      signif(BT_lav_shtest$p.value[[1]], 4), signif(BV_lav_shtest$p.value[[1]], 4),
      signif(BR_lav_shtest$p.value[[1]], 4), signif(BZ_lav_shtest$p.value[[1]], 4),
      signif(CS_lav_shtest$p.value[[1]], 4), signif(CL_lav_shtest$p.value[[1]], 4),
      signif(CJ_lav_shtest$p.value[[1]], 4), signif(CT_lav_shtest$p.value[[1]], 4),
      signif(CV_lav_shtest$p.value[[1]], 4), signif(DB_lav_shtest$p.value[[1]], 4),
      signif(DJ_lav_shtest$p.value[[1]], 4), signif(GL_lav_shtest$p.value[[1]], 4),
      signif(GR_lav_shtest$p.value[[1]], 4), signif(GJ_lav_shtest$p.value[[1]], 4),
      signif(HR_lav_shtest$p.value[[1]], 4), signif(HD_lav_shtest$p.value[[1]], 4),
      signif(IL_lav_shtest$p.value[[1]], 4), signif(IS_lav_shtest$p.value[[1]], 4),
      signif(IF_lav_shtest$p.value[[1]], 4), signif(MM_lav_shtest$p.value[[1]], 4),
      signif(MH_lav_shtest$p.value[[1]], 4), signif(MS_lav_shtest$p.value[[1]], 4),
      signif(NT_lav_shtest$p.value[[1]], 4), signif(OT_lav_shtest$p.value[[1]], 4),
      signif(PH_lav_shtest$p.value[[1]], 4), signif(SM_lav_shtest$p.value[[1]], 4),
      signif(SJ_lav_shtest$p.value[[1]], 4), signif(SB_lav_shtest$p.value[[1]], 4),
      signif(SV_lav_shtest$p.value[[1]], 4), signif(TR_lav_shtest$p.value[[1]], 4),
      signif(TM_lav_shtest$p.value[[1]], 4), signif(TL_lav_shtest$p.value[[1]], 4),
      signif(VS_lav_shtest$p.value[[1]], 4), signif(VL_lav_shtest$p.value[[1]], 4),
      signif(VN_lav_shtest$p.value[[1]], 4))
  ))
) %>% mutate(Normal_Distribution = if_else(SH_PValue > 0.05, "Y", "N"))

# IV. Export files
#write_csv(RSquared_t, "./RO_Census_RSquared.csv")

write.xlsx(list("R-Squared" = RSquared_t, "Adjusted R-Squared" = Adj_RSquared_t,
                "F-Stat" = F_Stat_t, "AIC" = AIC_t, "White Test" = White_test_t,
                "Breusch-Pagan Test" = BP_test_t, "Durbin-Watson Test" = DW_test_t,
                "Breusch-Godfrey Test" = BG_test_t, "Jarque-Bera Test" = JB_test_t,
                "Shapiro-Wilks" = SH_test_t), 
           file = 'RO_Census_Indicators_lm_new_popsize.xlsx')
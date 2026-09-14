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

RSquared_vals_df <- read_excel("./RO_Census_Indicators_lm.xlsx", sheet = 1)

# Load Census with predicts
RO_Census_Alba <- read_excel("./RO_Census_Predict.xlsx", sheet = "Alba")
RO_Census_Arad <- read_excel("./RO_Census_Predict.xlsx", sheet = "Arad")
RO_Census_Arges <- read_excel("./RO_Census_Predict.xlsx", sheet = "Arges")
RO_Census_Bacau <- read_excel("./RO_Census_Predict.xlsx", sheet = "Bacau")
RO_Census_Bihor <- read_excel("./RO_Census_Predict.xlsx", sheet = "Bihor")
RO_Census_BistNsd <- read_excel("./RO_Census_Predict.xlsx", sheet = "Bistrita-Nasaud")
RO_Census_Botosani <- read_excel("./RO_Census_Predict.xlsx", sheet = "Botosani")
RO_Census_Braila <- read_excel("./RO_Census_Predict.xlsx", sheet = "Braila")
RO_Census_Brasov <- read_excel("./RO_Census_Predict.xlsx", sheet = "Brasov")
RO_Census_Buzau <- read_excel("./RO_Census_Predict.xlsx", sheet = "Buzau")
RO_Census_CarSev <- read_excel("./RO_Census_Predict.xlsx", sheet = "Caras-Severin")
RO_Census_Calarasi <- read_excel("./RO_Census_Predict.xlsx", sheet = "Calarasi")
RO_Census_Cluj <- read_excel("./RO_Census_Predict.xlsx", sheet = "Cluj")
RO_Census_Constanta <- read_excel("./RO_Census_Predict.xlsx", sheet = "Constanta")
RO_Census_Covasna <- read_excel("./RO_Census_Predict.xlsx", sheet = "Covasna")
RO_Census_Dambovita <- read_excel("./RO_Census_Predict.xlsx", sheet = "Dambovita")
RO_Census_Dolj <- read_excel("./RO_Census_Predict.xlsx", sheet = "Dolj")
RO_Census_Galati <- read_excel("./RO_Census_Predict.xlsx", sheet = "Galati")
RO_Census_Giurgiu <- read_excel("./RO_Census_Predict.xlsx", sheet = "Giurgiu")
RO_Census_Gorj <- read_excel("./RO_Census_Predict.xlsx", sheet = "Gorj")
RO_Census_Harghita <- read_excel("./RO_Census_Predict.xlsx", sheet = "Harghita")
RO_Census_Hunedoara <- read_excel("./RO_Census_Predict.xlsx", sheet = "Hunedoara")
RO_Census_Ialomita <- read_excel("./RO_Census_Predict.xlsx", sheet = "Ialomita")
RO_Census_Iasi <- read_excel("./RO_Census_Predict.xlsx", sheet = "Iasi")
RO_Census_Ilfov <- read_excel("./RO_Census_Predict.xlsx", sheet = "Ilfov")
RO_Census_Maramures <- read_excel("./RO_Census_Predict.xlsx", sheet = "Maramures")
RO_Census_Mehedinti <- read_excel("./RO_Census_Predict.xlsx", sheet = "Mehedinti")
RO_Census_Mures <- read_excel("./RO_Census_Predict.xlsx", sheet = "Mures")
RO_Census_Neamt <- read_excel("./RO_Census_Predict.xlsx", sheet = "Neamt")
RO_Census_Olt <- read_excel("./RO_Census_Predict.xlsx", sheet = "Olt")
RO_Census_Prahova <- read_excel("./RO_Census_Predict.xlsx", sheet = "Prahova")
RO_Census_SatuMare <- read_excel("./RO_Census_Predict.xlsx", sheet = "Satu Mare")
RO_Census_Salaj <- read_excel("./RO_Census_Predict.xlsx", sheet = "Salaj")
RO_Census_Sibiu <- read_excel("./RO_Census_Predict.xlsx", sheet = "Sibiu")
RO_Census_Suceava <- read_excel("./RO_Census_Predict.xlsx", sheet = "Suceava")
RO_Census_Teleorman <- read_excel("./RO_Census_Predict.xlsx", sheet = "Teleorman")
RO_Census_Timis <- read_excel("./RO_Census_Predict.xlsx", sheet = "Timis")
RO_Census_Tulcea <- read_excel("./RO_Census_Predict.xlsx", sheet = "Tulcea")
RO_Census_Vaslui <- read_excel("./RO_Census_Predict.xlsx", sheet = "Vaslui")
RO_Census_Valcea <- read_excel("./RO_Census_Predict.xlsx", sheet = "Valcea")
RO_Census_Vrancea <- read_excel("./RO_Census_Predict.xlsx", sheet = "Vrancea")

# Load Census with outliers
Alba_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Alba")
Arad_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Arad")
Arges_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Arges")
Bacau_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Bacau")
Bihor_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Bihor")
BistNsd_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Bistrita-Nasaud")
Botosani_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Botosani")
Braila_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Braila")
Brasov_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Brasov")
Buzau_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Buzau")
CarSev_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Caras-Severin")
Calarasi_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Calarasi")
Cluj_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Cluj")
Constanta_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Constanta")
Covasna_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Covasna")
Dambovita_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Dambovita")
Dolj_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Dolj")
Galati_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Galati")
Giurgiu_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Giurgiu")
Gorj_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Gorj")
Harghita_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Harghita")
Hunedoara_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Hunedoara")
Ialomita_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Ialomita")
Iasi_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Iasi")
Ilfov_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Ilfov")
Maramures_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Maramures")
Mehedinti_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Mehedinti")
Mures_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Mures")
Neamt_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Neamt")
Olt_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Olt")
Prahova_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Prahova")
SatuMare_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Satu Mare")
Salaj_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Salaj")
Sibiu_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Sibiu")
Suceava_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Suceava")
Teleorman_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Teleorman")
Timis_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Timis")
Tulcea_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Tulcea")
Vaslui_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Vaslui")
Valcea_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Valcea")
Vrancea_Outliers <- read_excel("./RO_Census_Outliers.xlsx", sheet = "Vrancea")

# I. Analyze the R-Squared values from initial models

# 1. Verify the maximum R-Squared value from each Judet
RSquared_vals_df %>% inner_join(
RSquared_vals_df %>% select(-Function_Name) %>% group_by(Judet_Name) %>% 
  summarize(R_Squared = max(R_Squared))
) %>% select(Function_Name) %>% group_by(Function_Name) %>% count() %>% 
  rename('Counts' = 'n')
View(RSquared_vals_df %>% inner_join(
  RSquared_vals_df %>% select(-Function_Name) %>% group_by(Judet_Name) %>% 
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
RSquared_vals_df %>% filter(R_Squared >= 0.90) %>% select(Function_Name) %>% 
  group_by(Function_Name) %>% count() %>% rename('Counts' = 'n')
View(RSquared_vals_df %>% filter(R_Squared >= 0.90))

RSquared_vals_df %>% filter(R_Squared >= 0.95) %>% select(Function_Name) %>% 
  group_by(Function_Name) %>% count() %>% rename('Counts' = 'n')
View(RSquared_vals_df %>% filter(R_Squared >= 0.95))

# II. Detect outliers

# 1. Alba
# RO_Census_Alba_New <- RO_Census_df %>%
#   inner_join(
#   RO_Census_Alba %>%
#     filter(Pop_Freq < max(Pop_Freq)) %>%
#     select(Judet_Name, Unit_Type, Unit_Name),
#   by = c("Judet_Name", "Unit_Type", "Unit_Name")) %>%
#   left_join((RO_Census_df %>%
#                inner_join(
#                  RO_Census_Alba %>%
#                    filter(Pop_Freq < max(Pop_Freq)) %>%
#                    select(Judet_Name, Unit_Type, Unit_Name),
#                  by = c("Judet_Name", "Unit_Type", "Unit_Name"))) %>%
#               group_by(Judet_Name) %>%
#               summarize(Total_Pop = sum(Population))) %>%
#   arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>%
#   mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
#   ungroup()

# RO_Census_Alba_New <- RO_Census_df %>% filter(Judet_Name == "ALBA") %>%
#   inner_join(
#     RO_Census_Alba %>% mutate(LOF = lofactor(Pop_Freq, k = 5)) %>%
#       filter(round(LOF, 2) < 1.5) %>% select(Judet_Name, Unit_Type, Unit_Name),
#     by = c("Judet_Name", "Unit_Type", "Unit_Name")) %>%
#   left_join((RO_Census_df %>% filter(Judet_Name == "ALBA") %>%
#                inner_join(
#                  RO_Census_Alba %>% mutate(LOF = lofactor(Pop_Freq, k = 5)) %>%
#                    filter(round(LOF, 2) < 1.5) %>%
#                    select(Judet_Name, Unit_Type, Unit_Name),
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
         select(Rank) %>% distinct()),
      (AB_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (AB_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (AB_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
      ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
    ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Alba_New <- RO_Census_Alba_New %>% 
  left_join(RO_Census_Alba_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
AB_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Alba_New)
summary(AB_pw_lm)
AB_pred_pw_lm <- exp(predict(AB_pw_lm, newdata = RO_Census_Alba_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
AB_ZM_prm <- get_ZM_Param("RO_Census_Alba_New", "Pop_Freq", "Rank")
AB_ZM_m <- AB_ZM_prm$m
AB_ZM_alpha <- AB_ZM_prm$alpha_ZM
AB_ZM_cst <- AB_ZM_prm$constant_ZM
AB_ZM_lm <- lm(log(RO_Census_Alba_New$Pop_Freq) ~ log(RO_Census_Alba_New$Rank + AB_ZM_m))
summary(AB_ZM_lm)
AB_pred_ZM_lm <- exp(predict(AB_ZM_lm, newdata = RO_Census_Alba_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
AB_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Alba_New)
summary(AB_exp_lm)
AB_pred_exp_lm <- exp(predict(AB_exp_lm, newdata = RO_Census_Alba_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Alba_New <- RO_Census_Alba_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Alba_New) - Rank + 1))
)
AB_lav_lm <- lm(log(RO_Census_Alba_New$Pop_Freq) ~ RO_Census_Alba_New$Lav_exp)
summary(AB_lav_lm)
AB_lav_kst <- exp(signif(AB_lav_lm$coef[[1]], 4))
AB_lav_chi <- signif(AB_lav_lm$coef[[2]], 4)
AB_pred_lav_lm <- exp(predict(AB_lav_lm, newdata = RO_Census_Alba_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Alba judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Alba_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Alba judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Alba_New %>%
     ggplot(aes(x = log(Rank + AB_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Alba judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Alba_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (AR_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (AR_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (AR_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Arad_New <- RO_Census_Arad_New %>% 
  left_join(RO_Census_Arad_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
AR_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Arad_New)
summary(AR_pw_lm)
AR_pred_pw_lm <- exp(predict(AR_pw_lm, newdata = RO_Census_Arad_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
AR_ZM_prm <- get_ZM_Param("RO_Census_Arad_New", "Pop_Freq", "Rank")
AR_ZM_m <- AR_ZM_prm$m
AR_ZM_alpha <- AR_ZM_prm$alpha_ZM
AR_ZM_cst <- AR_ZM_prm$constant_ZM
AR_ZM_lm <- lm(log(RO_Census_Arad_New$Pop_Freq) ~ log(RO_Census_Arad_New$Rank + AR_ZM_m))
summary(AR_ZM_lm)
AR_pred_ZM_lm <- exp(predict(AR_ZM_lm, newdata = RO_Census_Arad_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
AR_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Arad_New)
summary(AR_exp_lm)
AR_pred_exp_lm <- exp(predict(AR_exp_lm, newdata = RO_Census_Arad_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Arad_New <- RO_Census_Arad_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Arad_New) - Rank + 1))
)
AR_lav_lm <- lm(log(RO_Census_Arad_New$Pop_Freq) ~ RO_Census_Arad_New$Lav_exp)
summary(AR_lav_lm)
AR_lav_kst <- exp(signif(AR_lav_lm$coef[[1]], 4))
AR_lav_chi <- signif(AR_lav_lm$coef[[2]], 4)
AR_pred_lav_lm <- exp(predict(AR_lav_lm, newdata = RO_Census_Arad_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Arad judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arad_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Arad judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arad_New %>%
     ggplot(aes(x = log(Rank + AR_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Arad judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arad_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (AG_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (AG_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (AG_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Arges_New <- RO_Census_Arges_New %>% 
  left_join(RO_Census_Arges_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
AG_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Arges_New)
summary(AG_pw_lm)
AG_pred_pw_lm <- exp(predict(AG_pw_lm, newdata = RO_Census_Arges_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
AG_ZM_prm <- get_ZM_Param("RO_Census_Arges_New", "Pop_Freq", "Rank")
AG_ZM_m <- AG_ZM_prm$m
AG_ZM_alpha <- AG_ZM_prm$alpha_ZM
AG_ZM_cst <- AG_ZM_prm$constant_ZM
AG_ZM_lm <- lm(log(RO_Census_Arges_New$Pop_Freq) ~ log(RO_Census_Arges_New$Rank + AG_ZM_m))
summary(AG_ZM_lm)
AG_pred_ZM_lm <- exp(predict(AG_ZM_lm, newdata = RO_Census_Arges_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
AG_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Arges_New)
summary(AG_exp_lm)
AG_pred_exp_lm <- exp(predict(AG_exp_lm, newdata = RO_Census_Arges_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Arges_New <- RO_Census_Arges_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Arges_New) - Rank + 1))
)
AG_lav_lm <- lm(log(RO_Census_Arges_New$Pop_Freq) ~ RO_Census_Arges_New$Lav_exp)
summary(AG_lav_lm)
AG_lav_kst <- exp(signif(AG_lav_lm$coef[[1]], 4))
AG_lav_chi <- signif(AG_lav_lm$coef[[2]], 4)
AG_pred_lav_lm <- exp(predict(AG_lav_lm, newdata = RO_Census_Arges_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Arges judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arges_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Arges judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arges_New %>%
     ggplot(aes(x = log(Rank + AG_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Arges judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arges_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (BC_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BC_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BC_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Bacau_New <- RO_Census_Bacau_New %>% 
  left_join(RO_Census_Bacau_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BC_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Bacau_New)
summary(BC_pw_lm)
BC_pred_pw_lm <- exp(predict(BC_pw_lm, newdata = RO_Census_Bacau_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
BC_ZM_prm <- get_ZM_Param("RO_Census_Bacau_New", "Pop_Freq", "Rank")
BC_ZM_m <- BC_ZM_prm$m
BC_ZM_alpha <- BC_ZM_prm$alpha_ZM
BC_ZM_cst <- BC_ZM_prm$constant_ZM
BC_ZM_lm <- lm(log(RO_Census_Bacau_New$Pop_Freq) ~ log(RO_Census_Bacau_New$Rank + BC_ZM_m))
summary(BC_ZM_lm)
BC_pred_ZM_lm <- exp(predict(BC_ZM_lm, newdata = RO_Census_Bacau_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
BC_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Bacau_New)
summary(BC_exp_lm)
BC_pred_exp_lm <- exp(predict(BC_exp_lm, newdata = RO_Census_Bacau_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Bacau_New <- RO_Census_Bacau_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Bacau_New) - Rank + 1))
)
BC_lav_lm <- lm(log(RO_Census_Bacau_New$Pop_Freq) ~ RO_Census_Bacau_New$Lav_exp)
summary(BC_lav_lm)
BC_lav_kst <- exp(signif(BC_lav_lm$coef[[1]], 4))
BC_lav_chi <- signif(BC_lav_lm$coef[[2]], 4)
BC_pred_lav_lm <- exp(predict(BC_lav_lm, newdata = RO_Census_Bacau_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Bacau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bacau_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Bacau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bacau_New %>%
     ggplot(aes(x = log(Rank + BC_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Bacau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bacau_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (BH_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BH_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BH_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Bihor_New <- RO_Census_Bihor_New %>% 
  left_join(RO_Census_Bihor_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BH_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Bihor_New)
summary(BH_pw_lm)
BH_pred_pw_lm <- exp(predict(BH_pw_lm, newdata = RO_Census_Bihor_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
BH_ZM_prm <- get_ZM_Param("RO_Census_Bihor_New", "Pop_Freq", "Rank")
BH_ZM_m <- BH_ZM_prm$m
BH_ZM_alpha <- BH_ZM_prm$alpha_ZM
BH_ZM_cst <- BH_ZM_prm$constant_ZM
BH_ZM_lm <- lm(log(RO_Census_Bihor_New$Pop_Freq) ~ log(RO_Census_Bihor_New$Rank + BH_ZM_m))
summary(BH_ZM_lm)
BH_pred_ZM_lm <- exp(predict(BH_ZM_lm, newdata = RO_Census_Bihor_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
BH_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Bihor_New)
summary(BH_exp_lm)
BH_pred_exp_lm <- exp(predict(BH_exp_lm, newdata = RO_Census_Bihor_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Bihor_New <- RO_Census_Bihor_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Bihor_New) - Rank + 1))
)
BH_lav_lm <- lm(log(RO_Census_Bihor_New$Pop_Freq) ~ RO_Census_Bihor_New$Lav_exp)
summary(BH_lav_lm)
BH_lav_kst <- exp(signif(BH_lav_lm$coef[[1]], 4))
BH_lav_chi <- signif(BH_lav_lm$coef[[2]], 4)
BH_pred_lav_lm <- exp(predict(BH_lav_lm, newdata = RO_Census_Bihor_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Bihor judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bihor_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Bihor judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bihor_New %>%
     ggplot(aes(x = log(Rank + BH_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Bihor judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bihor_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (BN_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BN_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BN_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_BistNsd_New <- RO_Census_BistNsd_New %>% 
  left_join(RO_Census_BistNsd_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BN_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_BistNsd_New)
summary(BN_pw_lm)
BN_pred_pw_lm <- exp(predict(BN_pw_lm, newdata = RO_Census_BistNsd_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
BN_ZM_prm <- get_ZM_Param("RO_Census_BistNsd_New", "Pop_Freq", "Rank")
BN_ZM_m <- BN_ZM_prm$m
BN_ZM_alpha <- BN_ZM_prm$alpha_ZM
BN_ZM_cst <- BN_ZM_prm$constant_ZM
BN_ZM_lm <- lm(log(RO_Census_BistNsd_New$Pop_Freq) ~ log(RO_Census_BistNsd_New$Rank + BN_ZM_m))
summary(BN_ZM_lm)
BN_pred_ZM_lm <- exp(predict(BN_ZM_lm, newdata = RO_Census_BistNsd_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
BN_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_BistNsd_New)
summary(BN_exp_lm)
BN_pred_exp_lm <- exp(predict(BN_exp_lm, newdata = RO_Census_BistNsd_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_BistNsd_New <- RO_Census_BistNsd_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_BistNsd_New) - Rank + 1))
)
BN_lav_lm <- lm(log(RO_Census_BistNsd_New$Pop_Freq) ~ RO_Census_BistNsd_New$Lav_exp)
summary(BN_lav_lm)
BN_lav_kst <- exp(signif(BN_lav_lm$coef[[1]], 4))
BN_lav_chi <- signif(BN_lav_lm$coef[[2]], 4)
BN_pred_lav_lm <- exp(predict(BN_lav_lm, newdata = RO_Census_BistNsd_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Bistrita-Nasaud judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_BistNsd_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Bistrita-Nasaud judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_BistNsd_New %>%
     ggplot(aes(x = log(Rank + BN_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Bistrita-Nasaud judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_BistNsd_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (BT_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BT_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BT_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n == max(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Botosani_New <- RO_Census_Botosani_New %>% 
  left_join(RO_Census_Botosani_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BT_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Botosani_New)
summary(BT_pw_lm)
BT_pred_pw_lm <- exp(predict(BT_pw_lm, newdata = RO_Census_Botosani_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
BT_ZM_prm <- get_ZM_Param("RO_Census_Botosani_New", "Pop_Freq", "Rank")
BT_ZM_m <- BT_ZM_prm$m
BT_ZM_alpha <- BT_ZM_prm$alpha_ZM
BT_ZM_cst <- BT_ZM_prm$constant_ZM
BT_ZM_lm <- lm(log(RO_Census_Botosani_New$Pop_Freq) ~ log(RO_Census_Botosani_New$Rank + BT_ZM_m))
summary(BT_ZM_lm)
BT_pred_ZM_lm <- exp(predict(BT_ZM_lm, newdata = RO_Census_Botosani_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
BT_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Botosani_New)
summary(BT_exp_lm)
BT_pred_exp_lm <- exp(predict(BT_exp_lm, newdata = RO_Census_Botosani_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Botosani_New <- RO_Census_Botosani_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Botosani_New) - Rank + 1))
)
BT_lav_lm <- lm(log(RO_Census_Botosani_New$Pop_Freq) ~ RO_Census_Botosani_New$Lav_exp)
summary(BT_lav_lm)
BT_lav_kst <- exp(signif(BT_lav_lm$coef[[1]], 4))
BT_lav_chi <- signif(BT_lav_lm$coef[[2]], 4)
BT_pred_lav_lm <- exp(predict(BT_lav_lm, newdata = RO_Census_Botosani_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Botosani judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Botosani_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Botosani judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Botosani_New %>%
     ggplot(aes(x = log(Rank + BT_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Botosani judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Botosani_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
#       filter(round(LOF, 2) < 1.5) %>% select(Judet_Name, Unit_Type, Unit_Name),
#     by = c("Judet_Name", "Unit_Type", "Unit_Name")) %>%
#   left_join((RO_Census_df %>% filter(Judet_Name == "BRAILA") %>%
#                inner_join(
#                  RO_Census_Braila %>% mutate(LOF = lofactor(Pop_Freq, k = 3)) %>%
#                    filter(round(LOF, 2) < 1.5) %>%
#                    select(Judet_Name, Unit_Type, Unit_Name),
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
         select(Rank) %>% distinct()),
      (BR_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BR_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BR_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Braila_New <- RO_Census_Braila_New %>% 
  left_join(RO_Census_Braila_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BR_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Braila_New)
summary(BR_pw_lm)
BR_pred_pw_lm <- exp(predict(BR_pw_lm, newdata = RO_Census_Braila_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
BR_ZM_prm <- get_ZM_Param("RO_Census_Braila_New", "Pop_Freq", "Rank")
BR_ZM_m <- BR_ZM_prm$m
BR_ZM_alpha <- BR_ZM_prm$alpha_ZM
BR_ZM_cst <- BR_ZM_prm$constant_ZM
BR_ZM_lm <- lm(log(RO_Census_Braila_New$Pop_Freq) ~ log(RO_Census_Braila_New$Rank + BR_ZM_m))
summary(BR_ZM_lm)
BR_pred_ZM_lm <- exp(predict(BR_ZM_lm, newdata = RO_Census_Braila_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
BR_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Braila_New)
summary(BR_exp_lm)
BR_pred_exp_lm <- exp(predict(BR_exp_lm, newdata = RO_Census_Braila_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Braila_New <- RO_Census_Braila_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Braila_New) - Rank + 1))
)
BR_lav_lm <- lm(log(RO_Census_Braila_New$Pop_Freq) ~ RO_Census_Braila_New$Lav_exp)
summary(BR_lav_lm)
BR_lav_kst <- exp(signif(BR_lav_lm$coef[[1]], 4))
BR_lav_chi <- signif(BR_lav_lm$coef[[2]], 4)
BR_pred_lav_lm <- exp(predict(BR_lav_lm, newdata = RO_Census_Braila_New, 
                              interval = "prediction", level = 0.95))

# Data Distributions
RO_Census_Braila_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila_New$Rank, 
                              Pop_Freq = BR_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.08,
            label = paste0("y", "==", signif(exp(BR_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BR_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 30, y = 0.08,
            label = paste0("R^2 ==", signif(summary(BR_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila_New$Rank, 
                              Pop_Freq = BR_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.07,
            label = paste0("y", "==", signif(exp(BR_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BR_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 30, y = 0.07,
            label = paste0("R^2 ==", signif(summary(BR_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila_New$Rank, 
                              Pop_Freq = BR_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.06,
            label = paste0("y", "==", signif(BR_ZM_cst, 4), "%.%", "(",
                           signif(BR_ZM_m, 4), "+ x)^", signif(BR_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 30, y = 0.06,
            label = paste0("R^2 ==", signif(summary(BR_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila_New$Rank, 
                              Pop_Freq = BR_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.05,
            label = paste0("y", "==", signif(BR_lav_kst, 4), "%.%", "x^",
                           -signif(BR_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 30, y = 0.05,
            label = paste0("R^2 ==", signif(summary(BR_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Braila judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Braila_New %>%
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Braila judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Braila_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Braila judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Braila_New %>%
     ggplot(aes(x = log(Rank + BR_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Braila judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Braila_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (BV_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BV_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BV_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Brasov_New <- RO_Census_Brasov_New %>% 
  left_join(RO_Census_Brasov_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BV_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Brasov_New)
summary(BV_pw_lm)
BV_pred_pw_lm <- exp(predict(BV_pw_lm, newdata = RO_Census_Brasov_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
BV_ZM_prm <- get_ZM_Param("RO_Census_Brasov_New", "Pop_Freq", "Rank")
BV_ZM_m <- BV_ZM_prm$m
BV_ZM_alpha <- BV_ZM_prm$alpha_ZM
BV_ZM_cst <- BV_ZM_prm$constant_ZM
BV_ZM_lm <- lm(log(RO_Census_Brasov_New$Pop_Freq) ~ log(RO_Census_Brasov_New$Rank + BV_ZM_m))
summary(BV_ZM_lm)
BV_pred_ZM_lm <- exp(predict(BV_ZM_lm, newdata = RO_Census_Brasov_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
BV_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Brasov_New)
summary(BV_exp_lm)
BV_pred_exp_lm <- exp(predict(BV_exp_lm, newdata = RO_Census_Brasov_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Brasov_New <- RO_Census_Brasov_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Brasov_New) - Rank + 1))
)
BV_lav_lm <- lm(log(RO_Census_Brasov_New$Pop_Freq) ~ RO_Census_Brasov_New$Lav_exp)
summary(BV_lav_lm)
BV_lav_kst <- exp(signif(BV_lav_lm$coef[[1]], 4))
BV_lav_chi <- signif(BV_lav_lm$coef[[2]], 4)
BV_pred_lav_lm <- exp(predict(BV_lav_lm, newdata = RO_Census_Brasov_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Brasov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Brasov_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Brasov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Brasov_New %>%
     ggplot(aes(x = log(Rank + BV_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Brasov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Brasov_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (BZ_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BZ_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (BZ_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Buzau_New <- RO_Census_Buzau_New %>% 
  left_join(RO_Census_Buzau_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
BZ_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Buzau_New)
summary(BZ_pw_lm)
BZ_pred_pw_lm <- exp(predict(BZ_pw_lm, newdata = RO_Census_Buzau_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
BZ_ZM_prm <- get_ZM_Param("RO_Census_Buzau_New", "Pop_Freq", "Rank")
BZ_ZM_m <- BZ_ZM_prm$m
BZ_ZM_alpha <- BZ_ZM_prm$alpha_ZM
BZ_ZM_cst <- BZ_ZM_prm$constant_ZM
BZ_ZM_lm <- lm(log(RO_Census_Buzau_New$Pop_Freq) ~ log(RO_Census_Buzau_New$Rank + BZ_ZM_m))
summary(BZ_ZM_lm)
BZ_pred_ZM_lm <- exp(predict(BZ_ZM_lm, newdata = RO_Census_Buzau_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
BZ_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Buzau_New)
summary(BZ_exp_lm)
BZ_pred_exp_lm <- exp(predict(BZ_exp_lm, newdata = RO_Census_Buzau_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Buzau_New <- RO_Census_Buzau_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Buzau_New) - Rank + 1))
)
BZ_lav_lm <- lm(log(RO_Census_Buzau_New$Pop_Freq) ~ RO_Census_Buzau_New$Lav_exp)
summary(BZ_lav_lm)
BZ_lav_kst <- exp(signif(BZ_lav_lm$coef[[1]], 4))
BZ_lav_chi <- signif(BZ_lav_lm$coef[[2]], 4)
BZ_pred_lav_lm <- exp(predict(BZ_lav_lm, newdata = RO_Census_Buzau_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Buzau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Buzau_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Buzau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Buzau_New %>%
     ggplot(aes(x = log(Rank + BZ_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Buzau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Buzau_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (CS_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (CS_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (CS_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_CarSev_New <- RO_Census_CarSev_New %>% 
  left_join(RO_Census_CarSev_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
CS_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_CarSev_New)
summary(CS_pw_lm)
CS_pred_pw_lm <- exp(predict(CS_pw_lm, newdata = RO_Census_CarSev_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
CS_ZM_prm <- get_ZM_Param("RO_Census_CarSev_New", "Pop_Freq", "Rank")
CS_ZM_m <- CS_ZM_prm$m
CS_ZM_alpha <- CS_ZM_prm$alpha_ZM
CS_ZM_cst <- CS_ZM_prm$constant_ZM
CS_ZM_lm <- lm(log(RO_Census_CarSev_New$Pop_Freq) ~ log(RO_Census_CarSev_New$Rank + CS_ZM_m))
summary(CS_ZM_lm)
CS_pred_ZM_lm <- exp(predict(CS_ZM_lm, newdata = RO_Census_CarSev_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
CS_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_CarSev_New)
summary(CS_exp_lm)
CS_pred_exp_lm <- exp(predict(CS_exp_lm, newdata = RO_Census_CarSev_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_CarSev_New <- RO_Census_CarSev_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_CarSev_New) - Rank + 1))
)
CS_lav_lm <- lm(log(RO_Census_CarSev_New$Pop_Freq) ~ RO_Census_CarSev_New$Lav_exp)
summary(CS_lav_lm)
CS_lav_kst <- exp(signif(CS_lav_lm$coef[[1]], 4))
CS_lav_chi <- signif(CS_lav_lm$coef[[2]], 4)
CS_pred_lav_lm <- exp(predict(CS_lav_lm, newdata = RO_Census_CarSev_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Caras-Severin judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_CarSev_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Caras-Severin judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_CarSev_New %>%
     ggplot(aes(x = log(Rank + CS_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Caras-Severin judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_CarSev_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (CL_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (CL_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (CL_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Calarasi_New <- RO_Census_Calarasi_New %>% 
  left_join(RO_Census_Calarasi_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
CL_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Calarasi_New)
summary(CL_pw_lm)
CL_pred_pw_lm <- exp(predict(CL_pw_lm, newdata = RO_Census_Calarasi_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
CL_ZM_prm <- get_ZM_Param("RO_Census_Calarasi_New", "Pop_Freq", "Rank")
CL_ZM_m <- CL_ZM_prm$m
CL_ZM_alpha <- CL_ZM_prm$alpha_ZM
CL_ZM_cst <- CL_ZM_prm$constant_ZM
CL_ZM_lm <- lm(log(RO_Census_Calarasi_New$Pop_Freq) ~ log(RO_Census_Calarasi_New$Rank + CL_ZM_m))
summary(CL_ZM_lm)
CL_pred_ZM_lm <- exp(predict(CL_ZM_lm, newdata = RO_Census_Calarasi_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
CL_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Calarasi_New)
summary(CL_exp_lm)
CL_pred_exp_lm <- exp(predict(CL_exp_lm, newdata = RO_Census_Calarasi_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Calarasi_New <- RO_Census_Calarasi_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Calarasi_New) - Rank + 1))
)
CL_lav_lm <- lm(log(RO_Census_Calarasi_New$Pop_Freq) ~ RO_Census_Calarasi_New$Lav_exp)
summary(CL_lav_lm)
CL_lav_kst <- exp(signif(CL_lav_lm$coef[[1]], 4))
CL_lav_chi <- signif(CL_lav_lm$coef[[2]], 4)
CL_pred_lav_lm <- exp(predict(CL_lav_lm, newdata = RO_Census_Calarasi_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Calarasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Calarasi_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Calarasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Calarasi_New %>%
     ggplot(aes(x = log(Rank + CL_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Calarasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Calarasi_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
#       filter(round(LOF, 2) < 1.5) %>% select(Judet_Name, Unit_Type, Unit_Name),
#     by = c("Judet_Name", "Unit_Type", "Unit_Name")) %>%
#   left_join((RO_Census_df %>% filter(Judet_Name == "CLUJ") %>%
#                inner_join(
#                  RO_Census_Cluj %>% mutate(LOF = lofactor(Pop_Freq, k = 7)) %>%
#                    filter(round(LOF, 2) < 1.5) %>%
#                    select(Judet_Name, Unit_Type, Unit_Name),
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
         select(Rank) %>% distinct()),
      (CJ_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (CJ_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (CJ_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Cluj_New <- RO_Census_Cluj_New %>% 
  left_join(RO_Census_Cluj_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
CJ_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Cluj_New)
summary(CJ_pw_lm)
CJ_pred_pw_lm <- exp(predict(CJ_pw_lm, newdata = RO_Census_Cluj_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
CJ_ZM_prm <- get_ZM_Param("RO_Census_Cluj_New", "Pop_Freq", "Rank")
CJ_ZM_m <- CJ_ZM_prm$m
CJ_ZM_alpha <- CJ_ZM_prm$alpha_ZM
CJ_ZM_cst <- CJ_ZM_prm$constant_ZM
CJ_ZM_lm <- lm(log(RO_Census_Cluj_New$Pop_Freq) ~ log(RO_Census_Cluj_New$Rank + CJ_ZM_m))
summary(CJ_ZM_lm)
CJ_pred_ZM_lm <- exp(predict(CJ_ZM_lm, newdata = RO_Census_Cluj_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
CJ_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Cluj_New)
summary(CJ_exp_lm)
CJ_pred_exp_lm <- exp(predict(CJ_exp_lm, newdata = RO_Census_Cluj_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Cluj_New <- RO_Census_Cluj_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Cluj_New) - Rank + 1))
)
CJ_lav_lm <- lm(log(RO_Census_Cluj_New$Pop_Freq) ~ RO_Census_Cluj_New$Lav_exp)
summary(CJ_lav_lm)
CJ_lav_kst <- exp(signif(CJ_lav_lm$coef[[1]], 4))
CJ_lav_chi <- signif(CJ_lav_lm$coef[[2]], 4)
CJ_pred_lav_lm <- exp(predict(CJ_lav_lm, newdata = RO_Census_Cluj_New, 
                              interval = "prediction", level = 0.95))

# Data Distributions
RO_Census_Cluj_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj_New$Rank, 
                              Pop_Freq = CJ_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.15,
            label = paste0("y", "==", signif(exp(CJ_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(CJ_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.15,
            label = paste0("R^2 ==", signif(summary(CJ_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj_New$Rank, 
                              Pop_Freq = CJ_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.13,
            label = paste0("y", "==", signif(exp(CJ_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(CJ_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.13,
            label = paste0("R^2 ==", signif(summary(CJ_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj_New$Rank, 
                              Pop_Freq = CJ_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.11,
            label = paste0("y", "==", signif(CJ_ZM_cst, 4), "%.%", "(",
                           signif(CJ_ZM_m, 4), "+ x)^", signif(CJ_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.11,
            label = paste0("R^2 ==", signif(summary(CJ_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj_New$Rank, 
                              Pop_Freq = CJ_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.09,
            label = paste0("y", "==", signif(CJ_lav_kst, 4), "%.%", "x^",
                           -signif(CJ_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.09,
            label = paste0("R^2 ==", signif(summary(CJ_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Cluj judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Cluj_New %>%
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Cluj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Cluj_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Cluj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Cluj_New %>%
     ggplot(aes(x = log(Rank + CJ_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Cluj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Cluj_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (CT_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (CT_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (CT_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Constanta_New <- RO_Census_Constanta_New %>% 
  left_join(RO_Census_Constanta_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
CT_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Constanta_New)
summary(CT_pw_lm)
CT_pred_pw_lm <- exp(predict(CT_pw_lm, newdata = RO_Census_Constanta_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
CT_ZM_prm <- get_ZM_Param("RO_Census_Constanta_New", "Pop_Freq", "Rank")
CT_ZM_m <- CT_ZM_prm$m
CT_ZM_alpha <- CT_ZM_prm$alpha_ZM
CT_ZM_cst <- CT_ZM_prm$constant_ZM
CT_ZM_lm <- lm(log(RO_Census_Constanta_New$Pop_Freq) ~ log(RO_Census_Constanta_New$Rank + CT_ZM_m))
summary(CT_ZM_lm)
CT_pred_ZM_lm <- exp(predict(CT_ZM_lm, newdata = RO_Census_Constanta_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
CT_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Constanta_New)
summary(CT_exp_lm)
CT_pred_exp_lm <- exp(predict(CT_exp_lm, newdata = RO_Census_Constanta_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Constanta_New <- RO_Census_Constanta_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Constanta_New) - Rank + 1))
)
CT_lav_lm <- lm(log(RO_Census_Constanta_New$Pop_Freq) ~ RO_Census_Constanta_New$Lav_exp)
summary(CT_lav_lm)
CT_lav_kst <- exp(signif(CT_lav_lm$coef[[1]], 4))
CT_lav_chi <- signif(CT_lav_lm$coef[[2]], 4)
CT_pred_lav_lm <- exp(predict(CT_lav_lm, newdata = RO_Census_Constanta_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Constanta judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Constanta_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Constanta judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Constanta_New %>%
     ggplot(aes(x = log(Rank + CT_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Constanta judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Constanta_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (CV_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (CV_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (CV_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Covasna_New <- RO_Census_Covasna_New %>% 
  left_join(RO_Census_Covasna_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
CV_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Covasna_New)
summary(CV_pw_lm)
CV_pred_pw_lm <- exp(predict(CV_pw_lm, newdata = RO_Census_Covasna_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
CV_ZM_prm <- get_ZM_Param("RO_Census_Covasna_New", "Pop_Freq", "Rank")
CV_ZM_m <- CV_ZM_prm$m
CV_ZM_alpha <- CV_ZM_prm$alpha_ZM
CV_ZM_cst <- CV_ZM_prm$constant_ZM
CV_ZM_lm <- lm(log(RO_Census_Covasna_New$Pop_Freq) ~ log(RO_Census_Covasna_New$Rank + CV_ZM_m))
summary(CV_ZM_lm)
CV_pred_ZM_lm <- exp(predict(CV_ZM_lm, newdata = RO_Census_Covasna_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
CV_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Covasna_New)
summary(CV_exp_lm)
CV_pred_exp_lm <- exp(predict(CV_exp_lm, newdata = RO_Census_Covasna_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Covasna_New <- RO_Census_Covasna_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Covasna_New) - Rank + 1))
)
CV_lav_lm <- lm(log(RO_Census_Covasna_New$Pop_Freq) ~ RO_Census_Covasna_New$Lav_exp)
summary(CV_lav_lm)
CV_lav_kst <- exp(signif(CV_lav_lm$coef[[1]], 4))
CV_lav_chi <- signif(CV_lav_lm$coef[[2]], 4)
CV_pred_lav_lm <- exp(predict(CV_lav_lm, newdata = RO_Census_Covasna_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Covasna judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Covasna_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Covasna judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Covasna_New %>%
     ggplot(aes(x = log(Rank + CV_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Covasna judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Covasna_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (DB_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (DB_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (DB_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Dambovita_New <- RO_Census_Dambovita_New %>% 
  left_join(RO_Census_Dambovita_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
DB_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Dambovita_New)
summary(DB_pw_lm)
DB_pred_pw_lm <- exp(predict(DB_pw_lm, newdata = RO_Census_Dambovita_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
DB_ZM_prm <- get_ZM_Param("RO_Census_Dambovita_New", "Pop_Freq", "Rank")
DB_ZM_m <- DB_ZM_prm$m
DB_ZM_alpha <- DB_ZM_prm$alpha_ZM
DB_ZM_cst <- DB_ZM_prm$constant_ZM
DB_ZM_lm <- lm(log(RO_Census_Dambovita_New$Pop_Freq) ~ log(RO_Census_Dambovita_New$Rank + DB_ZM_m))
summary(DB_ZM_lm)
DB_pred_ZM_lm <- exp(predict(DB_ZM_lm, newdata = RO_Census_Dambovita_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
DB_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Dambovita_New)
summary(DB_exp_lm)
DB_pred_exp_lm <- exp(predict(DB_exp_lm, newdata = RO_Census_Dambovita_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Dambovita_New <- RO_Census_Dambovita_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Dambovita_New) - Rank + 1))
)
DB_lav_lm <- lm(log(RO_Census_Dambovita_New$Pop_Freq) ~ RO_Census_Dambovita_New$Lav_exp)
summary(DB_lav_lm)
DB_lav_kst <- exp(signif(DB_lav_lm$coef[[1]], 4))
DB_lav_chi <- signif(DB_lav_lm$coef[[2]], 4)
DB_pred_lav_lm <- exp(predict(DB_lav_lm, newdata = RO_Census_Dambovita_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Dambovita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dambovita_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Dambovita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dambovita_New %>%
     ggplot(aes(x = log(Rank + DB_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Dambovita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dambovita_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (DJ_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (DJ_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (DJ_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Dolj_New <- RO_Census_Dolj_New %>% 
  left_join(RO_Census_Dolj_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
DJ_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Dolj_New)
summary(DJ_pw_lm)
DJ_pred_pw_lm <- exp(predict(DJ_pw_lm, newdata = RO_Census_Dolj_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
DJ_ZM_prm <- get_ZM_Param("RO_Census_Dolj_New", "Pop_Freq", "Rank")
DJ_ZM_m <- DJ_ZM_prm$m
DJ_ZM_alpha <- DJ_ZM_prm$alpha_ZM
DJ_ZM_cst <- DJ_ZM_prm$constant_ZM
DJ_ZM_lm <- lm(log(RO_Census_Dolj_New$Pop_Freq) ~ log(RO_Census_Dolj_New$Rank + DJ_ZM_m))
summary(DJ_ZM_lm)
DJ_pred_ZM_lm <- exp(predict(DJ_ZM_lm, newdata = RO_Census_Dolj_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
DJ_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Dolj_New)
summary(DJ_exp_lm)
DJ_pred_exp_lm <- exp(predict(DJ_exp_lm, newdata = RO_Census_Dolj_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Dolj_New <- RO_Census_Dolj_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Dolj_New) - Rank + 1))
)
DJ_lav_lm <- lm(log(RO_Census_Dolj_New$Pop_Freq) ~ RO_Census_Dolj_New$Lav_exp)
summary(DJ_lav_lm)
DJ_lav_kst <- exp(signif(DJ_lav_lm$coef[[1]], 4))
DJ_lav_chi <- signif(DJ_lav_lm$coef[[2]], 4)
DJ_pred_lav_lm <- exp(predict(DJ_lav_lm, newdata = RO_Census_Dolj_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Dolj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dolj_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Dolj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dolj_New %>%
     ggplot(aes(x = log(Rank + DJ_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Dolj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dolj_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (GL_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (GL_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (GL_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Galati_New <- RO_Census_Galati_New %>% 
  left_join(RO_Census_Galati_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
GL_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Galati_New)
summary(GL_pw_lm)
GL_pred_pw_lm <- exp(predict(GL_pw_lm, newdata = RO_Census_Galati_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
GL_ZM_prm <- get_ZM_Param("RO_Census_Galati_New", "Pop_Freq", "Rank")
GL_ZM_m <- GL_ZM_prm$m
GL_ZM_alpha <- GL_ZM_prm$alpha_ZM
GL_ZM_cst <- GL_ZM_prm$constant_ZM
GL_ZM_lm <- lm(log(RO_Census_Galati_New$Pop_Freq) ~ log(RO_Census_Galati_New$Rank + GL_ZM_m))
summary(GL_ZM_lm)
GL_pred_ZM_lm <- exp(predict(GL_ZM_lm, newdata = RO_Census_Galati_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
GL_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Galati_New)
summary(GL_exp_lm)
GL_pred_exp_lm <- exp(predict(GL_exp_lm, newdata = RO_Census_Galati_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Galati_New <- RO_Census_Galati_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Galati_New) - Rank + 1))
)
GL_lav_lm <- lm(log(RO_Census_Galati_New$Pop_Freq) ~ RO_Census_Galati_New$Lav_exp)
summary(GL_lav_lm)
GL_lav_kst <- exp(signif(GL_lav_lm$coef[[1]], 4))
GL_lav_chi <- signif(GL_lav_lm$coef[[2]], 4)
GL_pred_lav_lm <- exp(predict(GL_lav_lm, newdata = RO_Census_Galati_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Galati judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Galati_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Galati judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Galati_New %>%
     ggplot(aes(x = log(Rank + GL_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Galati judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Galati_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (GR_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (GR_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (GR_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n == max(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Giurgiu_New <- RO_Census_Giurgiu_New %>% 
  left_join(RO_Census_Giurgiu_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
GR_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Giurgiu_New)
summary(GR_pw_lm)
GR_pred_pw_lm <- exp(predict(GR_pw_lm, newdata = RO_Census_Giurgiu_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
GR_ZM_prm <- get_ZM_Param("RO_Census_Giurgiu_New", "Pop_Freq", "Rank")
GR_ZM_m <- GR_ZM_prm$m
GR_ZM_alpha <- GR_ZM_prm$alpha_ZM
GR_ZM_cst <- GR_ZM_prm$constant_ZM
GR_ZM_lm <- lm(log(RO_Census_Giurgiu_New$Pop_Freq) ~ log(RO_Census_Giurgiu_New$Rank + GR_ZM_m))
summary(GR_ZM_lm)
GR_pred_ZM_lm <- exp(predict(GR_ZM_lm, newdata = RO_Census_Giurgiu_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
GR_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Giurgiu_New)
summary(GR_exp_lm)
GR_pred_exp_lm <- exp(predict(GR_exp_lm, newdata = RO_Census_Giurgiu_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Giurgiu_New <- RO_Census_Giurgiu_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Giurgiu_New) - Rank + 1))
)
GR_lav_lm <- lm(log(RO_Census_Giurgiu_New$Pop_Freq) ~ RO_Census_Giurgiu_New$Lav_exp)
summary(GR_lav_lm)
GR_lav_kst <- exp(signif(GR_lav_lm$coef[[1]], 4))
GR_lav_chi <- signif(GR_lav_lm$coef[[2]], 4)
GR_pred_lav_lm <- exp(predict(GR_lav_lm, newdata = RO_Census_Giurgiu_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Giurgiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Giurgiu_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Giurgiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Giurgiu_New %>%
     ggplot(aes(x = log(Rank + GR_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Giurgiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Giurgiu_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (GJ_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (GJ_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (GJ_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Gorj_New <- RO_Census_Gorj_New %>% 
  left_join(RO_Census_Gorj_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
GJ_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Gorj_New)
summary(GJ_pw_lm)
GJ_pred_pw_lm <- exp(predict(GJ_pw_lm, newdata = RO_Census_Gorj_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
GJ_ZM_prm <- get_ZM_Param("RO_Census_Gorj_New", "Pop_Freq", "Rank")
GJ_ZM_m <- GJ_ZM_prm$m
GJ_ZM_alpha <- GJ_ZM_prm$alpha_ZM
GJ_ZM_cst <- GJ_ZM_prm$constant_ZM
GJ_ZM_lm <- lm(log(RO_Census_Gorj_New$Pop_Freq) ~ log(RO_Census_Gorj_New$Rank + GJ_ZM_m))
summary(GJ_ZM_lm)
GJ_pred_ZM_lm <- exp(predict(GJ_ZM_lm, newdata = RO_Census_Gorj_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
GJ_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Gorj_New)
summary(GJ_exp_lm)
GJ_pred_exp_lm <- exp(predict(GJ_exp_lm, newdata = RO_Census_Gorj_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Gorj_New <- RO_Census_Gorj_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Gorj_New) - Rank + 1))
)
GJ_lav_lm <- lm(log(RO_Census_Gorj_New$Pop_Freq) ~ RO_Census_Gorj_New$Lav_exp)
summary(GJ_lav_lm)
GJ_lav_kst <- exp(signif(GJ_lav_lm$coef[[1]], 4))
GJ_lav_chi <- signif(GJ_lav_lm$coef[[2]], 4)
GJ_pred_lav_lm <- exp(predict(GJ_lav_lm, newdata = RO_Census_Gorj_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Gorj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Gorj_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Gorj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Gorj_New %>%
     ggplot(aes(x = log(Rank + GJ_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Gorj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Gorj_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (HR_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (HR_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (HR_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Harghita_New <- RO_Census_Harghita_New %>% 
  left_join(RO_Census_Harghita_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
HR_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Harghita_New)
summary(HR_pw_lm)
HR_pred_pw_lm <- exp(predict(HR_pw_lm, newdata = RO_Census_Harghita_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
HR_ZM_prm <- get_ZM_Param("RO_Census_Harghita_New", "Pop_Freq", "Rank")
HR_ZM_m <- HR_ZM_prm$m
HR_ZM_alpha <- HR_ZM_prm$alpha_ZM
HR_ZM_cst <- HR_ZM_prm$constant_ZM
HR_ZM_lm <- lm(log(RO_Census_Harghita_New$Pop_Freq) ~ log(RO_Census_Harghita_New$Rank + HR_ZM_m))
summary(HR_ZM_lm)
HR_pred_ZM_lm <- exp(predict(HR_ZM_lm, newdata = RO_Census_Harghita_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
HR_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Harghita_New)
summary(HR_exp_lm)
HR_pred_exp_lm <- exp(predict(HR_exp_lm, newdata = RO_Census_Harghita_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Harghita_New <- RO_Census_Harghita_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Harghita_New) - Rank + 1))
)
HR_lav_lm <- lm(log(RO_Census_Harghita_New$Pop_Freq) ~ RO_Census_Harghita_New$Lav_exp)
summary(HR_lav_lm)
HR_lav_kst <- exp(signif(HR_lav_lm$coef[[1]], 4))
HR_lav_chi <- signif(HR_lav_lm$coef[[2]], 4)
HR_pred_lav_lm <- exp(predict(HR_lav_lm, newdata = RO_Census_Harghita_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Harghita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Harghita_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Harghita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Harghita_New %>%
     ggplot(aes(x = log(Rank + HR_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Harghita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Harghita_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (HD_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (HD_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (HD_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Hunedoara_New <- RO_Census_Hunedoara_New %>% 
  left_join(RO_Census_Hunedoara_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
HD_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Hunedoara_New)
summary(HD_pw_lm)
HD_pred_pw_lm <- exp(predict(HD_pw_lm, newdata = RO_Census_Hunedoara_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
HD_ZM_prm <- get_ZM_Param("RO_Census_Hunedoara_New", "Pop_Freq", "Rank")
HD_ZM_m <- HD_ZM_prm$m
HD_ZM_alpha <- HD_ZM_prm$alpha_ZM
HD_ZM_cst <- HD_ZM_prm$constant_ZM
HD_ZM_lm <- lm(log(RO_Census_Hunedoara_New$Pop_Freq) ~ log(RO_Census_Hunedoara_New$Rank + HD_ZM_m))
summary(HD_ZM_lm)
HD_pred_ZM_lm <- exp(predict(HD_ZM_lm, newdata = RO_Census_Hunedoara_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
HD_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Hunedoara_New)
summary(HD_exp_lm)
HD_pred_exp_lm <- exp(predict(HD_exp_lm, newdata = RO_Census_Hunedoara_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Hunedoara_New <- RO_Census_Hunedoara_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Hunedoara_New) - Rank + 1))
)
HD_lav_lm <- lm(log(RO_Census_Hunedoara_New$Pop_Freq) ~ RO_Census_Hunedoara_New$Lav_exp)
summary(HD_lav_lm)
HD_lav_kst <- exp(signif(HD_lav_lm$coef[[1]], 4))
HD_lav_chi <- signif(HD_lav_lm$coef[[2]], 4)
HD_pred_lav_lm <- exp(predict(HD_lav_lm, newdata = RO_Census_Hunedoara_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Hunedoara judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Hunedoara_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Hunedoara judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Hunedoara_New %>%
     ggplot(aes(x = log(Rank + HD_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Hunedoara judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Hunedoara_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (IL_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (IL_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (IL_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Ialomita_New <- RO_Census_Ialomita_New %>% 
  left_join(RO_Census_Ialomita_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
IL_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Ialomita_New)
summary(IL_pw_lm)
IL_pred_pw_lm <- exp(predict(IL_pw_lm, newdata = RO_Census_Ialomita_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
IL_ZM_prm <- get_ZM_Param("RO_Census_Ialomita_New", "Pop_Freq", "Rank")
IL_ZM_m <- IL_ZM_prm$m
IL_ZM_alpha <- IL_ZM_prm$alpha_ZM
IL_ZM_cst <- IL_ZM_prm$constant_ZM
IL_ZM_lm <- lm(log(RO_Census_Ialomita_New$Pop_Freq) ~ log(RO_Census_Ialomita_New$Rank + IL_ZM_m))
summary(IL_ZM_lm)
IL_pred_ZM_lm <- exp(predict(IL_ZM_lm, newdata = RO_Census_Ialomita_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
IL_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Ialomita_New)
summary(IL_exp_lm)
IL_pred_exp_lm <- exp(predict(IL_exp_lm, newdata = RO_Census_Ialomita_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Ialomita_New <- RO_Census_Ialomita_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Ialomita_New) - Rank + 1))
)
IL_lav_lm <- lm(log(RO_Census_Ialomita_New$Pop_Freq) ~ RO_Census_Ialomita_New$Lav_exp)
summary(IL_lav_lm)
IL_lav_kst <- exp(signif(IL_lav_lm$coef[[1]], 4))
IL_lav_chi <- signif(IL_lav_lm$coef[[2]], 4)
IL_pred_lav_lm <- exp(predict(IL_lav_lm, newdata = RO_Census_Ialomita_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Ialomita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ialomita_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Ialomita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ialomita_New %>%
     ggplot(aes(x = log(Rank + IL_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Ialomita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ialomita_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (IS_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (IS_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (IS_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Iasi_New <- RO_Census_Iasi_New %>% 
  left_join(RO_Census_Iasi_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
IS_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Iasi_New)
summary(IS_pw_lm)
IS_pred_pw_lm <- exp(predict(IS_pw_lm, newdata = RO_Census_Iasi_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
IS_ZM_prm <- get_ZM_Param("RO_Census_Iasi_New", "Pop_Freq", "Rank")
IS_ZM_m <- IS_ZM_prm$m
IS_ZM_alpha <- IS_ZM_prm$alpha_ZM
IS_ZM_cst <- IS_ZM_prm$constant_ZM
IS_ZM_lm <- lm(log(RO_Census_Iasi_New$Pop_Freq) ~ log(RO_Census_Iasi_New$Rank + IS_ZM_m))
summary(IS_ZM_lm)
IS_pred_ZM_lm <- exp(predict(IS_ZM_lm, newdata = RO_Census_Iasi_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
IS_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Iasi_New)
summary(IS_exp_lm)
IS_pred_exp_lm <- exp(predict(IS_exp_lm, newdata = RO_Census_Iasi_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Iasi_New <- RO_Census_Iasi_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Iasi_New) - Rank + 1))
)
IS_lav_lm <- lm(log(RO_Census_Iasi_New$Pop_Freq) ~ RO_Census_Iasi_New$Lav_exp)
summary(IS_lav_lm)
IS_lav_kst <- exp(signif(IS_lav_lm$coef[[1]], 4))
IS_lav_chi <- signif(IS_lav_lm$coef[[2]], 4)
IS_pred_lav_lm <- exp(predict(IS_lav_lm, newdata = RO_Census_Iasi_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Iasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Iasi_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Iasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Iasi_New %>%
     ggplot(aes(x = log(Rank + IS_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Iasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Iasi_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (IF_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (IF_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (IF_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Ilfov_New <- RO_Census_Ilfov_New %>% 
  left_join(RO_Census_Ilfov_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
IF_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Ilfov_New)
summary(IF_pw_lm)
IF_pred_pw_lm <- exp(predict(IF_pw_lm, newdata = RO_Census_Ilfov_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
IF_ZM_prm <- get_ZM_Param("RO_Census_Ilfov_New", "Pop_Freq", "Rank")
IF_ZM_m <- IF_ZM_prm$m
IF_ZM_alpha <- IF_ZM_prm$alpha_ZM
IF_ZM_cst <- IF_ZM_prm$constant_ZM
IF_ZM_lm <- lm(log(RO_Census_Ilfov_New$Pop_Freq) ~ log(RO_Census_Ilfov_New$Rank + IF_ZM_m))
summary(IF_ZM_lm)
IF_pred_ZM_lm <- exp(predict(IF_ZM_lm, newdata = RO_Census_Ilfov_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
IF_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Ilfov_New)
summary(IF_exp_lm)
IF_pred_exp_lm <- exp(predict(IF_exp_lm, newdata = RO_Census_Ilfov_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Ilfov_New <- RO_Census_Ilfov_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Ilfov_New) - Rank + 1))
)
IF_lav_lm <- lm(log(RO_Census_Ilfov_New$Pop_Freq) ~ RO_Census_Ilfov_New$Lav_exp)
summary(IF_lav_lm)
IF_lav_kst <- exp(signif(IF_lav_lm$coef[[1]], 4))
IF_lav_chi <- signif(IF_lav_lm$coef[[2]], 4)
IF_pred_lav_lm <- exp(predict(IF_lav_lm, newdata = RO_Census_Ilfov_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Ilfov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ilfov_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Ilfov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ilfov_New %>%
     ggplot(aes(x = log(Rank + IF_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Ilfov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ilfov_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (MM_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (MM_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (MM_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Maramures_New <- RO_Census_Maramures_New %>% 
  left_join(RO_Census_Maramures_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
MM_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Maramures_New)
summary(MM_pw_lm)
MM_pred_pw_lm <- exp(predict(MM_pw_lm, newdata = RO_Census_Maramures_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
MM_ZM_prm <- get_ZM_Param("RO_Census_Maramures_New", "Pop_Freq", "Rank")
MM_ZM_m <- MM_ZM_prm$m
MM_ZM_alpha <- MM_ZM_prm$alpha_ZM
MM_ZM_cst <- MM_ZM_prm$constant_ZM
MM_ZM_lm <- lm(log(RO_Census_Maramures_New$Pop_Freq) ~ log(RO_Census_Maramures_New$Rank + MM_ZM_m))
summary(MM_ZM_lm)
MM_pred_ZM_lm <- exp(predict(MM_ZM_lm, newdata = RO_Census_Maramures_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
MM_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Maramures_New)
summary(MM_exp_lm)
MM_pred_exp_lm <- exp(predict(MM_exp_lm, newdata = RO_Census_Maramures_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Maramures_New <- RO_Census_Maramures_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Maramures_New) - Rank + 1))
)
MM_lav_lm <- lm(log(RO_Census_Maramures_New$Pop_Freq) ~ RO_Census_Maramures_New$Lav_exp)
summary(MM_lav_lm)
MM_lav_kst <- exp(signif(MM_lav_lm$coef[[1]], 4))
MM_lav_chi <- signif(MM_lav_lm$coef[[2]], 4)
MM_pred_lav_lm <- exp(predict(MM_lav_lm, newdata = RO_Census_Maramures_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Maramures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Maramures_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Maramures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Maramures_New %>%
     ggplot(aes(x = log(Rank + MM_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Maramures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Maramures_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (MH_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (MH_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (MH_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Mehedinti_New <- RO_Census_Mehedinti_New %>% 
  left_join(RO_Census_Mehedinti_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
MH_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Mehedinti_New)
summary(MH_pw_lm)
MH_pred_pw_lm <- exp(predict(MH_pw_lm, newdata = RO_Census_Mehedinti_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
MH_ZM_prm <- get_ZM_Param("RO_Census_Mehedinti_New", "Pop_Freq", "Rank")
MH_ZM_m <- MH_ZM_prm$m
MH_ZM_alpha <- MH_ZM_prm$alpha_ZM
MH_ZM_cst <- MH_ZM_prm$constant_ZM
MH_ZM_lm <- lm(log(RO_Census_Mehedinti_New$Pop_Freq) ~ log(RO_Census_Mehedinti_New$Rank + MH_ZM_m))
summary(MH_ZM_lm)
MH_pred_ZM_lm <- exp(predict(MH_ZM_lm, newdata = RO_Census_Mehedinti_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
MH_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Mehedinti_New)
summary(MH_exp_lm)
MH_pred_exp_lm <- exp(predict(MH_exp_lm, newdata = RO_Census_Mehedinti_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Mehedinti_New <- RO_Census_Mehedinti_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Mehedinti_New) - Rank + 1))
)
MH_lav_lm <- lm(log(RO_Census_Mehedinti_New$Pop_Freq) ~ RO_Census_Mehedinti_New$Lav_exp)
summary(MH_lav_lm)
MH_lav_kst <- exp(signif(MH_lav_lm$coef[[1]], 4))
MH_lav_chi <- signif(MH_lav_lm$coef[[2]], 4)
MH_pred_lav_lm <- exp(predict(MH_lav_lm, newdata = RO_Census_Mehedinti_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Mehedinti judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mehedinti_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Mehedinti judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mehedinti_New %>%
     ggplot(aes(x = log(Rank + MH_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Mehedinti judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mehedinti_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (MS_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (MS_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (MS_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Mures_New <- RO_Census_Mures_New %>% 
  left_join(RO_Census_Mures_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
MS_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Mures_New)
summary(MS_pw_lm)
MS_pred_pw_lm <- exp(predict(MS_pw_lm, newdata = RO_Census_Mures_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
MS_ZM_prm <- get_ZM_Param("RO_Census_Mures_New", "Pop_Freq", "Rank")
MS_ZM_m <- MS_ZM_prm$m
MS_ZM_alpha <- MS_ZM_prm$alpha_ZM
MS_ZM_cst <- MS_ZM_prm$constant_ZM
MS_ZM_lm <- lm(log(RO_Census_Mures_New$Pop_Freq) ~ log(RO_Census_Mures_New$Rank + MS_ZM_m))
summary(MS_ZM_lm)
MS_pred_ZM_lm <- exp(predict(MS_ZM_lm, newdata = RO_Census_Mures_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
MS_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Mures_New)
summary(MS_exp_lm)
MS_pred_exp_lm <- exp(predict(MS_exp_lm, newdata = RO_Census_Mures_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Mures_New <- RO_Census_Mures_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Mures_New) - Rank + 1))
)
MS_lav_lm <- lm(log(RO_Census_Mures_New$Pop_Freq) ~ RO_Census_Mures_New$Lav_exp)
summary(MS_lav_lm)
MS_lav_kst <- exp(signif(MS_lav_lm$coef[[1]], 4))
MS_lav_chi <- signif(MS_lav_lm$coef[[2]], 4)
MS_pred_lav_lm <- exp(predict(MS_lav_lm, newdata = RO_Census_Mures_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Mures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mures_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Mures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mures_New %>%
     ggplot(aes(x = log(Rank + MS_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Mures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mures_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (NT_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (NT_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (NT_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Neamt_New <- RO_Census_Neamt_New %>% 
  left_join(RO_Census_Neamt_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
NT_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Neamt_New)
summary(NT_pw_lm)
NT_pred_pw_lm <- exp(predict(NT_pw_lm, newdata = RO_Census_Neamt_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
NT_ZM_prm <- get_ZM_Param("RO_Census_Neamt_New", "Pop_Freq", "Rank")
NT_ZM_m <- NT_ZM_prm$m
NT_ZM_alpha <- NT_ZM_prm$alpha_ZM
NT_ZM_cst <- NT_ZM_prm$constant_ZM
NT_ZM_lm <- lm(log(RO_Census_Neamt_New$Pop_Freq) ~ log(RO_Census_Neamt_New$Rank + NT_ZM_m))
summary(NT_ZM_lm)
NT_pred_ZM_lm <- exp(predict(NT_ZM_lm, newdata = RO_Census_Neamt_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
NT_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Neamt_New)
summary(NT_exp_lm)
NT_pred_exp_lm <- exp(predict(NT_exp_lm, newdata = RO_Census_Neamt_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Neamt_New <- RO_Census_Neamt_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Neamt_New) - Rank + 1))
)
NT_lav_lm <- lm(log(RO_Census_Neamt_New$Pop_Freq) ~ RO_Census_Neamt_New$Lav_exp)
summary(NT_lav_lm)
NT_lav_kst <- exp(signif(NT_lav_lm$coef[[1]], 4))
NT_lav_chi <- signif(NT_lav_lm$coef[[2]], 4)
NT_pred_lav_lm <- exp(predict(NT_lav_lm, newdata = RO_Census_Neamt_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Neamt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Neamt_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Neamt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Neamt_New %>%
     ggplot(aes(x = log(Rank + NT_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Neamt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Neamt_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (OT_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (OT_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (OT_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Olt_New <- RO_Census_Olt_New %>% 
  left_join(RO_Census_Olt_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
OT_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Olt_New)
summary(OT_pw_lm)
OT_pred_pw_lm <- exp(predict(OT_pw_lm, newdata = RO_Census_Olt_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
OT_ZM_prm <- get_ZM_Param("RO_Census_Olt_New", "Pop_Freq", "Rank")
OT_ZM_m <- OT_ZM_prm$m
OT_ZM_alpha <- OT_ZM_prm$alpha_ZM
OT_ZM_cst <- OT_ZM_prm$constant_ZM
OT_ZM_lm <- lm(log(RO_Census_Olt_New$Pop_Freq) ~ log(RO_Census_Olt_New$Rank + OT_ZM_m))
summary(OT_ZM_lm)
OT_pred_ZM_lm <- exp(predict(OT_ZM_lm, newdata = RO_Census_Olt_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
OT_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Olt_New)
summary(OT_exp_lm)
OT_pred_exp_lm <- exp(predict(OT_exp_lm, newdata = RO_Census_Olt_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Olt_New <- RO_Census_Olt_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Olt_New) - Rank + 1))
)
OT_lav_lm <- lm(log(RO_Census_Olt_New$Pop_Freq) ~ RO_Census_Olt_New$Lav_exp)
summary(OT_lav_lm)
OT_lav_kst <- exp(signif(OT_lav_lm$coef[[1]], 4))
OT_lav_chi <- signif(OT_lav_lm$coef[[2]], 4)
OT_pred_lav_lm <- exp(predict(OT_lav_lm, newdata = RO_Census_Olt_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Olt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Olt_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Olt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Olt_New %>%
     ggplot(aes(x = log(Rank + OT_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Olt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Olt_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (PH_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (PH_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (PH_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Prahova_New <- RO_Census_Prahova_New %>% 
  left_join(RO_Census_Prahova_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
PH_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Prahova_New)
summary(PH_pw_lm)
PH_pred_pw_lm <- exp(predict(PH_pw_lm, newdata = RO_Census_Prahova_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
PH_ZM_prm <- get_ZM_Param("RO_Census_Prahova_New", "Pop_Freq", "Rank")
PH_ZM_m <- PH_ZM_prm$m
PH_ZM_alpha <- PH_ZM_prm$alpha_ZM
PH_ZM_cst <- PH_ZM_prm$constant_ZM
PH_ZM_lm <- lm(log(RO_Census_Prahova_New$Pop_Freq) ~ log(RO_Census_Prahova_New$Rank + PH_ZM_m))
summary(PH_ZM_lm)
PH_pred_ZM_lm <- exp(predict(PH_ZM_lm, newdata = RO_Census_Prahova_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
PH_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Prahova_New)
summary(PH_exp_lm)
PH_pred_exp_lm <- exp(predict(PH_exp_lm, newdata = RO_Census_Prahova_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Prahova_New <- RO_Census_Prahova_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Prahova_New) - Rank + 1))
)
PH_lav_lm <- lm(log(RO_Census_Prahova_New$Pop_Freq) ~ RO_Census_Prahova_New$Lav_exp)
summary(PH_lav_lm)
PH_lav_kst <- exp(signif(PH_lav_lm$coef[[1]], 4))
PH_lav_chi <- signif(PH_lav_lm$coef[[2]], 4)
PH_pred_lav_lm <- exp(predict(PH_lav_lm, newdata = RO_Census_Prahova_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Prahova judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Prahova_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Prahova judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Prahova_New %>%
     ggplot(aes(x = log(Rank + PH_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Prahova judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Prahova_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (SM_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (SM_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (SM_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n == max(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_SatuMare_New <- RO_Census_SatuMare_New %>% 
  left_join(RO_Census_SatuMare_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
SM_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_SatuMare_New)
summary(SM_pw_lm)
SM_pred_pw_lm <- exp(predict(SM_pw_lm, newdata = RO_Census_SatuMare_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
SM_ZM_prm <- get_ZM_Param("RO_Census_SatuMare_New", "Pop_Freq", "Rank")
SM_ZM_m <- SM_ZM_prm$m
SM_ZM_alpha <- SM_ZM_prm$alpha_ZM
SM_ZM_cst <- SM_ZM_prm$constant_ZM
SM_ZM_lm <- lm(log(RO_Census_SatuMare_New$Pop_Freq) ~ log(RO_Census_SatuMare_New$Rank + SM_ZM_m))
summary(SM_ZM_lm)
SM_pred_ZM_lm <- exp(predict(SM_ZM_lm, newdata = RO_Census_SatuMare_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
SM_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_SatuMare_New)
summary(SM_exp_lm)
SM_pred_exp_lm <- exp(predict(SM_exp_lm, newdata = RO_Census_SatuMare_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_SatuMare_New <- RO_Census_SatuMare_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_SatuMare_New) - Rank + 1))
)
SM_lav_lm <- lm(log(RO_Census_SatuMare_New$Pop_Freq) ~ RO_Census_SatuMare_New$Lav_exp)
summary(SM_lav_lm)
SM_lav_kst <- exp(signif(SM_lav_lm$coef[[1]], 4))
SM_lav_chi <- signif(SM_lav_lm$coef[[2]], 4)
SM_pred_lav_lm <- exp(predict(SM_lav_lm, newdata = RO_Census_SatuMare_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Satu Mare judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_SatuMare_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Satu Mare judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_SatuMare_New %>%
     ggplot(aes(x = log(Rank + SM_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Satu Mare judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_SatuMare_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (SJ_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (SJ_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (SJ_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Salaj_New <- RO_Census_Salaj_New %>% 
  left_join(RO_Census_Salaj_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
SJ_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Salaj_New)
summary(SJ_pw_lm)
SJ_pred_pw_lm <- exp(predict(SJ_pw_lm, newdata = RO_Census_Salaj_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
SJ_ZM_prm <- get_ZM_Param("RO_Census_Salaj_New", "Pop_Freq", "Rank")
SJ_ZM_m <- SJ_ZM_prm$m
SJ_ZM_alpha <- SJ_ZM_prm$alpha_ZM
SJ_ZM_cst <- SJ_ZM_prm$constant_ZM
SJ_ZM_lm <- lm(log(RO_Census_Salaj_New$Pop_Freq) ~ log(RO_Census_Salaj_New$Rank + SJ_ZM_m))
summary(SJ_ZM_lm)
SJ_pred_ZM_lm <- exp(predict(SJ_ZM_lm, newdata = RO_Census_Salaj_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
SJ_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Salaj_New)
summary(SJ_exp_lm)
SJ_pred_exp_lm <- exp(predict(SJ_exp_lm, newdata = RO_Census_Salaj_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Salaj_New <- RO_Census_Salaj_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Salaj_New) - Rank + 1))
)
SJ_lav_lm <- lm(log(RO_Census_Salaj_New$Pop_Freq) ~ RO_Census_Salaj_New$Lav_exp)
summary(SJ_lav_lm)
SJ_lav_kst <- exp(signif(SJ_lav_lm$coef[[1]], 4))
SJ_lav_chi <- signif(SJ_lav_lm$coef[[2]], 4)
SJ_pred_lav_lm <- exp(predict(SJ_lav_lm, newdata = RO_Census_Salaj_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Salaj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Salaj_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Salaj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Salaj_New %>%
     ggplot(aes(x = log(Rank + SJ_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Salaj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Salaj_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (SB_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (SB_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (SB_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Sibiu_New <- RO_Census_Sibiu_New %>% 
  left_join(RO_Census_Sibiu_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
SB_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Sibiu_New)
summary(SB_pw_lm)
SB_pred_pw_lm <- exp(predict(SB_pw_lm, newdata = RO_Census_Sibiu_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
SB_ZM_prm <- get_ZM_Param("RO_Census_Sibiu_New", "Pop_Freq", "Rank")
SB_ZM_m <- SB_ZM_prm$m
SB_ZM_alpha <- SB_ZM_prm$alpha_ZM
SB_ZM_cst <- SB_ZM_prm$constant_ZM
SB_ZM_lm <- lm(log(RO_Census_Sibiu_New$Pop_Freq) ~ log(RO_Census_Sibiu_New$Rank + SB_ZM_m))
summary(SB_ZM_lm)
SB_pred_ZM_lm <- exp(predict(SB_ZM_lm, newdata = RO_Census_Sibiu_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
SB_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Sibiu_New)
summary(SB_exp_lm)
SB_pred_exp_lm <- exp(predict(SB_exp_lm, newdata = RO_Census_Sibiu_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Sibiu_New <- RO_Census_Sibiu_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Sibiu_New) - Rank + 1))
)
SB_lav_lm <- lm(log(RO_Census_Sibiu_New$Pop_Freq) ~ RO_Census_Sibiu_New$Lav_exp)
summary(SB_lav_lm)
SB_lav_kst <- exp(signif(SB_lav_lm$coef[[1]], 4))
SB_lav_chi <- signif(SB_lav_lm$coef[[2]], 4)
SB_pred_lav_lm <- exp(predict(SB_lav_lm, newdata = RO_Census_Sibiu_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Sibiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Sibiu_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Sibiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Sibiu_New %>%
     ggplot(aes(x = log(Rank + SB_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Sibiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Sibiu_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (SV_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (SV_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (SV_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Suceava_New <- RO_Census_Suceava_New %>% 
  left_join(RO_Census_Suceava_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
SV_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Suceava_New)
summary(SV_pw_lm)
SV_pred_pw_lm <- exp(predict(SV_pw_lm, newdata = RO_Census_Suceava_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
SV_ZM_prm <- get_ZM_Param("RO_Census_Suceava_New", "Pop_Freq", "Rank")
SV_ZM_m <- SV_ZM_prm$m
SV_ZM_alpha <- SV_ZM_prm$alpha_ZM
SV_ZM_cst <- SV_ZM_prm$constant_ZM
SV_ZM_lm <- lm(log(RO_Census_Suceava_New$Pop_Freq) ~ log(RO_Census_Suceava_New$Rank + SV_ZM_m))
summary(SV_ZM_lm)
SV_pred_ZM_lm <- exp(predict(SV_ZM_lm, newdata = RO_Census_Suceava_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
SV_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Suceava_New)
summary(SV_exp_lm)
SV_pred_exp_lm <- exp(predict(SV_exp_lm, newdata = RO_Census_Suceava_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Suceava_New <- RO_Census_Suceava_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Suceava_New) - Rank + 1))
)
SV_lav_lm <- lm(log(RO_Census_Suceava_New$Pop_Freq) ~ RO_Census_Suceava_New$Lav_exp)
summary(SV_lav_lm)
SV_lav_kst <- exp(signif(SV_lav_lm$coef[[1]], 4))
SV_lav_chi <- signif(SV_lav_lm$coef[[2]], 4)
SV_pred_lav_lm <- exp(predict(SV_lav_lm, newdata = RO_Census_Suceava_New, 
                              interval = "prediction", level = 0.95))

# Data Distributions
RO_Census_Suceava_New %>%
  ggplot(aes(x = Rank, y = Pop_Freq)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava_New$Rank, 
                              Pop_Freq = SV_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 30, y = 0.06,
            label = paste0("y", "==", signif(exp(SV_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(SV_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 60, y = 0.06,
            label = paste0("R^2 ==", signif(summary(SV_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava_New$Rank, 
                              Pop_Freq = SV_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 30, y = 0.05,
            label = paste0("y", "==", signif(exp(SV_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(SV_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 60, y = 0.05,
            label = paste0("R^2 ==", signif(summary(SV_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava_New$Rank, 
                              Pop_Freq = SV_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 30, y = 0.04,
            label = paste0("y", "==", signif(SV_ZM_cst, 4), "%.%", "(",
                           signif(SV_ZM_m, 4), "+ x)^", signif(SV_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 60, y = 0.04,
            label = paste0("R^2 ==", signif(summary(SV_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava_New$Rank, 
                              Pop_Freq = SV_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 30, y = 0.03,
            label = paste0("y", "==", signif(SV_lav_kst, 4), "%.%", "x^",
                           -signif(SV_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 60, y = 0.03,
            label = paste0("R^2 ==", signif(summary(SV_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Suceava judet (Without Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Suceava_New %>%
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Suceava judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Suceava_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Suceava judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Suceava_New %>%
     ggplot(aes(x = log(Rank + SV_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Suceava judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Suceava_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (TR_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (TR_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (TR_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Teleorman_New <- RO_Census_Teleorman_New %>% 
  left_join(RO_Census_Teleorman_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
TR_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Teleorman_New)
summary(TR_pw_lm)
TR_pred_pw_lm <- exp(predict(TR_pw_lm, newdata = RO_Census_Teleorman_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
TR_ZM_prm <- get_ZM_Param("RO_Census_Teleorman_New", "Pop_Freq", "Rank")
TR_ZM_m <- TR_ZM_prm$m
TR_ZM_alpha <- TR_ZM_prm$alpha_ZM
TR_ZM_cst <- TR_ZM_prm$constant_ZM
TR_ZM_lm <- lm(log(RO_Census_Teleorman_New$Pop_Freq) ~ log(RO_Census_Teleorman_New$Rank + TR_ZM_m))
summary(TR_ZM_lm)
TR_pred_ZM_lm <- exp(predict(TR_ZM_lm, newdata = RO_Census_Teleorman_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
TR_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Teleorman_New)
summary(TR_exp_lm)
TR_pred_exp_lm <- exp(predict(TR_exp_lm, newdata = RO_Census_Teleorman_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Teleorman_New <- RO_Census_Teleorman_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Teleorman_New) - Rank + 1))
)
TR_lav_lm <- lm(log(RO_Census_Teleorman_New$Pop_Freq) ~ RO_Census_Teleorman_New$Lav_exp)
summary(TR_lav_lm)
TR_lav_kst <- exp(signif(TR_lav_lm$coef[[1]], 4))
TR_lav_chi <- signif(TR_lav_lm$coef[[2]], 4)
TR_pred_lav_lm <- exp(predict(TR_lav_lm, newdata = RO_Census_Teleorman_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Teleorman judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Teleorman_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Teleorman judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Teleorman_New %>%
     ggplot(aes(x = log(Rank + TR_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Teleorman judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Teleorman_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (TM_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (TM_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (TM_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Timis_New <- RO_Census_Timis_New %>% 
  left_join(RO_Census_Timis_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
TM_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Timis_New)
summary(TM_pw_lm)
TM_pred_pw_lm <- exp(predict(TM_pw_lm, newdata = RO_Census_Timis_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
TM_ZM_prm <- get_ZM_Param("RO_Census_Timis_New", "Pop_Freq", "Rank")
TM_ZM_m <- TM_ZM_prm$m
TM_ZM_alpha <- TM_ZM_prm$alpha_ZM
TM_ZM_cst <- TM_ZM_prm$constant_ZM
TM_ZM_lm <- lm(log(RO_Census_Timis_New$Pop_Freq) ~ log(RO_Census_Timis_New$Rank + TM_ZM_m))
summary(TM_ZM_lm)
TM_pred_ZM_lm <- exp(predict(TM_ZM_lm, newdata = RO_Census_Timis_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
TM_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Timis_New)
summary(TM_exp_lm)
TM_pred_exp_lm <- exp(predict(TM_exp_lm, newdata = RO_Census_Timis_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Timis_New <- RO_Census_Timis_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Timis_New) - Rank + 1))
)
TM_lav_lm <- lm(log(RO_Census_Timis_New$Pop_Freq) ~ RO_Census_Timis_New$Lav_exp)
summary(TM_lav_lm)
TM_lav_kst <- exp(signif(TM_lav_lm$coef[[1]], 4))
TM_lav_chi <- signif(TM_lav_lm$coef[[2]], 4)
TM_pred_lav_lm <- exp(predict(TM_lav_lm, newdata = RO_Census_Timis_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Timis judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Timis_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Timis judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Timis_New %>%
     ggplot(aes(x = log(Rank + TM_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Timis judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Timis_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (TL_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (TL_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (TL_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Tulcea_New <- RO_Census_Tulcea_New %>% 
  left_join(RO_Census_Tulcea_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
TL_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Tulcea_New)
summary(TL_pw_lm)
TL_pred_pw_lm <- exp(predict(TL_pw_lm, newdata = RO_Census_Tulcea_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
TL_ZM_prm <- get_ZM_Param("RO_Census_Tulcea_New", "Pop_Freq", "Rank")
TL_ZM_m <- TL_ZM_prm$m
TL_ZM_alpha <- TL_ZM_prm$alpha_ZM
TL_ZM_cst <- TL_ZM_prm$constant_ZM
TL_ZM_lm <- lm(log(RO_Census_Tulcea_New$Pop_Freq) ~ log(RO_Census_Tulcea_New$Rank + TL_ZM_m))
summary(TL_ZM_lm)
TL_pred_ZM_lm <- exp(predict(TL_ZM_lm, newdata = RO_Census_Tulcea_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
TL_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Tulcea_New)
summary(TL_exp_lm)
TL_pred_exp_lm <- exp(predict(TL_exp_lm, newdata = RO_Census_Tulcea_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Tulcea_New <- RO_Census_Tulcea_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Tulcea_New) - Rank + 1))
)
TL_lav_lm <- lm(log(RO_Census_Tulcea_New$Pop_Freq) ~ RO_Census_Tulcea_New$Lav_exp)
summary(TL_lav_lm)
TL_lav_kst <- exp(signif(TL_lav_lm$coef[[1]], 4))
TL_lav_chi <- signif(TL_lav_lm$coef[[2]], 4)
TL_pred_lav_lm <- exp(predict(TL_lav_lm, newdata = RO_Census_Tulcea_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Tulcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Tulcea_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Tulcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Tulcea_New %>%
     ggplot(aes(x = log(Rank + TL_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Tulcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Tulcea_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (VS_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (VS_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (VS_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Vaslui_New <- RO_Census_Vaslui_New %>% 
  left_join(RO_Census_Vaslui_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
VS_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Vaslui_New)
summary(VS_pw_lm)
VS_pred_pw_lm <- exp(predict(VS_pw_lm, newdata = RO_Census_Vaslui_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
VS_ZM_prm <- get_ZM_Param("RO_Census_Vaslui_New", "Pop_Freq", "Rank")
VS_ZM_m <- VS_ZM_prm$m
VS_ZM_alpha <- VS_ZM_prm$alpha_ZM
VS_ZM_cst <- VS_ZM_prm$constant_ZM
VS_ZM_lm <- lm(log(RO_Census_Vaslui_New$Pop_Freq) ~ log(RO_Census_Vaslui_New$Rank + VS_ZM_m))
summary(VS_ZM_lm)
VS_pred_ZM_lm <- exp(predict(VS_ZM_lm, newdata = RO_Census_Vaslui_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
VS_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Vaslui_New)
summary(VS_exp_lm)
VS_pred_exp_lm <- exp(predict(VS_exp_lm, newdata = RO_Census_Vaslui_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Vaslui_New <- RO_Census_Vaslui_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Vaslui_New) - Rank + 1))
)
VS_lav_lm <- lm(log(RO_Census_Vaslui_New$Pop_Freq) ~ RO_Census_Vaslui_New$Lav_exp)
summary(VS_lav_lm)
VS_lav_kst <- exp(signif(VS_lav_lm$coef[[1]], 4))
VS_lav_chi <- signif(VS_lav_lm$coef[[2]], 4)
VS_pred_lav_lm <- exp(predict(VS_lav_lm, newdata = RO_Census_Vaslui_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Vaslui judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vaslui_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Vaslui judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vaslui_New %>%
     ggplot(aes(x = log(Rank + VS_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Vaslui judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vaslui_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (VL_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (VL_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (VL_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Valcea_New <- RO_Census_Valcea_New %>% 
  left_join(RO_Census_Valcea_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
VL_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Valcea_New)
summary(VL_pw_lm)
VL_pred_pw_lm <- exp(predict(VL_pw_lm, newdata = RO_Census_Valcea_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
VL_ZM_prm <- get_ZM_Param("RO_Census_Valcea_New", "Pop_Freq", "Rank")
VL_ZM_m <- VL_ZM_prm$m
VL_ZM_alpha <- VL_ZM_prm$alpha_ZM
VL_ZM_cst <- VL_ZM_prm$constant_ZM
VL_ZM_lm <- lm(log(RO_Census_Valcea_New$Pop_Freq) ~ log(RO_Census_Valcea_New$Rank + VL_ZM_m))
summary(VL_ZM_lm)
VL_pred_ZM_lm <- exp(predict(VL_ZM_lm, newdata = RO_Census_Valcea_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
VL_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Valcea_New)
summary(VL_exp_lm)
VL_pred_exp_lm <- exp(predict(VL_exp_lm, newdata = RO_Census_Valcea_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Valcea_New <- RO_Census_Valcea_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Valcea_New) - Rank + 1))
)
VL_lav_lm <- lm(log(RO_Census_Valcea_New$Pop_Freq) ~ RO_Census_Valcea_New$Lav_exp)
summary(VL_lav_lm)
VL_lav_kst <- exp(signif(VL_lav_lm$coef[[1]], 4))
VL_lav_chi <- signif(VL_lav_lm$coef[[2]], 4)
VL_pred_lav_lm <- exp(predict(VL_lav_lm, newdata = RO_Census_Valcea_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Valcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Valcea_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Valcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Valcea_New %>%
     ggplot(aes(x = log(Rank + VL_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Valcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Valcea_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
         select(Rank) %>% distinct()),
      (VN_out$ZM %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (VN_out$LAV %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct()),
      (VN_out$EXP %>% filter(
        !Out_Method %in% c("Leverage", "DFBETA_intercept", "DFBETA_var_x")) %>%
         select(Rank) %>% distinct())
    ) %>% count(Rank) %>% arrange(desc(n)) %>% filter(n >= mean(n))
  ) %>% select(Unit_Name, Unit_Type, Judet_Name), 
  by = c("Unit_Name", "Unit_Type", "Judet_Name"))
RO_Census_Vrancea_New <- RO_Census_Vrancea_New %>% 
  left_join(RO_Census_Vrancea_New %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()

# Power Law
VN_pw_lm <- lm(log(Pop_Freq) ~ log(Rank), data = RO_Census_Vrancea_New)
summary(VN_pw_lm)
VN_pred_pw_lm <- exp(predict(VN_pw_lm, newdata = RO_Census_Vrancea_New, 
                             interval = "prediction", level = 0.95))

# Zipf_Mandelbrot Law
VN_ZM_prm <- get_ZM_Param("RO_Census_Vrancea_New", "Pop_Freq", "Rank")
VN_ZM_m <- VN_ZM_prm$m
VN_ZM_alpha <- VN_ZM_prm$alpha_ZM
VN_ZM_cst <- VN_ZM_prm$constant_ZM
VN_ZM_lm <- lm(log(RO_Census_Vrancea_New$Pop_Freq) ~ log(RO_Census_Vrancea_New$Rank + VN_ZM_m))
summary(VN_ZM_lm)
VN_pred_ZM_lm <- exp(predict(VN_ZM_lm, newdata = RO_Census_Vrancea_New, 
                             interval = "prediction", level = 0.95))

# Exponential Law
VN_exp_lm <- lm(log(Pop_Freq) ~ Rank, RO_Census_Vrancea_New)
summary(VN_exp_lm)
VN_pred_exp_lm <- exp(predict(VN_exp_lm, newdata = RO_Census_Vrancea_New, 
                              interval = "prediction", level = 0.95))

# Lavalette Function
RO_Census_Vrancea_New <- RO_Census_Vrancea_New %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Vrancea_New) - Rank + 1))
)
VN_lav_lm <- lm(log(RO_Census_Vrancea_New$Pop_Freq) ~ RO_Census_Vrancea_New$Lav_exp)
summary(VN_lav_lm)
VN_lav_kst <- exp(signif(VN_lav_lm$coef[[1]], 4))
VN_lav_chi <- signif(VN_lav_lm$coef[[2]], 4)
VN_pred_lav_lm <- exp(predict(VN_lav_lm, newdata = RO_Census_Vrancea_New, 
                              interval = "prediction", level = 0.95))

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
     ggplot(aes(x = log(Rank), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Vrancea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vrancea_New %>%
     ggplot(aes(x = Rank, y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Vrancea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vrancea_New %>%
     ggplot(aes(x = log(Rank + VN_ZM_m), y = log(Pop_Freq))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Vrancea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vrancea_New %>%
     ggplot(aes(x = Lav_exp, y = log(Pop_Freq))) +
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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
    Judet_Name = (RO_Census_df %>% select(Judet_Name) %>% distinct() %>% 
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

# IV. Export files
#write_csv(RSquared_t, "./RO_Census_RSquared.csv")

write.xlsx(list("R-Squared" = RSquared_t, "Adjusted R-Squared" = Adj_RSquared_t,
                "F-Stat" = F_Stat_t, "AIC" = AIC_t), 
           file = 'RO_Census_Indicators_lm_new.xlsx')
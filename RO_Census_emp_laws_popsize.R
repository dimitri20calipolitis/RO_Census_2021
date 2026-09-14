# Load the library
library(tidyverse)
library(readxl)
library(scales)
library(paletteer)
library(treemapify)
library(ggpmisc)
library(gridExtra)
library(openxlsx)
library(olsrr)
library(jmv)
library(gtsummary)
library(skedastic)
library(lmtest)
library(car)
library(tseries)
library(e1071)
source("RO_Census_functions.R")

# Load from RO Census 2021
RO_Census_df <- read_excel("./Ro_Census_2021.xlsx", sheet = 1)
View(RO_Census_df)

# Apply the analysis

# I. Descriptive Statistics

RO_Census_df %>% dplyr::select(Population) %>% 
  descriptives(sd = TRUE, se = TRUE, skew = TRUE, kurt = TRUE, 
               pcEqGr = TRUE, pcNEqGr = 4)

RO_Census_df %>% tbl_summary(include = c(Unit_Type, Environment, 
                                         Judet_Name, Is_Residence))

RO_Census_Pop_DescStat <- RO_Census_df %>% group_by(Judet_Name) %>% 
  summarise(Unit_Number = n(), Minimun = min(Population, na.rm = TRUE), 
            Mean = mean(Population, na.rm = TRUE), 
            Median = median(Population, na.rm = TRUE),
            Maximum = max(Population, na.rm = TRUE), 
            Std_Deviation = sd(Population, na.rm = TRUE), 
            Std_Error_Mean = sd(Population, na.rm = TRUE) / sqrt(n()), 
            Skewness = skewness(Population, na.rm = TRUE), 
            Kurtosis = kurtosis(Population, na.rm = TRUE)) %>%
  inner_join(RO_Census_df %>% dplyr::select(Judet_Name, Population) %>% 
               group_by(Judet_Name) %>% summarize(Population = sum(Population))) %>%
  relocate(Population, .after = Unit_Number)

# 1. Number of units by unit type
RO_Census_df %>% group_by(Unit_Type) %>% tally() %>% 
  rename('Units_Number' = 'n') %>% 
  mutate(Percs = round((Units_Number/nrow(RO_Census_df)) * 100, 2))

# 2. Number of units by environment
RO_Census_df %>% group_by(Environment) %>% tally() %>% 
  rename('Units_Number' = 'n') %>% 
  mutate(Percs = round((Units_Number/nrow(RO_Census_df)) * 100, 2))

# 3. Top 10 units by Population

# All Units
RO_Census_df %>% arrange(desc(Population)) %>% .[1:10,] %>%
  ggplot(aes(reorder(Unit_Name, -Population), Population, 
             fill = Population)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label = Population), vjust=-0.3, size=3.5) +
  scale_fill_paletteer_c("pals::coolwarm") +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Unit Name", y = "Population") + 
  ggtitle("Top 10 Big Units by Population") +
  theme(plot.title = element_text(hjust = 0.5))

RO_Census_df %>% filter(!is.na(Judet_Name)) %>% 
  arrange(desc(Population)) %>% .[1:10,] %>%
  ggplot(aes(reorder(Unit_Name, -Population), Population, 
             fill = Population)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label = Population), vjust=-0.3, size=3.5) +
  scale_fill_paletteer_c("pals::coolwarm") +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Unit Name", y = "Population") + 
  #ggtitle("Top 10 Big Units by Population (Without Bucharest)") +
  theme(plot.title = element_text(hjust = 0.5))

# On Municipality
RO_Census_df %>% filter(Unit_Type == 'municipiu') %>% 
  arrange(desc(Population)) %>% .[1:10,] %>%
  ggplot(aes(reorder(Unit_Name, -Population), Population, 
             fill = Population)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label = Population), vjust=-0.3, size=3.5) +
  scale_fill_paletteer_c("pals::coolwarm") +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Municipality Name", y = "Population") + 
  ggtitle("Top 10 Big Municipalities by Population") +
  theme(plot.title = element_text(hjust = 0.5))

RO_Census_df %>% filter(!is.na(Judet_Name) & Unit_Type == 'municipiu') %>% 
  arrange(desc(Population)) %>% .[1:10,] %>%
  ggplot(aes(reorder(Unit_Name, -Population), Population, 
             fill = Population)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label = Population), vjust=-0.3, size=3.5) +
  scale_fill_paletteer_c("pals::coolwarm") +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Municipality Name", y = "Population") + 
  ggtitle("Top 10 Big Municipalities by Population (Without Bucharest)") +
  theme(plot.title = element_text(hjust = 0.5))

# On City
RO_Census_df %>% filter(Unit_Type == 'oras') %>% 
  arrange(desc(Population)) %>% .[1:10,] %>%
  ggplot(aes(reorder(Unit_Name, -Population), Population, 
             fill = Population)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label = Population), vjust=-0.3, size=3.5) +
  scale_fill_paletteer_c("pals::coolwarm") +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "City Name", y = "Population") + 
  ggtitle("Top 10 Big Cities by Population") +
  theme(plot.title = element_text(hjust = 0.5))

# On Rural Unit
RO_Census_df %>% filter(Unit_Type == 'comuna') %>% 
  arrange(desc(Population)) %>% .[1:10,] %>%
  ggplot(aes(reorder(Unit_Name, -Population), Population, 
             fill = Population)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_text(aes(label = Population), vjust=-0.3, size=3.5) +
  scale_fill_paletteer_c("pals::coolwarm") +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Rural Unit Name", y = "Population") + 
  ggtitle("Top 10 Big Rural Units by Population") +
  theme(plot.title = element_text(hjust = 0.5))

# 4. Distribution of Population by Judet
Judet_Pop <- RO_Census_df %>% dplyr::select(Judet_Name, Population) %>% 
  group_by(Judet_Name) %>% summarize(Population = sum(Population)) %>% 
  mutate(Judet_Name = if_else(is.na(Judet_Name), "BUCURESTI", Judet_Name)) %>%
  mutate(Proc_Pop = round((Population/sum(Population)) * 100, 2)) %>%
  arrange(desc(Population))

Judet_Pop %>% dplyr::select(Judet_Name, Proc_Pop) %>%
ggplot(aes(fill = Proc_Pop, area = Proc_Pop)) +
  geom_treemap() + 
  scale_fill_paletteer_c("ggthemes::Sunset-Sunrise Diverging") +
  geom_treemap_text(aes(label = Judet_Name), 
                    colour ="white", place = "centre", cex = 20) + 
  labs(fill = 'Percentages') +
  ggtitle("Percentages of Population by Each Judet") +
  theme(plot.title = element_text(hjust = 0.5))

# 5. Boxplot on population
RO_Census_df %>% filter(!is.na(Judet_Name)) %>%
  #mutate(Judet_Name = if_else(is.na(Judet_Name), "BUCURESTI", Judet_Name)) %>%
  ggplot(aes(x = "", y = Population, fill = Judet_Name)) + 
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot() + geom_jitter() + facet_wrap(~Judet_Name, scale="free") +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Judet", y = "Population") + 
  ggtitle("Boxplot of Population by each Judet") +
  theme(plot.title = element_text(hjust = 0.5))

# II. Check Empirical Laws (with all data)
RO_Census_rank<- RO_Census_df %>% 
  left_join(RO_Census_df %>% group_by(Judet_Name) %>% 
              summarize(Total_Pop = sum(Population))) %>%
  arrange(Judet_Name, desc(Population)) %>% group_by(Judet_Name) %>% 
  mutate(Rank = row_number(), Pop_Freq = Population/Total_Pop) %>%
  ungroup()
View(RO_Census_rank)

# 1. Alba
RO_Census_Alba <- RO_Census_rank %>% filter(Judet_Name == "ALBA")
#View(RO_Census_Alba)

# Power Law
AB_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Alba)
summary(AB_pw_lm)
AB_pred_pw_lm <- exp(predict(AB_pw_lm, newdata = RO_Census_Alba, 
                          interval = "prediction", level = 0.95))
AB_pw_out <- RO_Census_Alba %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AB_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Alba), 
         DFFIT = dffits(AB_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AB_pw_lm$coefficients) / nrow(RO_Census_Alba)), 
         Rezid_Std = rstudent(AB_pw_lm), Leverage = hatvalues(AB_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AB_pw_lm)), 
         DFBETA = dfbetas(AB_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Alba)))

AB_pw_whtest <- white(AB_pw_lm, interactions = TRUE)
AB_pw_bptest <- bptest(AB_pw_lm, studentize = TRUE)
AB_pw_DWtest <- dwtest(AB_pw_lm)
AB_pw_bgtest <- bgtest(AB_pw_lm, order = 1)
AB_pw_jbtest <- jarque.bera.test(resid(AB_pw_lm))
AB_pw_shtest <- shapiro.test(resid(AB_pw_lm))

# Zipf_Mandelbrot Law
AB_ZM_prm <- get_ZM_Param("RO_Census_Alba", "Population", "Rank")
AB_ZM_m <- AB_ZM_prm$m
AB_ZM_alpha <- AB_ZM_prm$alpha_ZM
AB_ZM_cst <- AB_ZM_prm$constant_ZM
AB_ZM_lm <- lm(log(RO_Census_Alba$Population) ~ log(RO_Census_Alba$Rank + AB_ZM_m))
summary(AB_ZM_lm)
AB_pred_ZM_lm <- exp(predict(AB_ZM_lm, newdata = RO_Census_Alba, 
                          interval = "prediction", level = 0.95))
AB_ZM_out <- RO_Census_Alba %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AB_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Alba), 
         DFFIT = dffits(AB_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AB_ZM_lm$coefficients) / nrow(RO_Census_Alba)), 
         Rezid_Std = rstudent(AB_ZM_lm), Leverage = hatvalues(AB_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AB_ZM_lm)), 
         DFBETA = dfbetas(AB_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Alba)))
AB_ZM_whtest <- white(AB_ZM_lm, interactions = TRUE)
AB_ZM_bptest <- bptest(AB_ZM_lm, studentize = TRUE)
AB_ZM_DWtest <- dwtest(AB_ZM_lm)
AB_ZM_bgtest <- bgtest(AB_ZM_lm, order = 1)
AB_ZM_jbtest <- jarque.bera.test(resid(AB_ZM_lm))
AB_ZM_shtest <- shapiro.test(resid(AB_ZM_lm))

# Exponential Law
AB_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Alba)
summary(AB_exp_lm)
AB_pred_exp_lm <- exp(predict(AB_exp_lm, newdata = RO_Census_Alba, 
                           interval = "prediction", level = 0.95))
AB_exp_out <- RO_Census_Alba %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AB_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Alba), 
         DFFIT = dffits(AB_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AB_exp_lm$coefficients) / nrow(RO_Census_Alba)), 
         Rezid_Std = rstudent(AB_exp_lm), Leverage = hatvalues(AB_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AB_exp_lm)), 
         DFBETA = dfbetas(AB_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Alba)))
AB_exp_whtest <- white(AB_exp_lm, interactions = TRUE)
AB_exp_bptest <- bptest(AB_exp_lm, studentize = TRUE)
AB_exp_DWtest <- dwtest(AB_exp_lm)
AB_exp_bgtest <- bgtest(AB_exp_lm, order = 1)
AB_exp_jbtest <- jarque.bera.test(resid(AB_exp_lm))
AB_exp_shtest <- shapiro.test(resid(AB_exp_lm))

# Lavalette Function
RO_Census_Alba <- RO_Census_Alba %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Alba) - Rank + 1))
)
AB_lav_lm <- lm(log(RO_Census_Alba$Population) ~ RO_Census_Alba$Lav_exp)
summary(AB_lav_lm)
AB_lav_kst <- exp(signif(AB_lav_lm$coef[[1]], 4))
AB_lav_chi <- signif(AB_lav_lm$coef[[2]], 4)
AB_pred_lav_lm <- exp(predict(AB_lav_lm, newdata = RO_Census_Alba, 
                              interval = "prediction", level = 0.95))
AB_lav_out <- RO_Census_Alba %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AB_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Alba), 
         DFFIT = dffits(AB_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AB_lav_lm$coefficients) / nrow(RO_Census_Alba)), 
         Rezid_Std = rstudent(AB_lav_lm), Leverage = hatvalues(AB_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AB_lav_lm)), 
         DFBETA = dfbetas(AB_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Alba)))
AB_lav_whtest <- white(AB_lav_lm, interactions = TRUE)
AB_lav_bptest <- bptest(AB_lav_lm, studentize = TRUE)
AB_lav_DWtest <- dwtest(AB_lav_lm)
AB_lav_bgtest <- bgtest(AB_lav_lm, order = 1)
AB_lav_jbtest <- jarque.bera.test(resid(AB_lav_lm))
AB_lav_shtest <- shapiro.test(resid(AB_lav_lm))

# Data Distributions
RO_Census_Alba %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Alba$Rank, 
                              Population = AB_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.20,
            label = paste0("y", "==", signif(exp(AB_pw_lm$coefficients[[1]]), 4), "%.%", 
                          "x^", signif(AB_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 57, y = 0.20,
            label = paste0("R^2 ==", signif(summary(AB_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Alba$Rank, 
                                Population = AB_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.18,
            label = paste0("y", "==", signif(exp(AB_exp_lm$coefficients[[1]]), 4), "%.%", 
                            signif(exp(AB_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 57, y = 0.18,
            label = paste0("R^2 ==", signif(summary(AB_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Alba$Rank, 
                              Population = AB_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.16,
            label = paste0("y", "==", signif(AB_ZM_cst, 4), "%.%", "(",
                           signif(AB_ZM_m, 4), "+ x)^", signif(AB_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 57, y = 0.16,
            label = paste0("R^2 ==", signif(summary(AB_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Alba$Rank, 
                              Population = AB_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.14,
            label = paste0("y", "==", signif(AB_lav_kst, 4), "%.%", "x^",
                           -signif(AB_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 57, y = 0.14,
            label = paste0("R^2 ==", signif(summary(AB_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Alba judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
(RO_Census_Alba %>%
  ggplot(aes(x = log(Rank), y = log(Population))) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
  labs(x = "Rank", y = "Population Frequency") +
  ggtitle("Power Linear Regression on Alba judet") +
  theme(plot.title = element_text(hjust = 0.5))
),
(RO_Census_Alba %>%
  ggplot(aes(x = Rank, y = log(Population))) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
  labs(x = "Rank", y = "Population Frequency") +
  ggtitle("Exponential Linear Regression on Alba judet") +
  theme(plot.title = element_text(hjust = 0.5))
),
(RO_Census_Alba %>%
  ggplot(aes(x = log(Rank + AB_ZM_m), y = log(Population))) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
  labs(x = "Rank", y = "Population Frequency") +
  ggtitle("Zipf-Mandelbrot Linear Regression on Alba judet") +
  theme(plot.title = element_text(hjust = 0.5))
),
(RO_Census_Alba %>%
   ggplot(aes(x = Lav_exp, y = log(Population))) +
   geom_point(alpha = 0.8, show.legend = FALSE) +
   geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
   labs(x = "Rank", y = "Population Frequency") +
   ggtitle("Lavalette Linear Regression on Alba judet") +
   theme(plot.title = element_text(hjust = 0.5))
),
ncol = 2, nrow = 2)

# 2. Arad
RO_Census_Arad <- RO_Census_rank %>% filter(Judet_Name == "ARAD")
#View(RO_Census_Arad)

# Power Law
AR_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Arad)
summary(AR_pw_lm)
AR_pred_pw_lm <- exp(predict(AR_pw_lm, newdata = RO_Census_Arad, 
                             interval = "prediction", level = 0.95))
AR_pw_out <- RO_Census_Arad %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AR_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Arad), 
         DFFIT = dffits(AR_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AR_pw_lm$coefficients) / nrow(RO_Census_Arad)), 
         Rezid_Std = rstudent(AR_pw_lm), Leverage = hatvalues(AR_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AR_pw_lm)), 
         DFBETA = dfbetas(AR_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Arad)))
AR_pw_whtest <- white(AR_pw_lm, interactions = TRUE)
AR_pw_bptest <- bptest(AR_pw_lm, studentize = TRUE)
AR_pw_DWtest <- dwtest(AR_pw_lm)
AR_pw_bgtest <- bgtest(AR_pw_lm, order = 1)
AR_pw_jbtest <- jarque.bera.test(resid(AR_pw_lm))
AR_pw_shtest <- shapiro.test(resid(AR_pw_lm))

# Zipf_Mandelbrot Law
AR_ZM_prm <- get_ZM_Param("RO_Census_Arad", "Population", "Rank")
AR_ZM_m <- AR_ZM_prm$m
AR_ZM_alpha <- AR_ZM_prm$alpha_ZM
AR_ZM_cst <- AR_ZM_prm$constant_ZM
AR_ZM_lm <- lm(log(RO_Census_Arad$Population) ~ log(RO_Census_Arad$Rank + AR_ZM_m))
summary(AR_ZM_lm)
AR_pred_ZM_lm <- exp(predict(AR_ZM_lm, newdata = RO_Census_Arad, 
                             interval = "prediction", level = 0.95))
AR_ZM_out <- RO_Census_Arad %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AR_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Arad), 
         DFFIT = dffits(AR_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AR_ZM_lm$coefficients) / nrow(RO_Census_Arad)), 
         Rezid_Std = rstudent(AR_ZM_lm), Leverage = hatvalues(AR_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AR_ZM_lm)), 
         DFBETA = dfbetas(AR_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Arad)))
AR_ZM_whtest <- white(AR_ZM_lm, interactions = TRUE)
AR_ZM_bptest <- bptest(AR_ZM_lm, studentize = TRUE)
AR_ZM_DWtest <- dwtest(AR_ZM_lm)
AR_ZM_bgtest <- bgtest(AR_ZM_lm, order = 1)
AR_ZM_jbtest <- jarque.bera.test(resid(AR_ZM_lm))
AR_ZM_shtest <- shapiro.test(resid(AR_ZM_lm))

# Exponential Law
AR_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Arad)
summary(AR_exp_lm)
AR_pred_exp_lm <- exp(predict(AR_exp_lm, newdata = RO_Census_Arad, 
                              interval = "prediction", level = 0.95))
AR_exp_out <- RO_Census_Arad %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AR_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Arad), 
         DFFIT = dffits(AR_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AR_exp_lm$coefficients) / nrow(RO_Census_Arad)), 
         Rezid_Std = rstudent(AR_exp_lm), Leverage = hatvalues(AR_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AR_exp_lm)), 
         DFBETA = dfbetas(AR_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Arad)))
AR_exp_whtest <- white(AR_exp_lm, interactions = TRUE)
AR_exp_bptest <- bptest(AR_exp_lm, studentize = TRUE)
AR_exp_DWtest <- dwtest(AR_exp_lm)
AR_exp_bgtest <- bgtest(AR_exp_lm, order = 1)
AR_exp_jbtest <- jarque.bera.test(resid(AR_exp_lm))
AR_exp_shtest <- shapiro.test(resid(AR_exp_lm))

# Lavalette Function
RO_Census_Arad <- RO_Census_Arad %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Arad) - Rank + 1))
)
AR_lav_lm <- lm(log(RO_Census_Arad$Population) ~ RO_Census_Arad$Lav_exp)
summary(AR_lav_lm)
AR_lav_kst <- exp(signif(AR_lav_lm$coef[[1]], 4))
AR_lav_chi <- signif(AR_lav_lm$coef[[2]], 4)
AR_pred_lav_lm <- exp(predict(AR_lav_lm, newdata = RO_Census_Arad, 
                              interval = "prediction", level = 0.95))
AR_lav_out <- RO_Census_Arad %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AR_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Arad), 
         DFFIT = dffits(AR_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AR_lav_lm$coefficients) / nrow(RO_Census_Arad)), 
         Rezid_Std = rstudent(AR_lav_lm), Leverage = hatvalues(AR_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AR_lav_lm)), 
         DFBETA = dfbetas(AR_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Arad)))
AR_lav_whtest <- white(AR_lav_lm, interactions = TRUE)
AR_lav_bptest <- bptest(AR_lav_lm, studentize = TRUE)
AR_lav_DWtest <- dwtest(AR_lav_lm)
AR_lav_bgtest <- bgtest(AR_lav_lm, order = 1)
AR_lav_jbtest <- jarque.bera.test(resid(AR_lav_lm))
AR_lav_shtest <- shapiro.test(resid(AR_lav_lm))

# Data Distributions
RO_Census_Arad %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Arad$Rank, 
                              Population = AR_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.3,
            label = paste0("y", "==", signif(exp(AR_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(AR_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 57, y = 0.3,
            label = paste0("R^2 ==", signif(summary(AR_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arad$Rank, 
                              Population = AR_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.28,
            label = paste0("y", "==", signif(exp(AR_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(AR_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 57, y = 0.28,
            label = paste0("R^2 ==", signif(summary(AR_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arad$Rank, 
                              Population = AR_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.26,
            label = paste0("y", "==", signif(AR_ZM_cst, 4), "%.%", "(",
                           signif(AR_ZM_m, 4), "+ x)^", signif(AR_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 57, y = 0.26,
            label = paste0("R^2 ==", signif(summary(AR_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arad$Rank, 
                              Population = AR_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.24,
            label = paste0("y", "==", signif(AR_lav_kst, 4), "%.%", "x^",
                           -signif(AR_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 57, y = 0.24,
            label = paste0("R^2 ==", signif(summary(AR_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Arad judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Arad %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Arad judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arad %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Arad judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arad %>%
     ggplot(aes(x = log(Rank + AR_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Arad judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arad %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Arad judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 3. Arges
RO_Census_Arges <- RO_Census_rank %>% filter(Judet_Name == "ARGES")
#View(RO_Census_Arges)

# Power Law
AG_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Arges)
summary(AG_pw_lm)
AG_pred_pw_lm <- exp(predict(AG_pw_lm, newdata = RO_Census_Arges, 
                             interval = "prediction", level = 0.95))
AG_pw_out <- RO_Census_Arges %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AG_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Arges), 
         DFFIT = dffits(AG_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AG_pw_lm$coefficients) / nrow(RO_Census_Arges)), 
         Rezid_Std = rstudent(AG_pw_lm), Leverage = hatvalues(AG_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AG_pw_lm)), 
         DFBETA = dfbetas(AG_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Arges)))
AG_pw_whtest <- white(AG_pw_lm, interactions = TRUE)
AG_pw_bptest <- bptest(AG_pw_lm, studentize = TRUE)
AG_pw_DWtest <- dwtest(AG_pw_lm)
AG_pw_bgtest <- bgtest(AG_pw_lm, order = 1)
AG_pw_jbtest <- jarque.bera.test(resid(AG_pw_lm))
AG_pw_shtest <- shapiro.test(resid(AG_pw_lm))

# Zipf_Mandelbrot Law
AG_ZM_prm <- get_ZM_Param("RO_Census_Arges", "Population", "Rank")
AG_ZM_m <- AG_ZM_prm$m
AG_ZM_alpha <- AG_ZM_prm$alpha_ZM
AG_ZM_cst <- AG_ZM_prm$constant_ZM
AG_ZM_lm <- lm(log(RO_Census_Arges$Population) ~ log(RO_Census_Arges$Rank + AG_ZM_m))
summary(AG_ZM_lm)
AG_pred_ZM_lm <- exp(predict(AG_ZM_lm, newdata = RO_Census_Arges, 
                             interval = "prediction", level = 0.95))
AG_ZM_out <- RO_Census_Arges %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AG_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Arges), 
         DFFIT = dffits(AG_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AG_ZM_lm$coefficients) / nrow(RO_Census_Arges)), 
         Rezid_Std = rstudent(AG_ZM_lm), Leverage = hatvalues(AG_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AG_ZM_lm)), 
         DFBETA = dfbetas(AG_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Arges)))
AG_ZM_whtest <- white(AG_ZM_lm, interactions = TRUE)
AG_ZM_bptest <- bptest(AG_ZM_lm, studentize = TRUE)
AG_ZM_DWtest <- dwtest(AG_ZM_lm)
AG_ZM_bgtest <- bgtest(AG_ZM_lm, order = 1)
AG_ZM_jbtest <- jarque.bera.test(resid(AG_ZM_lm))
AG_ZM_shtest <- shapiro.test(resid(AG_ZM_lm))

# Exponential Law
AG_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Arges)
summary(AG_exp_lm)
AG_pred_exp_lm <- exp(predict(AG_exp_lm, newdata = RO_Census_Arges, 
                              interval = "prediction", level = 0.95))
AG_exp_out <- RO_Census_Arges %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AG_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Arges), 
         DFFIT = dffits(AG_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AG_exp_lm$coefficients) / nrow(RO_Census_Arges)), 
         Rezid_Std = rstudent(AG_exp_lm), Leverage = hatvalues(AG_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AG_exp_lm)), 
         DFBETA = dfbetas(AG_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Arges)))
AG_exp_whtest <- white(AG_exp_lm, interactions = TRUE)
AG_exp_bptest <- bptest(AG_exp_lm, studentize = TRUE)
AG_exp_DWtest <- dwtest(AG_exp_lm)
AG_exp_bgtest <- bgtest(AG_exp_lm, order = 1)
AG_exp_jbtest <- jarque.bera.test(resid(AG_exp_lm))
AG_exp_shtest <- shapiro.test(resid(AG_exp_lm))

# Lavalette Function
RO_Census_Arges <- RO_Census_Arges %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Arges) - Rank + 1))
)
AG_lav_lm <- lm(log(RO_Census_Arges$Population) ~ RO_Census_Arges$Lav_exp)
summary(AG_lav_lm)
AG_lav_kst <- exp(signif(AG_lav_lm$coef[[1]], 4))
AG_lav_chi <- signif(AG_lav_lm$coef[[2]], 4)
AG_pred_lav_lm <- exp(predict(AG_lav_lm, newdata = RO_Census_Arges, 
                              interval = "prediction", level = 0.95))
AG_lav_out <- RO_Census_Arges %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(AG_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Arges), 
         DFFIT = dffits(AG_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(AG_lav_lm$coefficients) / nrow(RO_Census_Arges)), 
         Rezid_Std = rstudent(AG_lav_lm), Leverage = hatvalues(AG_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(AG_lav_lm)), 
         DFBETA = dfbetas(AG_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Arges)))
AG_lav_whtest <- white(AG_lav_lm, interactions = TRUE)
AG_lav_bptest <- bptest(AG_lav_lm, studentize = TRUE)
AG_lav_DWtest <- dwtest(AG_lav_lm)
AG_lav_bgtest <- bgtest(AG_lav_lm, order = 1)
AG_lav_jbtest <- jarque.bera.test(resid(AG_lav_lm))
AG_lav_shtest <- shapiro.test(resid(AG_lav_lm))

# Data Distributions
RO_Census_Arges %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Arges$Rank, 
                              Population = AG_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.25,
            label = paste0("y", "==", signif(exp(AG_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(AG_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.25,
            label = paste0("R^2 ==", signif(summary(AG_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arges$Rank, 
                              Population = AG_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.23,
            label = paste0("y", "==", signif(exp(AG_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(AG_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.23,
            label = paste0("R^2 ==", signif(summary(AG_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arges$Rank, 
                              Population = AG_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.21,
            label = paste0("y", "==", signif(AG_ZM_cst, 4), "%.%", "(",
                           signif(AG_ZM_m, 4), "+ x)^", signif(AG_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.21,
            label = paste0("R^2 ==", signif(summary(AG_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Arges$Rank, 
                              Population = AG_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.19,
            label = paste0("y", "==", signif(AG_lav_kst, 4), "%.%", "x^",
                           -signif(AG_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.19,
            label = paste0("R^2 ==", signif(summary(AG_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Arges judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Arges %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Arges judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arges %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Arges judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arges %>%
     ggplot(aes(x = log(Rank + AG_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Arges judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Arges %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Arges judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 4. Bacau
RO_Census_Bacau <- RO_Census_rank %>% filter(Judet_Name == "BACAU")
#View(RO_Census_Bacau)

# Power Law
BC_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Bacau)
summary(BC_pw_lm)
BC_pred_pw_lm <- exp(predict(BC_pw_lm, newdata = RO_Census_Bacau, 
                             interval = "prediction", level = 0.95))
BC_pw_out <- RO_Census_Bacau %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BC_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Bacau), 
         DFFIT = dffits(BC_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BC_pw_lm$coefficients) / nrow(RO_Census_Bacau)), 
         Rezid_Std = rstudent(BC_pw_lm), Leverage = hatvalues(BC_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BC_pw_lm)), 
         DFBETA = dfbetas(BC_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Bacau)))
BC_pw_whtest <- white(BC_pw_lm, interactions = TRUE)
BC_pw_bptest <- bptest(BC_pw_lm, studentize = TRUE)
BC_pw_DWtest <- dwtest(BC_pw_lm)
BC_pw_bgtest <- bgtest(BC_pw_lm, order = 1)
BC_pw_jbtest <- jarque.bera.test(resid(BC_pw_lm))
BC_pw_shtest <- shapiro.test(resid(BC_pw_lm))

# Zipf_Mandelbrot Law
BC_ZM_prm <- get_ZM_Param("RO_Census_Bacau", "Population", "Rank")
BC_ZM_m <- BC_ZM_prm$m
BC_ZM_alpha <- BC_ZM_prm$alpha_ZM
BC_ZM_cst <- BC_ZM_prm$constant_ZM
BC_ZM_lm <- lm(log(RO_Census_Bacau$Population) ~ log(RO_Census_Bacau$Rank + BC_ZM_m))
summary(BC_ZM_lm)
BC_pred_ZM_lm <- exp(predict(BC_ZM_lm, newdata = RO_Census_Bacau, 
                             interval = "prediction", level = 0.95))
BC_ZM_out <- RO_Census_Bacau %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BC_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Bacau), 
         DFFIT = dffits(BC_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BC_ZM_lm$coefficients) / nrow(RO_Census_Bacau)), 
         Rezid_Std = rstudent(BC_ZM_lm), Leverage = hatvalues(BC_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BC_ZM_lm)), 
         DFBETA = dfbetas(BC_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Bacau)))
BC_ZM_whtest <- white(BC_ZM_lm, interactions = TRUE)
BC_ZM_bptest <- bptest(BC_ZM_lm, studentize = TRUE)
BC_ZM_DWtest <- dwtest(BC_ZM_lm)
BC_ZM_bgtest <- bgtest(BC_ZM_lm, order = 1)
BC_ZM_jbtest <- jarque.bera.test(resid(BC_ZM_lm))
BC_ZM_shtest <- shapiro.test(resid(BC_ZM_lm))

# Exponential Law
BC_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Bacau)
summary(BC_exp_lm)
BC_pred_exp_lm <- exp(predict(BC_exp_lm, newdata = RO_Census_Bacau, 
                              interval = "prediction", level = 0.95))
BC_exp_out <- RO_Census_Bacau %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BC_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Bacau), 
         DFFIT = dffits(BC_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BC_exp_lm$coefficients) / nrow(RO_Census_Bacau)), 
         Rezid_Std = rstudent(BC_exp_lm), Leverage = hatvalues(BC_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BC_exp_lm)), 
         DFBETA = dfbetas(BC_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Bacau)))
BC_exp_whtest <- white(BC_exp_lm, interactions = TRUE)
BC_exp_bptest <- bptest(BC_exp_lm, studentize = TRUE)
BC_exp_DWtest <- dwtest(BC_exp_lm)
BC_exp_bgtest <- bgtest(BC_exp_lm, order = 1)
BC_exp_jbtest <- jarque.bera.test(resid(BC_exp_lm))
BC_exp_shtest <- shapiro.test(resid(BC_exp_lm))

# Lavalette Function
RO_Census_Bacau <- RO_Census_Bacau %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Bacau) - Rank + 1))
)
BC_lav_lm <- lm(log(RO_Census_Bacau$Population) ~ RO_Census_Bacau$Lav_exp)
summary(BC_lav_lm)
BC_lav_kst <- exp(signif(BC_lav_lm$coef[[1]], 4))
BC_lav_chi <- signif(BC_lav_lm$coef[[2]], 4)
BC_pred_lav_lm <- exp(predict(BC_lav_lm, newdata = RO_Census_Bacau, 
                              interval = "prediction", level = 0.95))
BC_lav_out <- RO_Census_Bacau %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BC_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Bacau), 
         DFFIT = dffits(BC_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BC_lav_lm$coefficients) / nrow(RO_Census_Bacau)), 
         Rezid_Std = rstudent(BC_lav_lm), Leverage = hatvalues(BC_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BC_lav_lm)), 
         DFBETA = dfbetas(BC_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Bacau)))
BC_lav_whtest <- white(BC_lav_lm, interactions = TRUE)
BC_lav_bptest <- bptest(BC_lav_lm, studentize = TRUE)
BC_lav_DWtest <- dwtest(BC_lav_lm)
BC_lav_bgtest <- bgtest(BC_lav_lm, order = 1)
BC_lav_jbtest <- jarque.bera.test(resid(BC_lav_lm))
BC_lav_shtest <- shapiro.test(resid(BC_lav_lm))

# Data Distributions
RO_Census_Bacau %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Bacau$Rank, 
                              Population = BC_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.22,
            label = paste0("y", "==", signif(exp(BC_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BC_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.22,
            label = paste0("R^2 ==", signif(summary(BC_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bacau$Rank, 
                              Population = BC_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.20,
            label = paste0("y", "==", signif(exp(BC_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BC_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.20,
            label = paste0("R^2 ==", signif(summary(BC_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bacau$Rank, 
                              Population = BC_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.18,
            label = paste0("y", "==", signif(BC_ZM_cst, 4), "%.%", "(",
                           signif(BC_ZM_m, 4), "+ x)^", signif(BC_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.18,
            label = paste0("R^2 ==", signif(summary(BC_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bacau$Rank, 
                              Population = BC_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.16,
            label = paste0("y", "==", signif(BC_lav_kst, 4), "%.%", "x^",
                           -signif(BC_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.16,
            label = paste0("R^2 ==", signif(summary(BC_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Bacau judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Bacau %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Bacau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bacau %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Bacau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bacau %>%
     ggplot(aes(x = log(Rank + BC_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Bacau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bacau %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Bacau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 5. Bihor
RO_Census_Bihor <- RO_Census_rank %>% filter(Judet_Name == "BIHOR")
#View(RO_Census_Bihor)

# Power Law
BH_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Bihor)
summary(BH_pw_lm)
BH_pred_pw_lm <- exp(predict(BH_pw_lm, newdata = RO_Census_Bihor, 
                             interval = "prediction", level = 0.95))
BH_pw_out <- RO_Census_Bihor %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BH_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Bihor), 
         DFFIT = dffits(BH_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BH_pw_lm$coefficients) / nrow(RO_Census_Bihor)), 
         Rezid_Std = rstudent(BH_pw_lm), Leverage = hatvalues(BH_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BH_pw_lm)), 
         DFBETA = dfbetas(BH_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Bihor)))
BH_pw_whtest <- white(BH_pw_lm, interactions = TRUE)
BH_pw_bptest <- bptest(BH_pw_lm, studentize = TRUE)
BH_pw_DWtest <- dwtest(BH_pw_lm)
BH_pw_bgtest <- bgtest(BH_pw_lm, order = 1)
BH_pw_jbtest <- jarque.bera.test(resid(BH_pw_lm))
BH_pw_shtest <- shapiro.test(resid(BH_pw_lm))

# Zipf_Mandelbrot Law
BH_ZM_prm <- get_ZM_Param("RO_Census_Bihor", "Population", "Rank")
BH_ZM_m <- BH_ZM_prm$m
BH_ZM_alpha <- BH_ZM_prm$alpha_ZM
BH_ZM_cst <- BH_ZM_prm$constant_ZM
BH_ZM_lm <- lm(log(RO_Census_Bihor$Population) ~ log(RO_Census_Bihor$Rank + BH_ZM_m))
summary(BH_ZM_lm)
BH_pred_ZM_lm <- exp(predict(BH_ZM_lm, newdata = RO_Census_Bihor, 
                             interval = "prediction", level = 0.95))
BH_ZM_out <- RO_Census_Bihor %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BH_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Bihor), 
         DFFIT = dffits(BH_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BH_ZM_lm$coefficients) / nrow(RO_Census_Bihor)), 
         Rezid_Std = rstudent(BH_ZM_lm), Leverage = hatvalues(BH_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BH_ZM_lm)), 
         DFBETA = dfbetas(BH_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Bihor)))
BH_ZM_whtest <- white(BH_ZM_lm, interactions = TRUE)
BH_ZM_bptest <- bptest(BH_ZM_lm, studentize = TRUE)
BH_ZM_DWtest <- dwtest(BH_ZM_lm)
BH_ZM_bgtest <- bgtest(BH_ZM_lm, order = 1)
BH_ZM_jbtest <- jarque.bera.test(resid(BH_ZM_lm))
BH_ZM_shtest <- shapiro.test(resid(BH_ZM_lm))

# Exponential Law
BH_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Bihor)
summary(BH_exp_lm)
BH_pred_exp_lm <- exp(predict(BH_exp_lm, newdata = RO_Census_Bihor, 
                              interval = "prediction", level = 0.95))
BH_exp_out <- RO_Census_Bihor %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BH_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Bihor), 
         DFFIT = dffits(BH_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BH_exp_lm$coefficients) / nrow(RO_Census_Bihor)), 
         Rezid_Std = rstudent(BH_exp_lm), Leverage = hatvalues(BH_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BH_exp_lm)), 
         DFBETA = dfbetas(BH_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Bihor)))
BH_exp_whtest <- white(BH_exp_lm, interactions = TRUE)
BH_exp_bptest <- bptest(BH_exp_lm, studentize = TRUE)
BH_exp_DWtest <- dwtest(BH_exp_lm)
BH_exp_bgtest <- bgtest(BH_exp_lm, order = 1)
BH_exp_jbtest <- jarque.bera.test(resid(BH_exp_lm))
BH_exp_shtest <- shapiro.test(resid(BH_exp_lm))

# Lavalette Function
RO_Census_Bihor <- RO_Census_Bihor %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Bihor) - Rank + 1))
)
BH_lav_lm <- lm(log(RO_Census_Bihor$Population) ~ RO_Census_Bihor$Lav_exp)
summary(BH_lav_lm)
BH_lav_kst <- exp(signif(BH_lav_lm$coef[[1]], 4))
BH_lav_chi <- signif(BH_lav_lm$coef[[2]], 4)
BH_pred_lav_lm <- exp(predict(BH_lav_lm, newdata = RO_Census_Bihor, 
                              interval = "prediction", level = 0.95))
BH_lav_out <- RO_Census_Bihor %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BH_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Bihor), 
         DFFIT = dffits(BH_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BH_lav_lm$coefficients) / nrow(RO_Census_Bihor)), 
         Rezid_Std = rstudent(BH_lav_lm), Leverage = hatvalues(BH_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BH_lav_lm)), 
         DFBETA = dfbetas(BH_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Bihor)))
BH_lav_whtest <- white(BH_lav_lm, interactions = TRUE)
BH_lav_bptest <- bptest(BH_lav_lm, studentize = TRUE)
BH_lav_DWtest <- dwtest(BH_lav_lm)
BH_lav_bgtest <- bgtest(BH_lav_lm, order = 1)
BH_lav_jbtest <- jarque.bera.test(resid(BH_lav_lm))
BH_lav_shtest <- shapiro.test(resid(BH_lav_lm))

# Data Distributions
RO_Census_Bihor %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Bihor$Rank, 
                              Population = BH_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.33,
            label = paste0("y", "==", signif(exp(BH_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BH_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.33,
            label = paste0("R^2 ==", signif(summary(BH_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bihor$Rank, 
                              Population = BH_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.31,
            label = paste0("y", "==", signif(exp(BH_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BH_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.31,
            label = paste0("R^2 ==", signif(summary(BH_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bihor$Rank, 
                              Population = BH_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.29,
            label = paste0("y", "==", signif(BH_ZM_cst, 4), "%.%", "(",
                           signif(BH_ZM_m, 4), "+ x)^", signif(BH_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.29,
            label = paste0("R^2 ==", signif(summary(BH_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Bihor$Rank, 
                              Population = BH_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.27,
            label = paste0("y", "==", signif(BH_lav_kst, 4), "%.%", "x^",
                           -signif(BH_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.27,
            label = paste0("R^2 ==", signif(summary(BH_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Bihor judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Bihor %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Bihor judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bihor %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Bihor judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bihor %>%
     ggplot(aes(x = log(Rank + BH_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Bihor judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Bihor %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Bihor judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 6. Bistrita-Nasaud
RO_Census_BistNsd <- RO_Census_rank %>% filter(Judet_Name == "BISTRITA-NASAUD")
#View(RO_Census_BistNsd)

# Power Law
BN_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_BistNsd)
summary(BN_pw_lm)
BN_pred_pw_lm <- exp(predict(BN_pw_lm, newdata = RO_Census_BistNsd, 
                             interval = "prediction", level = 0.95))
BN_pw_out <- RO_Census_BistNsd %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BN_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_BistNsd), 
         DFFIT = dffits(BN_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BN_pw_lm$coefficients) / nrow(RO_Census_BistNsd)), 
         Rezid_Std = rstudent(BN_pw_lm), Leverage = hatvalues(BN_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BN_pw_lm)), 
         DFBETA = dfbetas(BN_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_BistNsd)))
BN_pw_whtest <- white(BN_pw_lm, interactions = TRUE)
BN_pw_bptest <- bptest(BN_pw_lm, studentize = TRUE)
BN_pw_DWtest <- dwtest(BN_pw_lm)
BN_pw_bgtest <- bgtest(BN_pw_lm, order = 1)
BN_pw_jbtest <- jarque.bera.test(resid(BN_pw_lm))
BN_pw_shtest <- shapiro.test(resid(BN_pw_lm))

# Zipf_Mandelbrot Law
BN_ZM_prm <- get_ZM_Param("RO_Census_BistNsd", "Population", "Rank")
BN_ZM_m <- BN_ZM_prm$m
BN_ZM_alpha <- BN_ZM_prm$alpha_ZM
BN_ZM_cst <- BN_ZM_prm$constant_ZM
BN_ZM_lm <- lm(log(RO_Census_BistNsd$Population) ~ log(RO_Census_BistNsd$Rank + BN_ZM_m))
summary(BN_ZM_lm)
BN_pred_ZM_lm <- exp(predict(BN_ZM_lm, newdata = RO_Census_BistNsd, 
                             interval = "prediction", level = 0.95))
BN_ZM_out <- RO_Census_BistNsd %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BN_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_BistNsd), 
         DFFIT = dffits(BN_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BN_ZM_lm$coefficients) / nrow(RO_Census_BistNsd)), 
         Rezid_Std = rstudent(BN_ZM_lm), Leverage = hatvalues(BN_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BN_ZM_lm)), 
         DFBETA = dfbetas(BN_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_BistNsd)))
BN_ZM_whtest <- white(BN_ZM_lm, interactions = TRUE)
BN_ZM_bptest <- bptest(BN_ZM_lm, studentize = TRUE)
BN_ZM_DWtest <- dwtest(BN_ZM_lm)
BN_ZM_bgtest <- bgtest(BN_ZM_lm, order = 1)
BN_ZM_jbtest <- jarque.bera.test(resid(BN_ZM_lm))
BN_ZM_shtest <- shapiro.test(resid(BN_ZM_lm))

# Exponential Law
BN_exp_lm <- lm(log(Population) ~ Rank, RO_Census_BistNsd)
summary(BN_exp_lm)
BN_pred_exp_lm <- exp(predict(BN_exp_lm, newdata = RO_Census_BistNsd, 
                              interval = "prediction", level = 0.95))
BN_exp_out <- RO_Census_BistNsd %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BN_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_BistNsd), 
         DFFIT = dffits(BN_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BN_exp_lm$coefficients) / nrow(RO_Census_BistNsd)), 
         Rezid_Std = rstudent(BN_exp_lm), Leverage = hatvalues(BN_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BN_exp_lm)), 
         DFBETA = dfbetas(BN_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_BistNsd)))
BN_exp_whtest <- white(BN_exp_lm, interactions = TRUE)
BN_exp_bptest <- bptest(BN_exp_lm, studentize = TRUE)
BN_exp_DWtest <- dwtest(BN_exp_lm)
BN_exp_bgtest <- bgtest(BN_exp_lm, order = 1)
BN_exp_jbtest <- jarque.bera.test(resid(BN_exp_lm))
BN_exp_shtest <- shapiro.test(resid(BN_exp_lm))

# Lavalette Function
RO_Census_BistNsd <- RO_Census_BistNsd %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_BistNsd) - Rank + 1))
)
BN_lav_lm <- lm(log(RO_Census_BistNsd$Population) ~ RO_Census_BistNsd$Lav_exp)
summary(BN_lav_lm)
BN_lav_kst <- exp(signif(BN_lav_lm$coef[[1]], 4))
BN_lav_chi <- signif(BN_lav_lm$coef[[2]], 4)
BN_pred_lav_lm <- exp(predict(BN_lav_lm, newdata = RO_Census_BistNsd, 
                              interval = "prediction", level = 0.95))
BN_lav_out <- RO_Census_BistNsd %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BN_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_BistNsd), 
         DFFIT = dffits(BN_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BN_lav_lm$coefficients) / nrow(RO_Census_BistNsd)), 
         Rezid_Std = rstudent(BN_lav_lm), Leverage = hatvalues(BN_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BN_lav_lm)), 
         DFBETA = dfbetas(BN_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_BistNsd)))
BN_lav_whtest <- white(BN_lav_lm, interactions = TRUE)
BN_lav_bptest <- bptest(BN_lav_lm, studentize = TRUE)
BN_lav_DWtest <- dwtest(BN_lav_lm)
BN_lav_bgtest <- bgtest(BN_lav_lm, order = 1)
BN_lav_jbtest <- jarque.bera.test(resid(BN_lav_lm))
BN_lav_shtest <- shapiro.test(resid(BN_lav_lm))

# Data Distributions
RO_Census_BistNsd %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_BistNsd$Rank, 
                              Population = BN_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 30, y = 0.26,
            label = paste0("y", "==", signif(exp(BN_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BN_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 50, y = 0.26,
            label = paste0("R^2 ==", signif(summary(BN_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_BistNsd$Rank, 
                              Population = BN_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 30, y = 0.24,
            label = paste0("y", "==", signif(exp(BN_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BN_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 50, y = 0.24,
            label = paste0("R^2 ==", signif(summary(BN_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_BistNsd$Rank, 
                              Population = BN_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 30, y = 0.22,
            label = paste0("y", "==", signif(BN_ZM_cst, 4), "%.%", "(",
                           signif(BN_ZM_m, 4), "+ x)^", signif(BN_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 50, y = 0.22,
            label = paste0("R^2 ==", signif(summary(BN_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_BistNsd$Rank, 
                              Population = BN_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 30, y = 0.20,
            label = paste0("y", "==", signif(BN_lav_kst, 4), "%.%", "x^",
                           -signif(BN_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 50, y = 0.20,
            label = paste0("R^2 ==", signif(summary(BN_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Bistrita-Nasaud judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_BistNsd %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Bistrita-Nasaud judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_BistNsd %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Bistrita-Nasaud judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_BistNsd %>%
     ggplot(aes(x = log(Rank + BN_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Bistrita-Nasaud judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_BistNsd %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Bistrita-Nasaud judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 7. Botosani
RO_Census_Botosani <- RO_Census_rank %>% filter(Judet_Name == "BOTOSANI")
#View(RO_Census_Botosani)

# Power Law
BT_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Botosani)
summary(BT_pw_lm)
BT_pred_pw_lm <- exp(predict(BT_pw_lm, newdata = RO_Census_Botosani, 
                             interval = "prediction", level = 0.95))
BT_pw_out <- RO_Census_Botosani %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BT_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Botosani), 
         DFFIT = dffits(BT_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BT_pw_lm$coefficients) / nrow(RO_Census_Botosani)), 
         Rezid_Std = rstudent(BT_pw_lm), Leverage = hatvalues(BT_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BT_pw_lm)), 
         DFBETA = dfbetas(BT_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Botosani)))
BT_pw_whtest <- white(BT_pw_lm, interactions = TRUE)
BT_pw_bptest <- bptest(BT_pw_lm, studentize = TRUE)
BT_pw_DWtest <- dwtest(BT_pw_lm)
BT_pw_bgtest <- bgtest(BT_pw_lm, order = 1)
BT_pw_jbtest <- jarque.bera.test(resid(BT_pw_lm))
BT_pw_shtest <- shapiro.test(resid(BT_pw_lm))

# Zipf_Mandelbrot Law
BT_ZM_prm <- get_ZM_Param("RO_Census_Botosani", "Population", "Rank")
BT_ZM_m <- BT_ZM_prm$m
BT_ZM_alpha <- BT_ZM_prm$alpha_ZM
BT_ZM_cst <- BT_ZM_prm$constant_ZM
BT_ZM_lm <- lm(log(RO_Census_Botosani$Population) ~ log(RO_Census_Botosani$Rank + BT_ZM_m))
summary(BT_ZM_lm)
BT_pred_ZM_lm <- exp(predict(BT_ZM_lm, newdata = RO_Census_Botosani, 
                             interval = "prediction", level = 0.95))
BT_ZM_out <- RO_Census_Botosani %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BT_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Botosani), 
         DFFIT = dffits(BT_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BT_ZM_lm$coefficients) / nrow(RO_Census_Botosani)), 
         Rezid_Std = rstudent(BT_ZM_lm), Leverage = hatvalues(BT_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BT_ZM_lm)), 
         DFBETA = dfbetas(BT_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Botosani)))
BT_ZM_whtest <- white(BT_ZM_lm, interactions = TRUE)
BT_ZM_bptest <- bptest(BT_ZM_lm, studentize = TRUE)
BT_ZM_DWtest <- dwtest(BT_ZM_lm)
BT_ZM_bgtest <- bgtest(BT_ZM_lm, order = 1)
BT_ZM_jbtest <- jarque.bera.test(resid(BT_ZM_lm))
BT_ZM_shtest <- shapiro.test(resid(BT_ZM_lm))

# Exponential Law
BT_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Botosani)
summary(BT_exp_lm)
BT_pred_exp_lm <- exp(predict(BT_exp_lm, newdata = RO_Census_Botosani, 
                              interval = "prediction", level = 0.95))
BT_exp_out <- RO_Census_Botosani %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BT_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Botosani), 
         DFFIT = dffits(BT_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BT_exp_lm$coefficients) / nrow(RO_Census_Botosani)), 
         Rezid_Std = rstudent(BT_exp_lm), Leverage = hatvalues(BT_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BT_exp_lm)), 
         DFBETA = dfbetas(BT_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Botosani)))
BT_exp_whtest <- white(BT_exp_lm, interactions = TRUE)
BT_exp_bptest <- bptest(BT_exp_lm, studentize = TRUE)
BT_exp_DWtest <- dwtest(BT_exp_lm)
BT_exp_bgtest <- bgtest(BT_exp_lm, order = 1)
BT_exp_jbtest <- jarque.bera.test(resid(BT_exp_lm))
BT_exp_shtest <- shapiro.test(resid(BT_exp_lm))

# Lavalette Function
RO_Census_Botosani <- RO_Census_Botosani %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Botosani) - Rank + 1))
)
BT_lav_lm <- lm(log(RO_Census_Botosani$Population) ~ RO_Census_Botosani$Lav_exp)
summary(BT_lav_lm)
BT_lav_kst <- exp(signif(BT_lav_lm$coef[[1]], 4))
BT_lav_chi <- signif(BT_lav_lm$coef[[2]], 4)
BT_pred_lav_lm <- exp(predict(BT_lav_lm, newdata = RO_Census_Botosani, 
                              interval = "prediction", level = 0.95))
BT_lav_out <- RO_Census_Botosani %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BT_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Botosani), 
         DFFIT = dffits(BT_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BT_lav_lm$coefficients) / nrow(RO_Census_Botosani)), 
         Rezid_Std = rstudent(BT_lav_lm), Leverage = hatvalues(BT_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BT_lav_lm)), 
         DFBETA = dfbetas(BT_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Botosani)))
BT_lav_whtest <- white(BT_lav_lm, interactions = TRUE)
BT_lav_bptest <- bptest(BT_lav_lm, studentize = TRUE)
BT_lav_DWtest <- dwtest(BT_lav_lm)
BT_lav_bgtest <- bgtest(BT_lav_lm, order = 1)
BT_lav_jbtest <- jarque.bera.test(resid(BT_lav_lm))
BT_lav_shtest <- shapiro.test(resid(BT_lav_lm))

# Data Distributions
RO_Census_Botosani %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Botosani$Rank, 
                              Population = BT_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.22,
            label = paste0("y", "==", signif(exp(BT_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BT_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.22,
            label = paste0("R^2 ==", signif(summary(BT_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Botosani$Rank, 
                              Population = BT_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.20,
            label = paste0("y", "==", signif(exp(BT_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BT_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.20,
            label = paste0("R^2 ==", signif(summary(BT_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Botosani$Rank, 
                              Population = BT_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.18,
            label = paste0("y", "==", signif(BT_ZM_cst, 4), "%.%", "(",
                           signif(BT_ZM_m, 4), "+ x)^", signif(BT_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.18,
            label = paste0("R^2 ==", signif(summary(BT_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Botosani$Rank, 
                              Population = BT_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.16,
            label = paste0("y", "==", signif(BT_lav_kst, 4), "%.%", "x^",
                           -signif(BT_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.16,
            label = paste0("R^2 ==", signif(summary(BT_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Botosani judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Botosani %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Botosani judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Botosani %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Botosani judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Botosani %>%
     ggplot(aes(x = log(Rank + BT_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Botosani judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Botosani %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Botosani judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 8. Braila
RO_Census_Braila <- RO_Census_rank %>% filter(Judet_Name == "BRAILA")
#View(RO_Census_Braila)

# Power Law
BR_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Braila)
summary(BR_pw_lm)
BR_pred_pw_lm <- exp(predict(BR_pw_lm, newdata = RO_Census_Braila, 
                             interval = "prediction", level = 0.95))
BR_pw_out <- RO_Census_Braila %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BR_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Braila), 
         DFFIT = dffits(BR_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BR_pw_lm$coefficients) / nrow(RO_Census_Braila)), 
         Rezid_Std = rstudent(BR_pw_lm), Leverage = hatvalues(BR_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BR_pw_lm)), 
         DFBETA = dfbetas(BR_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Braila)))
BR_pw_whtest <- white(BR_pw_lm, interactions = TRUE)
BR_pw_bptest <- bptest(BR_pw_lm, studentize = TRUE)
BR_pw_DWtest <- dwtest(BR_pw_lm)
BR_pw_bgtest <- bgtest(BR_pw_lm, order = 1)
BR_pw_jbtest <- jarque.bera.test(resid(BR_pw_lm))
BR_pw_shtest <- shapiro.test(resid(BR_pw_lm))

# Zipf_Mandelbrot Law
BR_ZM_prm <- get_ZM_Param("RO_Census_Braila", "Population", "Rank")
BR_ZM_m <- BR_ZM_prm$m
BR_ZM_alpha <- BR_ZM_prm$alpha_ZM
BR_ZM_cst <- BR_ZM_prm$constant_ZM
BR_ZM_lm <- lm(log(RO_Census_Braila$Population) ~ log(RO_Census_Braila$Rank + BR_ZM_m))
summary(BR_ZM_lm)
BR_pred_ZM_lm <- exp(predict(BR_ZM_lm, newdata = RO_Census_Braila, 
                             interval = "prediction", level = 0.95))
BR_ZM_out <- RO_Census_Braila %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BR_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Braila), 
         DFFIT = dffits(BR_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BR_ZM_lm$coefficients) / nrow(RO_Census_Braila)), 
         Rezid_Std = rstudent(BR_ZM_lm), Leverage = hatvalues(BR_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BR_ZM_lm)), 
         DFBETA = dfbetas(BR_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Braila)))
BR_ZM_whtest <- white(BR_ZM_lm, interactions = TRUE)
BR_ZM_bptest <- bptest(BR_ZM_lm, studentize = TRUE)
BR_ZM_DWtest <- dwtest(BR_ZM_lm)
BR_ZM_bgtest <- bgtest(BR_ZM_lm, order = 1)
BR_ZM_jbtest <- jarque.bera.test(resid(BR_ZM_lm))
BR_ZM_shtest <- shapiro.test(resid(BR_ZM_lm))

# Exponential Law
BR_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Braila)
summary(BR_exp_lm)
BR_pred_exp_lm <- exp(predict(BR_exp_lm, newdata = RO_Census_Braila, 
                              interval = "prediction", level = 0.95))
BR_exp_out <- RO_Census_Braila %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BR_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Braila), 
         DFFIT = dffits(BR_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BR_exp_lm$coefficients) / nrow(RO_Census_Braila)), 
         Rezid_Std = rstudent(BR_exp_lm), Leverage = hatvalues(BR_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BR_exp_lm)), 
         DFBETA = dfbetas(BR_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Braila)))
BR_exp_whtest <- white(BR_exp_lm, interactions = TRUE)
BR_exp_bptest <- bptest(BR_exp_lm, studentize = TRUE)
BR_exp_DWtest <- dwtest(BR_exp_lm)
BR_exp_bgtest <- bgtest(BR_exp_lm, order = 1)
BR_exp_jbtest <- jarque.bera.test(resid(BR_exp_lm))
BR_exp_shtest <- shapiro.test(resid(BR_exp_lm))

# Lavalette Function
RO_Census_Braila <- RO_Census_Braila %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Braila) - Rank + 1))
)
BR_lav_lm <- lm(log(RO_Census_Braila$Population) ~ RO_Census_Braila$Lav_exp)
summary(BR_lav_lm)
BR_lav_kst <- exp(signif(BR_lav_lm$coef[[1]], 4))
BR_lav_chi <- signif(BR_lav_lm$coef[[2]], 4)
BR_pred_lav_lm <- exp(predict(BR_lav_lm, newdata = RO_Census_Braila, 
                              interval = "prediction", level = 0.95))
BR_lav_out <- RO_Census_Braila %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BR_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Braila), 
         DFFIT = dffits(BR_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BR_lav_lm$coefficients) / nrow(RO_Census_Braila)), 
         Rezid_Std = rstudent(BR_lav_lm), Leverage = hatvalues(BR_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BR_lav_lm)), 
         DFBETA = dfbetas(BR_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Braila)))
BR_lav_whtest <- white(BR_lav_lm, interactions = TRUE)
BR_lav_bptest <- bptest(BR_lav_lm, studentize = TRUE)
BR_lav_DWtest <- dwtest(BR_lav_lm)
BR_lav_bgtest <- bgtest(BR_lav_lm, order = 1)
BR_lav_jbtest <- jarque.bera.test(resid(BR_lav_lm))
BR_lav_shtest <- shapiro.test(resid(BR_lav_lm))

# Data Distributions
RO_Census_Braila %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila$Rank, 
                              Population = BR_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 100000,
            label = paste0("y", "==", signif(exp(BR_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BR_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 30, y = 100000,
            label = paste0("R^2 ==", signif(summary(BR_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila$Rank, 
                              Population = BR_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 80000,
            label = paste0("y", "==", signif(exp(BR_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BR_exp_lm$coefficients[[2]]), 4),"^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 30, y = 80000,
            label = paste0("R^2 ==", signif(summary(BR_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila$Rank, 
                              Population = BR_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 60000,
            label = paste0("y", "==", signif(BR_ZM_cst, 4), "%.%", "(",
                           signif(BR_ZM_m, 4), "+ x)^", signif(BR_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 30, y = 60000,
            label = paste0("R^2 ==", signif(summary(BR_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Braila$Rank, 
                              Population = BR_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 40000,
            label = paste0("y", "==", signif(BR_lav_kst, 4), "%.%", "x^",
                           -signif(BR_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 30, y = 40000,
            label = paste0("R^2 ==", signif(summary(BR_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_y_continuous(labels = scales::label_number(
    big.mark = ",", decimal.mark = ".")) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population", color = "Legend") +
  #ggtitle("Data Distribution on Braila judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Braila %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Braila judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Braila %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Braila judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Braila %>%
     ggplot(aes(x = log(Rank + BR_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Braila judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Braila %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Braila judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 9. Brasov
RO_Census_Brasov <- RO_Census_rank %>% filter(Judet_Name == "BRASOV")
#View(RO_Census_Brasov)

# Power Law
BV_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Brasov)
summary(BV_pw_lm)
BV_pred_pw_lm <- exp(predict(BV_pw_lm, newdata = RO_Census_Brasov, 
                             interval = "prediction", level = 0.95))
BV_pw_out <- RO_Census_Brasov %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BV_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Brasov), 
         DFFIT = dffits(BV_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BV_pw_lm$coefficients) / nrow(RO_Census_Brasov)), 
         Rezid_Std = rstudent(BV_pw_lm), Leverage = hatvalues(BV_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BV_pw_lm)), 
         DFBETA = dfbetas(BV_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Brasov)))
BV_pw_whtest <- white(BV_pw_lm, interactions = TRUE)
BV_pw_bptest <- bptest(BV_pw_lm, studentize = TRUE)
BV_pw_DWtest <- dwtest(BV_pw_lm)
BV_pw_bgtest <- bgtest(BV_pw_lm, order = 1)
BV_pw_jbtest <- jarque.bera.test(resid(BV_pw_lm))
BV_pw_shtest <- shapiro.test(resid(BV_pw_lm))

# Zipf_Mandelbrot Law
BV_ZM_prm <- get_ZM_Param("RO_Census_Brasov", "Population", "Rank")
BV_ZM_m <- BV_ZM_prm$m
BV_ZM_alpha <- BV_ZM_prm$alpha_ZM
BV_ZM_cst <- BV_ZM_prm$constant_ZM
BV_ZM_lm <- lm(log(RO_Census_Brasov$Population) ~ log(RO_Census_Brasov$Rank + BV_ZM_m))
summary(BV_ZM_lm)
BV_pred_ZM_lm <- exp(predict(BV_ZM_lm, newdata = RO_Census_Brasov, 
                             interval = "prediction", level = 0.95))
BV_ZM_out <- RO_Census_Brasov %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BV_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Brasov), 
         DFFIT = dffits(BV_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BV_ZM_lm$coefficients) / nrow(RO_Census_Brasov)), 
         Rezid_Std = rstudent(BV_ZM_lm), Leverage = hatvalues(BV_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BV_ZM_lm)), 
         DFBETA = dfbetas(BV_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Brasov)))
BV_ZM_whtest <- white(BV_ZM_lm, interactions = TRUE)
BV_ZM_bptest <- bptest(BV_ZM_lm, studentize = TRUE)
BV_ZM_DWtest <- dwtest(BV_ZM_lm)
BV_ZM_bgtest <- bgtest(BV_ZM_lm, order = 1)
BV_ZM_jbtest <- jarque.bera.test(resid(BV_ZM_lm))
BV_ZM_shtest <- shapiro.test(resid(BV_ZM_lm))

# Exponential Law
BV_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Brasov)
summary(BV_exp_lm)
BV_pred_exp_lm <- exp(predict(BV_exp_lm, newdata = RO_Census_Brasov, 
                              interval = "prediction", level = 0.95))
BV_exp_out <- RO_Census_Brasov %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BV_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Brasov), 
         DFFIT = dffits(BV_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BV_exp_lm$coefficients) / nrow(RO_Census_Brasov)), 
         Rezid_Std = rstudent(BV_exp_lm), Leverage = hatvalues(BV_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BV_exp_lm)), 
         DFBETA = dfbetas(BV_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Brasov)))
BV_exp_whtest <- white(BV_exp_lm, interactions = TRUE)
BV_exp_bptest <- bptest(BV_exp_lm, studentize = TRUE)
BV_exp_DWtest <- dwtest(BV_exp_lm)
BV_exp_bgtest <- bgtest(BV_exp_lm, order = 1)
BV_exp_jbtest <- jarque.bera.test(resid(BV_exp_lm))
BV_exp_shtest <- shapiro.test(resid(BV_exp_lm))

# Lavalette Function
RO_Census_Brasov <- RO_Census_Brasov %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Brasov) - Rank + 1))
)
BV_lav_lm <- lm(log(RO_Census_Brasov$Population) ~ RO_Census_Brasov$Lav_exp)
summary(BV_lav_lm)
BV_lav_kst <- exp(signif(BV_lav_lm$coef[[1]], 4))
BV_lav_chi <- signif(BV_lav_lm$coef[[2]], 4)
BV_pred_lav_lm <- exp(predict(BV_lav_lm, newdata = RO_Census_Brasov, 
                              interval = "prediction", level = 0.95))
BV_lav_out <- RO_Census_Brasov %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BV_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Brasov), 
         DFFIT = dffits(BV_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BV_lav_lm$coefficients) / nrow(RO_Census_Brasov)), 
         Rezid_Std = rstudent(BV_lav_lm), Leverage = hatvalues(BV_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BV_lav_lm)), 
         DFBETA = dfbetas(BV_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Brasov)))
BV_lav_whtest <- white(BV_lav_lm, interactions = TRUE)
BV_lav_bptest <- bptest(BV_lav_lm, studentize = TRUE)
BV_lav_DWtest <- dwtest(BV_lav_lm)
BV_lav_bgtest <- bgtest(BV_lav_lm, order = 1)
BV_lav_jbtest <- jarque.bera.test(resid(BV_lav_lm))
BV_lav_shtest <- shapiro.test(resid(BV_lav_lm))

# Data Distributions
RO_Census_Brasov %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Brasov$Rank, 
                              Population = BV_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.4,
            label = paste0("y", "==", signif(exp(BV_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BV_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 35, y = 0.4,
            label = paste0("R^2 ==", signif(summary(BV_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Brasov$Rank, 
                              Population = BV_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.35,
            label = paste0("y", "==", signif(exp(BV_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BV_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 35, y = 0.35,
            label = paste0("R^2 ==", signif(summary(BV_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Brasov$Rank, 
                              Population = BV_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.30,
            label = paste0("y", "==", signif(BV_ZM_cst, 4), "%.%", "(",
                           signif(BV_ZM_m, 4), "+ x)^", signif(BV_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 35, y = 0.30,
            label = paste0("R^2 ==", signif(summary(BV_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Brasov$Rank, 
                              Population = BV_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.25,
            label = paste0("y", "==", signif(BV_lav_kst, 4), "%.%", "x^",
                           -signif(BV_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 35, y = 0.25,
            label = paste0("R^2 ==", signif(summary(BV_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Brasov judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Brasov %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Brasov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Brasov %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Brasov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Brasov %>%
     ggplot(aes(x = log(Rank + BV_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Brasov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Brasov %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Brasov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 10. Buzau
RO_Census_Buzau <- RO_Census_rank %>% filter(Judet_Name == "BUZAU")
#View(RO_Census_Buzau)

# Power Law
BZ_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Buzau)
summary(BZ_pw_lm)
BZ_pred_pw_lm <- exp(predict(BZ_pw_lm, newdata = RO_Census_Buzau, 
                             interval = "prediction", level = 0.95))
BZ_pw_out <- RO_Census_Buzau %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BZ_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Buzau), 
         DFFIT = dffits(BZ_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BZ_pw_lm$coefficients) / nrow(RO_Census_Buzau)), 
         Rezid_Std = rstudent(BZ_pw_lm), Leverage = hatvalues(BZ_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BZ_pw_lm)), 
         DFBETA = dfbetas(BZ_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Buzau)))
BZ_pw_whtest <- white(BZ_pw_lm, interactions = TRUE)
BZ_pw_bptest <- bptest(BZ_pw_lm, studentize = TRUE)
BZ_pw_DWtest <- dwtest(BZ_pw_lm)
BZ_pw_bgtest <- bgtest(BZ_pw_lm, order = 1)
BZ_pw_jbtest <- jarque.bera.test(resid(BZ_pw_lm))
BZ_pw_shtest <- shapiro.test(resid(BZ_pw_lm))

# Zipf_Mandelbrot Law
BZ_ZM_prm <- get_ZM_Param("RO_Census_Buzau", "Population", "Rank")
BZ_ZM_m <- BZ_ZM_prm$m
BZ_ZM_alpha <- BZ_ZM_prm$alpha_ZM
BZ_ZM_cst <- BZ_ZM_prm$constant_ZM
BZ_ZM_lm <- lm(log(RO_Census_Buzau$Population) ~ log(RO_Census_Buzau$Rank + BZ_ZM_m))
summary(BZ_ZM_lm)
BZ_pred_ZM_lm <- exp(predict(BZ_ZM_lm, newdata = RO_Census_Buzau, 
                             interval = "prediction", level = 0.95))
BZ_ZM_out <- RO_Census_Buzau %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BZ_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Buzau), 
         DFFIT = dffits(BZ_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BZ_ZM_lm$coefficients) / nrow(RO_Census_Buzau)), 
         Rezid_Std = rstudent(BZ_ZM_lm), Leverage = hatvalues(BZ_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BZ_ZM_lm)), 
         DFBETA = dfbetas(BZ_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Buzau)))
BZ_ZM_whtest <- white(BZ_ZM_lm, interactions = TRUE)
BZ_ZM_bptest <- bptest(BZ_ZM_lm, studentize = TRUE)
BZ_ZM_DWtest <- dwtest(BZ_ZM_lm)
BZ_ZM_bgtest <- bgtest(BZ_ZM_lm, order = 1)
BZ_ZM_jbtest <- jarque.bera.test(resid(BZ_ZM_lm))
BZ_ZM_shtest <- shapiro.test(resid(BZ_ZM_lm))

# Exponential Law
BZ_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Buzau)
summary(BZ_exp_lm)
BZ_pred_exp_lm <- exp(predict(BZ_exp_lm, newdata = RO_Census_Buzau, 
                              interval = "prediction", level = 0.95))
BZ_exp_out <- RO_Census_Buzau %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BZ_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Buzau), 
         DFFIT = dffits(BZ_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BZ_exp_lm$coefficients) / nrow(RO_Census_Buzau)), 
         Rezid_Std = rstudent(BZ_exp_lm), Leverage = hatvalues(BZ_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BZ_exp_lm)), 
         DFBETA = dfbetas(BZ_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Buzau)))
BZ_exp_whtest <- white(BZ_exp_lm, interactions = TRUE)
BZ_exp_bptest <- bptest(BZ_exp_lm, studentize = TRUE)
BZ_exp_DWtest <- dwtest(BZ_exp_lm)
BZ_exp_bgtest <- bgtest(BZ_exp_lm, order = 1)
BZ_exp_jbtest <- jarque.bera.test(resid(BZ_exp_lm))
BZ_exp_shtest <- shapiro.test(resid(BZ_exp_lm))

# Lavalette Function
RO_Census_Buzau <- RO_Census_Buzau %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Buzau) - Rank + 1))
)
BZ_lav_lm <- lm(log(RO_Census_Buzau$Population) ~ RO_Census_Buzau$Lav_exp)
summary(BZ_lav_lm)
BZ_lav_kst <- exp(signif(BZ_lav_lm$coef[[1]], 4))
BZ_lav_chi <- signif(BZ_lav_lm$coef[[2]], 4)
BZ_pred_lav_lm <- exp(predict(BZ_lav_lm, newdata = RO_Census_Buzau, 
                              interval = "prediction", level = 0.95))
BZ_lav_out <- RO_Census_Buzau %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(BZ_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Buzau), 
         DFFIT = dffits(BZ_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(BZ_lav_lm$coefficients) / nrow(RO_Census_Buzau)), 
         Rezid_Std = rstudent(BZ_lav_lm), Leverage = hatvalues(BZ_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(BZ_lav_lm)), 
         DFBETA = dfbetas(BZ_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Buzau)))
BZ_lav_whtest <- white(BZ_lav_lm, interactions = TRUE)
BZ_lav_bptest <- bptest(BZ_lav_lm, studentize = TRUE)
BZ_lav_DWtest <- dwtest(BZ_lav_lm)
BZ_lav_bgtest <- bgtest(BZ_lav_lm, order = 1)
BZ_lav_jbtest <- jarque.bera.test(resid(BZ_lav_lm))
BZ_lav_shtest <- shapiro.test(resid(BZ_lav_lm))

# Data Distributions
RO_Census_Buzau %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Buzau$Rank, 
                              Population = BZ_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.25,
            label = paste0("y", "==", signif(exp(BZ_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(BZ_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.25,
            label = paste0("R^2 ==", signif(summary(BZ_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Buzau$Rank, 
                              Population = BZ_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.22,
            label = paste0("y", "==", signif(exp(BZ_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(BZ_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.22,
            label = paste0("R^2 ==", signif(summary(BZ_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Buzau$Rank, 
                              Population = BZ_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.19,
            label = paste0("y", "==", signif(BZ_ZM_cst, 4), "%.%", "(",
                           signif(BZ_ZM_m, 4), "+ x)^", signif(BZ_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.19,
            label = paste0("R^2 ==", signif(summary(BZ_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Buzau$Rank, 
                              Population = BZ_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.16,
            label = paste0("y", "==", signif(BZ_lav_kst, 4), "%.%", "x^",
                           -signif(BZ_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.16,
            label = paste0("R^2 ==", signif(summary(BZ_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Buzau judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Buzau %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Buzau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Buzau %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Buzau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Buzau %>%
     ggplot(aes(x = log(Rank + BZ_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Buzau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Buzau %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Buzau judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 11. Caras-Severin
RO_Census_CarSev <- RO_Census_rank %>% filter(Judet_Name == "CARAS-SEVERIN")
#View(RO_Census_CarSev)

# Power Law
CS_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_CarSev)
summary(CS_pw_lm)
CS_pred_pw_lm <- exp(predict(CS_pw_lm, newdata = RO_Census_CarSev, 
                             interval = "prediction", level = 0.95))
CS_pw_out <- RO_Census_CarSev %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CS_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_CarSev), 
         DFFIT = dffits(CS_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CS_pw_lm$coefficients) / nrow(RO_Census_CarSev)), 
         Rezid_Std = rstudent(CS_pw_lm), Leverage = hatvalues(CS_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CS_pw_lm)), 
         DFBETA = dfbetas(CS_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_CarSev)))
CS_pw_whtest <- white(CS_pw_lm, interactions = TRUE)
CS_pw_bptest <- bptest(CS_pw_lm, studentize = TRUE)
CS_pw_DWtest <- dwtest(CS_pw_lm)
CS_pw_bgtest <- bgtest(CS_pw_lm, order = 1)
CS_pw_jbtest <- jarque.bera.test(resid(CS_pw_lm))
CS_pw_shtest <- shapiro.test(resid(CS_pw_lm))

# Zipf_Mandelbrot Law
CS_ZM_prm <- get_ZM_Param("RO_Census_CarSev", "Population", "Rank")
CS_ZM_m <- CS_ZM_prm$m
CS_ZM_alpha <- CS_ZM_prm$alpha_ZM
CS_ZM_cst <- CS_ZM_prm$constant_ZM
CS_ZM_lm <- lm(log(RO_Census_CarSev$Population) ~ log(RO_Census_CarSev$Rank + CS_ZM_m))
summary(CS_ZM_lm)
CS_pred_ZM_lm <- exp(predict(CS_ZM_lm, newdata = RO_Census_CarSev, 
                             interval = "prediction", level = 0.95))
CS_ZM_out <- RO_Census_CarSev %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CS_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_CarSev), 
         DFFIT = dffits(CS_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CS_ZM_lm$coefficients) / nrow(RO_Census_CarSev)), 
         Rezid_Std = rstudent(CS_ZM_lm), Leverage = hatvalues(CS_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CS_ZM_lm)), 
         DFBETA = dfbetas(CS_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_CarSev)))
CS_ZM_whtest <- white(CS_ZM_lm, interactions = TRUE)
CS_ZM_bptest <- bptest(CS_ZM_lm, studentize = TRUE)
CS_ZM_DWtest <- dwtest(CS_ZM_lm)
CS_ZM_bgtest <- bgtest(CS_ZM_lm, order = 1)
CS_ZM_jbtest <- jarque.bera.test(resid(CS_ZM_lm))
CS_ZM_shtest <- shapiro.test(resid(CS_ZM_lm))

# Exponential Law
CS_exp_lm <- lm(log(Population) ~ Rank, RO_Census_CarSev)
summary(CS_exp_lm)
CS_pred_exp_lm <- exp(predict(CS_exp_lm, newdata = RO_Census_CarSev, 
                              interval = "prediction", level = 0.95))
CS_exp_out <- RO_Census_CarSev %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CS_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_CarSev), 
         DFFIT = dffits(CS_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CS_exp_lm$coefficients) / nrow(RO_Census_CarSev)), 
         Rezid_Std = rstudent(CS_exp_lm), Leverage = hatvalues(CS_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CS_exp_lm)), 
         DFBETA = dfbetas(CS_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_CarSev)))
CS_exp_whtest <- white(CS_exp_lm, interactions = TRUE)
CS_exp_bptest <- bptest(CS_exp_lm, studentize = TRUE)
CS_exp_DWtest <- dwtest(CS_exp_lm)
CS_exp_bgtest <- bgtest(CS_exp_lm, order = 1)
CS_exp_jbtest <- jarque.bera.test(resid(CS_exp_lm))
CS_exp_shtest <- shapiro.test(resid(CS_exp_lm))

# Lavalette Function
RO_Census_CarSev <- RO_Census_CarSev %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_CarSev) - Rank + 1))
)
CS_lav_lm <- lm(log(RO_Census_CarSev$Population) ~ RO_Census_CarSev$Lav_exp)
summary(CS_lav_lm)
CS_lav_kst <- exp(signif(CS_lav_lm$coef[[1]], 4))
CS_lav_chi <- signif(CS_lav_lm$coef[[2]], 4)
CS_pred_lav_lm <- exp(predict(CS_lav_lm, newdata = RO_Census_CarSev, 
                              interval = "prediction", level = 0.95))
CS_lav_out <- RO_Census_CarSev %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CS_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_CarSev), 
         DFFIT = dffits(CS_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CS_lav_lm$coefficients) / nrow(RO_Census_CarSev)), 
         Rezid_Std = rstudent(CS_lav_lm), Leverage = hatvalues(CS_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CS_lav_lm)), 
         DFBETA = dfbetas(CS_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_CarSev)))
CS_lav_whtest <- white(CS_lav_lm, interactions = TRUE)
CS_lav_bptest <- bptest(CS_lav_lm, studentize = TRUE)
CS_lav_DWtest <- dwtest(CS_lav_lm)
CS_lav_bgtest <- bgtest(CS_lav_lm, order = 1)
CS_lav_jbtest <- jarque.bera.test(resid(CS_lav_lm))
CS_lav_shtest <- shapiro.test(resid(CS_lav_lm))

# Data Distributions
RO_Census_CarSev %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_CarSev$Rank, 
                              Population = CS_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.23,
            label = paste0("y", "==", signif(exp(CS_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(CS_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.23,
            label = paste0("R^2 ==", signif(summary(CS_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_CarSev$Rank, 
                              Population = CS_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.20,
            label = paste0("y", "==", signif(exp(CS_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(CS_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.20,
            label = paste0("R^2 ==", signif(summary(CS_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_CarSev$Rank, 
                              Population = CS_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.17,
            label = paste0("y", "==", signif(CS_ZM_cst, 4), "%.%", "(",
                           signif(CS_ZM_m, 4), "+ x)^", signif(CS_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.17,
            label = paste0("R^2 ==", signif(summary(CS_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_CarSev$Rank, 
                              Population = CS_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.14,
            label = paste0("y", "==", signif(CS_lav_kst, 4), "%.%", "x^",
                           -signif(CS_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.14,
            label = paste0("R^2 ==", signif(summary(CS_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Caras-Severin judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_CarSev %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Caras-Severin judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_CarSev %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Caras-Severin judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_CarSev %>%
     ggplot(aes(x = log(Rank + CS_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Caras-Severin judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_CarSev %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Caras-Severin judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 12. Calarasi
RO_Census_Calarasi <- RO_Census_rank %>% filter(Judet_Name == "CALARASI")
#View(RO_Census_Calarasi)

# Power Law
CL_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Calarasi)
summary(CL_pw_lm)
CL_pred_pw_lm <- exp(predict(CL_pw_lm, newdata = RO_Census_Calarasi, 
                             interval = "prediction", level = 0.95))
CL_pw_out <- RO_Census_Calarasi %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CL_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Calarasi), 
         DFFIT = dffits(CL_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CL_pw_lm$coefficients) / nrow(RO_Census_Calarasi)), 
         Rezid_Std = rstudent(CL_pw_lm), Leverage = hatvalues(CL_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CL_pw_lm)), 
         DFBETA = dfbetas(CL_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Calarasi)))
CL_pw_whtest <- white(CL_pw_lm, interactions = TRUE)
CL_pw_bptest <- bptest(CL_pw_lm, studentize = TRUE)
CL_pw_DWtest <- dwtest(CL_pw_lm)
CL_pw_bgtest <- bgtest(CL_pw_lm, order = 1)
CL_pw_jbtest <- jarque.bera.test(resid(CL_pw_lm))
CL_pw_shtest <- shapiro.test(resid(CL_pw_lm))

# Zipf_Mandelbrot Law
CL_ZM_prm <- get_ZM_Param("RO_Census_Calarasi", "Population", "Rank")
CL_ZM_m <- CL_ZM_prm$m
CL_ZM_alpha <- CL_ZM_prm$alpha_ZM
CL_ZM_cst <- CL_ZM_prm$constant_ZM
CL_ZM_lm <- lm(log(RO_Census_Calarasi$Population) ~ log(RO_Census_Calarasi$Rank + CL_ZM_m))
summary(CL_ZM_lm)
CL_pred_ZM_lm <- exp(predict(CL_ZM_lm, newdata = RO_Census_Calarasi, 
                             interval = "prediction", level = 0.95))
CL_ZM_out <- RO_Census_Calarasi %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CL_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Calarasi), 
         DFFIT = dffits(CL_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CL_ZM_lm$coefficients) / nrow(RO_Census_Calarasi)), 
         Rezid_Std = rstudent(CL_ZM_lm), Leverage = hatvalues(CL_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CL_ZM_lm)), 
         DFBETA = dfbetas(CL_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Calarasi)))
CL_ZM_whtest <- white(CL_ZM_lm, interactions = TRUE)
CL_ZM_bptest <- bptest(CL_ZM_lm, studentize = TRUE)
CL_ZM_DWtest <- dwtest(CL_ZM_lm)
CL_ZM_bgtest <- bgtest(CL_ZM_lm, order = 1)
CL_ZM_jbtest <- jarque.bera.test(resid(CL_ZM_lm))
CL_ZM_shtest <- shapiro.test(resid(CL_ZM_lm))

# Exponential Law
CL_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Calarasi)
summary(CL_exp_lm)
CL_pred_exp_lm <- exp(predict(CL_exp_lm, newdata = RO_Census_Calarasi, 
                              interval = "prediction", level = 0.95))
CL_exp_out <- RO_Census_Calarasi %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CL_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Calarasi), 
         DFFIT = dffits(CL_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CL_exp_lm$coefficients) / nrow(RO_Census_Calarasi)), 
         Rezid_Std = rstudent(CL_exp_lm), Leverage = hatvalues(CL_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CL_exp_lm)), 
         DFBETA = dfbetas(CL_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Calarasi)))
CL_exp_whtest <- white(CL_exp_lm, interactions = TRUE)
CL_exp_bptest <- bptest(CL_exp_lm, studentize = TRUE)
CL_exp_DWtest <- dwtest(CL_exp_lm)
CL_exp_bgtest <- bgtest(CL_exp_lm, order = 1)
CL_exp_jbtest <- jarque.bera.test(resid(CL_exp_lm))
CL_exp_shtest <- shapiro.test(resid(CL_exp_lm))

# Lavalette Function
RO_Census_Calarasi <- RO_Census_Calarasi %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Calarasi) - Rank + 1))
)
CL_lav_lm <- lm(log(RO_Census_Calarasi$Population) ~ RO_Census_Calarasi$Lav_exp)
summary(CL_lav_lm)
CL_lav_kst <- exp(signif(CL_lav_lm$coef[[1]], 4))
CL_lav_chi <- signif(CL_lav_lm$coef[[2]], 4)
CL_pred_lav_lm <- exp(predict(CL_lav_lm, newdata = RO_Census_Calarasi, 
                              interval = "prediction", level = 0.95))
CL_lav_out <- RO_Census_Calarasi %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CL_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Calarasi), 
         DFFIT = dffits(CL_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CL_lav_lm$coefficients) / nrow(RO_Census_Calarasi)), 
         Rezid_Std = rstudent(CL_lav_lm), Leverage = hatvalues(CL_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CL_lav_lm)), 
         DFBETA = dfbetas(CL_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Calarasi)))
CL_lav_whtest <- white(CL_lav_lm, interactions = TRUE)
CL_lav_bptest <- bptest(CL_lav_lm, studentize = TRUE)
CL_lav_DWtest <- dwtest(CL_lav_lm)
CL_lav_bgtest <- bgtest(CL_lav_lm, order = 1)
CL_lav_jbtest <- jarque.bera.test(resid(CL_lav_lm))
CL_lav_shtest <- shapiro.test(resid(CL_lav_lm))

# Data Distributions
RO_Census_Calarasi %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Calarasi$Rank, 
                              Population = CL_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.21,
            label = paste0("y", "==", signif(exp(CL_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(CL_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 35, y = 0.21,
            label = paste0("R^2 ==", signif(summary(CL_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Calarasi$Rank, 
                              Population = CL_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.19,
            label = paste0("y", "==", signif(exp(CL_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(CL_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 35, y = 0.19,
            label = paste0("R^2 ==", signif(summary(CL_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Calarasi$Rank, 
                              Population = CL_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.17,
            label = paste0("y", "==", signif(CL_ZM_cst, 4), "%.%", "(",
                           signif(CL_ZM_m, 4), "+ x)^", signif(CL_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 35, y = 0.17,
            label = paste0("R^2 ==", signif(summary(CL_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Calarasi$Rank, 
                              Population = CL_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.15,
            label = paste0("y", "==", signif(CL_lav_kst, 4), "%.%", "x^",
                           -signif(CL_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 35, y = 0.15,
            label = paste0("R^2 ==", signif(summary(CL_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Calarasi judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Calarasi %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Calarasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Calarasi %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Calarasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Calarasi %>%
     ggplot(aes(x = log(Rank + CL_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Calarasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Calarasi %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Calarasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 13. Cluj
RO_Census_Cluj <- RO_Census_rank %>% filter(Judet_Name == "CLUJ")
#View(RO_Census_Cluj)

# Power Law
CJ_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Cluj)
summary(CJ_pw_lm)
CJ_pred_pw_lm <- exp(predict(CJ_pw_lm, newdata = RO_Census_Cluj, 
                             interval = "prediction", level = 0.95))
CJ_pw_out <- RO_Census_Cluj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CJ_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Cluj), 
         DFFIT = dffits(CJ_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CJ_pw_lm$coefficients) / nrow(RO_Census_Cluj)), 
         Rezid_Std = rstudent(CJ_pw_lm), Leverage = hatvalues(CJ_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CJ_pw_lm)), 
         DFBETA = dfbetas(CJ_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Cluj)))
CJ_pw_whtest <- white(CJ_pw_lm, interactions = TRUE)
CJ_pw_bptest <- bptest(CJ_pw_lm, studentize = TRUE)
CJ_pw_DWtest <- dwtest(CJ_pw_lm)
CJ_pw_bgtest <- bgtest(CJ_pw_lm, order = 1)
CJ_pw_jbtest <- jarque.bera.test(resid(CJ_pw_lm))
CJ_pw_shtest <- shapiro.test(resid(CJ_pw_lm))

# Zipf_Mandelbrot Law
CJ_ZM_prm <- get_ZM_Param("RO_Census_Cluj", "Population", "Rank")
CJ_ZM_m <- CJ_ZM_prm$m
CJ_ZM_alpha <- CJ_ZM_prm$alpha_ZM
CJ_ZM_cst <- CJ_ZM_prm$constant_ZM
CJ_ZM_lm <- lm(log(RO_Census_Cluj$Population) ~ log(RO_Census_Cluj$Rank + CJ_ZM_m))
summary(CJ_ZM_lm)
CJ_pred_ZM_lm <- exp(predict(CJ_ZM_lm, newdata = RO_Census_Cluj, 
                             interval = "prediction", level = 0.95))
CJ_ZM_out <- RO_Census_Cluj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CJ_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Cluj), 
         DFFIT = dffits(CJ_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CJ_ZM_lm$coefficients) / nrow(RO_Census_Cluj)), 
         Rezid_Std = rstudent(CJ_ZM_lm), Leverage = hatvalues(CJ_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CJ_ZM_lm)), 
         DFBETA = dfbetas(CJ_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Cluj)))
CJ_ZM_whtest <- white(CJ_ZM_lm, interactions = TRUE)
CJ_ZM_bptest <- bptest(CJ_ZM_lm, studentize = TRUE)
CJ_ZM_DWtest <- dwtest(CJ_ZM_lm)
CJ_ZM_bgtest <- bgtest(CJ_ZM_lm, order = 1)
CJ_ZM_jbtest <- jarque.bera.test(resid(CJ_ZM_lm))
CJ_ZM_shtest <- shapiro.test(resid(CJ_ZM_lm))

# Exponential Law
CJ_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Cluj)
summary(CJ_exp_lm)
CJ_pred_exp_lm <- exp(predict(CJ_exp_lm, newdata = RO_Census_Cluj, 
                              interval = "prediction", level = 0.95))
CJ_exp_out <- RO_Census_Cluj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CJ_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Cluj), 
         DFFIT = dffits(CJ_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CJ_exp_lm$coefficients) / nrow(RO_Census_Cluj)), 
         Rezid_Std = rstudent(CJ_exp_lm), Leverage = hatvalues(CJ_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CJ_exp_lm)), 
         DFBETA = dfbetas(CJ_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Cluj)))
CJ_exp_whtest <- white(CJ_exp_lm, interactions = TRUE)
CJ_exp_bptest <- bptest(CJ_exp_lm, studentize = TRUE)
CJ_exp_DWtest <- dwtest(CJ_exp_lm)
CJ_exp_bgtest <- bgtest(CJ_exp_lm, order = 1)
CJ_exp_jbtest <- jarque.bera.test(resid(CJ_exp_lm))
CJ_exp_shtest <- shapiro.test(resid(CJ_exp_lm))

# Lavalette Function
RO_Census_Cluj <- RO_Census_Cluj %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Cluj) - Rank + 1))
)
CJ_lav_lm <- lm(log(RO_Census_Cluj$Population) ~ RO_Census_Cluj$Lav_exp)
summary(CJ_lav_lm)
CJ_lav_kst <- exp(signif(CJ_lav_lm$coef[[1]], 4))
CJ_lav_chi <- signif(CJ_lav_lm$coef[[2]], 4)
CJ_pred_lav_lm <- exp(predict(CJ_lav_lm, newdata = RO_Census_Cluj, 
                              interval = "prediction", level = 0.95))
CJ_lav_out <- RO_Census_Cluj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CJ_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Cluj), 
         DFFIT = dffits(CJ_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CJ_lav_lm$coefficients) / nrow(RO_Census_Cluj)), 
         Rezid_Std = rstudent(CJ_lav_lm), Leverage = hatvalues(CJ_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CJ_lav_lm)), 
         DFBETA = dfbetas(CJ_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Cluj)))
CJ_lav_whtest <- white(CJ_lav_lm, interactions = TRUE)
CJ_lav_bptest <- bptest(CJ_lav_lm, studentize = TRUE)
CJ_lav_DWtest <- dwtest(CJ_lav_lm)
CJ_lav_bgtest <- bgtest(CJ_lav_lm, order = 1)
CJ_lav_jbtest <- jarque.bera.test(resid(CJ_lav_lm))
CJ_lav_shtest <- shapiro.test(resid(CJ_lav_lm))

# Data Distributions
RO_Census_Cluj %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj$Rank, 
                              Population = CJ_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 240000,
            label = paste0("y", "==", signif(exp(CJ_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(CJ_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 240000,
            label = paste0("R^2 ==", signif(summary(CJ_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj$Rank, 
                              Population = CJ_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 200000,
            label = paste0("y", "==", signif(exp(CJ_exp_lm$coefficients[[1]]), 4), "%.%", 
                            signif(exp(CJ_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 200000,
            label = paste0("R^2 ==", signif(summary(CJ_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj$Rank, 
                              Population = CJ_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 160000,
            label = paste0("y", "==", signif(CJ_ZM_cst, 4), "%.%", "(",
                           signif(CJ_ZM_m, 4), "+ x)^", signif(CJ_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 160000,
            label = paste0("R^2 ==", signif(summary(CJ_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Cluj$Rank, 
                              Population = CJ_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 120000,
            label = paste0("y", "==", signif(CJ_lav_kst, 4), "%.%", "x^",
                           -signif(CJ_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 120000,
            label = paste0("R^2 ==", signif(summary(CJ_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_y_continuous(labels = scales::label_number(
    big.mark = ",", decimal.mark = ".")) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population", color = "Legend") +
  #ggtitle("Data Distribution on Cluj judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Cluj %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Cluj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Cluj %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Cluj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Cluj %>%
     ggplot(aes(x = log(Rank + CJ_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Cluj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Cluj %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Cluj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 14. Constanta
RO_Census_Constanta <- RO_Census_rank %>% filter(Judet_Name == "CONSTANTA")
#View(RO_Census_Constanta)

# Power Law
CT_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Constanta)
summary(CT_pw_lm)
CT_pred_pw_lm <- exp(predict(CT_pw_lm, newdata = RO_Census_Constanta, 
                             interval = "prediction", level = 0.95))
CT_pw_out <- RO_Census_Constanta %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CT_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Constanta), 
         DFFIT = dffits(CT_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CT_pw_lm$coefficients) / nrow(RO_Census_Constanta)), 
         Rezid_Std = rstudent(CT_pw_lm), Leverage = hatvalues(CT_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CT_pw_lm)), 
         DFBETA = dfbetas(CT_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Constanta)))
CT_pw_whtest <- white(CT_pw_lm, interactions = TRUE)
CT_pw_bptest <- bptest(CT_pw_lm, studentize = TRUE)
CT_pw_DWtest <- dwtest(CT_pw_lm)
CT_pw_bgtest <- bgtest(CT_pw_lm, order = 1)
CT_pw_jbtest <- jarque.bera.test(resid(CT_pw_lm))
CT_pw_shtest <- shapiro.test(resid(CT_pw_lm))

# Zipf_Mandelbrot Law
CT_ZM_prm <- get_ZM_Param("RO_Census_Constanta", "Population", "Rank")
CT_ZM_m <- CT_ZM_prm$m
CT_ZM_alpha <- CT_ZM_prm$alpha_ZM
CT_ZM_cst <- CT_ZM_prm$constant_ZM
CT_ZM_lm <- lm(log(RO_Census_Constanta$Population) ~ log(RO_Census_Constanta$Rank + CT_ZM_m))
summary(CT_ZM_lm)
CT_pred_ZM_lm <- exp(predict(CT_ZM_lm, newdata = RO_Census_Constanta, 
                             interval = "prediction", level = 0.95))
CT_ZM_out <- RO_Census_Constanta %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CT_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Constanta), 
         DFFIT = dffits(CT_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CT_ZM_lm$coefficients) / nrow(RO_Census_Constanta)), 
         Rezid_Std = rstudent(CT_ZM_lm), Leverage = hatvalues(CT_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CT_ZM_lm)), 
         DFBETA = dfbetas(CT_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Constanta)))
CT_ZM_whtest <- white(CT_ZM_lm, interactions = TRUE)
CT_ZM_bptest <- bptest(CT_ZM_lm, studentize = TRUE)
CT_ZM_DWtest <- dwtest(CT_ZM_lm)
CT_ZM_bgtest <- bgtest(CT_ZM_lm, order = 1)
CT_ZM_jbtest <- jarque.bera.test(resid(CT_ZM_lm))
CT_ZM_shtest <- shapiro.test(resid(CT_ZM_lm))

# Exponential Law
CT_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Constanta)
summary(CT_exp_lm)
CT_pred_exp_lm <- exp(predict(CT_exp_lm, newdata = RO_Census_Constanta, 
                              interval = "prediction", level = 0.95))
CT_exp_out <- RO_Census_Constanta %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CT_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Constanta), 
         DFFIT = dffits(CT_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CT_exp_lm$coefficients) / nrow(RO_Census_Constanta)), 
         Rezid_Std = rstudent(CT_exp_lm), Leverage = hatvalues(CT_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CT_exp_lm)), 
         DFBETA = dfbetas(CT_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Constanta)))
CT_exp_whtest <- white(CT_exp_lm, interactions = TRUE)
CT_exp_bptest <- bptest(CT_exp_lm, studentize = TRUE)
CT_exp_DWtest <- dwtest(CT_exp_lm)
CT_exp_bgtest <- bgtest(CT_exp_lm, order = 1)
CT_exp_jbtest <- jarque.bera.test(resid(CT_exp_lm))
CT_exp_shtest <- shapiro.test(resid(CT_exp_lm))

# Lavalette Function
RO_Census_Constanta <- RO_Census_Constanta %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Constanta) - Rank + 1))
)
CT_lav_lm <- lm(log(RO_Census_Constanta$Population) ~ RO_Census_Constanta$Lav_exp)
summary(CT_lav_lm)
CT_lav_kst <- exp(signif(CT_lav_lm$coef[[1]], 4))
CT_lav_chi <- signif(CT_lav_lm$coef[[2]], 4)
CT_pred_lav_lm <- exp(predict(CT_lav_lm, newdata = RO_Census_Constanta, 
                              interval = "prediction", level = 0.95))
CT_lav_out <- RO_Census_Constanta %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CT_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Constanta), 
         DFFIT = dffits(CT_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CT_lav_lm$coefficients) / nrow(RO_Census_Constanta)), 
         Rezid_Std = rstudent(CT_lav_lm), Leverage = hatvalues(CT_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CT_lav_lm)), 
         DFBETA = dfbetas(CT_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Constanta)))
CT_lav_whtest <- white(CT_lav_lm, interactions = TRUE)
CT_lav_bptest <- bptest(CT_lav_lm, studentize = TRUE)
CT_lav_DWtest <- dwtest(CT_lav_lm)
CT_lav_bgtest <- bgtest(CT_lav_lm, order = 1)
CT_lav_jbtest <- jarque.bera.test(resid(CT_lav_lm))
CT_lav_shtest <- shapiro.test(resid(CT_lav_lm))

# Data Distributions
RO_Census_Constanta %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Constanta$Rank, 
                              Population = CT_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.40,
            label = paste0("y", "==", signif(exp(CT_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(CT_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.40,
            label = paste0("R^2 ==", signif(summary(CT_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Constanta$Rank, 
                              Population = CT_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.37,
            label = paste0("y", "==", signif(exp(CT_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(CT_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.37,
            label = paste0("R^2 ==", signif(summary(CT_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Constanta$Rank, 
                              Population = CT_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.34,
            label = paste0("y", "==", signif(CT_ZM_cst, 4), "%.%", "(",
                           signif(CT_ZM_m, 4), "+ x)^", signif(CT_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.34,
            label = paste0("R^2 ==", signif(summary(CT_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Constanta$Rank, 
                              Population = CT_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.31,
            label = paste0("y", "==", signif(CT_lav_kst, 4), "%.%", "x^",
                           -signif(CT_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.31,
            label = paste0("R^2 ==", signif(summary(CT_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Constanta judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Constanta %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Constanta judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Constanta %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Constanta judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Constanta %>%
     ggplot(aes(x = log(Rank + CT_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Constanta judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Constanta %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Constanta judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 15. Covasna
RO_Census_Covasna <- RO_Census_rank %>% filter(Judet_Name == "COVASNA")
#View(RO_Census_Covasna)

# Power Law
CV_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Covasna)
summary(CT_pw_lm)
CV_pred_pw_lm <- exp(predict(CV_pw_lm, newdata = RO_Census_Covasna, 
                             interval = "prediction", level = 0.95))
CV_pw_out <- RO_Census_Covasna %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CV_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Covasna), 
         DFFIT = dffits(CV_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CV_pw_lm$coefficients) / nrow(RO_Census_Covasna)), 
         Rezid_Std = rstudent(CV_pw_lm), Leverage = hatvalues(CV_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CV_pw_lm)), 
         DFBETA = dfbetas(CV_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Covasna)))
CV_pw_whtest <- white(CV_pw_lm, interactions = TRUE)
CV_pw_bptest <- bptest(CV_pw_lm, studentize = TRUE)
CV_pw_DWtest <- dwtest(CV_pw_lm)
CV_pw_bgtest <- bgtest(CV_pw_lm, order = 1)
CV_pw_jbtest <- jarque.bera.test(resid(CV_pw_lm))
CV_pw_shtest <- shapiro.test(resid(CV_pw_lm))

# Zipf_Mandelbrot Law
CV_ZM_prm <- get_ZM_Param("RO_Census_Covasna", "Population", "Rank")
CV_ZM_m <- CV_ZM_prm$m
CV_ZM_alpha <- CV_ZM_prm$alpha_ZM
CV_ZM_cst <- CV_ZM_prm$constant_ZM
CV_ZM_lm <- lm(log(RO_Census_Covasna$Population) ~ log(RO_Census_Covasna$Rank + CV_ZM_m))
summary(CV_ZM_lm)
CV_pred_ZM_lm <- exp(predict(CV_ZM_lm, newdata = RO_Census_Covasna, 
                             interval = "prediction", level = 0.95))
CV_ZM_out <- RO_Census_Covasna %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CV_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Covasna), 
         DFFIT = dffits(CV_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CV_ZM_lm$coefficients) / nrow(RO_Census_Covasna)), 
         Rezid_Std = rstudent(CV_ZM_lm), Leverage = hatvalues(CV_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CV_ZM_lm)), 
         DFBETA = dfbetas(CV_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Covasna)))
CV_ZM_whtest <- white(CV_ZM_lm, interactions = TRUE)
CV_ZM_bptest <- bptest(CV_ZM_lm, studentize = TRUE)
CV_ZM_DWtest <- dwtest(CV_ZM_lm)
CV_ZM_bgtest <- bgtest(CV_ZM_lm, order = 1)
CV_ZM_jbtest <- jarque.bera.test(resid(CV_ZM_lm))
CV_ZM_shtest <- shapiro.test(resid(CV_ZM_lm))

# Exponential Law
CV_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Covasna)
summary(CV_exp_lm)
CV_pred_exp_lm <- exp(predict(CV_exp_lm, newdata = RO_Census_Covasna, 
                              interval = "prediction", level = 0.95))
CV_exp_out <- RO_Census_Covasna %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CV_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Covasna), 
         DFFIT = dffits(CV_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CV_exp_lm$coefficients) / nrow(RO_Census_Covasna)), 
         Rezid_Std = rstudent(CV_exp_lm), Leverage = hatvalues(CV_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CV_exp_lm)), 
         DFBETA = dfbetas(CV_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Covasna)))
CV_exp_whtest <- white(CV_exp_lm, interactions = TRUE)
CV_exp_bptest <- bptest(CV_exp_lm, studentize = TRUE)
CV_exp_DWtest <- dwtest(CV_exp_lm)
CV_exp_bgtest <- bgtest(CV_exp_lm, order = 1)
CV_exp_jbtest <- jarque.bera.test(resid(CV_exp_lm))
CV_exp_shtest <- shapiro.test(resid(CV_exp_lm))

# Lavalette Function
RO_Census_Covasna <- RO_Census_Covasna %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Covasna) - Rank + 1))
)
CV_lav_lm <- lm(log(RO_Census_Covasna$Population) ~ RO_Census_Covasna$Lav_exp)
summary(CV_lav_lm)
CV_lav_kst <- exp(signif(CV_lav_lm$coef[[1]], 4))
CV_lav_chi <- signif(CV_lav_lm$coef[[2]], 4)
CV_pred_lav_lm <- exp(predict(CV_lav_lm, newdata = RO_Census_Covasna, 
                              interval = "prediction", level = 0.95))
CV_lav_out <- RO_Census_Covasna %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(CV_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Covasna), 
         DFFIT = dffits(CV_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(CV_lav_lm$coefficients) / nrow(RO_Census_Covasna)), 
         Rezid_Std = rstudent(CV_lav_lm), Leverage = hatvalues(CV_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(CV_lav_lm)), 
         DFBETA = dfbetas(CV_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Covasna)))
CV_lav_whtest <- white(CV_lav_lm, interactions = TRUE)
CV_lav_bptest <- bptest(CV_lav_lm, studentize = TRUE)
CV_lav_DWtest <- dwtest(CV_lav_lm)
CV_lav_bgtest <- bgtest(CV_lav_lm, order = 1)
CV_lav_jbtest <- jarque.bera.test(resid(CV_lav_lm))
CV_lav_shtest <- shapiro.test(resid(CV_lav_lm))

# Data Distributions
RO_Census_Covasna %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Covasna$Rank, 
                              Population = CV_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.25,
            label = paste0("y", "==", signif(exp(CV_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(CV_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 30, y = 0.25,
            label = paste0("R^2 ==", signif(summary(CV_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Covasna$Rank, 
                              Population = CV_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.23,
            label = paste0("y", "==", signif(exp(CV_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(CV_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 30, y = 0.23,
            label = paste0("R^2 ==", signif(summary(CV_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Covasna$Rank, 
                              Population = CV_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.21,
            label = paste0("y", "==", signif(CV_ZM_cst, 4), "%.%", "(",
                           signif(CV_ZM_m, 4), "+ x)^", signif(CV_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 30, y = 0.21,
            label = paste0("R^2 ==", signif(summary(CV_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Covasna$Rank, 
                              Population = CV_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.19,
            label = paste0("y", "==", signif(CV_lav_kst, 4), "%.%", "x^",
                           -signif(CV_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 30, y = 0.19,
            label = paste0("R^2 ==", signif(summary(CV_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Covasna judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Covasna %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Covasna judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Covasna %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Covasna judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Covasna %>%
     ggplot(aes(x = log(Rank + CV_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Covasna judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Covasna %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Covasna judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 16. Dambovita
RO_Census_Dambovita <- RO_Census_rank %>% filter(Judet_Name == "DAMBOVITA")
#View(RO_Census_Dambovita)

# Power Law
DB_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Dambovita)
summary(DB_pw_lm)
DB_pred_pw_lm <- exp(predict(DB_pw_lm, newdata = RO_Census_Dambovita, 
                             interval = "prediction", level = 0.95))
DB_pw_out <- RO_Census_Dambovita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(DB_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Dambovita), 
         DFFIT = dffits(DB_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(DB_pw_lm$coefficients) / nrow(RO_Census_Dambovita)), 
         Rezid_Std = rstudent(DB_pw_lm), Leverage = hatvalues(DB_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(DB_pw_lm)), 
         DFBETA = dfbetas(DB_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Dambovita)))
DB_pw_whtest <- white(DB_pw_lm, interactions = TRUE)
DB_pw_bptest <- bptest(DB_pw_lm, studentize = TRUE)
DB_pw_DWtest <- dwtest(DB_pw_lm)
DB_pw_bgtest <- bgtest(DB_pw_lm, order = 1)
DB_pw_jbtest <- jarque.bera.test(resid(DB_pw_lm))
DB_pw_shtest <- shapiro.test(resid(DB_pw_lm))

# Zipf_Mandelbrot Law
DB_ZM_prm <- get_ZM_Param("RO_Census_Dambovita", "Population", "Rank")
DB_ZM_m <- DB_ZM_prm$m
DB_ZM_alpha <- DB_ZM_prm$alpha_ZM
DB_ZM_cst <- DB_ZM_prm$constant_ZM
DB_ZM_lm <- lm(log(RO_Census_Dambovita$Population) ~ log(RO_Census_Dambovita$Rank + DB_ZM_m))
summary(DB_ZM_lm)
DB_pred_ZM_lm <- exp(predict(DB_ZM_lm, newdata = RO_Census_Dambovita, 
                             interval = "prediction", level = 0.95))
DB_ZM_out <- RO_Census_Dambovita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(DB_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Dambovita), 
         DFFIT = dffits(DB_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(DB_ZM_lm$coefficients) / nrow(RO_Census_Dambovita)), 
         Rezid_Std = rstudent(DB_ZM_lm), Leverage = hatvalues(DB_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(DB_ZM_lm)), 
         DFBETA = dfbetas(DB_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Dambovita)))
DB_ZM_whtest <- white(DB_ZM_lm, interactions = TRUE)
DB_ZM_bptest <- bptest(DB_ZM_lm, studentize = TRUE)
DB_ZM_DWtest <- dwtest(DB_ZM_lm)
DB_ZM_bgtest <- bgtest(DB_ZM_lm, order = 1)
DB_ZM_jbtest <- jarque.bera.test(resid(DB_ZM_lm))
DB_ZM_shtest <- shapiro.test(resid(DB_ZM_lm))

# Exponential Law
DB_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Dambovita)
summary(DB_exp_lm)
DB_pred_exp_lm <- exp(predict(DB_exp_lm, newdata = RO_Census_Dambovita, 
                              interval = "prediction", level = 0.95))
DB_exp_out <- RO_Census_Dambovita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(DB_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Dambovita), 
         DFFIT = dffits(DB_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(DB_exp_lm$coefficients) / nrow(RO_Census_Dambovita)), 
         Rezid_Std = rstudent(DB_exp_lm), Leverage = hatvalues(DB_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(DB_exp_lm)), 
         DFBETA = dfbetas(DB_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Dambovita)))
DB_exp_whtest <- white(DB_exp_lm, interactions = TRUE)
DB_exp_bptest <- bptest(DB_exp_lm, studentize = TRUE)
DB_exp_DWtest <- dwtest(DB_exp_lm)
DB_exp_bgtest <- bgtest(DB_exp_lm, order = 1)
DB_exp_jbtest <- jarque.bera.test(resid(DB_exp_lm))
DB_exp_shtest <- shapiro.test(resid(DB_exp_lm))

# Lavalette Function
RO_Census_Dambovita <- RO_Census_Dambovita %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Dambovita) - Rank + 1))
)
DB_lav_lm <- lm(log(RO_Census_Dambovita$Population) ~ RO_Census_Dambovita$Lav_exp)
summary(DB_lav_lm)
DB_lav_kst <- exp(signif(DB_lav_lm$coef[[1]], 4))
DB_lav_chi <- signif(DB_lav_lm$coef[[2]], 4)
DB_pred_lav_lm <- exp(predict(DB_lav_lm, newdata = RO_Census_Dambovita, 
                              interval = "prediction", level = 0.95))
DB_lav_out <- RO_Census_Dambovita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(DB_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Dambovita), 
         DFFIT = dffits(DB_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(DB_lav_lm$coefficients) / nrow(RO_Census_Dambovita)), 
         Rezid_Std = rstudent(DB_lav_lm), Leverage = hatvalues(DB_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(DB_lav_lm)), 
         DFBETA = dfbetas(DB_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Dambovita)))
DB_lav_whtest <- white(DB_lav_lm, interactions = TRUE)
DB_lav_bptest <- bptest(DB_lav_lm, studentize = TRUE)
DB_lav_DWtest <- dwtest(DB_lav_lm)
DB_lav_bgtest <- bgtest(DB_lav_lm, order = 1)
DB_lav_jbtest <- jarque.bera.test(resid(DB_lav_lm))
DB_lav_shtest <- shapiro.test(resid(DB_lav_lm))

# Data Distributions
RO_Census_Dambovita %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Dambovita$Rank, 
                              Population = DB_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.14,
            label = paste0("y", "==", signif(exp(DB_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(DB_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.14,
            label = paste0("R^2 ==", signif(summary(DB_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dambovita$Rank, 
                              Population = DB_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.12,
            label = paste0("y", "==", signif(exp(DB_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(DB_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.12,
            label = paste0("R^2 ==", signif(summary(DB_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dambovita$Rank, 
                              Population = DB_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.10,
            label = paste0("y", "==", signif(DB_ZM_cst, 4), "%.%", "(",
                           signif(DB_ZM_m, 4), "+ x)^", signif(DB_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.10,
            label = paste0("R^2 ==", signif(summary(DB_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dambovita$Rank, 
                              Population = DB_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.08,
            label = paste0("y", "==", signif(DB_lav_kst, 4), "%.%", "x^",
                           -signif(DB_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.08,
            label = paste0("R^2 ==", signif(summary(DB_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Dambovita judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Dambovita %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Dambovita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dambovita %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Dambovita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dambovita %>%
     ggplot(aes(x = log(Rank + DB_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Dambovita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dambovita %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Dambovita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 17. Dolj
RO_Census_Dolj <- RO_Census_rank %>% filter(Judet_Name == "DOLJ")
#View(RO_Census_Dolj)

# Power Law
DJ_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Dolj)
summary(DJ_pw_lm)
DJ_pred_pw_lm <- exp(predict(DJ_pw_lm, newdata = RO_Census_Dolj, 
                             interval = "prediction", level = 0.95))
DJ_pw_out <- RO_Census_Dolj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(DJ_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Dolj), 
         DFFIT = dffits(DJ_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(DJ_pw_lm$coefficients) / nrow(RO_Census_Dolj)), 
         Rezid_Std = rstudent(DJ_pw_lm), Leverage = hatvalues(DJ_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(DJ_pw_lm)), 
         DFBETA = dfbetas(DJ_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Dolj)))
DJ_pw_whtest <- white(DJ_pw_lm, interactions = TRUE)
DJ_pw_bptest <- bptest(DJ_pw_lm, studentize = TRUE)
DJ_pw_DWtest <- dwtest(DJ_pw_lm)
DJ_pw_bgtest <- bgtest(DJ_pw_lm, order = 1)
DJ_pw_jbtest <- jarque.bera.test(resid(DJ_pw_lm))
DJ_pw_shtest <- shapiro.test(resid(DJ_pw_lm))

# Zipf_Mandelbrot Law
DJ_ZM_prm <- get_ZM_Param("RO_Census_Dolj", "Population", "Rank")
DJ_ZM_m <- DJ_ZM_prm$m
DJ_ZM_alpha <- DJ_ZM_prm$alpha_ZM
DJ_ZM_cst <- DJ_ZM_prm$constant_ZM
DJ_ZM_lm <- lm(log(RO_Census_Dolj$Population) ~ log(RO_Census_Dolj$Rank + DJ_ZM_m))
summary(DJ_ZM_lm)
DJ_pred_ZM_lm <- exp(predict(DJ_ZM_lm, newdata = RO_Census_Dolj, 
                             interval = "prediction", level = 0.95))
DJ_ZM_out <- RO_Census_Dolj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(DJ_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Dolj), 
         DFFIT = dffits(DJ_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(DJ_ZM_lm$coefficients) / nrow(RO_Census_Dolj)), 
         Rezid_Std = rstudent(DJ_ZM_lm), Leverage = hatvalues(DJ_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(DJ_ZM_lm)), 
         DFBETA = dfbetas(DJ_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Dolj)))
DJ_ZM_whtest <- white(DJ_ZM_lm, interactions = TRUE)
DJ_ZM_bptest <- bptest(DJ_ZM_lm, studentize = TRUE)
DJ_ZM_DWtest <- dwtest(DJ_ZM_lm)
DJ_ZM_bgtest <- bgtest(DJ_ZM_lm, order = 1)
DJ_ZM_jbtest <- jarque.bera.test(resid(DJ_ZM_lm))
DJ_ZM_shtest <- shapiro.test(resid(DJ_ZM_lm))

# Exponential Law
DJ_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Dolj)
summary(DJ_exp_lm)
DJ_pred_exp_lm <- exp(predict(DJ_exp_lm, newdata = RO_Census_Dolj, 
                              interval = "prediction", level = 0.95))
DJ_exp_out <- RO_Census_Dolj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(DJ_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Dolj), 
         DFFIT = dffits(DJ_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(DJ_exp_lm$coefficients) / nrow(RO_Census_Dolj)), 
         Rezid_Std = rstudent(DJ_exp_lm), Leverage = hatvalues(DJ_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(DJ_exp_lm)), 
         DFBETA = dfbetas(DJ_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Dolj)))
DJ_exp_whtest <- white(DJ_exp_lm, interactions = TRUE)
DJ_exp_bptest <- bptest(DJ_exp_lm, studentize = TRUE)
DJ_exp_DWtest <- dwtest(DJ_exp_lm)
DJ_exp_bgtest <- bgtest(DJ_exp_lm, order = 1)
DJ_exp_jbtest <- jarque.bera.test(resid(DJ_exp_lm))
DJ_exp_shtest <- shapiro.test(resid(DJ_exp_lm))

# Lavalette Function
RO_Census_Dolj <- RO_Census_Dolj %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Dolj) - Rank + 1))
)
DJ_lav_lm <- lm(log(RO_Census_Dolj$Population) ~ RO_Census_Dolj$Lav_exp)
summary(DJ_lav_lm)
DJ_lav_kst <- exp(signif(DJ_lav_lm$coef[[1]], 4))
DJ_lav_chi <- signif(DJ_lav_lm$coef[[2]], 4)
DJ_pred_lav_lm <- exp(predict(DJ_lav_lm, newdata = RO_Census_Dolj, 
                              interval = "prediction", level = 0.95))
DJ_lav_out <- RO_Census_Dolj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(DJ_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Dolj), 
         DFFIT = dffits(DJ_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(DJ_lav_lm$coefficients) / nrow(RO_Census_Dolj)), 
         Rezid_Std = rstudent(DJ_lav_lm), Leverage = hatvalues(DJ_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(DJ_lav_lm)), 
         DFBETA = dfbetas(DJ_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Dolj)))
DJ_lav_whtest <- white(DJ_lav_lm, interactions = TRUE)
DJ_lav_bptest <- bptest(DJ_lav_lm, studentize = TRUE)
DJ_lav_DWtest <- dwtest(DJ_lav_lm)
DJ_lav_bgtest <- bgtest(DJ_lav_lm, order = 1)
DJ_lav_jbtest <- jarque.bera.test(resid(DJ_lav_lm))
DJ_lav_shtest <- shapiro.test(resid(DJ_lav_lm))

# Data Distributions
RO_Census_Dolj %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Dolj$Rank, 
                              Population = DJ_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.39,
            label = paste0("y", "==", signif(exp(DJ_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(DJ_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 65, y = 0.39,
            label = paste0("R^2 ==", signif(summary(DJ_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dolj$Rank, 
                              Population = DJ_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.36,
            label = paste0("y", "==", signif(exp(DJ_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(DJ_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 65, y = 0.36,
            label = paste0("R^2 ==", signif(summary(DJ_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dolj$Rank, 
                              Population = DJ_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.33,
            label = paste0("y", "==", signif(DJ_ZM_cst, 4), "%.%", "(",
                           signif(DJ_ZM_m, 4), "+ x)^", signif(DJ_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 65, y = 0.33,
            label = paste0("R^2 ==", signif(summary(DJ_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Dolj$Rank, 
                              Population = DJ_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.30,
            label = paste0("y", "==", signif(DJ_lav_kst, 4), "%.%", "x^",
                           -signif(DJ_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 65, y = 0.30,
            label = paste0("R^2 ==", signif(summary(DJ_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Dolj judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Dolj %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Dolj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dolj %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Dolj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dolj %>%
     ggplot(aes(x = log(Rank + DJ_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Dolj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Dolj %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Dolj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 18. Galati
RO_Census_Galati <- RO_Census_rank %>% filter(Judet_Name == "GALATI")
#View(RO_Census_Galati)

# Power Law
GL_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Galati)
summary(GL_pw_lm)
GL_pred_pw_lm <- exp(predict(GL_pw_lm, newdata = RO_Census_Galati, 
                             interval = "prediction", level = 0.95))
GL_pw_out <- RO_Census_Galati %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GL_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Galati), 
         DFFIT = dffits(GL_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GL_pw_lm$coefficients) / nrow(RO_Census_Galati)), 
         Rezid_Std = rstudent(GL_pw_lm), Leverage = hatvalues(GL_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GL_pw_lm)), 
         DFBETA = dfbetas(GL_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Galati)))
GL_pw_whtest <- white(GL_pw_lm, interactions = TRUE)
GL_pw_bptest <- bptest(GL_pw_lm, studentize = TRUE)
GL_pw_DWtest <- dwtest(GL_pw_lm)
GL_pw_bgtest <- bgtest(GL_pw_lm, order = 1)
GL_pw_jbtest <- jarque.bera.test(resid(GL_pw_lm))
GL_pw_shtest <- shapiro.test(resid(GL_pw_lm))

# Zipf_Mandelbrot Law
GL_ZM_prm <- get_ZM_Param("RO_Census_Galati", "Population", "Rank")
GL_ZM_m <- GL_ZM_prm$m
GL_ZM_alpha <- GL_ZM_prm$alpha_ZM
GL_ZM_cst <- GL_ZM_prm$constant_ZM
GL_ZM_lm <- lm(log(RO_Census_Galati$Population) ~ log(RO_Census_Galati$Rank + GL_ZM_m))
summary(GL_ZM_lm)
GL_pred_ZM_lm <- exp(predict(GL_ZM_lm, newdata = RO_Census_Galati, 
                             interval = "prediction", level = 0.95))
GL_ZM_out <- RO_Census_Galati %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GL_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Galati), 
         DFFIT = dffits(GL_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GL_ZM_lm$coefficients) / nrow(RO_Census_Galati)), 
         Rezid_Std = rstudent(GL_ZM_lm), Leverage = hatvalues(GL_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GL_ZM_lm)), 
         DFBETA = dfbetas(GL_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Galati)))
GL_ZM_whtest <- white(GL_ZM_lm, interactions = TRUE)
GL_ZM_bptest <- bptest(GL_ZM_lm, studentize = TRUE)
GL_ZM_DWtest <- dwtest(GL_ZM_lm)
GL_ZM_bgtest <- bgtest(GL_ZM_lm, order = 1)
GL_ZM_jbtest <- jarque.bera.test(resid(GL_ZM_lm))
GL_ZM_shtest <- shapiro.test(resid(GL_ZM_lm))

# Exponential Law
GL_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Galati)
summary(GL_exp_lm)
GL_pred_exp_lm <- exp(predict(GL_exp_lm, newdata = RO_Census_Galati, 
                              interval = "prediction", level = 0.95))
GL_exp_out <- RO_Census_Galati %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GL_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Galati), 
         DFFIT = dffits(GL_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GL_exp_lm$coefficients) / nrow(RO_Census_Galati)), 
         Rezid_Std = rstudent(GL_exp_lm), Leverage = hatvalues(GL_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GL_exp_lm)), 
         DFBETA = dfbetas(GL_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Galati)))
GL_exp_whtest <- white(GL_exp_lm, interactions = TRUE)
GL_exp_bptest <- bptest(GL_exp_lm, studentize = TRUE)
GL_exp_DWtest <- dwtest(GL_exp_lm)
GL_exp_bgtest <- bgtest(GL_exp_lm, order = 1)
GL_exp_jbtest <- jarque.bera.test(resid(GL_exp_lm))
GL_exp_shtest <- shapiro.test(resid(GL_exp_lm))

# Lavalette Function
RO_Census_Galati <- RO_Census_Galati %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Galati) - Rank + 1))
)
GL_lav_lm <- lm(log(RO_Census_Galati$Population) ~ RO_Census_Galati$Lav_exp)
summary(GL_lav_lm)
GL_lav_kst <- exp(signif(GL_lav_lm$coef[[1]], 4))
GL_lav_chi <- signif(GL_lav_lm$coef[[2]], 4)
GL_pred_lav_lm <- exp(predict(GL_lav_lm, newdata = RO_Census_Galati, 
                              interval = "prediction", level = 0.95))
GL_lav_out <- RO_Census_Galati %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GL_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Galati), 
         DFFIT = dffits(GL_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GL_lav_lm$coefficients) / nrow(RO_Census_Galati)), 
         Rezid_Std = rstudent(GL_lav_lm), Leverage = hatvalues(GL_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GL_lav_lm)), 
         DFBETA = dfbetas(GL_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Galati)))
GL_lav_whtest <- white(GL_lav_lm, interactions = TRUE)
GL_lav_bptest <- bptest(GL_lav_lm, studentize = TRUE)
GL_lav_DWtest <- dwtest(GL_lav_lm)
GL_lav_bgtest <- bgtest(GL_lav_lm, order = 1)
GL_lav_jbtest <- jarque.bera.test(resid(GL_lav_lm))
GL_lav_shtest <- shapiro.test(resid(GL_lav_lm))

# Data Distributions
RO_Census_Galati %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Galati$Rank, 
                              Population = GL_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.44,
            label = paste0("y", "==", signif(exp(GL_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(GL_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 35, y = 0.44,
            label = paste0("R^2 ==", signif(summary(GL_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Galati$Rank, 
                              Population = GL_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.40,
            label = paste0("y", "==", signif(exp(GL_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(GL_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 35, y = 0.40,
            label = paste0("R^2 ==", signif(summary(GL_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Galati$Rank, 
                              Population = GL_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.36,
            label = paste0("y", "==", signif(GL_ZM_cst, 4), "%.%", "(",
                           signif(GL_ZM_m, 4), "+ x)^", signif(GL_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 35, y = 0.36,
            label = paste0("R^2 ==", signif(summary(GL_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Galati$Rank, 
                              Population = GL_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.32,
            label = paste0("y", "==", signif(GL_lav_kst, 4), "%.%", "x^",
                           -signif(GL_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 35, y = 0.32,
            label = paste0("R^2 ==", signif(summary(GL_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Galati judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Galati %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Galati judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Galati %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Galati judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Galati %>%
     ggplot(aes(x = log(Rank + GL_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Galati judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Galati %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Galati judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 19. Giurgiu
RO_Census_Giurgiu <- RO_Census_rank %>% filter(Judet_Name == "GIURGIU")
#View(RO_Census_Giurgiu)

# Power Law
GR_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Giurgiu)
summary(GR_pw_lm)
GR_pred_pw_lm <- exp(predict(GR_pw_lm, newdata = RO_Census_Giurgiu, 
                             interval = "prediction", level = 0.95))
GR_pw_out <- RO_Census_Giurgiu %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GR_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Giurgiu), 
         DFFIT = dffits(GR_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GR_pw_lm$coefficients) / nrow(RO_Census_Giurgiu)), 
         Rezid_Std = rstudent(GR_pw_lm), Leverage = hatvalues(GR_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GR_pw_lm)), 
         DFBETA = dfbetas(GR_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Giurgiu)))
GR_pw_whtest <- white(GR_pw_lm, interactions = TRUE)
GR_pw_bptest <- bptest(GR_pw_lm, studentize = TRUE)
GR_pw_DWtest <- dwtest(GR_pw_lm)
GR_pw_bgtest <- bgtest(GR_pw_lm, order = 1)
GR_pw_jbtest <- jarque.bera.test(resid(GR_pw_lm))
GR_pw_shtest <- shapiro.test(resid(GR_pw_lm))

# Zipf_Mandelbrot Law
GR_ZM_prm <- get_ZM_Param("RO_Census_Giurgiu", "Population", "Rank")
GR_ZM_m <- GR_ZM_prm$m
GR_ZM_alpha <- GR_ZM_prm$alpha_ZM
GR_ZM_cst <- GR_ZM_prm$constant_ZM
GR_ZM_lm <- lm(log(RO_Census_Giurgiu$Population) ~ log(RO_Census_Giurgiu$Rank + GR_ZM_m))
summary(GR_ZM_lm)
GR_pred_ZM_lm <- exp(predict(GR_ZM_lm, newdata = RO_Census_Giurgiu, 
                             interval = "prediction", level = 0.95))
GR_ZM_out <- RO_Census_Giurgiu %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GR_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Giurgiu), 
         DFFIT = dffits(GR_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GR_ZM_lm$coefficients) / nrow(RO_Census_Giurgiu)), 
         Rezid_Std = rstudent(GR_ZM_lm), Leverage = hatvalues(GR_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GR_ZM_lm)), 
         DFBETA = dfbetas(GR_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Giurgiu)))
GR_ZM_whtest <- white(GR_ZM_lm, interactions = TRUE)
GR_ZM_bptest <- bptest(GR_ZM_lm, studentize = TRUE)
GR_ZM_DWtest <- dwtest(GR_ZM_lm)
GR_ZM_bgtest <- bgtest(GR_ZM_lm, order = 1)
GR_ZM_jbtest <- jarque.bera.test(resid(GR_ZM_lm))
GR_ZM_shtest <- shapiro.test(resid(GR_ZM_lm))

# Exponential Law
GR_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Giurgiu)
summary(GR_exp_lm)
GR_pred_exp_lm <- exp(predict(GR_exp_lm, newdata = RO_Census_Giurgiu, 
                              interval = "prediction", level = 0.95))
GR_exp_out <- RO_Census_Giurgiu %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GR_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Giurgiu), 
         DFFIT = dffits(GR_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GR_exp_lm$coefficients) / nrow(RO_Census_Giurgiu)), 
         Rezid_Std = rstudent(GR_exp_lm), Leverage = hatvalues(GR_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GR_exp_lm)), 
         DFBETA = dfbetas(GR_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Giurgiu)))
GR_exp_whtest <- white(GR_exp_lm, interactions = TRUE)
GR_exp_bptest <- bptest(GR_exp_lm, studentize = TRUE)
GR_exp_DWtest <- dwtest(GR_exp_lm)
GR_exp_bgtest <- bgtest(GR_exp_lm, order = 1)
GR_exp_jbtest <- jarque.bera.test(resid(GR_exp_lm))
GR_exp_shtest <- shapiro.test(resid(GR_exp_lm))

# Lavalette Function
RO_Census_Giurgiu <- RO_Census_Giurgiu %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Giurgiu) - Rank + 1))
)
GR_lav_lm <- lm(log(RO_Census_Giurgiu$Population) ~ RO_Census_Giurgiu$Lav_exp)
summary(GR_lav_lm)
GR_lav_kst <- exp(signif(GR_lav_lm$coef[[1]], 4))
GR_lav_chi <- signif(GR_lav_lm$coef[[2]], 4)
GR_pred_lav_lm <- exp(predict(GR_lav_lm, newdata = RO_Census_Giurgiu, 
                              interval = "prediction", level = 0.95))
GR_lav_out <- RO_Census_Giurgiu %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GR_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Giurgiu), 
         DFFIT = dffits(GR_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GR_lav_lm$coefficients) / nrow(RO_Census_Giurgiu)), 
         Rezid_Std = rstudent(GR_lav_lm), Leverage = hatvalues(GR_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GR_lav_lm)), 
         DFBETA = dfbetas(GR_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Giurgiu)))
GR_lav_whtest <- white(GR_lav_lm, interactions = TRUE)
GR_lav_bptest <- bptest(GR_lav_lm, studentize = TRUE)
GR_lav_DWtest <- dwtest(GR_lav_lm)
GR_lav_bgtest <- bgtest(GR_lav_lm, order = 1)
GR_lav_jbtest <- jarque.bera.test(resid(GR_lav_lm))
GR_lav_shtest <- shapiro.test(resid(GR_lav_lm))

# Data Distributions
RO_Census_Giurgiu %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Giurgiu$Rank, 
                              Population = GR_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.21,
            label = paste0("y", "==", signif(exp(GR_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(GR_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 35, y = 0.21,
            label = paste0("R^2 ==", signif(summary(GR_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Giurgiu$Rank, 
                              Population = GR_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.19,
            label = paste0("y", "==", signif(exp(GR_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(GR_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 35, y = 0.19,
            label = paste0("R^2 ==", signif(summary(GR_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Giurgiu$Rank, 
                              Population = GR_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.17,
            label = paste0("y", "==", signif(GR_ZM_cst, 4), "%.%", "(",
                           signif(GR_ZM_m, 4), "+ x)^", signif(GR_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 35, y = 0.17,
            label = paste0("R^2 ==", signif(summary(GR_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Giurgiu$Rank, 
                              Population = GR_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.15,
            label = paste0("y", "==", signif(GR_lav_kst, 4), "%.%", "x^",
                           -signif(GR_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 35, y = 0.15,
            label = paste0("R^2 ==", signif(summary(GR_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Giurgiu judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Giurgiu %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Giurgiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Giurgiu %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Giurgiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Giurgiu %>%
     ggplot(aes(x = log(Rank + GR_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Giurgiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Giurgiu %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Giurgiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 20. Gorj
RO_Census_Gorj <- RO_Census_rank %>% filter(Judet_Name == "GORJ")
#View(RO_Census_Gorj)

# Power Law
GJ_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Gorj)
summary(GJ_pw_lm)
GJ_pred_pw_lm <- exp(predict(GJ_pw_lm, newdata = RO_Census_Gorj, 
                             interval = "prediction", level = 0.95))
GJ_pw_out <- RO_Census_Gorj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GJ_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Gorj), 
         DFFIT = dffits(GJ_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GJ_pw_lm$coefficients) / nrow(RO_Census_Gorj)), 
         Rezid_Std = rstudent(GJ_pw_lm), Leverage = hatvalues(GJ_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GJ_pw_lm)), 
         DFBETA = dfbetas(GJ_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Gorj)))
GJ_pw_whtest <- white(GJ_pw_lm, interactions = TRUE)
GJ_pw_bptest <- bptest(GJ_pw_lm, studentize = TRUE)
GJ_pw_DWtest <- dwtest(GJ_pw_lm)
GJ_pw_bgtest <- bgtest(GJ_pw_lm, order = 1)
GJ_pw_jbtest <- jarque.bera.test(resid(GJ_pw_lm))
GJ_pw_shtest <- shapiro.test(resid(GJ_pw_lm))

# Zipf_Mandelbrot Law
GJ_ZM_prm <- get_ZM_Param("RO_Census_Gorj", "Population", "Rank")
GJ_ZM_m <- GJ_ZM_prm$m
GJ_ZM_alpha <- GJ_ZM_prm$alpha_ZM
GJ_ZM_cst <- GJ_ZM_prm$constant_ZM
GJ_ZM_lm <- lm(log(RO_Census_Gorj$Population) ~ log(RO_Census_Gorj$Rank + GJ_ZM_m))
summary(GJ_ZM_lm)
GJ_pred_ZM_lm <- exp(predict(GJ_ZM_lm, newdata = RO_Census_Gorj, 
                             interval = "prediction", level = 0.95))
GJ_ZM_out <- RO_Census_Gorj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GJ_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Gorj), 
         DFFIT = dffits(GJ_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GJ_ZM_lm$coefficients) / nrow(RO_Census_Gorj)), 
         Rezid_Std = rstudent(GJ_ZM_lm), Leverage = hatvalues(GJ_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GJ_ZM_lm)), 
         DFBETA = dfbetas(GJ_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Gorj)))
GJ_ZM_whtest <- white(GJ_ZM_lm, interactions = TRUE)
GJ_ZM_bptest <- bptest(GJ_ZM_lm, studentize = TRUE)
GJ_ZM_DWtest <- dwtest(GJ_ZM_lm)
GJ_ZM_bgtest <- bgtest(GJ_ZM_lm, order = 1)
GJ_ZM_jbtest <- jarque.bera.test(resid(GJ_ZM_lm))
GJ_ZM_shtest <- shapiro.test(resid(GJ_ZM_lm))

# Exponential Law
GJ_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Gorj)
summary(GJ_exp_lm)
GJ_pred_exp_lm <- exp(predict(GJ_exp_lm, newdata = RO_Census_Gorj, 
                              interval = "prediction", level = 0.95))
GJ_exp_out <- RO_Census_Gorj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GJ_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Gorj), 
         DFFIT = dffits(GJ_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GJ_exp_lm$coefficients) / nrow(RO_Census_Gorj)), 
         Rezid_Std = rstudent(GJ_exp_lm), Leverage = hatvalues(GJ_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GJ_exp_lm)), 
         DFBETA = dfbetas(GJ_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Gorj)))
GJ_exp_whtest <- white(GJ_exp_lm, interactions = TRUE)
GJ_exp_bptest <- bptest(GJ_exp_lm, studentize = TRUE)
GJ_exp_DWtest <- dwtest(GJ_exp_lm)
GJ_exp_bgtest <- bgtest(GJ_exp_lm, order = 1)
GJ_exp_jbtest <- jarque.bera.test(resid(GJ_exp_lm))
GJ_exp_shtest <- shapiro.test(resid(GJ_exp_lm))

# Lavalette Function
RO_Census_Gorj <- RO_Census_Gorj %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Gorj) - Rank + 1))
)
GJ_lav_lm <- lm(log(RO_Census_Gorj$Population) ~ RO_Census_Gorj$Lav_exp)
summary(GJ_lav_lm)
GJ_lav_kst <- exp(signif(GJ_lav_lm$coef[[1]], 4))
GJ_lav_chi <- signif(GJ_lav_lm$coef[[2]], 4)
GJ_pred_lav_lm <- exp(predict(GJ_lav_lm, newdata = RO_Census_Gorj, 
                              interval = "prediction", level = 0.95))
GJ_lav_out <- RO_Census_Gorj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(GJ_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Gorj), 
         DFFIT = dffits(GJ_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(GJ_lav_lm$coefficients) / nrow(RO_Census_Gorj)), 
         Rezid_Std = rstudent(GJ_lav_lm), Leverage = hatvalues(GJ_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(GJ_lav_lm)), 
         DFBETA = dfbetas(GJ_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Gorj)))
GJ_lav_whtest <- white(GJ_lav_lm, interactions = TRUE)
GJ_lav_bptest <- bptest(GJ_lav_lm, studentize = TRUE)
GJ_lav_DWtest <- dwtest(GJ_lav_lm)
GJ_lav_bgtest <- bgtest(GJ_lav_lm, order = 1)
GJ_lav_jbtest <- jarque.bera.test(resid(GJ_lav_lm))
GJ_lav_shtest <- shapiro.test(resid(GJ_lav_lm))

# Data Distributions
RO_Census_Gorj %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Gorj$Rank, 
                              Population = GJ_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.23,
            label = paste0("y", "==", signif(exp(GJ_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(GJ_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.23,
            label = paste0("R^2 ==", signif(summary(GJ_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Gorj$Rank, 
                              Population = GJ_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.21,
            label = paste0("y", "==", signif(exp(GJ_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(GJ_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.21,
            label = paste0("R^2 ==", signif(summary(GJ_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Gorj$Rank, 
                              Population = GJ_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.19,
            label = paste0("y", "==", signif(GJ_ZM_cst, 4), "%.%", "(",
                           signif(GJ_ZM_m, 4), "+ x)^", signif(GJ_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.19,
            label = paste0("R^2 ==", signif(summary(GJ_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Gorj$Rank, 
                              Population = GJ_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.17,
            label = paste0("y", "==", signif(GJ_lav_kst, 4), "%.%", "x^",
                           -signif(GJ_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.17,
            label = paste0("R^2 ==", signif(summary(GJ_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Gorj judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Gorj %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Gorj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Gorj %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Gorj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Gorj %>%
     ggplot(aes(x = log(Rank + GJ_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Gorj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Gorj %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Gorj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 21. Harghita
RO_Census_Harghita <- RO_Census_rank %>% filter(Judet_Name == "HARGHITA")
#View(RO_Census_Harghita)

# Power Law
HR_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Harghita)
summary(HR_pw_lm)
HR_pred_pw_lm <- exp(predict(HR_pw_lm, newdata = RO_Census_Harghita, 
                             interval = "prediction", level = 0.95))
HR_pw_out <- RO_Census_Harghita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(HR_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Harghita), 
         DFFIT = dffits(HR_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(HR_pw_lm$coefficients) / nrow(RO_Census_Harghita)), 
         Rezid_Std = rstudent(HR_pw_lm), Leverage = hatvalues(HR_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(HR_pw_lm)), 
         DFBETA = dfbetas(HR_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Harghita)))
HR_pw_whtest <- white(HR_pw_lm, interactions = TRUE)
HR_pw_bptest <- bptest(HR_pw_lm, studentize = TRUE)
HR_pw_DWtest <- dwtest(HR_pw_lm)
HR_pw_bgtest <- bgtest(HR_pw_lm, order = 1)
HR_pw_jbtest <- jarque.bera.test(resid(HR_pw_lm))
HR_pw_shtest <- shapiro.test(resid(HR_pw_lm))

# Zipf_Mandelbrot Law
HR_ZM_prm <- get_ZM_Param("RO_Census_Harghita", "Population", "Rank")
HR_ZM_m <- HR_ZM_prm$m
HR_ZM_alpha <- HR_ZM_prm$alpha_ZM
HR_ZM_cst <- HR_ZM_prm$constant_ZM
HR_ZM_lm <- lm(log(RO_Census_Harghita$Population) ~ log(RO_Census_Harghita$Rank + HR_ZM_m))
summary(HR_ZM_lm)
HR_pred_ZM_lm <- exp(predict(HR_ZM_lm, newdata = RO_Census_Harghita, 
                             interval = "prediction", level = 0.95))
HR_ZM_out <- RO_Census_Harghita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(HR_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Harghita), 
         DFFIT = dffits(HR_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(HR_ZM_lm$coefficients) / nrow(RO_Census_Harghita)), 
         Rezid_Std = rstudent(HR_ZM_lm), Leverage = hatvalues(HR_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(HR_ZM_lm)), 
         DFBETA = dfbetas(HR_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Harghita)))
HR_ZM_whtest <- white(HR_ZM_lm, interactions = TRUE)
HR_ZM_bptest <- bptest(HR_ZM_lm, studentize = TRUE)
HR_ZM_DWtest <- dwtest(HR_ZM_lm)
HR_ZM_bgtest <- bgtest(HR_ZM_lm, order = 1)
HR_ZM_jbtest <- jarque.bera.test(resid(HR_ZM_lm))
HR_ZM_shtest <- shapiro.test(resid(HR_ZM_lm))

# Exponential Law
HR_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Harghita)
summary(HR_exp_lm)
HR_pred_exp_lm <- exp(predict(HR_exp_lm, newdata = RO_Census_Harghita, 
                              interval = "prediction", level = 0.95))
HR_exp_out <- RO_Census_Harghita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(HR_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Harghita), 
         DFFIT = dffits(HR_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(HR_exp_lm$coefficients) / nrow(RO_Census_Harghita)), 
         Rezid_Std = rstudent(HR_exp_lm), Leverage = hatvalues(HR_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(HR_exp_lm)), 
         DFBETA = dfbetas(HR_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Harghita)))
HR_exp_whtest <- white(HR_exp_lm, interactions = TRUE)
HR_exp_bptest <- bptest(HR_exp_lm, studentize = TRUE)
HR_exp_DWtest <- dwtest(HR_exp_lm)
HR_exp_bgtest <- bgtest(HR_exp_lm, order = 1)
HR_exp_jbtest <- jarque.bera.test(resid(HR_exp_lm))
HR_exp_shtest <- shapiro.test(resid(HR_exp_lm))

# Lavalette Function
RO_Census_Harghita <- RO_Census_Harghita %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Harghita) - Rank + 1))
)
HR_lav_lm <- lm(log(RO_Census_Harghita$Population) ~ RO_Census_Harghita$Lav_exp)
summary(HR_lav_lm)
HR_lav_kst <- exp(signif(HR_lav_lm$coef[[1]], 4))
HR_lav_chi <- signif(HR_lav_lm$coef[[2]], 4)
HR_pred_lav_lm <- exp(predict(HR_lav_lm, newdata = RO_Census_Harghita, 
                              interval = "prediction", level = 0.95))
HR_lav_out <- RO_Census_Harghita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(HR_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Harghita), 
         DFFIT = dffits(HR_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(HR_lav_lm$coefficients) / nrow(RO_Census_Harghita)), 
         Rezid_Std = rstudent(HR_lav_lm), Leverage = hatvalues(HR_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(HR_lav_lm)), 
         DFBETA = dfbetas(HR_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Harghita)))
HR_lav_whtest <- white(HR_lav_lm, interactions = TRUE)
HR_lav_bptest <- bptest(HR_lav_lm, studentize = TRUE)
HR_lav_DWtest <- dwtest(HR_lav_lm)
HR_lav_bgtest <- bgtest(HR_lav_lm, order = 1)
HR_lav_jbtest <- jarque.bera.test(resid(HR_lav_lm))
HR_lav_shtest <- shapiro.test(resid(HR_lav_lm))

# Data Distributions
RO_Census_Harghita %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Harghita$Rank, 
                              Population = HR_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.11,
            label = paste0("y", "==", signif(exp(HR_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(HR_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.11,
            label = paste0("R^2 ==", signif(summary(HR_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Harghita$Rank, 
                              Population = HR_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.09,
            label = paste0("y", "==", signif(exp(HR_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(HR_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.09,
            label = paste0("R^2 ==", signif(summary(HR_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Harghita$Rank, 
                              Population = HR_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.07,
            label = paste0("y", "==", signif(HR_ZM_cst, 4), "%.%", "(",
                           signif(HR_ZM_m, 4), "+ x)^", signif(HR_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.07,
            label = paste0("R^2 ==", signif(summary(HR_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Harghita$Rank, 
                              Population = HR_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.05,
            label = paste0("y", "==", signif(HR_lav_kst, 4), "%.%", "x^",
                           -signif(HR_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.05,
            label = paste0("R^2 ==", signif(summary(HR_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Harghita judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Harghita %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Harghita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Harghita %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Harghita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Harghita %>%
     ggplot(aes(x = log(Rank + HR_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Harghita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Harghita %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Harghita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 22. Hunedoara
RO_Census_Hunedoara <- RO_Census_rank %>% filter(Judet_Name == "HUNEDOARA")
#View(RO_Census_Hunedoara)

# Power Law
HD_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Hunedoara)
summary(HD_pw_lm)
HD_pred_pw_lm <- exp(predict(HD_pw_lm, newdata = RO_Census_Hunedoara, 
                             interval = "prediction", level = 0.95))
HD_pw_out <- RO_Census_Hunedoara %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(HD_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Hunedoara), 
         DFFIT = dffits(HD_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(HD_pw_lm$coefficients) / nrow(RO_Census_Hunedoara)), 
         Rezid_Std = rstudent(HD_pw_lm), Leverage = hatvalues(HD_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(HD_pw_lm)), 
         DFBETA = dfbetas(HD_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Hunedoara)))
HD_pw_whtest <- white(HD_pw_lm, interactions = TRUE)
HD_pw_bptest <- bptest(HD_pw_lm, studentize = TRUE)
HD_pw_DWtest <- dwtest(HD_pw_lm)
HD_pw_bgtest <- bgtest(HD_pw_lm, order = 1)
HD_pw_jbtest <- jarque.bera.test(resid(HD_pw_lm))
HD_pw_shtest <- shapiro.test(resid(HD_pw_lm))

# Zipf_Mandelbrot Law
HD_ZM_prm <- get_ZM_Param("RO_Census_Hunedoara", "Population", "Rank")
HD_ZM_m <- HD_ZM_prm$m
HD_ZM_alpha <- HD_ZM_prm$alpha_ZM
HD_ZM_cst <- HD_ZM_prm$constant_ZM
HD_ZM_lm <- lm(log(RO_Census_Hunedoara$Population) ~ log(RO_Census_Hunedoara$Rank + HD_ZM_m))
summary(HD_ZM_lm)
HD_pred_ZM_lm <- exp(predict(HD_ZM_lm, newdata = RO_Census_Hunedoara, 
                             interval = "prediction", level = 0.95))
HD_ZM_out <- RO_Census_Hunedoara %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(HD_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Hunedoara), 
         DFFIT = dffits(HD_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(HD_ZM_lm$coefficients) / nrow(RO_Census_Hunedoara)), 
         Rezid_Std = rstudent(HD_ZM_lm), Leverage = hatvalues(HD_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(HD_ZM_lm)), 
         DFBETA = dfbetas(HD_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Hunedoara)))
HD_ZM_whtest <- white(HD_ZM_lm, interactions = TRUE)
HD_ZM_bptest <- bptest(HD_ZM_lm, studentize = TRUE)
HD_ZM_DWtest <- dwtest(HD_ZM_lm)
HD_ZM_bgtest <- bgtest(HD_ZM_lm, order = 1)
HD_ZM_jbtest <- jarque.bera.test(resid(HD_ZM_lm))
HD_ZM_shtest <- shapiro.test(resid(HD_ZM_lm))

# Exponential Law
HD_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Hunedoara)
summary(HD_exp_lm)
HD_pred_exp_lm <- exp(predict(HD_exp_lm, newdata = RO_Census_Hunedoara, 
                              interval = "prediction", level = 0.95))
HD_exp_out <- RO_Census_Hunedoara %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(HD_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Hunedoara), 
         DFFIT = dffits(HD_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(HD_exp_lm$coefficients) / nrow(RO_Census_Hunedoara)), 
         Rezid_Std = rstudent(HD_exp_lm), Leverage = hatvalues(HD_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(HD_exp_lm)), 
         DFBETA = dfbetas(HD_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Hunedoara)))
HD_exp_whtest <- white(HD_exp_lm, interactions = TRUE)
HD_exp_bptest <- bptest(HD_exp_lm, studentize = TRUE)
HD_exp_DWtest <- dwtest(HD_exp_lm)
HD_exp_bgtest <- bgtest(HD_exp_lm, order = 1)
HD_exp_jbtest <- jarque.bera.test(resid(HD_exp_lm))
HD_exp_shtest <- shapiro.test(resid(HD_exp_lm))

# Lavalette Function
RO_Census_Hunedoara <- RO_Census_Hunedoara %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Hunedoara) - Rank + 1))
)
HD_lav_lm <- lm(log(RO_Census_Hunedoara$Population) ~ RO_Census_Hunedoara$Lav_exp)
summary(HD_lav_lm)
HD_lav_kst <- exp(signif(HD_lav_lm$coef[[1]], 4))
HD_lav_chi <- signif(HD_lav_lm$coef[[2]], 4)
HD_pred_lav_lm <- exp(predict(HD_lav_lm, newdata = RO_Census_Hunedoara, 
                              interval = "prediction", level = 0.95))
HD_lav_out <- RO_Census_Hunedoara %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(HD_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Hunedoara), 
         DFFIT = dffits(HD_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(HD_lav_lm$coefficients) / nrow(RO_Census_Hunedoara)), 
         Rezid_Std = rstudent(HD_lav_lm), Leverage = hatvalues(HD_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(HD_lav_lm)), 
         DFBETA = dfbetas(HD_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Hunedoara)))
HD_lav_whtest <- white(HD_lav_lm, interactions = TRUE)
HD_lav_bptest <- bptest(HD_lav_lm, studentize = TRUE)
HD_lav_DWtest <- dwtest(HD_lav_lm)
HD_lav_bgtest <- bgtest(HD_lav_lm, order = 1)
HD_lav_jbtest <- jarque.bera.test(resid(HD_lav_lm))
HD_lav_shtest <- shapiro.test(resid(HD_lav_lm))

# Data Distributions
RO_Census_Hunedoara %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Hunedoara$Rank, 
                              Population = HD_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.4,
            label = paste0("y", "==", signif(exp(HD_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(HD_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.4,
            label = paste0("R^2 ==", signif(summary(HD_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Hunedoara$Rank, 
                              Population = HD_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.35,
            label = paste0("y", "==", signif(exp(HD_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(HD_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.35,
            label = paste0("R^2 ==", signif(summary(HD_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Hunedoara$Rank, 
                              Population = HD_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.3,
            label = paste0("y", "==", signif(HD_ZM_cst, 4), "%.%", "(",
                           signif(HD_ZM_m, 4), "+ x)^", signif(HD_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.3,
            label = paste0("R^2 ==", signif(summary(HD_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Hunedoara$Rank, 
                              Population = HD_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.25,
            label = paste0("y", "==", signif(HD_lav_kst, 4), "%.%", "x^",
                           -signif(HD_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.25,
            label = paste0("R^2 ==", signif(summary(HD_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Hunedoara judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Hunedoara %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Hunedoara judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Hunedoara %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Hunedoara judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Hunedoara %>%
     ggplot(aes(x = log(Rank + HD_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Hunedoara judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Hunedoara %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Hunedoara judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 23. Ialomita
RO_Census_Ialomita <- RO_Census_rank %>% filter(Judet_Name == "IALOMITA")
#View(RO_Census_Ialomita)

# Power Law
IL_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Ialomita)
summary(IL_pw_lm)
IL_pred_pw_lm <- exp(predict(IL_pw_lm, newdata = RO_Census_Ialomita, 
                             interval = "prediction", level = 0.95))
IL_pw_out <- RO_Census_Ialomita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IL_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Ialomita), 
         DFFIT = dffits(IL_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IL_pw_lm$coefficients) / nrow(RO_Census_Ialomita)), 
         Rezid_Std = rstudent(IL_pw_lm), Leverage = hatvalues(IL_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IL_pw_lm)), 
         DFBETA = dfbetas(IL_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Ialomita)))
IL_pw_whtest <- white(IL_pw_lm, interactions = TRUE)
IL_pw_bptest <- bptest(IL_pw_lm, studentize = TRUE)
IL_pw_DWtest <- dwtest(IL_pw_lm)
IL_pw_bgtest <- bgtest(IL_pw_lm, order = 1)
IL_pw_jbtest <- jarque.bera.test(resid(IL_pw_lm))
IL_pw_shtest <- shapiro.test(resid(IL_pw_lm))

# Zipf_Mandelbrot Law
IL_ZM_prm <- get_ZM_Param("RO_Census_Ialomita", "Population", "Rank")
IL_ZM_m <- IL_ZM_prm$m
IL_ZM_alpha <- IL_ZM_prm$alpha_ZM
IL_ZM_cst <- IL_ZM_prm$constant_ZM
IL_ZM_lm <- lm(log(RO_Census_Ialomita$Population) ~ log(RO_Census_Ialomita$Rank + IL_ZM_m))
summary(IL_ZM_lm)
IL_pred_ZM_lm <- exp(predict(IL_ZM_lm, newdata = RO_Census_Ialomita, 
                             interval = "prediction", level = 0.95))
IL_ZM_out <- RO_Census_Ialomita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IL_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Ialomita), 
         DFFIT = dffits(IL_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IL_ZM_lm$coefficients) / nrow(RO_Census_Ialomita)), 
         Rezid_Std = rstudent(IL_ZM_lm), Leverage = hatvalues(IL_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IL_ZM_lm)), 
         DFBETA = dfbetas(IL_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Ialomita)))
IL_ZM_whtest <- white(IL_ZM_lm, interactions = TRUE)
IL_ZM_bptest <- bptest(IL_ZM_lm, studentize = TRUE)
IL_ZM_DWtest <- dwtest(IL_ZM_lm)
IL_ZM_bgtest <- bgtest(IL_ZM_lm, order = 1)
IL_ZM_jbtest <- jarque.bera.test(resid(IL_ZM_lm))
IL_ZM_shtest <- shapiro.test(resid(IL_ZM_lm))

# Exponential Law
IL_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Ialomita)
summary(IL_exp_lm)
IL_pred_exp_lm <- exp(predict(IL_exp_lm, newdata = RO_Census_Ialomita, 
                              interval = "prediction", level = 0.95))
IL_exp_out <- RO_Census_Ialomita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IL_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Ialomita), 
         DFFIT = dffits(IL_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IL_exp_lm$coefficients) / nrow(RO_Census_Ialomita)), 
         Rezid_Std = rstudent(IL_exp_lm), Leverage = hatvalues(IL_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IL_exp_lm)), 
         DFBETA = dfbetas(IL_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Ialomita)))
IL_exp_whtest <- white(IL_exp_lm, interactions = TRUE)
IL_exp_bptest <- bptest(IL_exp_lm, studentize = TRUE)
IL_exp_DWtest <- dwtest(IL_exp_lm)
IL_exp_bgtest <- bgtest(IL_exp_lm, order = 1)
IL_exp_jbtest <- jarque.bera.test(resid(IL_exp_lm))
IL_exp_shtest <- shapiro.test(resid(IL_exp_lm))

# Lavalette Function
RO_Census_Ialomita <- RO_Census_Ialomita %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Ialomita) - Rank + 1))
)
IL_lav_lm <- lm(log(RO_Census_Ialomita$Population) ~ RO_Census_Ialomita$Lav_exp)
summary(IL_lav_lm)
IL_lav_kst <- exp(signif(IL_lav_lm$coef[[1]], 4))
IL_lav_chi <- signif(IL_lav_lm$coef[[2]], 4)
IL_pred_lav_lm <- exp(predict(IL_lav_lm, newdata = RO_Census_Ialomita, 
                              interval = "prediction", level = 0.95))
IL_lav_out <- RO_Census_Ialomita %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IL_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Ialomita), 
         DFFIT = dffits(IL_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IL_lav_lm$coefficients) / nrow(RO_Census_Ialomita)), 
         Rezid_Std = rstudent(IL_lav_lm), Leverage = hatvalues(IL_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IL_lav_lm)), 
         DFBETA = dfbetas(IL_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Ialomita)))
IL_lav_whtest <- white(IL_lav_lm, interactions = TRUE)
IL_lav_bptest <- bptest(IL_lav_lm, studentize = TRUE)
IL_lav_DWtest <- dwtest(IL_lav_lm)
IL_lav_bgtest <- bgtest(IL_lav_lm, order = 1)
IL_lav_jbtest <- jarque.bera.test(resid(IL_lav_lm))
IL_lav_shtest <- shapiro.test(resid(IL_lav_lm))

# Data Distributions
RO_Census_Ialomita %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Ialomita$Rank, 
                              Population = IL_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.16,
            label = paste0("y", "==", signif(exp(IL_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(IL_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.16,
            label = paste0("R^2 ==", signif(summary(IL_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ialomita$Rank, 
                              Population = IL_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.14,
            label = paste0("y", "==", signif(exp(IL_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(IL_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.14,
            label = paste0("R^2 ==", signif(summary(IL_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ialomita$Rank, 
                              Population = IL_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.12,
            label = paste0("y", "==", signif(IL_ZM_cst, 4), "%.%", "(",
                           signif(IL_ZM_m, 4), "+ x)^", signif(IL_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.12,
            label = paste0("R^2 ==", signif(summary(IL_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ialomita$Rank, 
                              Population = IL_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.10,
            label = paste0("y", "==", signif(IL_lav_kst, 4), "%.%", "x^",
                           -signif(IL_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.10,
            label = paste0("R^2 ==", signif(summary(IL_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Ialomita judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Ialomita %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Ialomita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ialomita %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Ialomita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ialomita %>%
     ggplot(aes(x = log(Rank + IL_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Ialomita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ialomita %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Ialomita judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 24. Iasi
RO_Census_Iasi <- RO_Census_rank %>% filter(Judet_Name == "IASI")
#View(RO_Census_Iasi)

# Power Law
IS_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Iasi)
summary(IS_pw_lm)
IS_pred_pw_lm <- exp(predict(IS_pw_lm, newdata = RO_Census_Iasi, 
                             interval = "prediction", level = 0.95))
IS_pw_out <- RO_Census_Iasi %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IS_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Iasi), 
         DFFIT = dffits(IS_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IS_pw_lm$coefficients) / nrow(RO_Census_Iasi)), 
         Rezid_Std = rstudent(IS_pw_lm), Leverage = hatvalues(IS_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IS_pw_lm)), 
         DFBETA = dfbetas(IS_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Iasi)))
IS_pw_whtest <- white(IS_pw_lm, interactions = TRUE)
IS_pw_bptest <- bptest(IS_pw_lm, studentize = TRUE)
IS_pw_DWtest <- dwtest(IS_pw_lm)
IS_pw_bgtest <- bgtest(IS_pw_lm, order = 1)
IS_pw_jbtest <- jarque.bera.test(resid(IS_pw_lm))
IS_pw_shtest <- shapiro.test(resid(IS_pw_lm))

# Zipf_Mandelbrot Law
IS_ZM_prm <- get_ZM_Param("RO_Census_Iasi", "Population", "Rank")
IS_ZM_m <- IS_ZM_prm$m
IS_ZM_alpha <- IS_ZM_prm$alpha_ZM
IS_ZM_cst <- IS_ZM_prm$constant_ZM
IS_ZM_lm <- lm(log(RO_Census_Iasi$Population) ~ log(RO_Census_Iasi$Rank + IS_ZM_m))
summary(IS_ZM_lm)
IS_pred_ZM_lm <- exp(predict(IS_ZM_lm, newdata = RO_Census_Iasi, 
                             interval = "prediction", level = 0.95))
IS_ZM_out <- RO_Census_Iasi %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IS_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Iasi), 
         DFFIT = dffits(IS_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IS_ZM_lm$coefficients) / nrow(RO_Census_Iasi)), 
         Rezid_Std = rstudent(IS_ZM_lm), Leverage = hatvalues(IS_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IS_ZM_lm)), 
         DFBETA = dfbetas(IS_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Iasi)))
IS_ZM_whtest <- white(IS_ZM_lm, interactions = TRUE)
IS_ZM_bptest <- bptest(IS_ZM_lm, studentize = TRUE)
IS_ZM_DWtest <- dwtest(IS_ZM_lm)
IS_ZM_bgtest <- bgtest(IS_ZM_lm, order = 1)
IS_ZM_jbtest <- jarque.bera.test(resid(IS_ZM_lm))
IS_ZM_shtest <- shapiro.test(resid(IS_ZM_lm))

# Exponential Law
IS_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Iasi)
summary(IS_exp_lm)
IS_pred_exp_lm <- exp(predict(IS_exp_lm, newdata = RO_Census_Iasi, 
                              interval = "prediction", level = 0.95))
IS_exp_out <- RO_Census_Iasi %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IS_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Iasi), 
         DFFIT = dffits(IS_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IS_exp_lm$coefficients) / nrow(RO_Census_Iasi)), 
         Rezid_Std = rstudent(IS_exp_lm), Leverage = hatvalues(IS_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IS_exp_lm)), 
         DFBETA = dfbetas(IS_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Iasi)))
IS_exp_whtest <- white(IS_exp_lm, interactions = TRUE)
IS_exp_bptest <- bptest(IS_exp_lm, studentize = TRUE)
IS_exp_DWtest <- dwtest(IS_exp_lm)
IS_exp_bgtest <- bgtest(IS_exp_lm, order = 1)
IS_exp_jbtest <- jarque.bera.test(resid(IS_exp_lm))
IS_exp_shtest <- shapiro.test(resid(IS_exp_lm))

# Lavalette Function
RO_Census_Iasi <- RO_Census_Iasi %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Iasi) - Rank + 1))
)
IS_lav_lm <- lm(log(RO_Census_Iasi$Population) ~ RO_Census_Iasi$Lav_exp)
summary(IS_lav_lm)
IS_lav_kst <- exp(signif(IS_lav_lm$coef[[1]], 4))
IS_lav_chi <- signif(IS_lav_lm$coef[[2]], 4)
IS_pred_lav_lm <- exp(predict(IS_lav_lm, newdata = RO_Census_Iasi, 
                              interval = "prediction", level = 0.95))
IS_lav_out <- RO_Census_Iasi %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IS_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Iasi), 
         DFFIT = dffits(IS_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IS_lav_lm$coefficients) / nrow(RO_Census_Iasi)), 
         Rezid_Std = rstudent(IS_lav_lm), Leverage = hatvalues(IS_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IS_lav_lm)), 
         DFBETA = dfbetas(IS_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Iasi)))
IS_lav_whtest <- white(IS_lav_lm, interactions = TRUE)
IS_lav_bptest <- bptest(IS_lav_lm, studentize = TRUE)
IS_lav_DWtest <- dwtest(IS_lav_lm)
IS_lav_bgtest <- bgtest(IS_lav_lm, order = 1)
IS_lav_jbtest <- jarque.bera.test(resid(IS_lav_lm))
IS_lav_shtest <- shapiro.test(resid(IS_lav_lm))

# Data Distributions
RO_Census_Iasi %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Iasi$Rank, 
                              Population = IS_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.35,
            label = paste0("y", "==", signif(exp(IS_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(IS_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.35,
            label = paste0("R^2 ==", signif(summary(IS_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Iasi$Rank, 
                              Population = IS_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.30,
            label = paste0("y", "==", signif(exp(IS_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(IS_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.30,
            label = paste0("R^2 ==", signif(summary(IS_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Iasi$Rank, 
                              Population = IS_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.25,
            label = paste0("y", "==", signif(IS_ZM_cst, 4), "%.%", "(",
                           signif(IS_ZM_m, 4), "+ x)^", signif(IS_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.25,
            label = paste0("R^2 ==", signif(summary(IS_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Iasi$Rank, 
                              Population = IS_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.20,
            label = paste0("y", "==", signif(IS_lav_kst, 4), "%.%", "x^",
                           -signif(IS_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.20,
            label = paste0("R^2 ==", signif(summary(IS_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Iasi judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Iasi %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Iasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Iasi %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Iasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Iasi %>%
     ggplot(aes(x = log(Rank + IS_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Iasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Iasi %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Iasi judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 25. Ilfov
RO_Census_Ilfov <- RO_Census_rank %>% filter(Judet_Name == "ILFOV")
#View(RO_Census_Ilfov)

# Power Law
IF_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Ilfov)
summary(IF_pw_lm)
IF_pred_pw_lm <- exp(predict(IF_pw_lm, newdata = RO_Census_Ilfov, 
                             interval = "prediction", level = 0.95))
IF_pw_out <- RO_Census_Ilfov %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IF_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Ilfov), 
         DFFIT = dffits(IF_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IF_pw_lm$coefficients) / nrow(RO_Census_Ilfov)), 
         Rezid_Std = rstudent(IF_pw_lm), Leverage = hatvalues(IF_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IF_pw_lm)), 
         DFBETA = dfbetas(IF_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Ilfov)))
IF_pw_whtest <- white(IF_pw_lm, interactions = TRUE)
IF_pw_bptest <- bptest(IF_pw_lm, studentize = TRUE)
IF_pw_DWtest <- dwtest(IF_pw_lm)
IF_pw_bgtest <- bgtest(IF_pw_lm, order = 1)
IF_pw_jbtest <- jarque.bera.test(resid(IF_pw_lm))
IF_pw_shtest <- shapiro.test(resid(IF_pw_lm))

# Zipf_Mandelbrot Law
IF_ZM_prm <- get_ZM_Param("RO_Census_Ilfov", "Population", "Rank")
IF_ZM_m <- IF_ZM_prm$m
IF_ZM_alpha <- IF_ZM_prm$alpha_ZM
IF_ZM_cst <- IF_ZM_prm$constant_ZM
IF_ZM_lm <- lm(log(RO_Census_Ilfov$Population) ~ log(RO_Census_Ilfov$Rank + IF_ZM_m))
summary(IF_ZM_lm)
IF_pred_ZM_lm <- exp(predict(IF_ZM_lm, newdata = RO_Census_Ilfov, 
                             interval = "prediction", level = 0.95))
IF_ZM_out <- RO_Census_Ilfov %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IF_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Ilfov), 
         DFFIT = dffits(IF_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IF_ZM_lm$coefficients) / nrow(RO_Census_Ilfov)), 
         Rezid_Std = rstudent(IF_ZM_lm), Leverage = hatvalues(IF_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IF_ZM_lm)), 
         DFBETA = dfbetas(IF_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Ilfov)))
IF_ZM_whtest <- white(IF_ZM_lm, interactions = TRUE)
IF_ZM_bptest <- bptest(IF_ZM_lm, studentize = TRUE)
IF_ZM_DWtest <- dwtest(IF_ZM_lm)
IF_ZM_bgtest <- bgtest(IF_ZM_lm, order = 1)
IF_ZM_jbtest <- jarque.bera.test(resid(IF_ZM_lm))
IF_ZM_shtest <- shapiro.test(resid(IF_ZM_lm))

# Exponential Law
IF_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Ilfov)
summary(IF_exp_lm)
IF_pred_exp_lm <- exp(predict(IF_exp_lm, newdata = RO_Census_Ilfov, 
                              interval = "prediction", level = 0.95))
IF_exp_out <- RO_Census_Ilfov %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IF_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Ilfov), 
         DFFIT = dffits(IF_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IF_exp_lm$coefficients) / nrow(RO_Census_Ilfov)), 
         Rezid_Std = rstudent(IF_exp_lm), Leverage = hatvalues(IF_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IF_exp_lm)), 
         DFBETA = dfbetas(IF_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Ilfov)))
IF_exp_whtest <- white(IF_exp_lm, interactions = TRUE)
IF_exp_bptest <- bptest(IF_exp_lm, studentize = TRUE)
IF_exp_DWtest <- dwtest(IF_exp_lm)
IF_exp_bgtest <- bgtest(IF_exp_lm, order = 1)
IF_exp_jbtest <- jarque.bera.test(resid(IF_exp_lm))
IF_exp_shtest <- shapiro.test(resid(IF_exp_lm))

# Lavalette Function
RO_Census_Ilfov <- RO_Census_Ilfov %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Ilfov) - Rank + 1))
)
IF_lav_lm <- lm(log(RO_Census_Ilfov$Population) ~ RO_Census_Ilfov$Lav_exp)
summary(IF_lav_lm)
IF_lav_kst <- exp(signif(IF_lav_lm$coef[[1]], 4))
IF_lav_chi <- signif(IF_lav_lm$coef[[2]], 4)
IF_pred_lav_lm <- exp(predict(IF_lav_lm, newdata = RO_Census_Ilfov, 
                              interval = "prediction", level = 0.95))
IF_lav_out <- RO_Census_Ilfov %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(IF_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Ilfov), 
         DFFIT = dffits(IF_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(IF_lav_lm$coefficients) / nrow(RO_Census_Ilfov)), 
         Rezid_Std = rstudent(IF_lav_lm), Leverage = hatvalues(IF_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(IF_lav_lm)), 
         DFBETA = dfbetas(IF_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Ilfov)))
IF_lav_whtest <- white(IF_lav_lm, interactions = TRUE)
IF_lav_bptest <- bptest(IF_lav_lm, studentize = TRUE)
IF_lav_DWtest <- dwtest(IF_lav_lm)
IF_lav_bgtest <- bgtest(IF_lav_lm, order = 1)
IF_lav_jbtest <- jarque.bera.test(resid(IF_lav_lm))
IF_lav_shtest <- shapiro.test(resid(IF_lav_lm))

# Data Distributions
RO_Census_Ilfov %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Ilfov$Rank, 
                              Population = IF_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.15,
            label = paste0("y", "==", signif(exp(IF_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(IF_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 30, y = 0.15,
            label = paste0("R^2 ==", signif(summary(IF_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ilfov$Rank, 
                              Population = IF_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.13,
            label = paste0("y", "==", signif(exp(IF_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(IF_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 30, y = 0.13,
            label = paste0("R^2 ==", signif(summary(IF_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ilfov$Rank, 
                              Population = IF_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.11,
            label = paste0("y", "==", signif(IF_ZM_cst, 4), "%.%", "(",
                           signif(IF_ZM_m, 4), "+ x)^", signif(IF_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 30, y = 0.11,
            label = paste0("R^2 ==", signif(summary(IF_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Ilfov$Rank, 
                              Population = IF_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.09,
            label = paste0("y", "==", signif(IF_lav_kst, 4), "%.%", "x^",
                           -signif(IF_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 30, y = 0.09,
            label = paste0("R^2 ==", signif(summary(IF_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Ilfov judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Ilfov %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Ilfov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ilfov %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Ilfov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ilfov %>%
     ggplot(aes(x = log(Rank + IF_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Ilfov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Ilfov %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Ilfov judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 26. Maramures
RO_Census_Maramures <- RO_Census_rank %>% filter(Judet_Name == "MARAMURES")
#View(RO_Census_Maramures)

# Power Law
MM_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Maramures)
summary(MM_pw_lm)
MM_pred_pw_lm <- exp(predict(MM_pw_lm, newdata = RO_Census_Maramures, 
                             interval = "prediction", level = 0.95))
MM_pw_out <- RO_Census_Maramures %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MM_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Maramures), 
         DFFIT = dffits(MM_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MM_pw_lm$coefficients) / nrow(RO_Census_Maramures)), 
         Rezid_Std = rstudent(MM_pw_lm), Leverage = hatvalues(MM_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MM_pw_lm)), 
         DFBETA = dfbetas(MM_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Maramures)))
MM_pw_whtest <- white(MM_pw_lm, interactions = TRUE)
MM_pw_bptest <- bptest(MM_pw_lm, studentize = TRUE)
MM_pw_DWtest <- dwtest(MM_pw_lm)
MM_pw_bgtest <- bgtest(MM_pw_lm, order = 1)
MM_pw_jbtest <- jarque.bera.test(resid(MM_pw_lm))
MM_pw_shtest <- shapiro.test(resid(MM_pw_lm))

# Zipf_Mandelbrot Law
MM_ZM_prm <- get_ZM_Param("RO_Census_Maramures", "Population", "Rank")
MM_ZM_m <- MM_ZM_prm$m
MM_ZM_alpha <- MM_ZM_prm$alpha_ZM
MM_ZM_cst <- MM_ZM_prm$constant_ZM
MM_ZM_lm <- lm(log(RO_Census_Maramures$Population) ~ log(RO_Census_Maramures$Rank + MM_ZM_m))
summary(MM_ZM_lm)
MM_pred_ZM_lm <- exp(predict(MM_ZM_lm, newdata = RO_Census_Maramures, 
                             interval = "prediction", level = 0.95))
MM_ZM_out <- RO_Census_Maramures %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MM_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Maramures), 
         DFFIT = dffits(MM_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MM_ZM_lm$coefficients) / nrow(RO_Census_Maramures)), 
         Rezid_Std = rstudent(MM_ZM_lm), Leverage = hatvalues(MM_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MM_ZM_lm)), 
         DFBETA = dfbetas(MM_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Maramures)))
MM_ZM_whtest <- white(MM_ZM_lm, interactions = TRUE)
MM_ZM_bptest <- bptest(MM_ZM_lm, studentize = TRUE)
MM_ZM_DWtest <- dwtest(MM_ZM_lm)
MM_ZM_bgtest <- bgtest(MM_ZM_lm, order = 1)
MM_ZM_jbtest <- jarque.bera.test(resid(MM_ZM_lm))
MM_ZM_shtest <- shapiro.test(resid(MM_ZM_lm))

# Exponential Law
MM_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Maramures)
summary(MM_exp_lm)
MM_pred_exp_lm <- exp(predict(MM_exp_lm, newdata = RO_Census_Maramures, 
                              interval = "prediction", level = 0.95))
MM_exp_out <- RO_Census_Maramures %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MM_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Maramures), 
         DFFIT = dffits(MM_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MM_exp_lm$coefficients) / nrow(RO_Census_Maramures)), 
         Rezid_Std = rstudent(MM_exp_lm), Leverage = hatvalues(MM_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MM_exp_lm)), 
         DFBETA = dfbetas(MM_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Maramures)))
MM_exp_whtest <- white(MM_exp_lm, interactions = TRUE)
MM_exp_bptest <- bptest(MM_exp_lm, studentize = TRUE)
MM_exp_DWtest <- dwtest(MM_exp_lm)
MM_exp_bgtest <- bgtest(MM_exp_lm, order = 1)
MM_exp_jbtest <- jarque.bera.test(resid(MM_exp_lm))
MM_exp_shtest <- shapiro.test(resid(MM_exp_lm))

# Lavalette Function
RO_Census_Maramures <- RO_Census_Maramures %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Maramures) - Rank + 1))
)
MM_lav_lm <- lm(log(RO_Census_Maramures$Population) ~ RO_Census_Maramures$Lav_exp)
summary(MM_lav_lm)
MM_lav_kst <- exp(signif(MM_lav_lm$coef[[1]], 4))
MM_lav_chi <- signif(MM_lav_lm$coef[[2]], 4)
MM_pred_lav_lm <- exp(predict(MM_lav_lm, newdata = RO_Census_Maramures, 
                              interval = "prediction", level = 0.95))
MM_lav_out <- RO_Census_Maramures %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MM_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Maramures), 
         DFFIT = dffits(MM_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MM_lav_lm$coefficients) / nrow(RO_Census_Maramures)), 
         Rezid_Std = rstudent(MM_lav_lm), Leverage = hatvalues(MM_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MM_lav_lm)), 
         DFBETA = dfbetas(MM_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Maramures)))
MM_lav_whtest <- white(MM_lav_lm, interactions = TRUE)
MM_lav_bptest <- bptest(MM_lav_lm, studentize = TRUE)
MM_lav_DWtest <- dwtest(MM_lav_lm)
MM_lav_bgtest <- bgtest(MM_lav_lm, order = 1)
MM_lav_jbtest <- jarque.bera.test(resid(MM_lav_lm))
MM_lav_shtest <- shapiro.test(resid(MM_lav_lm))

# Data Distributions
RO_Census_Maramures %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Maramures$Rank, 
                              Population = MM_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.24,
            label = paste0("y", "==", signif(exp(MM_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(MM_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.24,
            label = paste0("R^2 ==", signif(summary(MM_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Maramures$Rank, 
                              Population = MM_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.22,
            label = paste0("y", "==", signif(exp(MM_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(MM_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.22,
            label = paste0("R^2 ==", signif(summary(MM_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Maramures$Rank, 
                              Population = MM_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.20,
            label = paste0("y", "==", signif(MM_ZM_cst, 4), "%.%", "(",
                           signif(MM_ZM_m, 4), "+ x)^", signif(MM_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.20,
            label = paste0("R^2 ==", signif(summary(MM_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Maramures$Rank, 
                              Population = MM_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.18,
            label = paste0("y", "==", signif(MM_lav_kst, 4), "%.%", "x^",
                           -signif(MM_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.18,
            label = paste0("R^2 ==", signif(summary(MM_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Maramures judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Maramures %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Maramures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Maramures %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Maramures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Maramures %>%
     ggplot(aes(x = log(Rank + MM_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Maramures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Maramures %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Maramures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 27. Mehedinti
RO_Census_Mehedinti <- RO_Census_rank %>% filter(Judet_Name == "MEHEDINTI")
#View(RO_Census_Mehedinti)

# Power Law
MH_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Mehedinti)
summary(MH_pw_lm)
MH_pred_pw_lm <- exp(predict(MH_pw_lm, newdata = RO_Census_Mehedinti, 
                             interval = "prediction", level = 0.95))
MH_pw_out <- RO_Census_Mehedinti %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MH_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Mehedinti), 
         DFFIT = dffits(MH_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MH_pw_lm$coefficients) / nrow(RO_Census_Mehedinti)), 
         Rezid_Std = rstudent(MH_pw_lm), Leverage = hatvalues(MH_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MH_pw_lm)), 
         DFBETA = dfbetas(MH_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Mehedinti)))
MH_pw_whtest <- white(MH_pw_lm, interactions = TRUE)
MH_pw_bptest <- bptest(MH_pw_lm, studentize = TRUE)
MH_pw_DWtest <- dwtest(MH_pw_lm)
MH_pw_bgtest <- bgtest(MH_pw_lm, order = 1)
MH_pw_jbtest <- jarque.bera.test(resid(MH_pw_lm))
MH_pw_shtest <- shapiro.test(resid(MH_pw_lm))

# Zipf_Mandelbrot Law
MH_ZM_prm <- get_ZM_Param("RO_Census_Mehedinti", "Population", "Rank")
MH_ZM_m <- MH_ZM_prm$m
MH_ZM_alpha <- MH_ZM_prm$alpha_ZM
MH_ZM_cst <- MH_ZM_prm$constant_ZM
MH_ZM_lm <- lm(log(RO_Census_Mehedinti$Population) ~ log(RO_Census_Mehedinti$Rank + MH_ZM_m))
summary(MH_ZM_lm)
MH_pred_ZM_lm <- exp(predict(MH_ZM_lm, newdata = RO_Census_Mehedinti, 
                             interval = "prediction", level = 0.95))
MH_ZM_out <- RO_Census_Mehedinti %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MH_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Mehedinti), 
         DFFIT = dffits(MH_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MH_ZM_lm$coefficients) / nrow(RO_Census_Mehedinti)), 
         Rezid_Std = rstudent(MH_ZM_lm), Leverage = hatvalues(MH_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MH_ZM_lm)), 
         DFBETA = dfbetas(MH_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Mehedinti)))
MH_ZM_whtest <- white(MH_ZM_lm, interactions = TRUE)
MH_ZM_bptest <- bptest(MH_ZM_lm, studentize = TRUE)
MH_ZM_DWtest <- dwtest(MH_ZM_lm)
MH_ZM_bgtest <- bgtest(MH_ZM_lm, order = 1)
MH_ZM_jbtest <- jarque.bera.test(resid(MH_ZM_lm))
MH_ZM_shtest <- shapiro.test(resid(MH_ZM_lm))

# Exponential Law
MH_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Mehedinti)
summary(MH_exp_lm)
MH_pred_exp_lm <- exp(predict(MH_exp_lm, newdata = RO_Census_Mehedinti, 
                              interval = "prediction", level = 0.95))
MH_exp_out <- RO_Census_Mehedinti %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MH_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Mehedinti), 
         DFFIT = dffits(MH_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MH_exp_lm$coefficients) / nrow(RO_Census_Mehedinti)), 
         Rezid_Std = rstudent(MH_exp_lm), Leverage = hatvalues(MH_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MH_exp_lm)), 
         DFBETA = dfbetas(MH_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Mehedinti)))
MH_exp_whtest <- white(MH_exp_lm, interactions = TRUE)
MH_exp_bptest <- bptest(MH_exp_lm, studentize = TRUE)
MH_exp_DWtest <- dwtest(MH_exp_lm)
MH_exp_bgtest <- bgtest(MH_exp_lm, order = 1)
MH_exp_jbtest <- jarque.bera.test(resid(MH_exp_lm))
MH_exp_shtest <- shapiro.test(resid(MH_exp_lm))

# Lavalette Function
RO_Census_Mehedinti <- RO_Census_Mehedinti %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Mehedinti) - Rank + 1))
)
MH_lav_lm <- lm(log(RO_Census_Mehedinti$Population) ~ RO_Census_Mehedinti$Lav_exp)
summary(MH_lav_lm)
MH_lav_kst <- exp(signif(MH_lav_lm$coef[[1]], 4))
MH_lav_chi <- signif(MH_lav_lm$coef[[2]], 4)
MH_pred_lav_lm <- exp(predict(MH_lav_lm, newdata = RO_Census_Mehedinti, 
                              interval = "prediction", level = 0.95))
MH_lav_out <- RO_Census_Mehedinti %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MH_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Mehedinti), 
         DFFIT = dffits(MH_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MH_lav_lm$coefficients) / nrow(RO_Census_Mehedinti)), 
         Rezid_Std = rstudent(MH_lav_lm), Leverage = hatvalues(MH_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MH_lav_lm)), 
         DFBETA = dfbetas(MH_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Mehedinti)))
MH_lav_whtest <- white(MH_lav_lm, interactions = TRUE)
MH_lav_bptest <- bptest(MH_lav_lm, studentize = TRUE)
MH_lav_DWtest <- dwtest(MH_lav_lm)
MH_lav_bgtest <- bgtest(MH_lav_lm, order = 1)
MH_lav_jbtest <- jarque.bera.test(resid(MH_lav_lm))
MH_lav_shtest <- shapiro.test(resid(MH_lav_lm))

# Data Distributions
RO_Census_Mehedinti %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Mehedinti$Rank, 
                              Population = MH_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.34,
            label = paste0("y", "==", signif(exp(MH_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(MH_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 45, y = 0.34,
            label = paste0("R^2 ==", signif(summary(MH_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mehedinti$Rank, 
                              Population = MH_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.32,
            label = paste0("y", "==", signif(exp(MH_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(MH_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 45, y = 0.32,
            label = paste0("R^2 ==", signif(summary(MH_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mehedinti$Rank, 
                              Population = MH_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.30,
            label = paste0("y", "==", signif(MH_ZM_cst, 4), "%.%", "(",
                           signif(MH_ZM_m, 4), "+ x)^", signif(MH_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 45, y = 0.30,
            label = paste0("R^2 ==", signif(summary(MH_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mehedinti$Rank, 
                              Population = MH_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.28,
            label = paste0("y", "==", signif(MH_lav_kst, 4), "%.%", "x^",
                           -signif(MH_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 45, y = 0.28,
            label = paste0("R^2 ==", signif(summary(MH_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Mehedinti judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Mehedinti %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Mehedinti judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mehedinti %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Mehedinti judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mehedinti %>%
     ggplot(aes(x = log(Rank + MH_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Mehedinti judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mehedinti %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Mehedinti judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 28. Mures
RO_Census_Mures <- RO_Census_rank %>% filter(Judet_Name == "MURES")
#View(RO_Census_Mures)

# Power Law
MS_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Mures)
summary(MS_pw_lm)
MS_pred_pw_lm <- exp(predict(MS_pw_lm, newdata = RO_Census_Mures, 
                             interval = "prediction", level = 0.95))
MS_pw_out <- RO_Census_Mures %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MS_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Mures), 
         DFFIT = dffits(MS_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MS_pw_lm$coefficients) / nrow(RO_Census_Mures)), 
         Rezid_Std = rstudent(MS_pw_lm), Leverage = hatvalues(MS_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MS_pw_lm)), 
         DFBETA = dfbetas(MS_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Mures)))
MS_pw_whtest <- white(MS_pw_lm, interactions = TRUE)
MS_pw_bptest <- bptest(MS_pw_lm, studentize = TRUE)
MS_pw_DWtest <- dwtest(MS_pw_lm)
MS_pw_bgtest <- bgtest(MS_pw_lm, order = 1)
MS_pw_jbtest <- jarque.bera.test(resid(MS_pw_lm))
MS_pw_shtest <- shapiro.test(resid(MS_pw_lm))

# Zipf_Mandelbrot Law
MS_ZM_prm <- get_ZM_Param("RO_Census_Mures", "Population", "Rank")
MS_ZM_m <- MS_ZM_prm$m
MS_ZM_alpha <- MS_ZM_prm$alpha_ZM
MS_ZM_cst <- MS_ZM_prm$constant_ZM
MS_ZM_lm <- lm(log(RO_Census_Mures$Population) ~ log(RO_Census_Mures$Rank + MS_ZM_m))
summary(MS_ZM_lm)
MS_pred_ZM_lm <- exp(predict(MS_ZM_lm, newdata = RO_Census_Mures, 
                             interval = "prediction", level = 0.95))
MS_ZM_out <- RO_Census_Mures %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MS_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Mures), 
         DFFIT = dffits(MS_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MS_ZM_lm$coefficients) / nrow(RO_Census_Mures)), 
         Rezid_Std = rstudent(MS_ZM_lm), Leverage = hatvalues(MS_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MS_ZM_lm)), 
         DFBETA = dfbetas(MS_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Mures)))
MS_ZM_whtest <- white(MS_ZM_lm, interactions = TRUE)
MS_ZM_bptest <- bptest(MS_ZM_lm, studentize = TRUE)
MS_ZM_DWtest <- dwtest(MS_ZM_lm)
MS_ZM_bgtest <- bgtest(MS_ZM_lm, order = 1)
MS_ZM_jbtest <- jarque.bera.test(resid(MS_ZM_lm))
MS_ZM_shtest <- shapiro.test(resid(MS_ZM_lm))

# Exponential Law
MS_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Mures)
summary(MS_exp_lm)
MS_pred_exp_lm <- exp(predict(MS_exp_lm, newdata = RO_Census_Mures, 
                              interval = "prediction", level = 0.95))
MS_exp_out <- RO_Census_Mures %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MS_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Mures), 
         DFFIT = dffits(MS_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MS_exp_lm$coefficients) / nrow(RO_Census_Mures)), 
         Rezid_Std = rstudent(MS_exp_lm), Leverage = hatvalues(MS_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MS_exp_lm)), 
         DFBETA = dfbetas(MS_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Mures)))
MS_exp_whtest <- white(MS_exp_lm, interactions = TRUE)
MS_exp_bptest <- bptest(MS_exp_lm, studentize = TRUE)
MS_exp_DWtest <- dwtest(MS_exp_lm)
MS_exp_bgtest <- bgtest(MS_exp_lm, order = 1)
MS_exp_jbtest <- jarque.bera.test(resid(MS_exp_lm))
MS_exp_shtest <- shapiro.test(resid(MS_exp_lm))

# Lavalette Function
RO_Census_Mures <- RO_Census_Mures %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Mures) - Rank + 1))
)
MS_lav_lm <- lm(log(RO_Census_Mures$Population) ~ RO_Census_Mures$Lav_exp)
summary(MS_lav_lm)
MS_lav_kst <- exp(signif(MS_lav_lm$coef[[1]], 4))
MS_lav_chi <- signif(MS_lav_lm$coef[[2]], 4)
MS_pred_lav_lm <- exp(predict(MS_lav_lm, newdata = RO_Census_Mures, 
                              interval = "prediction", level = 0.95))
MS_lav_out <- RO_Census_Mures %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(MS_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Mures), 
         DFFIT = dffits(MS_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(MS_lav_lm$coefficients) / nrow(RO_Census_Mures)), 
         Rezid_Std = rstudent(MS_lav_lm), Leverage = hatvalues(MS_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(MS_lav_lm)), 
         DFBETA = dfbetas(MS_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Mures)))
MS_lav_whtest <- white(MS_lav_lm, interactions = TRUE)
MS_lav_bptest <- bptest(MS_lav_lm, studentize = TRUE)
MS_lav_DWtest <- dwtest(MS_lav_lm)
MS_lav_bgtest <- bgtest(MS_lav_lm, order = 1)
MS_lav_jbtest <- jarque.bera.test(resid(MS_lav_lm))
MS_lav_shtest <- shapiro.test(resid(MS_lav_lm))

# Data Distributions
RO_Census_Mures %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Mures$Rank, 
                              Population = MS_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.22,
            label = paste0("y", "==", signif(exp(MS_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(MS_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.22,
            label = paste0("R^2 ==", signif(summary(MS_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mures$Rank, 
                              Population = MS_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.20,
            label = paste0("y", "==", signif(exp(MS_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(MS_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.20,
            label = paste0("R^2 ==", signif(summary(MS_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mures$Rank, 
                              Population = MS_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.18,
            label = paste0("y", "==", signif(MS_ZM_cst, 4), "%.%", "(",
                           signif(MS_ZM_m, 4), "+ x)^", signif(MS_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.18,
            label = paste0("R^2 ==", signif(summary(MS_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Mures$Rank, 
                              Population = MS_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.16,
            label = paste0("y", "==", signif(MS_lav_kst, 4), "%.%", "x^",
                           -signif(MS_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.16,
            label = paste0("R^2 ==", signif(summary(MS_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Mures judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Mures %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Mures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mures %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Mures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mures %>%
     ggplot(aes(x = log(Rank + MS_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Mures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Mures %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Mures judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 29. Neamt
RO_Census_Neamt <- RO_Census_rank %>% filter(Judet_Name == "NEAMT")
#View(RO_Census_Neamt)

# Power Law
NT_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Neamt)
summary(NT_pw_lm)
NT_pred_pw_lm <- exp(predict(NT_pw_lm, newdata = RO_Census_Neamt, 
                             interval = "prediction", level = 0.95))
NT_pw_out <- RO_Census_Neamt %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(NT_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Neamt), 
         DFFIT = dffits(NT_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(NT_pw_lm$coefficients) / nrow(RO_Census_Neamt)), 
         Rezid_Std = rstudent(NT_pw_lm), Leverage = hatvalues(NT_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(NT_pw_lm)), 
         DFBETA = dfbetas(NT_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Neamt)))
NT_pw_whtest <- white(NT_pw_lm, interactions = TRUE)
NT_pw_bptest <- bptest(NT_pw_lm, studentize = TRUE)
NT_pw_DWtest <- dwtest(NT_pw_lm)
NT_pw_bgtest <- bgtest(NT_pw_lm, order = 1)
NT_pw_jbtest <- jarque.bera.test(resid(NT_pw_lm))
NT_pw_shtest <- shapiro.test(resid(NT_pw_lm))

# Zipf_Mandelbrot Law
NT_ZM_prm <- get_ZM_Param("RO_Census_Neamt", "Population", "Rank")
NT_ZM_m <- NT_ZM_prm$m
NT_ZM_alpha <- NT_ZM_prm$alpha_ZM
NT_ZM_cst <- NT_ZM_prm$constant_ZM
NT_ZM_lm <- lm(log(RO_Census_Neamt$Population) ~ log(RO_Census_Neamt$Rank + NT_ZM_m))
summary(NT_ZM_lm)
NT_pred_ZM_lm <- exp(predict(NT_ZM_lm, newdata = RO_Census_Neamt, 
                             interval = "prediction", level = 0.95))
NT_ZM_out <- RO_Census_Neamt %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(NT_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Neamt), 
         DFFIT = dffits(NT_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(NT_ZM_lm$coefficients) / nrow(RO_Census_Neamt)), 
         Rezid_Std = rstudent(NT_ZM_lm), Leverage = hatvalues(NT_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(NT_ZM_lm)), 
         DFBETA = dfbetas(NT_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Neamt)))
NT_ZM_whtest <- white(NT_ZM_lm, interactions = TRUE)
NT_ZM_bptest <- bptest(NT_ZM_lm, studentize = TRUE)
NT_ZM_DWtest <- dwtest(NT_ZM_lm)
NT_ZM_bgtest <- bgtest(NT_ZM_lm, order = 1)
NT_ZM_jbtest <- jarque.bera.test(resid(NT_ZM_lm))
NT_ZM_shtest <- shapiro.test(resid(NT_ZM_lm))

# Exponential Law
NT_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Neamt)
summary(NT_exp_lm)
NT_pred_exp_lm <- exp(predict(NT_exp_lm, newdata = RO_Census_Neamt, 
                              interval = "prediction", level = 0.95))
NT_exp_out <- RO_Census_Neamt %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(NT_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Neamt), 
         DFFIT = dffits(NT_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(NT_exp_lm$coefficients) / nrow(RO_Census_Neamt)), 
         Rezid_Std = rstudent(NT_exp_lm), Leverage = hatvalues(NT_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(NT_exp_lm)), 
         DFBETA = dfbetas(NT_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Neamt)))
NT_exp_whtest <- white(NT_exp_lm, interactions = TRUE)
NT_exp_bptest <- bptest(NT_exp_lm, studentize = TRUE)
NT_exp_DWtest <- dwtest(NT_exp_lm)
NT_exp_bgtest <- bgtest(NT_exp_lm, order = 1)
NT_exp_jbtest <- jarque.bera.test(resid(NT_exp_lm))
NT_exp_shtest <- shapiro.test(resid(NT_exp_lm))

# Lavalette Function
RO_Census_Neamt <- RO_Census_Neamt %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Neamt) - Rank + 1))
)
NT_lav_lm <- lm(log(RO_Census_Neamt$Population) ~ RO_Census_Neamt$Lav_exp)
summary(NT_lav_lm)
NT_lav_kst <- exp(signif(NT_lav_lm$coef[[1]], 4))
NT_lav_chi <- signif(NT_lav_lm$coef[[2]], 4)
NT_pred_lav_lm <- exp(predict(NT_lav_lm, newdata = RO_Census_Neamt, 
                              interval = "prediction", level = 0.95))
NT_lav_out <- RO_Census_Neamt %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(NT_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Neamt), 
         DFFIT = dffits(NT_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(NT_lav_lm$coefficients) / nrow(RO_Census_Neamt)), 
         Rezid_Std = rstudent(NT_lav_lm), Leverage = hatvalues(NT_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(NT_lav_lm)), 
         DFBETA = dfbetas(NT_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Neamt)))
NT_lav_whtest <- white(NT_lav_lm, interactions = TRUE)
NT_lav_bptest <- bptest(NT_lav_lm, studentize = TRUE)
NT_lav_DWtest <- dwtest(NT_lav_lm)
NT_lav_bgtest <- bgtest(NT_lav_lm, order = 1)
NT_lav_jbtest <- jarque.bera.test(resid(NT_lav_lm))
NT_lav_shtest <- shapiro.test(resid(NT_lav_lm))

# Data Distributions
RO_Census_Neamt %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Neamt$Rank, 
                              Population = NT_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.17,
            label = paste0("y", "==", signif(exp(NT_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(NT_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.17,
            label = paste0("R^2 ==", signif(summary(NT_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Neamt$Rank, 
                              Population = NT_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.15,
            label = paste0("y", "==", signif(exp(NT_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(NT_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.15,
            label = paste0("R^2 ==", signif(summary(NT_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Neamt$Rank, 
                              Population = NT_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.13,
            label = paste0("y", "==", signif(NT_ZM_cst, 4), "%.%", "(",
                           signif(NT_ZM_m, 4), "+ x)^", signif(NT_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.13,
            label = paste0("R^2 ==", signif(summary(NT_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Neamt$Rank, 
                              Population = NT_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.11,
            label = paste0("y", "==", signif(NT_lav_kst, 4), "%.%", "x^",
                           -signif(NT_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.11,
            label = paste0("R^2 ==", signif(summary(NT_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Neamt judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Neamt %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Neamt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Neamt %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Neamt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Neamt %>%
     ggplot(aes(x = log(Rank + NT_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Neamt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Neamt %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Neamt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 30. Olt
RO_Census_Olt <- RO_Census_rank %>% filter(Judet_Name == "OLT")
#View(RO_Census_Olt)

# Power Law
OT_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Olt)
summary(OT_pw_lm)
OT_pred_pw_lm <- exp(predict(OT_pw_lm, newdata = RO_Census_Olt, 
                             interval = "prediction", level = 0.95))
OT_pw_out <- RO_Census_Olt %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(OT_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Olt), 
         DFFIT = dffits(OT_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(OT_pw_lm$coefficients) / nrow(RO_Census_Olt)), 
         Rezid_Std = rstudent(OT_pw_lm), Leverage = hatvalues(OT_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(OT_pw_lm)), 
         DFBETA = dfbetas(OT_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Olt)))
OT_pw_whtest <- white(OT_pw_lm, interactions = TRUE)
OT_pw_bptest <- bptest(OT_pw_lm, studentize = TRUE)
OT_pw_DWtest <- dwtest(OT_pw_lm)
OT_pw_bgtest <- bgtest(OT_pw_lm, order = 1)
OT_pw_jbtest <- jarque.bera.test(resid(OT_pw_lm))
OT_pw_shtest <- shapiro.test(resid(OT_pw_lm))

# Zipf_Mandelbrot Law
OT_ZM_prm <- get_ZM_Param("RO_Census_Olt", "Population", "Rank")
OT_ZM_m <- OT_ZM_prm$m
OT_ZM_alpha <- OT_ZM_prm$alpha_ZM
OT_ZM_cst <- OT_ZM_prm$constant_ZM
OT_ZM_lm <- lm(log(RO_Census_Olt$Population) ~ log(RO_Census_Olt$Rank + OT_ZM_m))
summary(OT_ZM_lm)
OT_pred_ZM_lm <- exp(predict(OT_ZM_lm, newdata = RO_Census_Olt, 
                             interval = "prediction", level = 0.95))
OT_ZM_out <- RO_Census_Olt %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(OT_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Olt), 
         DFFIT = dffits(OT_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(OT_ZM_lm$coefficients) / nrow(RO_Census_Olt)), 
         Rezid_Std = rstudent(OT_ZM_lm), Leverage = hatvalues(OT_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(OT_ZM_lm)), 
         DFBETA = dfbetas(OT_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Olt)))
OT_ZM_whtest <- white(OT_ZM_lm, interactions = TRUE)
OT_ZM_bptest <- bptest(OT_ZM_lm, studentize = TRUE)
OT_ZM_DWtest <- dwtest(OT_ZM_lm)
OT_ZM_bgtest <- bgtest(OT_ZM_lm, order = 1)
OT_ZM_jbtest <- jarque.bera.test(resid(OT_ZM_lm))
OT_ZM_shtest <- shapiro.test(resid(OT_ZM_lm))

# Exponential Law
OT_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Olt)
summary(OT_exp_lm)
OT_pred_exp_lm <- exp(predict(OT_exp_lm, newdata = RO_Census_Olt, 
                              interval = "prediction", level = 0.95))
OT_exp_out <- RO_Census_Olt %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(OT_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Olt), 
         DFFIT = dffits(OT_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(OT_exp_lm$coefficients) / nrow(RO_Census_Olt)), 
         Rezid_Std = rstudent(OT_exp_lm), Leverage = hatvalues(OT_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(OT_exp_lm)), 
         DFBETA = dfbetas(OT_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Olt)))
OT_exp_whtest <- white(OT_exp_lm, interactions = TRUE)
OT_exp_bptest <- bptest(OT_exp_lm, studentize = TRUE)
OT_exp_DWtest <- dwtest(OT_exp_lm)
OT_exp_bgtest <- bgtest(OT_exp_lm, order = 1)
OT_exp_jbtest <- jarque.bera.test(resid(OT_exp_lm))
OT_exp_shtest <- shapiro.test(resid(OT_exp_lm))

# Lavalette Function
RO_Census_Olt <- RO_Census_Olt %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Olt) - Rank + 1))
)
OT_lav_lm <- lm(log(RO_Census_Olt$Population) ~ RO_Census_Olt$Lav_exp)
summary(OT_lav_lm)
OT_lav_kst <- exp(signif(OT_lav_lm$coef[[1]], 4))
OT_lav_chi <- signif(OT_lav_lm$coef[[2]], 4)
OT_pred_lav_lm <- exp(predict(OT_lav_lm, newdata = RO_Census_Olt, 
                              interval = "prediction", level = 0.95))
OT_lav_out <- RO_Census_Olt %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(OT_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Olt), 
         DFFIT = dffits(OT_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(OT_lav_lm$coefficients) / nrow(RO_Census_Olt)), 
         Rezid_Std = rstudent(OT_lav_lm), Leverage = hatvalues(OT_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(OT_lav_lm)), 
         DFBETA = dfbetas(OT_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Olt)))
OT_lav_whtest <- white(OT_lav_lm, interactions = TRUE)
OT_lav_bptest <- bptest(OT_lav_lm, studentize = TRUE)
OT_lav_DWtest <- dwtest(OT_lav_lm)
OT_lav_bgtest <- bgtest(OT_lav_lm, order = 1)
OT_lav_jbtest <- jarque.bera.test(resid(OT_lav_lm))
OT_lav_shtest <- shapiro.test(resid(OT_lav_lm))

# Data Distributions
RO_Census_Olt %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Olt$Rank, 
                              Population = OT_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.16,
            label = paste0("y", "==", signif(exp(OT_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(OT_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 65, y = 0.16,
            label = paste0("R^2 ==", signif(summary(OT_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Olt$Rank, 
                              Population = OT_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.14,
            label = paste0("y", "==", signif(exp(OT_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(OT_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 65, y = 0.14,
            label = paste0("R^2 ==", signif(summary(OT_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Olt$Rank, 
                              Population = OT_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.12,
            label = paste0("y", "==", signif(OT_ZM_cst, 4), "%.%", "(",
                           signif(OT_ZM_m, 4), "+ x)^", signif(OT_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 65, y = 0.12,
            label = paste0("R^2 ==", signif(summary(OT_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Olt$Rank, 
                              Population = OT_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.10,
            label = paste0("y", "==", signif(OT_lav_kst, 4), "%.%", "x^",
                           -signif(OT_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 65, y = 0.10,
            label = paste0("R^2 ==", signif(summary(OT_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Olt judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Olt %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Olt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Olt %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Olt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Olt %>%
     ggplot(aes(x = log(Rank + OT_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Olt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Olt %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Olt judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 31. Prahova
RO_Census_Prahova <- RO_Census_rank %>% filter(Judet_Name == "PRAHOVA")
#View(RO_Census_Prahova)

# Power Law
PH_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Prahova)
summary(PH_pw_lm)
PH_pred_pw_lm <- exp(predict(PH_pw_lm, newdata = RO_Census_Prahova, 
                             interval = "prediction", level = 0.95))
PH_pw_out <- RO_Census_Prahova %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(PH_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Prahova), 
         DFFIT = dffits(PH_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(PH_pw_lm$coefficients) / nrow(RO_Census_Prahova)), 
         Rezid_Std = rstudent(PH_pw_lm), Leverage = hatvalues(PH_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(PH_pw_lm)), 
         DFBETA = dfbetas(PH_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Prahova)))
PH_pw_whtest <- white(PH_pw_lm, interactions = TRUE)
PH_pw_bptest <- bptest(PH_pw_lm, studentize = TRUE)
PH_pw_DWtest <- dwtest(PH_pw_lm)
PH_pw_bgtest <- bgtest(PH_pw_lm, order = 1)
PH_pw_jbtest <- jarque.bera.test(resid(PH_pw_lm))
PH_pw_shtest <- shapiro.test(resid(PH_pw_lm))

# Zipf_Mandelbrot Law
PH_ZM_prm <- get_ZM_Param("RO_Census_Prahova", "Population", "Rank")
PH_ZM_m <- PH_ZM_prm$m
PH_ZM_alpha <- PH_ZM_prm$alpha_ZM
PH_ZM_cst <- PH_ZM_prm$constant_ZM
PH_ZM_lm <- lm(log(RO_Census_Prahova$Population) ~ log(RO_Census_Prahova$Rank + PH_ZM_m))
summary(PH_ZM_lm)
PH_pred_ZM_lm <- exp(predict(PH_ZM_lm, newdata = RO_Census_Prahova, 
                             interval = "prediction", level = 0.95))
PH_ZM_out <- RO_Census_Prahova %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(PH_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Prahova), 
         DFFIT = dffits(PH_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(PH_ZM_lm$coefficients) / nrow(RO_Census_Prahova)), 
         Rezid_Std = rstudent(PH_ZM_lm), Leverage = hatvalues(PH_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(PH_ZM_lm)), 
         DFBETA = dfbetas(PH_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Prahova)))
PH_ZM_whtest <- white(PH_ZM_lm, interactions = TRUE)
PH_ZM_bptest <- bptest(PH_ZM_lm, studentize = TRUE)
PH_ZM_DWtest <- dwtest(PH_ZM_lm)
PH_ZM_bgtest <- bgtest(PH_ZM_lm, order = 1)
PH_ZM_jbtest <- jarque.bera.test(resid(PH_ZM_lm))
PH_ZM_shtest <- shapiro.test(resid(PH_ZM_lm))

# Exponential Law
PH_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Prahova)
summary(PH_exp_lm)
PH_pred_exp_lm <- exp(predict(PH_exp_lm, newdata = RO_Census_Prahova, 
                              interval = "prediction", level = 0.95))
PH_exp_out <- RO_Census_Prahova %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(PH_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Prahova), 
         DFFIT = dffits(PH_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(PH_exp_lm$coefficients) / nrow(RO_Census_Prahova)), 
         Rezid_Std = rstudent(PH_exp_lm), Leverage = hatvalues(PH_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(PH_exp_lm)), 
         DFBETA = dfbetas(PH_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Prahova)))
PH_exp_whtest <- white(PH_exp_lm, interactions = TRUE)
PH_exp_bptest <- bptest(PH_exp_lm, studentize = TRUE)
PH_exp_DWtest <- dwtest(PH_exp_lm)
PH_exp_bgtest <- bgtest(PH_exp_lm, order = 1)
PH_exp_jbtest <- jarque.bera.test(resid(PH_exp_lm))
PH_exp_shtest <- shapiro.test(resid(PH_exp_lm))

# Lavalette Function
RO_Census_Prahova <- RO_Census_Prahova %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Prahova) - Rank + 1))
)
PH_lav_lm <- lm(log(RO_Census_Prahova$Population) ~ RO_Census_Prahova$Lav_exp)
summary(PH_lav_lm)
PH_lav_kst <- exp(signif(PH_lav_lm$coef[[1]], 4))
PH_lav_chi <- signif(PH_lav_lm$coef[[2]], 4)
PH_pred_lav_lm <- exp(predict(PH_lav_lm, newdata = RO_Census_Prahova, 
                              interval = "prediction", level = 0.95))
PH_lav_out <- RO_Census_Prahova %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(PH_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Prahova), 
         DFFIT = dffits(PH_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(PH_lav_lm$coefficients) / nrow(RO_Census_Prahova)), 
         Rezid_Std = rstudent(PH_lav_lm), Leverage = hatvalues(PH_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(PH_lav_lm)), 
         DFBETA = dfbetas(PH_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Prahova)))
PH_lav_whtest <- white(PH_lav_lm, interactions = TRUE)
PH_lav_bptest <- bptest(PH_lav_lm, studentize = TRUE)
PH_lav_DWtest <- dwtest(PH_lav_lm)
PH_lav_bgtest <- bgtest(PH_lav_lm, order = 1)
PH_lav_jbtest <- jarque.bera.test(resid(PH_lav_lm))
PH_lav_shtest <- shapiro.test(resid(PH_lav_lm))

# Data Distributions
RO_Census_Prahova %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Prahova$Rank, 
                              Population = PH_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.25,
            label = paste0("y", "==", signif(exp(PH_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(PH_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.25,
            label = paste0("R^2 ==", signif(summary(PH_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Prahova$Rank, 
                              Population = PH_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.23,
            label = paste0("y", "==", signif(exp(PH_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(PH_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.23,
            label = paste0("R^2 ==", signif(summary(PH_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Prahova$Rank, 
                              Population = PH_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.21,
            label = paste0("y", "==", signif(PH_ZM_cst, 4), "%.%", "(",
                           signif(PH_ZM_m, 4), "+ x)^", signif(PH_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.21,
            label = paste0("R^2 ==", signif(summary(PH_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Prahova$Rank, 
                              Population = PH_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.19,
            label = paste0("y", "==", signif(PH_lav_kst, 4), "%.%", "x^",
                           -signif(PH_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.19,
            label = paste0("R^2 ==", signif(summary(PH_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Prahova judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Prahova %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Prahova judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Prahova %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Prahova judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Prahova %>%
     ggplot(aes(x = log(Rank + PH_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Prahova judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Prahova %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Prahova judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 32. Satu Mare
RO_Census_SatuMare <- RO_Census_rank %>% filter(Judet_Name == "SATU MARE")
#View(RO_Census_SatuMare)

# Power Law
SM_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_SatuMare)
summary(SM_pw_lm)
SM_pred_pw_lm <- exp(predict(SM_pw_lm, newdata = RO_Census_SatuMare, 
                             interval = "prediction", level = 0.95))
SM_pw_out <- RO_Census_SatuMare %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SM_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_SatuMare), 
         DFFIT = dffits(SM_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SM_pw_lm$coefficients) / nrow(RO_Census_SatuMare)), 
         Rezid_Std = rstudent(SM_pw_lm), Leverage = hatvalues(SM_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SM_pw_lm)), 
         DFBETA = dfbetas(SM_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_SatuMare)))
SM_pw_whtest <- white(SM_pw_lm, interactions = TRUE)
SM_pw_bptest <- bptest(SM_pw_lm, studentize = TRUE)
SM_pw_DWtest <- dwtest(SM_pw_lm)
SM_pw_bgtest <- bgtest(SM_pw_lm, order = 1)
SM_pw_jbtest <- jarque.bera.test(resid(SM_pw_lm))
SM_pw_shtest <- shapiro.test(resid(SM_pw_lm))

# Zipf_Mandelbrot Law
SM_ZM_prm <- get_ZM_Param("RO_Census_SatuMare", "Population", "Rank")
SM_ZM_m <- SM_ZM_prm$m
SM_ZM_alpha <- SM_ZM_prm$alpha_ZM
SM_ZM_cst <- SM_ZM_prm$constant_ZM
SM_ZM_lm <- lm(log(RO_Census_SatuMare$Population) ~ log(RO_Census_SatuMare$Rank + SM_ZM_m))
summary(SM_ZM_lm)
SM_pred_ZM_lm <- exp(predict(SM_ZM_lm, newdata = RO_Census_SatuMare, 
                             interval = "prediction", level = 0.95))
SM_ZM_out <- RO_Census_SatuMare %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SM_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_SatuMare), 
         DFFIT = dffits(SM_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SM_ZM_lm$coefficients) / nrow(RO_Census_SatuMare)), 
         Rezid_Std = rstudent(SM_ZM_lm), Leverage = hatvalues(SM_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SM_ZM_lm)), 
         DFBETA = dfbetas(SM_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_SatuMare)))
SM_ZM_whtest <- white(SM_ZM_lm, interactions = TRUE)
SM_ZM_bptest <- bptest(SM_ZM_lm, studentize = TRUE)
SM_ZM_DWtest <- dwtest(SM_ZM_lm)
SM_ZM_bgtest <- bgtest(SM_ZM_lm, order = 1)
SM_ZM_jbtest <- jarque.bera.test(resid(SM_ZM_lm))
SM_ZM_shtest <- shapiro.test(resid(SM_ZM_lm))

# Exponential Law
SM_exp_lm <- lm(log(Population) ~ Rank, RO_Census_SatuMare)
summary(SM_exp_lm)
SM_pred_exp_lm <- exp(predict(SM_exp_lm, newdata = RO_Census_SatuMare, 
                              interval = "prediction", level = 0.95))
SM_exp_out <- RO_Census_SatuMare %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SM_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_SatuMare), 
         DFFIT = dffits(SM_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SM_exp_lm$coefficients) / nrow(RO_Census_SatuMare)), 
         Rezid_Std = rstudent(SM_exp_lm), Leverage = hatvalues(SM_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SM_exp_lm)), 
         DFBETA = dfbetas(SM_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_SatuMare)))
SM_exp_whtest <- white(SM_exp_lm, interactions = TRUE)
SM_exp_bptest <- bptest(SM_exp_lm, studentize = TRUE)
SM_exp_DWtest <- dwtest(SM_exp_lm)
SM_exp_bgtest <- bgtest(SM_exp_lm, order = 1)
SM_exp_jbtest <- jarque.bera.test(resid(SM_exp_lm))
SM_exp_shtest <- shapiro.test(resid(SM_exp_lm))

# Lavalette Function
RO_Census_SatuMare <- RO_Census_SatuMare %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_SatuMare) - Rank + 1))
)
SM_lav_lm <- lm(log(RO_Census_SatuMare$Population) ~ RO_Census_SatuMare$Lav_exp)
summary(SM_lav_lm)
SM_lav_kst <- exp(signif(SM_lav_lm$coef[[1]], 4))
SM_lav_chi <- signif(SM_lav_lm$coef[[2]], 4)
SM_pred_lav_lm <- exp(predict(SM_lav_lm, newdata = RO_Census_SatuMare, 
                              interval = "prediction", level = 0.95))
SM_lav_out <- RO_Census_SatuMare %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SM_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_SatuMare), 
         DFFIT = dffits(SM_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SM_lav_lm$coefficients) / nrow(RO_Census_SatuMare)), 
         Rezid_Std = rstudent(SM_lav_lm), Leverage = hatvalues(SM_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SM_lav_lm)), 
         DFBETA = dfbetas(SM_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_SatuMare)))
SM_lav_whtest <- white(SM_lav_lm, interactions = TRUE)
SM_lav_bptest <- bptest(SM_lav_lm, studentize = TRUE)
SM_lav_DWtest <- dwtest(SM_lav_lm)
SM_lav_bgtest <- bgtest(SM_lav_lm, order = 1)
SM_lav_jbtest <- jarque.bera.test(resid(SM_lav_lm))
SM_lav_shtest <- shapiro.test(resid(SM_lav_lm))

# Data Distributions
RO_Census_SatuMare %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_SatuMare$Rank, 
                              Population = SM_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.25,
            label = paste0("y", "==", signif(exp(SM_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(SM_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.25,
            label = paste0("R^2 ==", signif(summary(SM_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_SatuMare$Rank, 
                              Population = SM_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.23,
            label = paste0("y", "==", signif(exp(SM_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(SM_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.23,
            label = paste0("R^2 ==", signif(summary(SM_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_SatuMare$Rank, 
                              Population = SM_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.21,
            label = paste0("y", "==", signif(SM_ZM_cst, 4), "%.%", "(",
                           signif(SM_ZM_m, 4), "+ x)^", signif(SM_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.21,
            label = paste0("R^2 ==", signif(summary(SM_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_SatuMare$Rank, 
                              Population = SM_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.19,
            label = paste0("y", "==", signif(SM_lav_kst, 4), "%.%", "x^",
                           -signif(SM_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.19,
            label = paste0("R^2 ==", signif(summary(SM_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Satu Mare judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_SatuMare %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Satu Mare judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_SatuMare %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Satu Mare judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_SatuMare %>%
     ggplot(aes(x = log(Rank + SM_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Satu Mare judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_SatuMare %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Satu Mare judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 33. Salaj
RO_Census_Salaj <- RO_Census_rank %>% filter(Judet_Name == "SALAJ")
#View(RO_Census_Salaj)

# Power Law
SJ_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Salaj)
summary(SJ_pw_lm)
SJ_pred_pw_lm <- exp(predict(SJ_pw_lm, newdata = RO_Census_Salaj, 
                             interval = "prediction", level = 0.95))
SJ_pw_out <- RO_Census_Salaj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SJ_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Salaj), 
         DFFIT = dffits(SJ_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SJ_pw_lm$coefficients) / nrow(RO_Census_Salaj)), 
         Rezid_Std = rstudent(SJ_pw_lm), Leverage = hatvalues(SJ_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SJ_pw_lm)), 
         DFBETA = dfbetas(SJ_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Salaj)))
SJ_pw_whtest <- white(SJ_pw_lm, interactions = TRUE)
SJ_pw_bptest <- bptest(SJ_pw_lm, studentize = TRUE)
SJ_pw_DWtest <- dwtest(SJ_pw_lm)
SJ_pw_bgtest <- bgtest(SJ_pw_lm, order = 1)
SJ_pw_jbtest <- jarque.bera.test(resid(SJ_pw_lm))
SJ_pw_shtest <- shapiro.test(resid(SJ_pw_lm))

# Zipf_Mandelbrot Law
SJ_ZM_prm <- get_ZM_Param("RO_Census_Salaj", "Population", "Rank")
SJ_ZM_m <- SJ_ZM_prm$m
SJ_ZM_alpha <- SJ_ZM_prm$alpha_ZM
SJ_ZM_cst <- SJ_ZM_prm$constant_ZM
SJ_ZM_lm <- lm(log(RO_Census_Salaj$Population) ~ log(RO_Census_Salaj$Rank + SJ_ZM_m))
summary(SJ_ZM_lm)
SJ_pred_ZM_lm <- exp(predict(SJ_ZM_lm, newdata = RO_Census_Salaj, 
                             interval = "prediction", level = 0.95))
SJ_ZM_out <- RO_Census_Salaj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SJ_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Salaj), 
         DFFIT = dffits(SJ_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SJ_ZM_lm$coefficients) / nrow(RO_Census_Salaj)), 
         Rezid_Std = rstudent(SJ_ZM_lm), Leverage = hatvalues(SJ_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SJ_ZM_lm)), 
         DFBETA = dfbetas(SJ_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Salaj)))
SJ_ZM_whtest <- white(SJ_ZM_lm, interactions = TRUE)
SJ_ZM_bptest <- bptest(SJ_ZM_lm, studentize = TRUE)
SJ_ZM_DWtest <- dwtest(SJ_ZM_lm)
SJ_ZM_bgtest <- bgtest(SJ_ZM_lm, order = 1)
SJ_ZM_jbtest <- jarque.bera.test(resid(SJ_ZM_lm))
SJ_ZM_shtest <- shapiro.test(resid(SJ_ZM_lm))

# Exponential Law
SJ_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Salaj)
summary(SJ_exp_lm)
SJ_pred_exp_lm <- exp(predict(SJ_exp_lm, newdata = RO_Census_Salaj, 
                              interval = "prediction", level = 0.95))
SJ_exp_out <- RO_Census_Salaj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SJ_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Salaj), 
         DFFIT = dffits(SJ_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SJ_exp_lm$coefficients) / nrow(RO_Census_Salaj)), 
         Rezid_Std = rstudent(SJ_exp_lm), Leverage = hatvalues(SJ_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SJ_exp_lm)), 
         DFBETA = dfbetas(SJ_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Salaj)))
SJ_exp_whtest <- white(SJ_exp_lm, interactions = TRUE)
SJ_exp_bptest <- bptest(SJ_exp_lm, studentize = TRUE)
SJ_exp_DWtest <- dwtest(SJ_exp_lm)
SJ_exp_bgtest <- bgtest(SJ_exp_lm, order = 1)
SJ_exp_jbtest <- jarque.bera.test(resid(SJ_exp_lm))
SJ_exp_shtest <- shapiro.test(resid(SJ_exp_lm))

# Lavalette Function
RO_Census_Salaj <- RO_Census_Salaj %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Salaj) - Rank + 1))
)
SJ_lav_lm <- lm(log(RO_Census_Salaj$Population) ~ RO_Census_Salaj$Lav_exp)
summary(SJ_lav_lm)
SJ_lav_kst <- exp(signif(SJ_lav_lm$coef[[1]], 4))
SJ_lav_chi <- signif(SJ_lav_lm$coef[[2]], 4)
SJ_pred_lav_lm <- exp(predict(SJ_lav_lm, newdata = RO_Census_Salaj, 
                              interval = "prediction", level = 0.95))
SJ_lav_out <- RO_Census_Salaj %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SJ_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Salaj), 
         DFFIT = dffits(SJ_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SJ_lav_lm$coefficients) / nrow(RO_Census_Salaj)), 
         Rezid_Std = rstudent(SJ_lav_lm), Leverage = hatvalues(SJ_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SJ_lav_lm)), 
         DFBETA = dfbetas(SJ_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Salaj)))
SJ_lav_whtest <- white(SJ_lav_lm, interactions = TRUE)
SJ_lav_bptest <- bptest(SJ_lav_lm, studentize = TRUE)
SJ_lav_DWtest <- dwtest(SJ_lav_lm)
SJ_lav_bgtest <- bgtest(SJ_lav_lm, order = 1)
SJ_lav_jbtest <- jarque.bera.test(resid(SJ_lav_lm))
SJ_lav_shtest <- shapiro.test(resid(SJ_lav_lm))

# Data Distributions
RO_Census_Salaj %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Salaj$Rank, 
                              Population = SJ_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.25,
            label = paste0("y", "==", signif(exp(SJ_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(SJ_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 35, y = 0.25,
            label = paste0("R^2 ==", signif(summary(SJ_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Salaj$Rank, 
                              Population = SJ_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.23,
            label = paste0("y", "==", signif(exp(SJ_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(SJ_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 35, y = 0.23,
            label = paste0("R^2 ==", signif(summary(SJ_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Salaj$Rank, 
                              Population = SJ_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.21,
            label = paste0("y", "==", signif(SJ_ZM_cst, 4), "%.%", "(",
                           signif(SJ_ZM_m, 4), "+ x)^", signif(SJ_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 35, y = 0.21,
            label = paste0("R^2 ==", signif(summary(SJ_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Salaj$Rank, 
                              Population = SJ_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.19,
            label = paste0("y", "==", signif(SJ_lav_kst, 4), "%.%", "x^",
                           -signif(SJ_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 35, y = 0.19,
            label = paste0("R^2 ==", signif(summary(SJ_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Salaj judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Salaj %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Salaj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Salaj %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Salaj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Salaj %>%
     ggplot(aes(x = log(Rank + SJ_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Salaj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Salaj %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Salaj judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 34. Sibiu
RO_Census_Sibiu <- RO_Census_rank %>% filter(Judet_Name == "SIBIU")
#View(RO_Census_Sibiu)

# Power Law
SB_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Sibiu)
summary(SB_pw_lm)
SB_pred_pw_lm <- exp(predict(SB_pw_lm, newdata = RO_Census_Sibiu, 
                             interval = "prediction", level = 0.95))
SB_pw_out <- RO_Census_Sibiu %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SB_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Sibiu), 
         DFFIT = dffits(SB_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SB_pw_lm$coefficients) / nrow(RO_Census_Sibiu)), 
         Rezid_Std = rstudent(SB_pw_lm), Leverage = hatvalues(SB_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SB_pw_lm)), 
         DFBETA = dfbetas(SB_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Sibiu)))
SB_pw_whtest <- white(SB_pw_lm, interactions = TRUE)
SB_pw_bptest <- bptest(SB_pw_lm, studentize = TRUE)
SB_pw_DWtest <- dwtest(SB_pw_lm)
SB_pw_bgtest <- bgtest(SB_pw_lm, order = 1)
SB_pw_jbtest <- jarque.bera.test(resid(SB_pw_lm))
SB_pw_shtest <- shapiro.test(resid(SB_pw_lm))

# Zipf_Mandelbrot Law
SB_ZM_prm <- get_ZM_Param("RO_Census_Sibiu", "Population", "Rank")
SB_ZM_m <- SB_ZM_prm$m
SB_ZM_alpha <- SB_ZM_prm$alpha_ZM
SB_ZM_cst <- SB_ZM_prm$constant_ZM
SB_ZM_lm <- lm(log(RO_Census_Sibiu$Population) ~ log(RO_Census_Sibiu$Rank + SB_ZM_m))
summary(SB_ZM_lm)
SB_pred_ZM_lm <- exp(predict(SB_ZM_lm, newdata = RO_Census_Sibiu, 
                             interval = "prediction", level = 0.95))
SB_ZM_out <- RO_Census_Sibiu %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SB_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Sibiu), 
         DFFIT = dffits(SB_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SB_ZM_lm$coefficients) / nrow(RO_Census_Sibiu)), 
         Rezid_Std = rstudent(SB_ZM_lm), Leverage = hatvalues(SB_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SB_ZM_lm)), 
         DFBETA = dfbetas(SB_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Sibiu)))
SB_ZM_whtest <- white(SB_ZM_lm, interactions = TRUE)
SB_ZM_bptest <- bptest(SB_ZM_lm, studentize = TRUE)
SB_ZM_DWtest <- dwtest(SB_ZM_lm)
SB_ZM_bgtest <- bgtest(SB_ZM_lm, order = 1)
SB_ZM_jbtest <- jarque.bera.test(resid(SB_ZM_lm))
SB_ZM_shtest <- shapiro.test(resid(SB_ZM_lm))

# Exponential Law
SB_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Sibiu)
summary(SB_exp_lm)
SB_pred_exp_lm <- exp(predict(SB_exp_lm, newdata = RO_Census_Sibiu, 
                              interval = "prediction", level = 0.95))
SB_exp_out <- RO_Census_Sibiu %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SB_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Sibiu), 
         DFFIT = dffits(SB_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SB_exp_lm$coefficients) / nrow(RO_Census_Sibiu)), 
         Rezid_Std = rstudent(SB_exp_lm), Leverage = hatvalues(SB_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SB_exp_lm)), 
         DFBETA = dfbetas(SB_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Sibiu)))
SB_exp_whtest <- white(SB_exp_lm, interactions = TRUE)
SB_exp_bptest <- bptest(SB_exp_lm, studentize = TRUE)
SB_exp_DWtest <- dwtest(SB_exp_lm)
SB_exp_bgtest <- bgtest(SB_exp_lm, order = 1)
SB_exp_jbtest <- jarque.bera.test(resid(SB_exp_lm))
SB_exp_shtest <- shapiro.test(resid(SB_exp_lm))

# Lavalette Function
RO_Census_Sibiu <- RO_Census_Sibiu %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Sibiu) - Rank + 1))
)
SB_lav_lm <- lm(log(RO_Census_Sibiu$Population) ~ RO_Census_Sibiu$Lav_exp)
summary(SB_lav_lm)
SB_lav_kst <- exp(signif(SB_lav_lm$coef[[1]], 4))
SB_lav_chi <- signif(SB_lav_lm$coef[[2]], 4)
SB_pred_lav_lm <- exp(predict(SB_lav_lm, newdata = RO_Census_Sibiu, 
                              interval = "prediction", level = 0.95))
SB_lav_out <- RO_Census_Sibiu %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SB_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Sibiu), 
         DFFIT = dffits(SB_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SB_lav_lm$coefficients) / nrow(RO_Census_Sibiu)), 
         Rezid_Std = rstudent(SB_lav_lm), Leverage = hatvalues(SB_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SB_lav_lm)), 
         DFBETA = dfbetas(SB_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Sibiu)))
SB_lav_whtest <- white(SB_lav_lm, interactions = TRUE)
SB_lav_bptest <- bptest(SB_lav_lm, studentize = TRUE)
SB_lav_DWtest <- dwtest(SB_lav_lm)
SB_lav_bgtest <- bgtest(SB_lav_lm, order = 1)
SB_lav_jbtest <- jarque.bera.test(resid(SB_lav_lm))
SB_lav_shtest <- shapiro.test(resid(SB_lav_lm))

# Data Distributions
RO_Census_Sibiu %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Sibiu$Rank, 
                              Population = SB_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 25, y = 0.34,
            label = paste0("y", "==", signif(exp(SB_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(SB_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 40, y = 0.34,
            label = paste0("R^2 ==", signif(summary(SB_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Sibiu$Rank, 
                              Population = SB_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 25, y = 0.32,
            label = paste0("y", "==", signif(exp(SB_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(SB_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 40, y = 0.32,
            label = paste0("R^2 ==", signif(summary(SB_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Sibiu$Rank, 
                              Population = SB_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 25, y = 0.30,
            label = paste0("y", "==", signif(SB_ZM_cst, 4), "%.%", "(",
                           signif(SB_ZM_m, 4), "+ x)^", signif(SB_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 40, y = 0.30,
            label = paste0("R^2 ==", signif(summary(SB_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Sibiu$Rank, 
                              Population = SB_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 25, y = 0.28,
            label = paste0("y", "==", signif(SB_lav_kst, 4), "%.%", "x^",
                           -signif(SB_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 40, y = 0.28,
            label = paste0("R^2 ==", signif(summary(SB_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Sibiu judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Sibiu %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Sibiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Sibiu %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Sibiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Sibiu %>%
     ggplot(aes(x = log(Rank + SB_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Sibiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Sibiu %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Sibiu judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 35. Suceava
RO_Census_Suceava <- RO_Census_rank %>% filter(Judet_Name == "SUCEAVA")
#View(RO_Census_Suceava)

# Power Law
SV_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Suceava)
summary(SV_pw_lm)
SV_pred_pw_lm <- exp(predict(SV_pw_lm, newdata = RO_Census_Suceava, 
                             interval = "prediction", level = 0.95))
SV_pw_out <- RO_Census_Suceava %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SV_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Suceava), 
         DFFIT = dffits(SV_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SV_pw_lm$coefficients) / nrow(RO_Census_Suceava)), 
         Rezid_Std = rstudent(SV_pw_lm), Leverage = hatvalues(SV_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SV_pw_lm)), 
         DFBETA = dfbetas(SV_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Suceava)))
SV_pw_whtest <- white(SV_pw_lm, interactions = TRUE)
SV_pw_bptest <- bptest(SV_pw_lm, studentize = TRUE)
SV_pw_DWtest <- dwtest(SV_pw_lm)
SV_pw_bgtest <- bgtest(SV_pw_lm, order = 1)
SV_pw_jbtest <- jarque.bera.test(resid(SV_pw_lm))
SV_pw_shtest <- shapiro.test(resid(SV_pw_lm))

# Zipf_Mandelbrot Law
SV_ZM_prm <- get_ZM_Param("RO_Census_Suceava", "Population", "Rank")
SV_ZM_m <- SV_ZM_prm$m
SV_ZM_alpha <- SV_ZM_prm$alpha_ZM
SV_ZM_cst <- SV_ZM_prm$constant_ZM
SV_ZM_lm <- lm(log(RO_Census_Suceava$Population) ~ log(RO_Census_Suceava$Rank + SV_ZM_m))
summary(SV_ZM_lm)
SV_pred_ZM_lm <- exp(predict(SV_ZM_lm, newdata = RO_Census_Suceava, 
                             interval = "prediction", level = 0.95))
SV_ZM_out <- RO_Census_Suceava %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SV_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Suceava), 
         DFFIT = dffits(SV_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SV_ZM_lm$coefficients) / nrow(RO_Census_Suceava)), 
         Rezid_Std = rstudent(SV_ZM_lm), Leverage = hatvalues(SV_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SV_ZM_lm)), 
         DFBETA = dfbetas(SV_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Suceava)))
SV_ZM_whtest <- white(SV_ZM_lm, interactions = TRUE)
SV_ZM_bptest <- bptest(SV_ZM_lm, studentize = TRUE)
SV_ZM_DWtest <- dwtest(SV_ZM_lm)
SV_ZM_bgtest <- bgtest(SV_ZM_lm, order = 1)
SV_ZM_jbtest <- jarque.bera.test(resid(SV_ZM_lm))
SV_ZM_shtest <- shapiro.test(resid(SV_ZM_lm))

# Exponential Law
SV_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Suceava)
summary(SV_exp_lm)
SV_pred_exp_lm <- exp(predict(SV_exp_lm, newdata = RO_Census_Suceava, 
                              interval = "prediction", level = 0.95))
SV_exp_out <- RO_Census_Suceava %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SV_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Suceava), 
         DFFIT = dffits(SV_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SV_exp_lm$coefficients) / nrow(RO_Census_Suceava)), 
         Rezid_Std = rstudent(SV_exp_lm), Leverage = hatvalues(SV_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SV_exp_lm)), 
         DFBETA = dfbetas(SV_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Suceava)))
SV_exp_whtest <- white(SV_exp_lm, interactions = TRUE)
SV_exp_bptest <- bptest(SV_exp_lm, studentize = TRUE)
SV_exp_DWtest <- dwtest(SV_exp_lm)
SV_exp_bgtest <- bgtest(SV_exp_lm, order = 1)
SV_exp_jbtest <- jarque.bera.test(resid(SV_exp_lm))
SV_exp_shtest <- shapiro.test(resid(SV_exp_lm))

# Lavalette Function
RO_Census_Suceava <- RO_Census_Suceava %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Suceava) - Rank + 1))
)
SV_lav_lm <- lm(log(RO_Census_Suceava$Population) ~ RO_Census_Suceava$Lav_exp)
summary(SV_lav_lm)
SV_lav_kst <- exp(signif(SV_lav_lm$coef[[1]], 4))
SV_lav_chi <- signif(SV_lav_lm$coef[[2]], 4)
SV_pred_lav_lm <- exp(predict(SV_lav_lm, newdata = RO_Census_Suceava, 
                              interval = "prediction", level = 0.95))
SV_lav_out <- RO_Census_Suceava %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(SV_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Suceava), 
         DFFIT = dffits(SV_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(SV_lav_lm$coefficients) / nrow(RO_Census_Suceava)), 
         Rezid_Std = rstudent(SV_lav_lm), Leverage = hatvalues(SV_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(SV_lav_lm)), 
         DFBETA = dfbetas(SV_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Suceava)))
SV_lav_whtest <- white(SV_lav_lm, interactions = TRUE)
SV_lav_bptest <- bptest(SV_lav_lm, studentize = TRUE)
SV_lav_DWtest <- dwtest(SV_lav_lm)
SV_lav_bgtest <- bgtest(SV_lav_lm, order = 1)
SV_lav_jbtest <- jarque.bera.test(resid(SV_lav_lm))
SV_lav_shtest <- shapiro.test(resid(SV_lav_lm))

# Data Distributions
RO_Census_Suceava %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava$Rank, 
                              Population = SV_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 70000,
            label = paste0("y", "==", signif(exp(SV_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(SV_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 70, y = 70000,
            label = paste0("R^2 ==", signif(summary(SV_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava$Rank, 
                              Population = SV_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 60000,
            label = paste0("y", "==", signif(exp(SV_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(SV_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 70, y = 60000,
            label = paste0("R^2 ==", signif(summary(SV_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava$Rank, 
                              Population = SV_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 50000,
            label = paste0("y", "==", signif(SV_ZM_cst, 4), "%.%", "(",
                           signif(SV_ZM_m, 4), "+ x)^", signif(SV_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 70, y = 50000,
            label = paste0("R^2 ==", signif(summary(SV_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Suceava$Rank, 
                              Population = SV_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 40000,
            label = paste0("y", "==", signif(SV_lav_kst, 4), "%.%", "x^",
                           -signif(SV_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 70, y = 40000,
            label = paste0("R^2 ==", signif(summary(SV_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_y_continuous(labels = scales::label_number(
    big.mark = ",", decimal.mark = ".")) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population", color = "Legend") +
  #ggtitle("Data Distribution on Suceava judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Suceava %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Suceava judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Suceava %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Suceava judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Suceava %>%
     ggplot(aes(x = log(Rank + SV_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Suceava judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Suceava %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Suceava judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 36. Teleorman
RO_Census_Teleorman <- RO_Census_rank %>% filter(Judet_Name == "TELEORMAN")
#View(RO_Census_Teleorman)

# Power Law
TR_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Teleorman)
summary(TR_pw_lm)
TR_pred_pw_lm <- exp(predict(TR_pw_lm, newdata = RO_Census_Teleorman, 
                             interval = "prediction", level = 0.95))
TR_pw_out <- RO_Census_Teleorman %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TR_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Teleorman), 
         DFFIT = dffits(TR_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TR_pw_lm$coefficients) / nrow(RO_Census_Teleorman)), 
         Rezid_Std = rstudent(TR_pw_lm), Leverage = hatvalues(TR_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TR_pw_lm)), 
         DFBETA = dfbetas(TR_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Teleorman)))
TR_pw_whtest <- white(TR_pw_lm, interactions = TRUE)
TR_pw_bptest <- bptest(TR_pw_lm, studentize = TRUE)
TR_pw_DWtest <- dwtest(TR_pw_lm)
TR_pw_bgtest <- bgtest(TR_pw_lm, order = 1)
TR_pw_jbtest <- jarque.bera.test(resid(TR_pw_lm))
TR_pw_shtest <- shapiro.test(resid(TR_pw_lm))

# Zipf_Mandelbrot Law
TR_ZM_prm <- get_ZM_Param("RO_Census_Teleorman", "Population", "Rank")
TR_ZM_m <- TR_ZM_prm$m
TR_ZM_alpha <- TR_ZM_prm$alpha_ZM
TR_ZM_cst <- TR_ZM_prm$constant_ZM
TR_ZM_lm <- lm(log(RO_Census_Teleorman$Population) ~ log(RO_Census_Teleorman$Rank + TR_ZM_m))
summary(TR_ZM_lm)
TR_pred_ZM_lm <- exp(predict(TR_ZM_lm, newdata = RO_Census_Teleorman, 
                             interval = "prediction", level = 0.95))
TR_ZM_out <- RO_Census_Teleorman %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TR_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Teleorman), 
         DFFIT = dffits(TR_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TR_ZM_lm$coefficients) / nrow(RO_Census_Teleorman)), 
         Rezid_Std = rstudent(TR_ZM_lm), Leverage = hatvalues(TR_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TR_ZM_lm)), 
         DFBETA = dfbetas(TR_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Teleorman)))
TR_ZM_whtest <- white(TR_ZM_lm, interactions = TRUE)
TR_ZM_bptest <- bptest(TR_ZM_lm, studentize = TRUE)
TR_ZM_DWtest <- dwtest(TR_ZM_lm)
TR_ZM_bgtest <- bgtest(TR_ZM_lm, order = 1)
TR_ZM_jbtest <- jarque.bera.test(resid(TR_ZM_lm))
TR_ZM_shtest <- shapiro.test(resid(TR_ZM_lm))

# Exponential Law
TR_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Teleorman)
summary(TR_exp_lm)
TR_pred_exp_lm <- exp(predict(TR_exp_lm, newdata = RO_Census_Teleorman, 
                              interval = "prediction", level = 0.95))
TR_exp_out <- RO_Census_Teleorman %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TR_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Teleorman), 
         DFFIT = dffits(TR_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TR_exp_lm$coefficients) / nrow(RO_Census_Teleorman)), 
         Rezid_Std = rstudent(TR_exp_lm), Leverage = hatvalues(TR_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TR_exp_lm)), 
         DFBETA = dfbetas(TR_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Teleorman)))
TR_exp_whtest <- white(TR_exp_lm, interactions = TRUE)
TR_exp_bptest <- bptest(TR_exp_lm, studentize = TRUE)
TR_exp_DWtest <- dwtest(TR_exp_lm)
TR_exp_bgtest <- bgtest(TR_exp_lm, order = 1)
TR_exp_jbtest <- jarque.bera.test(resid(TR_exp_lm))
TR_exp_shtest <- shapiro.test(resid(TR_exp_lm))

# Lavalette Function
RO_Census_Teleorman <- RO_Census_Teleorman %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Teleorman) - Rank + 1))
)
TR_lav_lm <- lm(log(RO_Census_Teleorman$Population) ~ RO_Census_Teleorman$Lav_exp)
summary(TR_lav_lm)
TR_lav_kst <- exp(signif(TR_lav_lm$coef[[1]], 4))
TR_lav_chi <- signif(TR_lav_lm$coef[[2]], 4)
TR_pred_lav_lm <- exp(predict(TR_lav_lm, newdata = RO_Census_Teleorman, 
                              interval = "prediction", level = 0.95))
TR_lav_out <- RO_Census_Teleorman %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TR_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Teleorman), 
         DFFIT = dffits(TR_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TR_lav_lm$coefficients) / nrow(RO_Census_Teleorman)), 
         Rezid_Std = rstudent(TR_lav_lm), Leverage = hatvalues(TR_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TR_lav_lm)), 
         DFBETA = dfbetas(TR_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Teleorman)))
TR_lav_whtest <- white(TR_lav_lm, interactions = TRUE)
TR_lav_bptest <- bptest(TR_lav_lm, studentize = TRUE)
TR_lav_DWtest <- dwtest(TR_lav_lm)
TR_lav_bgtest <- bgtest(TR_lav_lm, order = 1)
TR_lav_jbtest <- jarque.bera.test(resid(TR_lav_lm))
TR_lav_shtest <- shapiro.test(resid(TR_lav_lm))

# Data Distributions
RO_Census_Teleorman %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Teleorman$Rank, 
                              Population = TR_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.12,
            label = paste0("y", "==", signif(exp(TR_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(TR_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.12,
            label = paste0("R^2 ==", signif(summary(TR_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Teleorman$Rank, 
                              Population = TR_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.10,
            label = paste0("y", "==", signif(exp(TR_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(TR_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.10,
            label = paste0("R^2 ==", signif(summary(TR_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Teleorman$Rank, 
                              Population = TR_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.08,
            label = paste0("y", "==", signif(TR_ZM_cst, 4), "%.%", "(",
                           signif(TR_ZM_m, 4), "+ x)^", signif(TR_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.08,
            label = paste0("R^2 ==", signif(summary(TR_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Teleorman$Rank, 
                              Population = TR_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.06,
            label = paste0("y", "==", signif(TR_lav_kst, 4), "%.%", "x^",
                           -signif(TR_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.06,
            label = paste0("R^2 ==", signif(summary(TR_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Teleorman judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Teleorman %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Teleorman judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Teleorman %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Teleorman judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Teleorman %>%
     ggplot(aes(x = log(Rank + TR_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Teleorman judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Teleorman %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Teleorman judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 37. Timis
RO_Census_Timis <- RO_Census_rank %>% filter(Judet_Name == "TIMIS")
#View(RO_Census_Timis)

# Power Law
TM_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Timis)
summary(TM_pw_lm)
TM_pred_pw_lm <- exp(predict(TM_pw_lm, newdata = RO_Census_Timis, 
                             interval = "prediction", level = 0.95))
TM_pw_out <- RO_Census_Timis %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TM_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Timis), 
         DFFIT = dffits(TM_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TM_pw_lm$coefficients) / nrow(RO_Census_Timis)), 
         Rezid_Std = rstudent(TM_pw_lm), Leverage = hatvalues(TM_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TM_pw_lm)), 
         DFBETA = dfbetas(TM_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Timis)))
TM_pw_whtest <- white(TM_pw_lm, interactions = TRUE)
TM_pw_bptest <- bptest(TM_pw_lm, studentize = TRUE)
TM_pw_DWtest <- dwtest(TM_pw_lm)
TM_pw_bgtest <- bgtest(TM_pw_lm, order = 1)
TM_pw_jbtest <- jarque.bera.test(resid(TM_pw_lm))
TM_pw_shtest <- shapiro.test(resid(TM_pw_lm))

# Zipf_Mandelbrot Law
TM_ZM_prm <- get_ZM_Param("RO_Census_Timis", "Population", "Rank")
TM_ZM_m <- TM_ZM_prm$m
TM_ZM_alpha <- TM_ZM_prm$alpha_ZM
TM_ZM_cst <- TM_ZM_prm$constant_ZM
TM_ZM_lm <- lm(log(RO_Census_Timis$Population) ~ log(RO_Census_Timis$Rank + TM_ZM_m))
summary(TM_ZM_lm)
TM_pred_ZM_lm <- exp(predict(TM_ZM_lm, newdata = RO_Census_Timis, 
                             interval = "prediction", level = 0.95))
TM_ZM_out <- RO_Census_Timis %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TM_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Timis), 
         DFFIT = dffits(TM_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TM_ZM_lm$coefficients) / nrow(RO_Census_Timis)), 
         Rezid_Std = rstudent(TM_ZM_lm), Leverage = hatvalues(TM_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TM_ZM_lm)), 
         DFBETA = dfbetas(TM_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Timis)))
TM_ZM_whtest <- white(TM_ZM_lm, interactions = TRUE)
TM_ZM_bptest <- bptest(TM_ZM_lm, studentize = TRUE)
TM_ZM_DWtest <- dwtest(TM_ZM_lm)
TM_ZM_bgtest <- bgtest(TM_ZM_lm, order = 1)
TM_ZM_jbtest <- jarque.bera.test(resid(TM_ZM_lm))
TM_ZM_shtest <- shapiro.test(resid(TM_ZM_lm))

# Exponential Law
TM_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Timis)
summary(TM_exp_lm)
TM_pred_exp_lm <- exp(predict(TM_exp_lm, newdata = RO_Census_Timis, 
                              interval = "prediction", level = 0.95))
TM_exp_out <- RO_Census_Timis %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TM_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Timis), 
         DFFIT = dffits(TM_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TM_exp_lm$coefficients) / nrow(RO_Census_Timis)), 
         Rezid_Std = rstudent(TM_exp_lm), Leverage = hatvalues(TM_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TM_exp_lm)), 
         DFBETA = dfbetas(TM_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Timis)))
TM_exp_whtest <- white(TM_exp_lm, interactions = TRUE)
TM_exp_bptest <- bptest(TM_exp_lm, studentize = TRUE)
TM_exp_DWtest <- dwtest(TM_exp_lm)
TM_exp_bgtest <- bgtest(TM_exp_lm, order = 1)
TM_exp_jbtest <- jarque.bera.test(resid(TM_exp_lm))
TM_exp_shtest <- shapiro.test(resid(TM_exp_lm))

# Lavalette Function
RO_Census_Timis <- RO_Census_Timis %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Timis) - Rank + 1))
)
TM_lav_lm <- lm(log(RO_Census_Timis$Population) ~ RO_Census_Timis$Lav_exp)
summary(TM_lav_lm)
TM_lav_kst <- exp(signif(TM_lav_lm$coef[[1]], 4))
TM_lav_chi <- signif(TM_lav_lm$coef[[2]], 4)
TM_pred_lav_lm <- exp(predict(TM_lav_lm, newdata = RO_Census_Timis, 
                              interval = "prediction", level = 0.95))
TM_lav_out <- RO_Census_Timis %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TM_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Timis), 
         DFFIT = dffits(TM_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TM_lav_lm$coefficients) / nrow(RO_Census_Timis)), 
         Rezid_Std = rstudent(TM_lav_lm), Leverage = hatvalues(TM_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TM_lav_lm)), 
         DFBETA = dfbetas(TM_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Timis)))
TM_lav_whtest <- white(TM_lav_lm, interactions = TRUE)
TM_lav_bptest <- bptest(TM_lav_lm, studentize = TRUE)
TM_lav_DWtest <- dwtest(TM_lav_lm)
TM_lav_bgtest <- bgtest(TM_lav_lm, order = 1)
TM_lav_jbtest <- jarque.bera.test(resid(TM_lav_lm))
TM_lav_shtest <- shapiro.test(resid(TM_lav_lm))

# Data Distributions
RO_Census_Timis %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Timis$Rank, 
                              Population = TM_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.35,
            label = paste0("y", "==", signif(exp(TM_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(TM_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.35,
            label = paste0("R^2 ==", signif(summary(TM_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Timis$Rank, 
                              Population = TM_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.30,
            label = paste0("y", "==", signif(exp(TM_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(TM_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.30,
            label = paste0("R^2 ==", signif(summary(TM_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Timis$Rank, 
                              Population = TM_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.25,
            label = paste0("y", "==", signif(TM_ZM_cst, 4), "%.%", "(",
                           signif(TM_ZM_m, 4), "+ x)^", signif(TM_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.25,
            label = paste0("R^2 ==", signif(summary(TM_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Timis$Rank, 
                              Population = TM_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.20,
            label = paste0("y", "==", signif(TM_lav_kst, 4), "%.%", "x^",
                           -signif(TM_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.20,
            label = paste0("R^2 ==", signif(summary(TM_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Timis judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Timis %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Timis judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Timis %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Timis judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Timis %>%
     ggplot(aes(x = log(Rank + TM_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Timis judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Timis %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Timis judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 38. Tulcea
RO_Census_Tulcea <- RO_Census_rank %>% filter(Judet_Name == "TULCEA")
#View(RO_Census_Tulcea)

# Power Law
TL_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Tulcea)
summary(TL_pw_lm)
TL_pred_pw_lm <- exp(predict(TL_pw_lm, newdata = RO_Census_Tulcea, 
                             interval = "prediction", level = 0.95))
TL_pw_out <- RO_Census_Tulcea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TL_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Tulcea), 
         DFFIT = dffits(TL_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TL_pw_lm$coefficients) / nrow(RO_Census_Tulcea)), 
         Rezid_Std = rstudent(TL_pw_lm), Leverage = hatvalues(TL_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TL_pw_lm)), 
         DFBETA = dfbetas(TL_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Tulcea)))
TL_pw_whtest <- white(TL_pw_lm, interactions = TRUE)
TL_pw_bptest <- bptest(TL_pw_lm, studentize = TRUE)
TL_pw_DWtest <- dwtest(TL_pw_lm)
TL_pw_bgtest <- bgtest(TL_pw_lm, order = 1)
TL_pw_jbtest <- jarque.bera.test(resid(TL_pw_lm))
TL_pw_shtest <- shapiro.test(resid(TL_pw_lm))

# Zipf_Mandelbrot Law
TL_ZM_prm <- get_ZM_Param("RO_Census_Tulcea", "Population", "Rank")
TL_ZM_m <- TL_ZM_prm$m
TL_ZM_alpha <- TL_ZM_prm$alpha_ZM
TL_ZM_cst <- TL_ZM_prm$constant_ZM
TL_ZM_lm <- lm(log(RO_Census_Tulcea$Population) ~ log(RO_Census_Tulcea$Rank + TL_ZM_m))
summary(TL_ZM_lm)
TL_pred_ZM_lm <- exp(predict(TL_ZM_lm, newdata = RO_Census_Tulcea, 
                             interval = "prediction", level = 0.95))
TL_ZM_out <- RO_Census_Tulcea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TL_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Tulcea), 
         DFFIT = dffits(TL_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TL_ZM_lm$coefficients) / nrow(RO_Census_Tulcea)), 
         Rezid_Std = rstudent(TL_ZM_lm), Leverage = hatvalues(TL_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TL_ZM_lm)), 
         DFBETA = dfbetas(TL_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Tulcea)))
TL_ZM_whtest <- white(TL_ZM_lm, interactions = TRUE)
TL_ZM_bptest <- bptest(TL_ZM_lm, studentize = TRUE)
TL_ZM_DWtest <- dwtest(TL_ZM_lm)
TL_ZM_bgtest <- bgtest(TL_ZM_lm, order = 1)
TL_ZM_jbtest <- jarque.bera.test(resid(TL_ZM_lm))
TL_ZM_shtest <- shapiro.test(resid(TL_ZM_lm))

# Exponential Law
TL_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Tulcea)
summary(TL_exp_lm)
TL_pred_exp_lm <- exp(predict(TL_exp_lm, newdata = RO_Census_Tulcea, 
                              interval = "prediction", level = 0.95))
TL_exp_out <- RO_Census_Tulcea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TL_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Tulcea), 
         DFFIT = dffits(TL_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TL_exp_lm$coefficients) / nrow(RO_Census_Tulcea)), 
         Rezid_Std = rstudent(TL_exp_lm), Leverage = hatvalues(TL_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TL_exp_lm)), 
         DFBETA = dfbetas(TL_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Tulcea)))
TL_exp_whtest <- white(TL_exp_lm, interactions = TRUE)
TL_exp_bptest <- bptest(TL_exp_lm, studentize = TRUE)
TL_exp_DWtest <- dwtest(TL_exp_lm)
TL_exp_bgtest <- bgtest(TL_exp_lm, order = 1)
TL_exp_jbtest <- jarque.bera.test(resid(TL_exp_lm))
TL_exp_shtest <- shapiro.test(resid(TL_exp_lm))

# Lavalette Function
RO_Census_Tulcea <- RO_Census_Tulcea %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Tulcea) - Rank + 1))
)
TL_lav_lm <- lm(log(RO_Census_Tulcea$Population) ~ RO_Census_Tulcea$Lav_exp)
summary(TL_lav_lm)
TL_lav_kst <- exp(signif(TL_lav_lm$coef[[1]], 4))
TL_lav_chi <- signif(TL_lav_lm$coef[[2]], 4)
TL_pred_lav_lm <- exp(predict(TL_lav_lm, newdata = RO_Census_Tulcea, 
                              interval = "prediction", level = 0.95))
TL_lav_out <- RO_Census_Tulcea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(TL_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Tulcea), 
         DFFIT = dffits(TL_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(TL_lav_lm$coefficients) / nrow(RO_Census_Tulcea)), 
         Rezid_Std = rstudent(TL_lav_lm), Leverage = hatvalues(TL_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(TL_lav_lm)), 
         DFBETA = dfbetas(TL_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Tulcea)))
TL_lav_whtest <- white(TL_lav_lm, interactions = TRUE)
TL_lav_bptest <- bptest(TL_lav_lm, studentize = TRUE)
TL_lav_DWtest <- dwtest(TL_lav_lm)
TL_lav_bgtest <- bgtest(TL_lav_lm, order = 1)
TL_lav_jbtest <- jarque.bera.test(resid(TL_lav_lm))
TL_lav_shtest <- shapiro.test(resid(TL_lav_lm))

# Data Distributions
RO_Census_Tulcea %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Tulcea$Rank, 
                              Population = TL_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 20, y = 0.30,
            label = paste0("y", "==", signif(exp(TL_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(TL_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 35, y = 0.30,
            label = paste0("R^2 ==", signif(summary(TL_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Tulcea$Rank, 
                              Population = TL_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 20, y = 0.27,
            label = paste0("y", "==", signif(exp(TL_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(TL_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 35, y = 0.27,
            label = paste0("R^2 ==", signif(summary(TL_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Tulcea$Rank, 
                              Population = TL_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 20, y = 0.24,
            label = paste0("y", "==", signif(TL_ZM_cst, 4), "%.%", "(",
                           signif(TL_ZM_m, 4), "+ x)^", signif(TL_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 35, y = 0.24,
            label = paste0("R^2 ==", signif(summary(TL_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Tulcea$Rank, 
                              Population = TL_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 20, y = 0.21,
            label = paste0("y", "==", signif(TL_lav_kst, 4), "%.%", "x^",
                           -signif(TL_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 35, y = 0.21,
            label = paste0("R^2 ==", signif(summary(TL_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Tulcea judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Tulcea %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Tulcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Tulcea %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Tulcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Tulcea %>%
     ggplot(aes(x = log(Rank + TL_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Tulcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Tulcea %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Tulcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 39. Vaslui
RO_Census_Vaslui <- RO_Census_rank %>% filter(Judet_Name == "VASLUI")
#View(RO_Census_Vaslui)

# Power Law
VS_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Vaslui)
summary(VS_pw_lm)
VS_pred_pw_lm <- exp(predict(VS_pw_lm, newdata = RO_Census_Vaslui, 
                             interval = "prediction", level = 0.95))
VS_pw_out <- RO_Census_Vaslui %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VS_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Vaslui), 
         DFFIT = dffits(VS_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VS_pw_lm$coefficients) / nrow(RO_Census_Vaslui)), 
         Rezid_Std = rstudent(VS_pw_lm), Leverage = hatvalues(VS_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VS_pw_lm)), 
         DFBETA = dfbetas(VS_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Vaslui)))
VS_pw_whtest <- white(VS_pw_lm, interactions = TRUE)
VS_pw_bptest <- bptest(VS_pw_lm, studentize = TRUE)
VS_pw_DWtest <- dwtest(VS_pw_lm)
VS_pw_bgtest <- bgtest(VS_pw_lm, order = 1)
VS_pw_jbtest <- jarque.bera.test(resid(VS_pw_lm))
VS_pw_shtest <- shapiro.test(resid(VS_pw_lm))

# Zipf_Mandelbrot Law
VS_ZM_prm <- get_ZM_Param("RO_Census_Vaslui", "Population", "Rank")
VS_ZM_m <- VS_ZM_prm$m
VS_ZM_alpha <- VS_ZM_prm$alpha_ZM
VS_ZM_cst <- VS_ZM_prm$constant_ZM
VS_ZM_lm <- lm(log(RO_Census_Vaslui$Population) ~ log(RO_Census_Vaslui$Rank + VS_ZM_m))
summary(VS_ZM_lm)
VS_pred_ZM_lm <- exp(predict(VS_ZM_lm, newdata = RO_Census_Vaslui, 
                             interval = "prediction", level = 0.95))
VS_ZM_out <- RO_Census_Vaslui %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VS_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Vaslui), 
         DFFIT = dffits(VS_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VS_ZM_lm$coefficients) / nrow(RO_Census_Vaslui)), 
         Rezid_Std = rstudent(VS_ZM_lm), Leverage = hatvalues(VS_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VS_ZM_lm)), 
         DFBETA = dfbetas(VS_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Vaslui)))
VS_ZM_whtest <- white(VS_ZM_lm, interactions = TRUE)
VS_ZM_bptest <- bptest(VS_ZM_lm, studentize = TRUE)
VS_ZM_DWtest <- dwtest(VS_ZM_lm)
VS_ZM_bgtest <- bgtest(VS_ZM_lm, order = 1)
VS_ZM_jbtest <- jarque.bera.test(resid(VS_ZM_lm))
VS_ZM_shtest <- shapiro.test(resid(VS_ZM_lm))

# Exponential Law
VS_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Vaslui)
summary(VS_exp_lm)
VS_pred_exp_lm <- exp(predict(VS_exp_lm, newdata = RO_Census_Vaslui, 
                              interval = "prediction", level = 0.95))
VS_exp_out <- RO_Census_Vaslui %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VS_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Vaslui), 
         DFFIT = dffits(VS_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VS_exp_lm$coefficients) / nrow(RO_Census_Vaslui)), 
         Rezid_Std = rstudent(VS_exp_lm), Leverage = hatvalues(VS_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VS_exp_lm)), 
         DFBETA = dfbetas(VS_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Vaslui)))
VS_exp_whtest <- white(VS_exp_lm, interactions = TRUE)
VS_exp_bptest <- bptest(VS_exp_lm, studentize = TRUE)
VS_exp_DWtest <- dwtest(VS_exp_lm)
VS_exp_bgtest <- bgtest(VS_exp_lm, order = 1)
VS_exp_jbtest <- jarque.bera.test(resid(VS_exp_lm))
VS_exp_shtest <- shapiro.test(resid(VS_exp_lm))

# Lavalette Function
RO_Census_Vaslui <- RO_Census_Vaslui %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Vaslui) - Rank + 1))
)
VS_lav_lm <- lm(log(RO_Census_Vaslui$Population) ~ RO_Census_Vaslui$Lav_exp)
summary(VS_lav_lm)
VS_lav_kst <- exp(signif(VS_lav_lm$coef[[1]], 4))
VS_lav_chi <- signif(VS_lav_lm$coef[[2]], 4)
VS_pred_lav_lm <- exp(predict(VS_lav_lm, newdata = RO_Census_Vaslui, 
                              interval = "prediction", level = 0.95))
VS_lav_out <- RO_Census_Vaslui %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VS_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Vaslui), 
         DFFIT = dffits(VS_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VS_lav_lm$coefficients) / nrow(RO_Census_Vaslui)), 
         Rezid_Std = rstudent(VS_lav_lm), Leverage = hatvalues(VS_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VS_lav_lm)), 
         DFBETA = dfbetas(VS_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Vaslui)))
VS_lav_whtest <- white(VS_lav_lm, interactions = TRUE)
VS_lav_bptest <- bptest(VS_lav_lm, studentize = TRUE)
VS_lav_DWtest <- dwtest(VS_lav_lm)
VS_lav_bgtest <- bgtest(VS_lav_lm, order = 1)
VS_lav_jbtest <- jarque.bera.test(resid(VS_lav_lm))
VS_lav_shtest <- shapiro.test(resid(VS_lav_lm))

# Data Distributions
RO_Census_Vaslui %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Vaslui$Rank, 
                              Population = VS_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.16,
            label = paste0("y", "==", signif(exp(VS_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(VS_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.16,
            label = paste0("R^2 ==", signif(summary(VS_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vaslui$Rank, 
                              Population = VS_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.14,
            label = paste0("y", "==", signif(exp(VS_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(VS_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.14,
            label = paste0("R^2 ==", signif(summary(VS_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vaslui$Rank, 
                              Population = VS_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.12,
            label = paste0("y", "==", signif(VS_ZM_cst, 4), "%.%", "(",
                           signif(VS_ZM_m, 4), "+ x)^", signif(VS_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.12,
            label = paste0("R^2 ==", signif(summary(VS_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vaslui$Rank, 
                              Population = VS_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.10,
            label = paste0("y", "==", signif(VS_lav_kst, 4), "%.%", "x^",
                           -signif(VS_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.10,
            label = paste0("R^2 ==", signif(summary(VS_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Vaslui judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Vaslui %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Vaslui judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vaslui %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Vaslui judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vaslui %>%
     ggplot(aes(x = log(Rank + VS_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Vaslui judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vaslui %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Vaslui judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 40. Valcea
RO_Census_Valcea <- RO_Census_rank %>% filter(Judet_Name == "VALCEA")
#View(RO_Census_Valcea)

# Power Law
VL_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Valcea)
summary(VL_pw_lm)
VL_pred_pw_lm <- exp(predict(VL_pw_lm, newdata = RO_Census_Valcea, 
                             interval = "prediction", level = 0.95))
VL_pw_out <- RO_Census_Valcea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VL_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Valcea), 
         DFFIT = dffits(VL_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VL_pw_lm$coefficients) / nrow(RO_Census_Valcea)), 
         Rezid_Std = rstudent(VL_pw_lm), Leverage = hatvalues(VL_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VL_pw_lm)), 
         DFBETA = dfbetas(VL_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Valcea)))
VL_pw_whtest <- white(VL_pw_lm, interactions = TRUE)
VL_pw_bptest <- bptest(VL_pw_lm, studentize = TRUE)
VL_pw_DWtest <- dwtest(VL_pw_lm)
VL_pw_bgtest <- bgtest(VL_pw_lm, order = 1)
VL_pw_jbtest <- jarque.bera.test(resid(VL_pw_lm))
VL_pw_shtest <- shapiro.test(resid(VL_pw_lm))

# Zipf_Mandelbrot Law
VL_ZM_prm <- get_ZM_Param("RO_Census_Valcea", "Population", "Rank")
VL_ZM_m <- VL_ZM_prm$m
VL_ZM_alpha <- VL_ZM_prm$alpha_ZM
VL_ZM_cst <- VL_ZM_prm$constant_ZM
VL_ZM_lm <- lm(log(RO_Census_Valcea$Population) ~ log(RO_Census_Valcea$Rank + VL_ZM_m))
summary(VL_ZM_lm)
VL_pred_ZM_lm <- exp(predict(VL_ZM_lm, newdata = RO_Census_Valcea, 
                             interval = "prediction", level = 0.95))
VL_ZM_out <- RO_Census_Valcea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VL_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Valcea), 
         DFFIT = dffits(VL_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VL_ZM_lm$coefficients) / nrow(RO_Census_Valcea)), 
         Rezid_Std = rstudent(VL_ZM_lm), Leverage = hatvalues(VL_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VL_ZM_lm)), 
         DFBETA = dfbetas(VL_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Valcea)))
VL_ZM_whtest <- white(VL_ZM_lm, interactions = TRUE)
VL_ZM_bptest <- bptest(VL_ZM_lm, studentize = TRUE)
VL_ZM_DWtest <- dwtest(VL_ZM_lm)
VL_ZM_bgtest <- bgtest(VL_ZM_lm, order = 1)
VL_ZM_jbtest <- jarque.bera.test(resid(VL_ZM_lm))
VL_ZM_shtest <- shapiro.test(resid(VL_ZM_lm))

# Exponential Law
VL_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Valcea)
summary(VL_exp_lm)
VL_pred_exp_lm <- exp(predict(VL_exp_lm, newdata = RO_Census_Valcea, 
                              interval = "prediction", level = 0.95))
VL_exp_out <- RO_Census_Valcea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VL_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Valcea), 
         DFFIT = dffits(VL_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VL_exp_lm$coefficients) / nrow(RO_Census_Valcea)), 
         Rezid_Std = rstudent(VL_exp_lm), Leverage = hatvalues(VL_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VL_exp_lm)), 
         DFBETA = dfbetas(VL_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Valcea)))
VL_exp_whtest <- white(VL_exp_lm, interactions = TRUE)
VL_exp_bptest <- bptest(VL_exp_lm, studentize = TRUE)
VL_exp_DWtest <- dwtest(VL_exp_lm)
VL_exp_bgtest <- bgtest(VL_exp_lm, order = 1)
VL_exp_jbtest <- jarque.bera.test(resid(VL_exp_lm))
VL_exp_shtest <- shapiro.test(resid(VL_exp_lm))

# Lavalette Function
RO_Census_Valcea <- RO_Census_Valcea %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Valcea) - Rank + 1))
)
VL_lav_lm <- lm(log(RO_Census_Valcea$Population) ~ RO_Census_Valcea$Lav_exp)
summary(VL_lav_lm)
VL_lav_kst <- exp(signif(VL_lav_lm$coef[[1]], 4))
VL_lav_chi <- signif(VL_lav_lm$coef[[2]], 4)
VL_pred_lav_lm <- exp(predict(VL_lav_lm, newdata = RO_Census_Valcea, 
                              interval = "prediction", level = 0.95))
VL_lav_out <- RO_Census_Valcea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VL_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Valcea), 
         DFFIT = dffits(VL_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VL_lav_lm$coefficients) / nrow(RO_Census_Valcea)), 
         Rezid_Std = rstudent(VL_lav_lm), Leverage = hatvalues(VL_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VL_lav_lm)), 
         DFBETA = dfbetas(VL_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Valcea)))
VL_lav_whtest <- white(VL_lav_lm, interactions = TRUE)
VL_lav_bptest <- bptest(VL_lav_lm, studentize = TRUE)
VL_lav_DWtest <- dwtest(VL_lav_lm)
VL_lav_bgtest <- bgtest(VL_lav_lm, order = 1)
VL_lav_jbtest <- jarque.bera.test(resid(VL_lav_lm))
VL_lav_shtest <- shapiro.test(resid(VL_lav_lm))

# Data Distributions
RO_Census_Valcea %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Valcea$Rank, 
                              Population = VL_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.25,
            label = paste0("y", "==", signif(exp(VL_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(VL_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.25,
            label = paste0("R^2 ==", signif(summary(VL_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Valcea$Rank, 
                              Population = VL_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.23,
            label = paste0("y", "==", signif(exp(VL_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(VL_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.23,
            label = paste0("R^2 ==", signif(summary(VL_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Valcea$Rank, 
                              Population = VL_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.21,
            label = paste0("y", "==", signif(VL_ZM_cst, 4), "%.%", "(",
                           signif(VL_ZM_m, 4), "+ x)^", signif(VL_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.21,
            label = paste0("R^2 ==", signif(summary(VL_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Valcea$Rank, 
                              Population = VL_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.19,
            label = paste0("y", "==", signif(VL_lav_kst, 4), "%.%", "x^",
                           -signif(VL_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.19,
            label = paste0("R^2 ==", signif(summary(VL_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Valcea judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Valcea %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Valcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Valcea %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Valcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Valcea %>%
     ggplot(aes(x = log(Rank + VL_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Valcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Valcea %>%
     ggplot(aes(x = Lav_exp, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#F8A02EFF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Lavalette Linear Regression on Valcea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  ncol = 2, nrow = 2)

# 41. Vrancea
RO_Census_Vrancea <- RO_Census_rank %>% filter(Judet_Name == "VRANCEA")
#View(RO_Census_Vrancea)

# Power Law
VN_pw_lm <- lm(log(Population) ~ log(Rank), data = RO_Census_Vrancea)
summary(VN_pw_lm)
VN_pred_pw_lm <- exp(predict(VN_pw_lm, newdata = RO_Census_Vrancea, 
                             interval = "prediction", level = 0.95))
VN_pw_out <- RO_Census_Vrancea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VN_pw_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Vrancea), 
         DFFIT = dffits(VN_pw_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VN_pw_lm$coefficients) / nrow(RO_Census_Vrancea)), 
         Rezid_Std = rstudent(VN_pw_lm), Leverage = hatvalues(VN_pw_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VN_pw_lm)), 
         DFBETA = dfbetas(VN_pw_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Vrancea)))
VN_pw_whtest <- white(VN_pw_lm, interactions = TRUE)
VN_pw_bptest <- bptest(VN_pw_lm, studentize = TRUE)
VN_pw_DWtest <- dwtest(VN_pw_lm)
VN_pw_bgtest <- bgtest(VN_pw_lm, order = 1)
VN_pw_jbtest <- jarque.bera.test(resid(VN_pw_lm))
VN_pw_shtest <- shapiro.test(resid(VN_pw_lm))

# Zipf_Mandelbrot Law
VN_ZM_prm <- get_ZM_Param("RO_Census_Vrancea", "Population", "Rank")
VN_ZM_m <- VN_ZM_prm$m
VN_ZM_alpha <- VN_ZM_prm$alpha_ZM
VN_ZM_cst <- VN_ZM_prm$constant_ZM
VN_ZM_lm <- lm(log(RO_Census_Vrancea$Population) ~ log(RO_Census_Vrancea$Rank + VN_ZM_m))
summary(VN_ZM_lm)
VN_pred_ZM_lm <- exp(predict(VN_ZM_lm, newdata = RO_Census_Vrancea, 
                             interval = "prediction", level = 0.95))
VN_ZM_out <- RO_Census_Vrancea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VN_ZM_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Vrancea), 
         DFFIT = dffits(VN_ZM_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VN_ZM_lm$coefficients) / nrow(RO_Census_Vrancea)), 
         Rezid_Std = rstudent(VN_ZM_lm), Leverage = hatvalues(VN_ZM_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VN_ZM_lm)), 
         DFBETA = dfbetas(VN_ZM_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Vrancea)))
VN_ZM_whtest <- white(VN_ZM_lm, interactions = TRUE)
VN_ZM_bptest <- bptest(VN_ZM_lm, studentize = TRUE)
VN_ZM_DWtest <- dwtest(VN_ZM_lm)
VN_ZM_bgtest <- bgtest(VN_ZM_lm, order = 1)
VN_ZM_jbtest <- jarque.bera.test(resid(VN_ZM_lm))
VN_ZM_shtest <- shapiro.test(resid(VN_ZM_lm))

# Exponential Law
VN_exp_lm <- lm(log(Population) ~ Rank, RO_Census_Vrancea)
summary(VN_exp_lm)
VN_pred_exp_lm <- exp(predict(VN_exp_lm, newdata = RO_Census_Vrancea, 
                              interval = "prediction", level = 0.95))
VN_exp_out <- RO_Census_Vrancea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VN_exp_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Vrancea), 
         DFFIT = dffits(VN_exp_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VN_exp_lm$coefficients) / nrow(RO_Census_Vrancea)), 
         Rezid_Std = rstudent(VN_exp_lm), Leverage = hatvalues(VN_exp_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VN_exp_lm)), 
         DFBETA = dfbetas(VN_exp_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Vrancea)))
VN_exp_whtest <- white(VN_exp_lm, interactions = TRUE)
VN_exp_bptest <- bptest(VN_exp_lm, studentize = TRUE)
VN_exp_DWtest <- dwtest(VN_exp_lm)
VN_exp_bgtest <- bgtest(VN_exp_lm, order = 1)
VN_exp_jbtest <- jarque.bera.test(resid(VN_exp_lm))
VN_exp_shtest <- shapiro.test(resid(VN_exp_lm))

# Lavalette Function
RO_Census_Vrancea <- RO_Census_Vrancea %>% mutate(
  Lav_exp = (log(Rank) - log(nrow(RO_Census_Vrancea) - Rank + 1))
)
VN_lav_lm <- lm(log(RO_Census_Vrancea$Population) ~ RO_Census_Vrancea$Lav_exp)
summary(VN_lav_lm)
VN_lav_kst <- exp(signif(VN_lav_lm$coef[[1]], 4))
VN_lav_chi <- signif(VN_lav_lm$coef[[2]], 4)
VN_pred_lav_lm <- exp(predict(VN_lav_lm, newdata = RO_Census_Vrancea, 
                              interval = "prediction", level = 0.95))
VN_lav_out <- RO_Census_Vrancea %>% dplyr::select(Unit_Name, Unit_Type, Environment, Judet_Name) %>%
  mutate(Cooks_Dist = cooks.distance(VN_lav_lm), Cooks_Dist_Trsh = 4/nrow(RO_Census_Vrancea), 
         DFFIT = dffits(VN_lav_lm), 
         DFFIT_Trsh = 2 * sqrt(length(VN_lav_lm$coefficients) / nrow(RO_Census_Vrancea)), 
         Rezid_Std = rstudent(VN_lav_lm), Leverage = hatvalues(VN_lav_lm), 
         Lev_Trsh = 2 * mean(hatvalues(VN_lav_lm)), 
         DFBETA = dfbetas(VN_lav_lm), DFBETA_Trsh = 2 / sqrt(nrow(RO_Census_Vrancea)))
VN_lav_whtest <- white(VN_lav_lm, interactions = TRUE)
VN_lav_bptest <- bptest(VN_lav_lm, studentize = TRUE)
VN_lav_DWtest <- dwtest(VN_lav_lm)
VN_lav_bgtest <- bgtest(VN_lav_lm, order = 1)
VN_lav_jbtest <- jarque.bera.test(resid(VN_lav_lm))
VN_lav_shtest <- shapiro.test(resid(VN_lav_lm))

# Data Distributions
RO_Census_Vrancea %>%
  ggplot(aes(x = Rank, y = Population)) +
  geom_point(alpha = 0.8, size = 2, show.legend = FALSE) +
  geom_line(data = data.frame(Rank = RO_Census_Vrancea$Rank, 
                              Population = VN_pred_pw_lm[,1]), 
            aes(color = "Power Law"), linetype = "solid", linewidth=0.9) +
  geom_text(x = 40, y = 0.19,
            label = paste0("y", "==", signif(exp(VN_pw_lm$coefficients[[1]]), 4), "%.%", 
                           "x^", signif(VN_pw_lm$coefficients[[2]], 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_text(x = 62, y = 0.19,
            label = paste0("R^2 ==", signif(summary(VN_pw_lm)$r.squared, 4)),
            color = '#ED3F39FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vrancea$Rank, 
                              Population = VN_pred_exp_lm[,1]), 
            aes(color = "Exponential Law"), linetype = "twodash", linewidth=1.2) +
  geom_text(x = 40, y = 0.17,
            label = paste0("y", "==", signif(exp(VN_exp_lm$coefficients[[1]]), 4), "%.%", 
                           signif(exp(VN_exp_lm$coefficients[[2]]), 4), "^x"),
            color = '#088BBEFF', parse = TRUE) +
  geom_text(x = 62, y = 0.17,
            label = paste0("R^2 ==", signif(summary(VN_exp_lm)$r.squared, 4)),
            color = '#088BBEFF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vrancea$Rank, 
                              Population = VN_pred_ZM_lm[,1]), 
            aes(color = "Zipf-Mandelbrot Law"), linetype = "longdash", linewidth=1.1) +
  geom_text(x = 40, y = 0.15,
            label = paste0("y", "==", signif(VN_ZM_cst, 4), "%.%", "(",
                           signif(VN_ZM_m, 4), "+ x)^", signif(VN_ZM_alpha, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_text(x = 62, y = 0.15,
            label = paste0("R^2 ==", signif(summary(VN_ZM_lm)$r.squared, 4)),
            color = '#1B7837FF', parse = TRUE) +
  geom_line(data = data.frame(Rank = RO_Census_Vrancea$Rank, 
                              Population = VN_pred_lav_lm[,1]), 
            aes(color = "Lavalette Function"), linetype = "dashed", linewidth=1.1) +
  geom_text(x = 40, y = 0.13,
            label = paste0("y", "==", signif(VN_lav_kst, 4), "%.%", "x^",
                           -signif(VN_lav_chi, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  geom_text(x = 62, y = 0.13,
            label = paste0("R^2 ==", signif(summary(VN_lav_lm)$r.squared, 4)),
            color = '#F8A02EFF', parse = TRUE) +
  scale_color_manual(values = c("Power Law" = "#ED3F39FF", 
                                "Exponential Law" = "#088BBEFF", 
                                "Zipf-Mandelbrot Law" = "#1B7837FF",
                                "Lavalette Function" = "#F8A02EFF")) +
  labs(x = "Rank", y = "Population Frequency", color = "Legend") +
  #ggtitle("Data Distribution on Vrancea judet (With Outliers)") +
  theme(plot.title = element_text(hjust = 0.5))

# Linear Regressions
grid.arrange(
  (RO_Census_Vrancea %>%
     ggplot(aes(x = log(Rank), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#ED3F39FF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Power Linear Regression on Vrancea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vrancea %>%
     ggplot(aes(x = Rank, y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = "#088BBEFF") +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Exponential Linear Regression on Vrancea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vrancea %>%
     ggplot(aes(x = log(Rank + VN_ZM_m), y = log(Population))) +
     geom_point(alpha = 0.8, show.legend = FALSE) +
     geom_smooth(method = "lm", se = FALSE, color = '#1B7837FF') +
     labs(x = "Rank", y = "Population Frequency") +
     ggtitle("Zipf-Mandelbrot Linear Regression on Vrancea judet") +
     theme(plot.title = element_text(hjust = 0.5))
  ),
  (RO_Census_Vrancea %>%
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

# 2. Predicted Values
RO_Census_Alba <- RO_Census_Alba %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = AB_pred_pw_lm[,1], EXP_Pred_Y = AB_pred_exp_lm[,1], 
         ZM_Pred_Y = AB_pred_ZM_lm[,1], Lav_Pred_Y = AB_pred_lav_lm[,1])
RO_Census_Arad <- RO_Census_Arad %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = AR_pred_pw_lm[,1], EXP_Pred_Y = AR_pred_exp_lm[,1], 
         ZM_Pred_Y = AR_pred_ZM_lm[,1], Lav_Pred_Y = AR_pred_lav_lm[,1])
RO_Census_Arges <- RO_Census_Arges %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = AG_pred_pw_lm[,1], EXP_Pred_Y = AG_pred_exp_lm[,1], 
         ZM_Pred_Y = AG_pred_ZM_lm[,1], Lav_Pred_Y = AG_pred_lav_lm[,1])
RO_Census_Bacau <- RO_Census_Bacau %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = BC_pred_pw_lm[,1], EXP_Pred_Y = BC_pred_exp_lm[,1], 
         ZM_Pred_Y = BC_pred_ZM_lm[,1], Lav_Pred_Y = BC_pred_lav_lm[,1])
RO_Census_Bihor <- RO_Census_Bihor %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = BH_pred_pw_lm[,1], EXP_Pred_Y = BH_pred_exp_lm[,1], 
         ZM_Pred_Y = BH_pred_ZM_lm[,1], Lav_Pred_Y = BH_pred_lav_lm[,1])
RO_Census_BistNsd <- RO_Census_BistNsd %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = BN_pred_pw_lm[,1], EXP_Pred_Y = BN_pred_exp_lm[,1], 
         ZM_Pred_Y = BN_pred_ZM_lm[,1], Lav_Pred_Y = BN_pred_lav_lm[,1])
RO_Census_Botosani <- RO_Census_Botosani %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = BT_pred_pw_lm[,1], EXP_Pred_Y = BT_pred_exp_lm[,1], 
         ZM_Pred_Y = BT_pred_ZM_lm[,1], Lav_Pred_Y = BT_pred_lav_lm[,1])
RO_Census_Brasov <- RO_Census_Brasov %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = BV_pred_pw_lm[,1], EXP_Pred_Y = BV_pred_exp_lm[,1], 
         ZM_Pred_Y = BV_pred_ZM_lm[,1], Lav_Pred_Y = BV_pred_lav_lm[,1])
RO_Census_Braila <- RO_Census_Braila %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = BR_pred_pw_lm[,1], EXP_Pred_Y = BR_pred_exp_lm[,1], 
         ZM_Pred_Y = BR_pred_ZM_lm[,1], Lav_Pred_Y = BR_pred_lav_lm[,1])
RO_Census_Buzau <- RO_Census_Buzau %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = BZ_pred_pw_lm[,1], EXP_Pred_Y = BZ_pred_exp_lm[,1], 
         ZM_Pred_Y = BZ_pred_ZM_lm[,1], Lav_Pred_Y = BZ_pred_lav_lm[,1])
RO_Census_CarSev <- RO_Census_CarSev %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = CS_pred_pw_lm[,1], EXP_Pred_Y = CS_pred_exp_lm[,1], 
         ZM_Pred_Y = CS_pred_ZM_lm[,1], Lav_Pred_Y = CS_pred_lav_lm[,1])
RO_Census_Calarasi <- RO_Census_Calarasi %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = CL_pred_pw_lm[,1], EXP_Pred_Y = CL_pred_exp_lm[,1], 
         ZM_Pred_Y = CL_pred_ZM_lm[,1], Lav_Pred_Y = CL_pred_lav_lm[,1])
RO_Census_Cluj <- RO_Census_Cluj %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = CJ_pred_pw_lm[,1], EXP_Pred_Y = CJ_pred_exp_lm[,1], 
         ZM_Pred_Y = CJ_pred_ZM_lm[,1], Lav_Pred_Y = CJ_pred_lav_lm[,1])
RO_Census_Constanta <- RO_Census_Constanta %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = CT_pred_pw_lm[,1], EXP_Pred_Y = CT_pred_exp_lm[,1], 
         ZM_Pred_Y = CT_pred_ZM_lm[,1], Lav_Pred_Y = CT_pred_lav_lm[,1])
RO_Census_Covasna <- RO_Census_Covasna %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = CV_pred_pw_lm[,1], EXP_Pred_Y = CV_pred_exp_lm[,1], 
         ZM_Pred_Y = CV_pred_ZM_lm[,1], Lav_Pred_Y = CV_pred_lav_lm[,1])
RO_Census_Dambovita <- RO_Census_Dambovita %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = DB_pred_pw_lm[,1], EXP_Pred_Y = DB_pred_exp_lm[,1], 
         ZM_Pred_Y = DB_pred_ZM_lm[,1], Lav_Pred_Y = DB_pred_lav_lm[,1])
RO_Census_Dolj <- RO_Census_Dolj %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = DJ_pred_pw_lm[,1], EXP_Pred_Y = DJ_pred_exp_lm[,1], 
         ZM_Pred_Y = DJ_pred_ZM_lm[,1], Lav_Pred_Y = DJ_pred_lav_lm[,1])
RO_Census_Galati <- RO_Census_Galati %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = GL_pred_pw_lm[,1], EXP_Pred_Y = GL_pred_exp_lm[,1], 
         ZM_Pred_Y = GL_pred_ZM_lm[,1], Lav_Pred_Y = GL_pred_lav_lm[,1])
RO_Census_Giurgiu <- RO_Census_Giurgiu %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = GR_pred_pw_lm[,1], EXP_Pred_Y = GR_pred_exp_lm[,1], 
         ZM_Pred_Y = GR_pred_ZM_lm[,1], Lav_Pred_Y = GR_pred_lav_lm[,1])
RO_Census_Gorj <- RO_Census_Gorj %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = GJ_pred_pw_lm[,1], EXP_Pred_Y = GJ_pred_exp_lm[,1], 
         ZM_Pred_Y = GJ_pred_ZM_lm[,1], Lav_Pred_Y = GJ_pred_lav_lm[,1])
RO_Census_Harghita <- RO_Census_Harghita %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = HR_pred_pw_lm[,1], EXP_Pred_Y = HR_pred_exp_lm[,1], 
         ZM_Pred_Y = HR_pred_ZM_lm[,1], Lav_Pred_Y = HR_pred_lav_lm[,1])
RO_Census_Hunedoara <- RO_Census_Hunedoara %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = HD_pred_pw_lm[,1], EXP_Pred_Y = HD_pred_exp_lm[,1], 
         ZM_Pred_Y = HD_pred_ZM_lm[,1], Lav_Pred_Y = HD_pred_lav_lm[,1])
RO_Census_Ialomita <- RO_Census_Ialomita %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = IL_pred_pw_lm[,1], EXP_Pred_Y = IL_pred_exp_lm[,1], 
         ZM_Pred_Y = IL_pred_ZM_lm[,1], Lav_Pred_Y = IL_pred_lav_lm[,1])
RO_Census_Iasi <- RO_Census_Iasi %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = IS_pred_pw_lm[,1], EXP_Pred_Y = IS_pred_exp_lm[,1], 
         ZM_Pred_Y = IS_pred_ZM_lm[,1], Lav_Pred_Y = IS_pred_lav_lm[,1])
RO_Census_Ilfov <- RO_Census_Ilfov %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = IF_pred_pw_lm[,1], EXP_Pred_Y = IF_pred_exp_lm[,1], 
         ZM_Pred_Y = IF_pred_ZM_lm[,1], Lav_Pred_Y = IF_pred_lav_lm[,1])
RO_Census_Maramures <- RO_Census_Maramures %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = MM_pred_pw_lm[,1], EXP_Pred_Y = MM_pred_exp_lm[,1], 
         ZM_Pred_Y = MM_pred_ZM_lm[,1], Lav_Pred_Y = MM_pred_lav_lm[,1])
RO_Census_Mehedinti <- RO_Census_Mehedinti %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = MH_pred_pw_lm[,1], EXP_Pred_Y = MH_pred_exp_lm[,1], 
         ZM_Pred_Y = MH_pred_ZM_lm[,1], Lav_Pred_Y = MH_pred_lav_lm[,1])
RO_Census_Mures <- RO_Census_Mures %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = MS_pred_pw_lm[,1], EXP_Pred_Y = MS_pred_exp_lm[,1], 
         ZM_Pred_Y = MS_pred_ZM_lm[,1], Lav_Pred_Y = MS_pred_lav_lm[,1])
RO_Census_Neamt <- RO_Census_Neamt %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = NT_pred_pw_lm[,1], EXP_Pred_Y = NT_pred_exp_lm[,1], 
         ZM_Pred_Y = NT_pred_ZM_lm[,1], Lav_Pred_Y = NT_pred_lav_lm[,1])
RO_Census_Olt <- RO_Census_Olt %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = OT_pred_pw_lm[,1], EXP_Pred_Y = OT_pred_exp_lm[,1], 
         ZM_Pred_Y = OT_pred_ZM_lm[,1], Lav_Pred_Y = OT_pred_lav_lm[,1])
RO_Census_Prahova <- RO_Census_Prahova %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = PH_pred_pw_lm[,1], EXP_Pred_Y = PH_pred_exp_lm[,1], 
         ZM_Pred_Y = PH_pred_ZM_lm[,1], Lav_Pred_Y = PH_pred_lav_lm[,1])
RO_Census_SatuMare <- RO_Census_SatuMare %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = SM_pred_pw_lm[,1], EXP_Pred_Y = SM_pred_exp_lm[,1], 
         ZM_Pred_Y = SM_pred_ZM_lm[,1], Lav_Pred_Y = SM_pred_lav_lm[,1])
RO_Census_Salaj <- RO_Census_Salaj %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = SJ_pred_pw_lm[,1], EXP_Pred_Y = SJ_pred_exp_lm[,1], 
         ZM_Pred_Y = SJ_pred_ZM_lm[,1], Lav_Pred_Y = SJ_pred_lav_lm[,1])
RO_Census_Sibiu <- RO_Census_Sibiu %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = SB_pred_pw_lm[,1], EXP_Pred_Y = SB_pred_exp_lm[,1], 
         ZM_Pred_Y = SB_pred_ZM_lm[,1], Lav_Pred_Y = SB_pred_lav_lm[,1])
RO_Census_Suceava <- RO_Census_Suceava %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = SV_pred_pw_lm[,1], EXP_Pred_Y = SV_pred_exp_lm[,1], 
         ZM_Pred_Y = SV_pred_ZM_lm[,1], Lav_Pred_Y = SV_pred_lav_lm[,1])
RO_Census_Teleorman <- RO_Census_Teleorman %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = TR_pred_pw_lm[,1], EXP_Pred_Y = TR_pred_exp_lm[,1], 
         ZM_Pred_Y = TR_pred_ZM_lm[,1], Lav_Pred_Y = TR_pred_lav_lm[,1])
RO_Census_Timis <- RO_Census_Timis %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = TM_pred_pw_lm[,1], EXP_Pred_Y = TM_pred_exp_lm[,1], 
         ZM_Pred_Y = TM_pred_ZM_lm[,1], Lav_Pred_Y = TM_pred_lav_lm[,1])
RO_Census_Tulcea <- RO_Census_Tulcea %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = TL_pred_pw_lm[,1], EXP_Pred_Y = TL_pred_exp_lm[,1], 
         ZM_Pred_Y = TL_pred_ZM_lm[,1], Lav_Pred_Y = TL_pred_lav_lm[,1])
RO_Census_Vaslui <- RO_Census_Vaslui %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = VS_pred_pw_lm[,1], EXP_Pred_Y = VS_pred_exp_lm[,1], 
         ZM_Pred_Y = VS_pred_ZM_lm[,1], Lav_Pred_Y = VS_pred_lav_lm[,1])
RO_Census_Valcea <- RO_Census_Valcea %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = VL_pred_pw_lm[,1], EXP_Pred_Y = VL_pred_exp_lm[,1], 
         ZM_Pred_Y = VL_pred_ZM_lm[,1], Lav_Pred_Y = VL_pred_lav_lm[,1])
RO_Census_Vrancea <- RO_Census_Vrancea %>% dplyr::select(-Lav_exp) %>% 
  mutate(PW_Pred_Y = VN_pred_pw_lm[,1], EXP_Pred_Y = VN_pred_exp_lm[,1], 
         ZM_Pred_Y = VN_pred_ZM_lm[,1], Lav_Pred_Y = VN_pred_lav_lm[,1])

# 3. Outliers metrics
Alba_Outliers <- bind_rows(
  (AB_pw_out %>% mutate(Function_Name = "Power")),
  (AB_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (AB_exp_out %>% mutate(Function_Name = "Exponential")),
  (AB_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Arad_Outliers <- bind_rows(
  (AR_pw_out %>% mutate(Function_Name = "Power")),
  (AR_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (AR_exp_out %>% mutate(Function_Name = "Exponential")),
  (AR_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Arges_Outliers <- bind_rows(
  (AG_pw_out %>% mutate(Function_Name = "Power")),
  (AG_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (AG_exp_out %>% mutate(Function_Name = "Exponential")),
  (AG_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Bacau_Outliers <- bind_rows(
  (BC_pw_out %>% mutate(Function_Name = "Power")),
  (BC_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (BC_exp_out %>% mutate(Function_Name = "Exponential")),
  (BC_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Bihor_Outliers <- bind_rows(
  (BH_pw_out %>% mutate(Function_Name = "Power")),
  (BH_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (BH_exp_out %>% mutate(Function_Name = "Exponential")),
  (BH_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
BistNsd_Outliers <- bind_rows(
  (BN_pw_out %>% mutate(Function_Name = "Power")),
  (BN_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (BN_exp_out %>% mutate(Function_Name = "Exponential")),
  (BN_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Botosani_Outliers <- bind_rows(
  (BT_pw_out %>% mutate(Function_Name = "Power")),
  (BT_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (BT_exp_out %>% mutate(Function_Name = "Exponential")),
  (BT_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Brasov_Outliers <- bind_rows(
  (BV_pw_out %>% mutate(Function_Name = "Power")),
  (BV_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (BV_exp_out %>% mutate(Function_Name = "Exponential")),
  (BV_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Braila_Outliers <- bind_rows(
  (BR_pw_out %>% mutate(Function_Name = "Power")),
  (BR_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (BR_exp_out %>% mutate(Function_Name = "Exponential")),
  (BR_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Buzau_Outliers <- bind_rows(
  (BZ_pw_out %>% mutate(Function_Name = "Power")),
  (BZ_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (BZ_exp_out %>% mutate(Function_Name = "Exponential")),
  (BZ_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
CarSev_Outliers <- bind_rows(
  (CS_pw_out %>% mutate(Function_Name = "Power")),
  (CS_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (CS_exp_out %>% mutate(Function_Name = "Exponential")),
  (CS_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Calarasi_Outliers <- bind_rows(
  (CL_pw_out %>% mutate(Function_Name = "Power")),
  (CL_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (CL_exp_out %>% mutate(Function_Name = "Exponential")),
  (CL_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Cluj_Outliers <- bind_rows(
  (CJ_pw_out %>% mutate(Function_Name = "Power")),
  (CJ_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (CJ_exp_out %>% mutate(Function_Name = "Exponential")),
  (CJ_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Constanta_Outliers <- bind_rows(
  (CT_pw_out %>% mutate(Function_Name = "Power")),
  (CT_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (CT_exp_out %>% mutate(Function_Name = "Exponential")),
  (CT_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Covasna_Outliers <- bind_rows(
  (CV_pw_out %>% mutate(Function_Name = "Power")),
  (CV_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (CV_exp_out %>% mutate(Function_Name = "Exponential")),
  (CV_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Dambovita_Outliers <- bind_rows(
  (DB_pw_out %>% mutate(Function_Name = "Power")),
  (DB_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (DB_exp_out %>% mutate(Function_Name = "Exponential")),
  (DB_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Dolj_Outliers <- bind_rows(
  (DJ_pw_out %>% mutate(Function_Name = "Power")),
  (DJ_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (DJ_exp_out %>% mutate(Function_Name = "Exponential")),
  (DJ_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Galati_Outliers <- bind_rows(
  (GL_pw_out %>% mutate(Function_Name = "Power")),
  (GL_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (GL_exp_out %>% mutate(Function_Name = "Exponential")),
  (GL_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Giurgiu_Outliers <- bind_rows(
  (GR_pw_out %>% mutate(Function_Name = "Power")),
  (GR_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (GR_exp_out %>% mutate(Function_Name = "Exponential")),
  (GR_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Gorj_Outliers <- bind_rows(
  (GJ_pw_out %>% mutate(Function_Name = "Power")),
  (GJ_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (GJ_exp_out %>% mutate(Function_Name = "Exponential")),
  (GJ_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Harghita_Outliers <- bind_rows(
  (HR_pw_out %>% mutate(Function_Name = "Power")),
  (HR_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (HR_exp_out %>% mutate(Function_Name = "Exponential")),
  (HR_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Hunedoara_Outliers <- bind_rows(
  (HD_pw_out %>% mutate(Function_Name = "Power")),
  (HD_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (HD_exp_out %>% mutate(Function_Name = "Exponential")),
  (HD_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Ialomita_Outliers <- bind_rows(
  (IL_pw_out %>% mutate(Function_Name = "Power")),
  (IL_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (IL_exp_out %>% mutate(Function_Name = "Exponential")),
  (IL_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Iasi_Outliers <- bind_rows(
  (IS_pw_out %>% mutate(Function_Name = "Power")),
  (IS_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (IS_exp_out %>% mutate(Function_Name = "Exponential")),
  (IS_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Ilfov_Outliers <- bind_rows(
  (IF_pw_out %>% mutate(Function_Name = "Power")),
  (IF_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (IF_exp_out %>% mutate(Function_Name = "Exponential")),
  (IF_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Maramures_Outliers <- bind_rows(
  (MM_pw_out %>% mutate(Function_Name = "Power")),
  (MM_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (MM_exp_out %>% mutate(Function_Name = "Exponential")),
  (MM_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Mehedinti_Outliers <- bind_rows(
  (MH_pw_out %>% mutate(Function_Name = "Power")),
  (MH_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (MH_exp_out %>% mutate(Function_Name = "Exponential")),
  (MH_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Mures_Outliers <- bind_rows(
  (MS_pw_out %>% mutate(Function_Name = "Power")),
  (MS_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (MS_exp_out %>% mutate(Function_Name = "Exponential")),
  (MS_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Neamt_Outliers <- bind_rows(
  (NT_pw_out %>% mutate(Function_Name = "Power")),
  (NT_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (NT_exp_out %>% mutate(Function_Name = "Exponential")),
  (NT_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Olt_Outliers <- bind_rows(
  (OT_pw_out %>% mutate(Function_Name = "Power")),
  (OT_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (OT_exp_out %>% mutate(Function_Name = "Exponential")),
  (OT_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Prahova_Outliers <- bind_rows(
  (PH_pw_out %>% mutate(Function_Name = "Power")),
  (PH_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (PH_exp_out %>% mutate(Function_Name = "Exponential")),
  (PH_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
SatuMare_Outliers <- bind_rows(
  (SM_pw_out %>% mutate(Function_Name = "Power")),
  (SM_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (SM_exp_out %>% mutate(Function_Name = "Exponential")),
  (SM_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Salaj_Outliers <- bind_rows(
  (SJ_pw_out %>% mutate(Function_Name = "Power")),
  (SJ_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (SJ_exp_out %>% mutate(Function_Name = "Exponential")),
  (SJ_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Sibiu_Outliers <- bind_rows(
  (SB_pw_out %>% mutate(Function_Name = "Power")),
  (SB_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (SB_exp_out %>% mutate(Function_Name = "Exponential")),
  (SB_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Suceava_Outliers <- bind_rows(
  (SV_pw_out %>% mutate(Function_Name = "Power")),
  (SV_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (SV_exp_out %>% mutate(Function_Name = "Exponential")),
  (SV_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Teleorman_Outliers <- bind_rows(
  (TR_pw_out %>% mutate(Function_Name = "Power")),
  (TR_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (TR_exp_out %>% mutate(Function_Name = "Exponential")),
  (TR_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Timis_Outliers <- bind_rows(
  (TM_pw_out %>% mutate(Function_Name = "Power")),
  (TM_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (TM_exp_out %>% mutate(Function_Name = "Exponential")),
  (TM_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Tulcea_Outliers <- bind_rows(
  (TL_pw_out %>% mutate(Function_Name = "Power")),
  (TL_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (TL_exp_out %>% mutate(Function_Name = "Exponential")),
  (TL_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Vaslui_Outliers <- bind_rows(
  (VS_pw_out %>% mutate(Function_Name = "Power")),
  (VS_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (VS_exp_out %>% mutate(Function_Name = "Exponential")),
  (VS_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Valcea_Outliers <- bind_rows(
  (VL_pw_out %>% mutate(Function_Name = "Power")),
  (VL_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (VL_exp_out %>% mutate(Function_Name = "Exponential")),
  (VL_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)
Vrancea_Outliers <- bind_rows(
  (VN_pw_out %>% mutate(Function_Name = "Power")),
  (VN_ZM_out %>% mutate(Function_Name = "Zipf-Mandelbrot")),
  (VN_exp_out %>% mutate(Function_Name = "Exponential")),
  (VN_lav_out %>% mutate(Function_Name = "Lavalette"))
) %>% mutate(DFBETA_intercept = round(DFBETA[,1], 8), 
             DFBETA_var_x = round(DFBETA[,2], 8)) %>% dplyr::select(-DFBETA)

# 4. Adjusted R-Squared
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

# 5. F-Statistic
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

# 6. AIC
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
           file = 'RO_Census_Indicators_lm_popsize.xlsx')

write.xlsx(list(
  "Alba" = RO_Census_Alba, "Arad" = RO_Census_Arad, "Arges" = RO_Census_Arges,
  "Bacau" = RO_Census_Bacau, "Bihor" = RO_Census_Bihor, "Bistrita-Nasaud" = RO_Census_BistNsd,
  "Botosani" = RO_Census_Botosani, "Brasov" = RO_Census_Brasov, "Braila" = RO_Census_Braila, 
  "Buzau" = RO_Census_Buzau, "Caras-Severin" = RO_Census_CarSev, "Calarasi" = RO_Census_Calarasi,
  "Cluj" = RO_Census_Cluj, "Constanta" = RO_Census_Constanta, "Covasna" = RO_Census_Covasna,
  "Dambovita" = RO_Census_Dambovita, "Dolj" = RO_Census_Dolj, "Galati" = RO_Census_Galati,
  "Giurgiu" = RO_Census_Giurgiu, "Gorj" = RO_Census_Gorj, "Harghita" = RO_Census_Harghita,
  "Hunedoara" = RO_Census_Hunedoara, "Ialomita" = RO_Census_Ialomita, "Iasi" = RO_Census_Iasi,
  "Ilfov" = RO_Census_Ilfov, "Maramures" = RO_Census_Maramures, "Mehedinti" = RO_Census_Mehedinti,
  "Mures" = RO_Census_Mures, "Neamt" = RO_Census_Neamt, "Olt" = RO_Census_Olt,
  "Prahova" = RO_Census_Prahova, "Satu Mare" = RO_Census_SatuMare, "Salaj" = RO_Census_Salaj,
  "Sibiu" = RO_Census_Sibiu, "Suceava" = RO_Census_Suceava, "Teleorman" = RO_Census_Teleorman,
  "Timis" = RO_Census_Timis, "Tulcea" = RO_Census_Tulcea, "Vaslui" = RO_Census_Vaslui,
  "Valcea" = RO_Census_Valcea, "Vrancea" = RO_Census_Vrancea
), file = 'RO_Census_Predict_popsize.xlsx')

write.xlsx(list(
  "Alba" = Alba_Outliers, "Arad" = Arad_Outliers, "Arges" = Arges_Outliers,
  "Bacau" = Bacau_Outliers, "Bihor" = Bihor_Outliers, "Bistrita-Nasaud" = BistNsd_Outliers,
  "Botosani" = Botosani_Outliers, "Brasov" = Brasov_Outliers, "Braila" = Braila_Outliers, 
  "Buzau" = Buzau_Outliers, "Caras-Severin" = CarSev_Outliers, "Calarasi" = Calarasi_Outliers,
  "Cluj" = Cluj_Outliers, "Constanta" = Constanta_Outliers, "Covasna" = Covasna_Outliers,
  "Dambovita" = Dambovita_Outliers, "Dolj" = Dolj_Outliers, "Galati" = Galati_Outliers,
  "Giurgiu" = Giurgiu_Outliers, "Gorj" = Gorj_Outliers, "Harghita" = Harghita_Outliers,
  "Hunedoara" = Hunedoara_Outliers, "Ialomita" = Ialomita_Outliers, "Iasi" = Iasi_Outliers,
  "Ilfov" = Ilfov_Outliers, "Maramures" = Maramures_Outliers, "Mehedinti" = Mehedinti_Outliers,
  "Mures" = Mures_Outliers, "Neamt" = Neamt_Outliers, "Olt" = Olt_Outliers,
  "Prahova" = Prahova_Outliers, "Satu Mare" = SatuMare_Outliers, "Salaj" = Salaj_Outliers,
  "Sibiu" = Sibiu_Outliers, "Suceava" = Suceava_Outliers, "Teleorman" = Teleorman_Outliers,
  "Timis" = Timis_Outliers, "Tulcea" = Tulcea_Outliers, "Vaslui" = Vaslui_Outliers,
  "Valcea" = Valcea_Outliers, "Vrancea" = Vrancea_Outliers
), file = 'RO_Census_Outliers_popsize.xlsx')



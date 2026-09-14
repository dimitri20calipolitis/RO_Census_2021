# Load the library
library(tidyverse)
library(readxl)

# Load data
RSquared_vals_df <- read_excel("./RO_Census_Indicators_lm_popsize.xlsx", sheet = "R-Squared")
Adj_RSquared_vals_df <- read_excel("./RO_Census_Indicators_lm_popsize.xlsx", sheet = "Adjusted R-Squared")
FStat_vals_df <- read_excel("./RO_Census_Indicators_lm_popsize.xlsx", sheet = "F-Stat")
AIC_vals_df <- read_excel("./RO_Census_Indicators_lm_popsize.xlsx", sheet = "AIC")
WhiteTest_vals_df <- read_excel("./RO_Census_Indicators_lm_popsize.xlsx", sheet = "White Test")
BPTest_vals_df <- read_excel("./RO_Census_Indicators_lm_popsize.xlsx", sheet = "Breusch-Pagan Test")
DWTest_vals_df <- read_excel("./RO_Census_Indicators_lm_popsize.xlsx", sheet = "Durbin-Watson Test")
BGTest_vals_df <- read_excel("./RO_Census_Indicators_lm_popsize.xlsx", sheet = "Breusch-Godfrey Test")
JBTest_vals_df <- read_excel("./RO_Census_Indicators_lm_popsize.xlsx", sheet = "Jarque-Bera Test")
SHTest_vals_df <- read_excel("./RO_Census_Indicators_lm_popsize.xlsx", sheet = "Shapiro-Wilks")

RSquared_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new_popsize.xlsx", sheet = "R-Squared")
Adj_RSquared_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new_popsize.xlsx", sheet = "Adjusted R-Squared")
FStat_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new_popsize.xlsx", sheet = "F-Stat")
AIC_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new_popsize.xlsx", sheet = "AIC")
WhiteTest_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new_popsize.xlsx", sheet = "White Test")
BPTest_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new_popsize.xlsx", sheet = "Breusch-Pagan Test")
DWTest_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new_popsize.xlsx", sheet = "Durbin-Watson Test")
BGTest_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new_popsize.xlsx", sheet = "Breusch-Godfrey Test")
JBTest_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new_popsize.xlsx", sheet = "Jarque-Bera Test")
SHTest_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new_popsize.xlsx", sheet = "Shapiro-Wilks")

# Check indicators

# 1. R-Squared
RSquared_all <- bind_rows((RSquared_vals_df %>% mutate(Model_Type = "Old")), 
                          (RSquared_vals_df_new %>% mutate(Model_Type = "New")))
View(RSquared_all)

View(RSquared_all %>% filter(R_Squared >= 0.95))

# 2. Adjusted R-Squared
Adj_RSquared_all <- bind_rows((Adj_RSquared_vals_df %>% mutate(Model_Type = "Old")), 
                          (Adj_RSquared_vals_df_new %>% mutate(Model_Type = "New")))
View(Adj_RSquared_all)

View(Adj_RSquared_all %>% filter(Adj_R_Squared >= 0.95))

# 3. F-Stat
FStat_all <- bind_rows((FStat_vals_df %>% mutate(Model_Type = "Old")), 
                          (FStat_vals_df_new %>% mutate(Model_Type = "New")))
View(FStat_all)

View(
FStat_all %>% inner_join(
  (FStat_all %>% group_by(Judet_Name, Model_Type) %>% 
     summarize(F_Stat = max(F_Stat))), 
  by = c("Judet_Name", "Model_Type", "F_Stat")
)
)

# 4. AIC
AIC_all <- bind_rows((AIC_vals_df %>% mutate(Model_Type = "Old")), 
                       (AIC_vals_df_new %>% mutate(Model_Type = "New")))
View(AIC_all)

View(
  AIC_all %>% inner_join(
    (AIC_all %>% group_by(Judet_Name, Model_Type) %>% 
       summarize(AIC = min(AIC))), 
    by = c("Judet_Name", "Model_Type", "AIC")
  )
)

# 5. White Test
White_all <- bind_rows((WhiteTest_vals_df %>% mutate(Model_Type = "Old")), 
                     (WhiteTest_vals_df_new %>% mutate(Model_Type = "New")))
View(White_all)

# 6. Breusch-Pagan Test
BP_all <- bind_rows((BPTest_vals_df %>% mutate(Model_Type = "Old")), 
                     (BPTest_vals_df_new %>% mutate(Model_Type = "New")))
View(BP_all)

# 7. Durbin-Watson Test
DW_all <- bind_rows((DWTest_vals_df %>% mutate(Model_Type = "Old")), 
                     (DWTest_vals_df_new %>% mutate(Model_Type = "New")))
View(DW_all)

# 8. Breusch-Godfrey Test
BG_all <- bind_rows((BGTest_vals_df %>% mutate(Model_Type = "Old")), 
                     (BGTest_vals_df_new %>% mutate(Model_Type = "New")))
View(BG_all)

# 9. Jarque-Bera Test
JB_all <- bind_rows((JBTest_vals_df %>% mutate(Model_Type = "Old")), 
                     (JBTest_vals_df_new %>% mutate(Model_Type = "New")))
View(JB_all)

# 10. Shapiro-Wilks Test
SH_all <- bind_rows((SHTest_vals_df %>% mutate(Model_Type = "Old")), 
                     (SHTest_vals_df_new %>% mutate(Model_Type = "New")))
View(SH_all)

# Create the final df
RO_Census_Indicators <- AIC_all %>% inner_join(
  (AIC_all %>% group_by(Judet_Name, Model_Type) %>% 
     summarize(AIC = min(AIC))), 
  by = c("Judet_Name", "Model_Type", "AIC")
) %>% inner_join(
  (FStat_all %>% inner_join(
    (FStat_all %>% group_by(Judet_Name, Model_Type) %>% 
       summarize(F_Stat = max(F_Stat))), 
    by = c("Judet_Name", "Model_Type", "F_Stat"))), 
  by = c("Judet_Name", "Function_Name", "Model_Type")
) %>% inner_join(Adj_RSquared_all, 
                 by = c("Judet_Name", "Function_Name", "Model_Type")) %>%
  inner_join(RSquared_all, by = c("Judet_Name", "Function_Name", "Model_Type")) %>%
  relocate(Model_Type, .before = AIC) %>% 
  relocate(c(R_Squared, Adj_R_Squared, F_Stat), .before = AIC) %>% 
  arrange(Judet_Name) %>% 
  inner_join(White_all, by = c("Judet_Name", "Function_Name", "Model_Type")) %>%
  rename("White_Homoscedasticity" = "Homoscedasticity") %>%
  inner_join(BP_all, by = c("Judet_Name", "Function_Name", "Model_Type")) %>%
  rename("BP_Homoscedasticity" = "Homoscedasticity") %>%
  inner_join(DW_all, by = c("Judet_Name", "Function_Name", "Model_Type")) %>%
  rename("DW_Autocorrelation" = "Autocorrelation") %>%
  inner_join(BG_all, by = c("Judet_Name", "Function_Name", "Model_Type")) %>%
  rename("BG_Autocorrelation" = "Autocorrelation") %>%
  inner_join(JB_all, by = c("Judet_Name", "Function_Name", "Model_Type")) %>%
  rename("JB_Normal_Distribution" = "Normal_Distribution") %>%
  inner_join(SH_all, by = c("Judet_Name", "Function_Name", "Model_Type")) %>%
  rename("SH_Normal_Distribution" = "Normal_Distribution")
View(RO_Census_Indicators)

View(
hite %>% inner_join(
  (RO_Census_Indicators %>% filter(Model_Type == "New", R_Squared < 0.95) %>%
     select(Judet_Name)), by = "Judet_Name"
)
)

jud_cnt <- RO_Census_Indicators %>% dplyr::select(Judet_Name) %>% 
  distinct() %>% count() %>% as.integer()

RO_Census_Indicators %>% select(Function_Name) %>% distinct() %>%


View(
bind_rows(
(  
RO_Census_Indicators %>% filter(Model_Type == "Old") %>% 
  dplyr::select(Judet_Name, Function_Name) %>% group_by(Function_Name) %>% tally() %>%
  rename('Cnt' = 'n') %>% group_by(Function_Name) %>%
  mutate(Percentage = Cnt/jud_cnt) %>%
  #dplyr::select(-Cnt) %>% 
  right_join(
    (RO_Census_Indicators %>% dplyr::select(Function_Name) %>% distinct())
    ) %>% mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(Model_Type = "Old") %>% relocate(Model_Type, .before = Function_Name)
),
(
RO_Census_Indicators %>% filter(Model_Type == "New") %>% 
  dplyr::select(Judet_Name, Function_Name) %>% group_by(Function_Name) %>% tally() %>%
  rename('Cnt' = 'n') %>% group_by(Function_Name) %>%
  mutate(Percentage = Cnt/jud_cnt) %>%
  #dplyr::select(-Cnt) %>% 
  right_join(
    (RO_Census_Indicators %>% dplyr::select(Function_Name) %>% distinct())
  ) %>% mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(Model_Type = "New") %>% relocate(Model_Type, .before = Function_Name)
)
)
)
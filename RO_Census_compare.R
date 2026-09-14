# Load the library
library(tidyverse)
library(readxl)

# Load data
RSquared_vals_df <- read_excel("./RO_Census_Indicators_lm.xlsx", sheet = "R-Squared")
Adj_RSquared_vals_df <- read_excel("./RO_Census_Indicators_lm.xlsx", sheet = "Adjusted R-Squared")
FStat_vals_df <- read_excel("./RO_Census_Indicators_lm.xlsx", sheet = "F-Stat")
AIC_vals_df <- read_excel("./RO_Census_Indicators_lm.xlsx", sheet = "AIC")

RSquared_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new.xlsx", sheet = "R-Squared")
Adj_RSquared_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new.xlsx", sheet = "Adjusted R-Squared")
FStat_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new.xlsx", sheet = "F-Stat")
AIC_vals_df_new <- read_excel("./RO_Census_Indicators_lm_new.xlsx", sheet = "AIC")

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
  relocate(c(R_Squared, Adj_R_Squared, F_Stat), .before = AIC) %>% arrange(Judet_Name)
View(RO_Census_Indicators)

View(
RO_Census_Indicators %>% inner_join(
  (RO_Census_Indicators %>% filter(Model_Type == "New", R_Squared < 0.95) %>%
     select(Judet_Name)), by = "Judet_Name"
)
)

jud_cnt <- RO_Census_Indicators %>% select(Judet_Name) %>% 
  distinct() %>% count() %>% as.integer()

RO_Census_Indicators %>% select(Function_Name) %>% distinct() %>%


View(
bind_rows(
(  
RO_Census_Indicators %>% filter(Model_Type == "Old") %>% 
  select(Judet_Name, Function_Name) %>% group_by(Function_Name) %>% tally() %>%
  rename('Cnt' = 'n') %>% group_by(Function_Name) %>%
  mutate(Percentage = Cnt/jud_cnt) %>%
  select(-Cnt) %>% right_join(
    (RO_Census_Indicators %>% select(Function_Name) %>% distinct())
    ) %>% mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(Model_Type = "Old") %>% relocate(Model_Type, .before = Function_Name)
),
(
RO_Census_Indicators %>% filter(Model_Type == "New") %>% 
  select(Judet_Name, Function_Name) %>% group_by(Function_Name) %>% tally() %>%
  rename('Cnt' = 'n') %>% group_by(Function_Name) %>%
  mutate(Percentage = Cnt/jud_cnt) %>%
  select(-Cnt) %>% right_join(
    (RO_Census_Indicators %>% select(Function_Name) %>% distinct())
  ) %>% mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(Model_Type = "New") %>% relocate(Model_Type, .before = Function_Name)
)
)
)
# Load the library
library(tidyverse)
library(tidytext)
library(MASS)

# Load custom functions

# 1. Get Zipf-Mandelbrot parameters
get_ZM_Param <- function(df, y_var, x_var){
  set.seed(2025)
  q.hats <- c()
  beta.hats <- c()
  ss.hats <- c()
  for (bx in 1:1000) {
    q.mc <- c()
    res.sq.mc <- c()
    for (b in 1:300) {
      q.b <- runif(1, 0, 100)
      q.mc <- append(q.mc, q.b)
      lm_b <- lm(paste("log(", df, "$", y_var, ")", "~", "log(", df, "$", x_var, "+", q.b, ")"))
      res.sq.b <- sum(lm_b$residuals^2)
      res.sq.mc <- append(res.sq.mc, res.sq.b)
    }
    res.q.mc.dat <- data.frame(q.mc,res.sq.mc)
    q.hat <- res.q.mc.dat[which(res.q.mc.dat$res.sq.mc == min(res.q.mc.dat$res.sq.mc) ),]$q.mc
    lm_q_hat <- lm(paste("log(", df, "$", y_var, ")", "~", "log(", df, "$", x_var, "+", q.hat, ")"))
    beta.hat <- lm_q_hat$coefficients[2]
    ss.hat <- sum(lm_q_hat$residuals^2)
    q.hats  <- append(q.hats, q.hat)
    beta.hats <- append(beta.hats, beta.hat)
    ss.hats <- append(ss.hats, ss.hat)
    qbeta.dat <- data.frame(q.hats,beta.hats)
  }
  mn_q.hats <- mean(q.hats)
  lm_fit <- lm(paste("log(", df, "$", y_var, ")", "~", "log(", df, "$", x_var, "+", mn_q.hats, ")"))
  fit <- lm_fit
  q <- signif(mean(q.hats), 4)
  z <- signif(fit$coef[[2]], 4) * (-1)
  Cst <- exp(signif(fit$coef[[1]], 4))
  t_param <- as.data.frame(list(m = q, alpha_ZM = z, constant_ZM = Cst))
  return(t_param)
}

# 2. Get Zipf-Mandelbrot parameters on robust
get_ZM_Param_rob <- function(df, y_var, x_var) {
  
  set.seed(2025)
  
  q.hats <- numeric(1000)
  beta.hats <- numeric(1000)
  ss.hats <- numeric(1000)
  
  y <- df[[y_var]]
  x <- df[[x_var]]
  
  # Validare date
  valid <- is.finite(y) &
    is.finite(x) &
    y > 0 &
    x >= 0
  
  y <- log(y[valid])
  x <- x[valid]
  
  for (bx in 1:1000) {
    
    q.mc <- numeric(300)
    res.sq.mc <- numeric(300)
    
    for (b in 1:300) {
      
      q.b <- runif(1, 0, 100)
      
      q.mc[b] <- q.b
      
      x_q <- log(x + q.b)
      
      valid_q <- is.finite(x_q) &
        is.finite(y)
      
      rlm_b <- MASS::rlm(
        y[valid_q] ~ x_q[valid_q],
        psi = MASS::psi.huber
      )
      
      res.sq.mc[b] <- sum(
        rlm_b$w * rlm_b$residuals^2
      )
    }
    
    res.q.mc.dat <- data.frame(
      q.mc = q.mc,
      res.sq.mc = res.sq.mc
    )
    
    q.hat <- res.q.mc.dat$q.mc[
      which.min(res.q.mc.dat$res.sq.mc)
    ]
    
    x_q_hat <- log(x + q.hat)
    
    valid_q <- is.finite(x_q_hat) &
      is.finite(y)
    
    rlm_q_hat <- MASS::rlm(
      y[valid_q] ~ x_q_hat[valid_q],
      psi = MASS::psi.huber
    )
    
    beta.hat <- coef(rlm_q_hat)[2]
    
    ss.hat <- sum(
      rlm_q_hat$w *
        rlm_q_hat$residuals^2
    )
    
    q.hats[bx] <- q.hat
    beta.hats[bx] <- beta.hat
    ss.hats[bx] <- ss.hat
  }
  
  mn_q.hats <- mean(q.hats)
  
  x_q_final <- log(x + mn_q.hats)
  
  valid_final <- is.finite(x_q_final) &
    is.finite(y)
  
  rlm_fit <- MASS::rlm(
    y[valid_final] ~ x_q_final[valid_final],
    psi = MASS::psi.huber
  )
  
  q <- signif(mn_q.hats, 4)
  
  z <- -signif(coef(rlm_fit)[2], 4)
  
  Cst <- exp(coef(rlm_fit)[1])
  
  t_param <- data.frame(
    m = q,
    alpha_ZM = z,
    constant_ZM = Cst
  )
  
  return(t_param)
}

# 3. Get outliers from models
get_outliers <- function(out_df, main_df){
  pw_out <- bind_rows(
    (out_df %>% filter(Function_Name == "Power") %>%
       filter(Cooks_Dist >= Cooks_Dist_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Cooks")),
    
    (out_df %>% filter(Function_Name == "Power") %>%
       filter(DFFIT >= DFFIT_Trsh | DFFIT <= -DFFIT_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFFIT")),
    
    (out_df %>% filter(Function_Name == "Power") %>%
       filter(Rezid_Std >= 2 | Rezid_Std <= -2) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Stud_Rezid")),
    
    (out_df %>% filter(Function_Name == "Power") %>%
       filter(Leverage >= Lev_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Leverage")),
    
    (out_df %>% filter(Function_Name == "Power") %>%
       filter(DFBETA_intercept >= DFBETA_Trsh | DFBETA_intercept <= -DFBETA_Trsh) %>% 
       inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFBETA_intercept")),
    
    (out_df %>% filter(Function_Name == "Power") %>%
       filter(DFBETA_var_x >= DFBETA_Trsh | DFBETA_var_x <= -DFBETA_Trsh) %>% 
       inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFBETA_var_x"))
  )
  
  ZM_out <- bind_rows(
    (out_df %>% filter(Function_Name == "Zipf-Mandelbrot") %>%
       filter(Cooks_Dist >= Cooks_Dist_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Cooks")),
    
    (out_df %>% filter(Function_Name == "Zipf-Mandelbrot") %>%
       filter(DFFIT >= DFFIT_Trsh | DFFIT <= -DFFIT_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFFIT")),
    
    (out_df %>% filter(Function_Name == "Zipf-Mandelbrot") %>%
       filter(Rezid_Std >= 2 | Rezid_Std <= -2) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Stud_Rezid")),
    
    (out_df %>% filter(Function_Name == "Zipf-Mandelbrot") %>%
       filter(Leverage >= Lev_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Leverage")),
    
    (out_df %>% filter(Function_Name == "Zipf-Mandelbrot") %>%
       filter(DFBETA_intercept >= DFBETA_Trsh | DFBETA_intercept <= -DFBETA_Trsh) %>% 
       inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFBETA_intercept")),
    
    (out_df %>% filter(Function_Name == "Zipf-Mandelbrot") %>%
       filter(DFBETA_var_x >= DFBETA_Trsh | DFBETA_var_x <= -DFBETA_Trsh) %>% 
       inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFBETA_var_x"))
  )
  
  lav_out <- bind_rows(
    (out_df %>% filter(Function_Name == "Lavalette") %>%
       filter(Cooks_Dist >= Cooks_Dist_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Cooks")),
    
    (out_df %>% filter(Function_Name == "Lavalette") %>%
       filter(DFFIT >= DFFIT_Trsh | DFFIT <= -DFFIT_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFFIT")),
    
    (out_df %>% filter(Function_Name == "Lavalette") %>%
       filter(Rezid_Std >= 2 | Rezid_Std <= -2) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Stud_Rezid")),
    
    (out_df %>% filter(Function_Name == "Lavalette") %>%
       filter(Leverage >= Lev_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Leverage")),
    
    (out_df %>% filter(Function_Name == "Lavalette") %>%
       filter(DFBETA_intercept >= DFBETA_Trsh | DFBETA_intercept <= -DFBETA_Trsh) %>% 
       inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFBETA_intercept")),
    
    (out_df %>% filter(Function_Name == "Lavalette") %>%
       filter(DFBETA_var_x >= DFBETA_Trsh | DFBETA_var_x <= -DFBETA_Trsh) %>% 
       inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFBETA_var_x"))
  )
  
  exp_out <- bind_rows(
    (out_df %>% filter(Function_Name == "Exponential") %>%
       filter(Cooks_Dist >= Cooks_Dist_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Cooks")),
    
    (out_df %>% filter(Function_Name == "Exponential") %>%
       filter(DFFIT >= DFFIT_Trsh | DFFIT <= -DFFIT_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFFIT")),
    
    (out_df %>% filter(Function_Name == "Exponential") %>%
       filter(Rezid_Std >= 2 | Rezid_Std <= -2) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Stud_Rezid")),
    
    (out_df %>% filter(Function_Name == "Exponential") %>%
       filter(Leverage >= Lev_Trsh) %>% inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "Leverage")),
    
    (out_df %>% filter(Function_Name == "Exponential") %>%
       filter(DFBETA_intercept >= DFBETA_Trsh | DFBETA_intercept <= -DFBETA_Trsh) %>% 
       inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFBETA_intercept")),
    
    (out_df %>% filter(Function_Name == "Exponential") %>%
       filter(DFBETA_var_x >= DFBETA_Trsh | DFBETA_var_x <= -DFBETA_Trsh) %>% 
       inner_join(
         main_df %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank), 
         by = c("Unit_Name", "Unit_Type", "Judet_Name")
       ) %>% dplyr::select(Unit_Name, Unit_Type, Judet_Name, Rank) %>% 
       mutate(Out_Method = "DFBETA_var_x"))
  )
  out_list <- list("PW" = pw_out, "ZM" = ZM_out, "LAV" = lav_out, "EXP" = exp_out)
  return(out_list)
}
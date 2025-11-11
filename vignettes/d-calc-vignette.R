## ----preview_contact_data-----------------------------------------------------
library(PaluckTools)
data(contact_data)

# View essential columns for effect size calculation
contact_data |>
  select(study, statistic, unstand, sd_c, n_t, n_c, d, var_d, se_d) |>
  head(3)


## ----PaluckMetaSOP------------------------------------------------------------

library(PaluckMetaSOP)
data(contact_data)


## ----ditullio_results---------------------------------------------------------
ditullio_results <- d_calc(stat_type = "d_i_m", stat = 5.708 - 3.0798, sample_sd = 1.0381)

ditullio_variance <- var_d_calc(d = ditullio_results, n_t = 38, n_c = 38)

ditullio_se <- sqrt(ditullio_variance)


## ----dessel_effects-----------------------------------------------------------
dessel_school_a_effects <- d_calc(stat_type = "d_i_d", stat = (111.1-92.7)-(102.9-99.4), sample_sd = 28.28)

dessel_school_a_var <- var_d_calc(d = dessel_school_a_effects, n_t = 15, n_c = 15)

dessel__school_a_se <- sqrt(dessel_school_a_var)


## ----boisjoly_results---------------------------------------------------------
# Ns
black_roommate_treatment_n <- 35
other_minority_treatment_n <- 98 + 40 + 69 # 207
boisjoly_n_c <- 1278 - 35 - 98 - 40 - 69 # 1036

# black roommates
Bois_d_black_roommates <- d_calc(stat_type = "reg_coef", stat = .366, sample_sd = 1.074)
Bois_var_black_roomamtes <- var_d_calc(d = Bois_d_black_roommates, 
                                       n_t = black_roommate_treatment_n, 
                                       n_c = boisjoly_n_c)
Bois_se_black_roommates <- sqrt(Bois_var_black_roomamtes)

# other minority roommates
Bois_d_other_roommates <- d_calc(stat_type = "reg_coef", stat = .032, sample_sd = 1.074)
Bois_var_other_roomamtes <- var_d_calc(d = Bois_d_black_roommates, 
                                       n_t = other_minority_treatment_n, 
                                       n_c = boisjoly_n_c)
Bois_se_other_roommates <- sqrt(Bois_var_other_roomamtes)


## ----camargo_results----------------------------------------------------------
Camargo_d <- d_calc(stat_type = "t_test", stat = 3.705, 
                                n_t = 44, n_c = 214)
                                
camargo_var <- var_d_calc(d = Camargo_d, n_t = 44, n_c = 214)

camargo_se <- sqrt(camargo_var)


## ----sayler_results-----------------------------------------------------------
sayler_results <- d_calc(stat_type = "F-test", stat = 0.87, n_t = 16, n_c = 19)
sayler_var <- var_d_calc(sayler_results, 16, 19)
sayler_se <- sqrt(sayler_var)


## ----d_i_p_intuition_and_demonstration----------------------------------------
# A visualization of the intuition behind this estimator: 
# distribution of SDs drawn from Bernoulli distribution
# SD gets larger as proportion approaches 50%, which is point of max randomness
proportions <- seq(from = 0, to = 1, by = 0.01)
SDs <- as.numeric(lapply(X = proportions, FUN = function(p){sqrt(p * (1 - p))}))

plot(proportions, SDs, type = "p", pch = 20, xlab = "Proportion", ylab = "Standard Deviation", main = "SD of Bernoulli Distribution")


# demonstration
# both of the following equations are implemented by d_calc, but here we spell them out

odds_ratio_to_d <- function(p1, p2) {
  # Calculate odds for each probability
  odds1 <- p1 / (1 - p1)
  odds2 <- p2 / (1 - p2)
  
  # Calculate odds ratio
  odds_ratio <- odds1 / odds2
  
  # Convert odds ratio to Cohen's $d$
  d <- log(odds_ratio) * sqrt(3) / pi
  return(d)
}

difference_in_proportion_calc <- function(p1, p2) {
  diff <- p1 - p2
  sd <- sqrt(p2 * (1 - p2))
  d <- round(diff / sd, digits = 3)
  return(d)
}

odds_ratio_to_d(.6, .3) 
difference_in_proportion_calc(.6, .3)  # pretty similar
# alternatively, use d_calc
PaluckMetaSOP::d_calc("d_i_p", 0.6 - 0.3, 0.3)

# with smaller numbers?
odds_ratio_to_d(p1 = 0.06, p2 = 0.03)
PaluckMetaSOP::d_calc("d_i_p", 0.06 - 0.03, 0.03)
# pretty different



## ----d_and_var_d_creation_script----------------------------------------------
library(dplyr, warn.conflicts = F) 
dat_with_new_ds <-  contact_data |> 
  select(-c(d, se_d, var_d)) |>  # delete vars and then reproduce them
  mutate(d = mapply(
          FUN = d_calc,
          stat_type = statistic,
          stat =  unstand,
          sample_sd = sd_c,
          n_t = n_t,
          n_c = n_c),
        var_d = mapply(
          FUN = var_d_calc,
          d = d,
          n_t = n_t,
          n_c = n_c)) |> 
    mutate(se_d = sqrt(var_d))


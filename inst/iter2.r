# Simulation & p-val extraction; sim. data generation

library(ggplot2)
library(dplyr)

sim_study <- function(
    nsub = 50,
    bias_b = 1.1,
    bias_c = 0.9,
    accuracy_b = 10,
    accuracy_c = 15,
    return_plot = FALSE
  ) {

  dat <- expand.grid(
    id = 1:nsub,
    method = c("a", "b", "c"),
    rr_obs = NA
    )

  real_rr <- data.frame(
    id = 1:nsub,
    real_rr = runif(nsub, 8, 40)
    )

  dat <- left_join(dat, real_rr, by = "id")

  dat <- dat %>%
    mutate(
      rr_obs = ifelse(method == "a", round(real_rr),
                      ifelse(method == "b", round(real_rr * bias_b + rnorm(nsub, 0, accuracy_b )),
                             round(real_rr * bias_c + rnorm(nsub, 0, accuracy_c))))
      ) %>%
    mutate( rr_obs = ifelse(rr_obs < 1, 1, rr_obs))


  #plot the 3 distributions
  fig <-
    ggplot(dat, aes(x = rr_obs, fill = method)) +
      geom_density(alpha = 0.5) +
      geom_vline(data = real_rr, aes(xintercept = real_rr), linetype = "dashed") +
      theme_minimal() +
      labs(title = "Simulated data",
           x = "Resp Rate",
           y = "Density") +
      theme(legend.position = "top")


  dat_trf <- dat %>%
    group_by(id) %>%
    mutate(rr_method_a = rr_obs[method == "a"],
           diff_rr = rr_obs - rr_method_a) %>%
    filter(method != "a")
  if (return_plot == TRUE) {
    return(fig)
  } else {
    return(dat_trf)
  }

}

eval_study <- function(dat_trf) {
  p_val <-
    var.test(dat_trf %>%
               filter(method == "b") %>%
               .$diff_rr,
             dat_trf %>%
               filter(method == "c") %>%
               .$diff_rr
             )$p.value

  return(p_val)
}

design_mat <- expand.grid(
  rep = 1:30,
  nsub = seq(25, 250, length.out = 5) %>% round,
  bias_b = c(1),
  bias_c = c(1),
  accuracy_diff = seq(-3,3,length.out = 7)
  #accuracy_b = c(0.25, 1/3, 0.5, 1, 2),
  #accuracy_c = c(0.25, 1/3, 0.5, 1, 2)
) %>%
  mutate(
    p_val = NA
    #accuracy_diff = abs(accuracy_b - accuracy_c)
    ,accuracy_b = runif( n(), 0.5, 2)
    ,accuracy_c = abs(accuracy_b + accuracy_diff)
  )  %>%
  filter(accuracy_diff != 0)

# shuffle rows
design_mat <- design_mat[sample(nrow(design_mat)), ]

pb <- txtProgressBar(min = 0, max = nrow(design_mat), style = 3)
for (i in 1:nrow(design_mat)) {
  dat_trf <- sim_study(
    nsub = design_mat$nsub[i],
    bias_b = design_mat$bias_b[i],
    bias_c = design_mat$bias_c[i],
    accuracy_b = design_mat$accuracy_b[i],
    accuracy_c = design_mat$accuracy_c[i]
  )

  design_mat$p_val[i] <- eval_study(dat_trf)
  setTxtProgressBar(pb, i)
}
close(pb)


# ggplot(dat_trf, aes(x = rr_method_a, y = diff_rr, color = method)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   theme_minimal() +
#   theme(legend.position = "top")


design_mat_trf <- design_mat %>%
  mutate(level = paste0("nsub = ", nsub,
                        ", bias_b = ", bias_b,
                        ", bias_c = ", bias_c,
                        ", accuracy_b = ", accuracy_b,
                        ", accuracy_c = ", accuracy_c)) %>%
  group_by(level) %>%
  mutate(
    n_succ = sum(p_val < 0.05),
    n_tot  = n(),
    p_succ = n_succ / n_tot
  ) %>%
  slice(1)

if( file.exists(here::here("data", "design_mat_trf.rds")) ) {
  design_mat_trf_old <- readRDS( here::here("data", "design_mat_trf.rds"))
  design_mat_trf_bu  <- design_mat_trf
  design_mat_trf     <- rbind( design_mat_trf_old, design_mat_trf)
}

saveRDS( design_mat_trf,
      file = here::here("data", "design_mat_trf.rds"))


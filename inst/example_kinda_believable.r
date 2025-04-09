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


d <- sim_study(
  accuracy_b = 2,
  accuracy_c = 10,
  bias_b = 1,
  bias_c = 1
)

ggstatsplot::ggbetweenstats(
  d,
  x = method,
  y = diff_rr

)

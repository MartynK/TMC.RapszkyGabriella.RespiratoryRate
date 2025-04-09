# Length of the confidence interval as a function of the sample size

library(ggplot2)
library(dplyr)

nsub <- 50
bias_b <- 1.1
bias_c <- 0.9
accuracy_b <- 10
accuracy_c <- 15

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
ggplot(dat, aes(x = rr_obs, fill = method)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = real_rr, aes(xintercept = real_rr), linetype = "dashed") +
  theme_minimal() +
  labs(title = "Simulated data",
       x = "Risk Ratio",
       y = "Density") +
  theme(legend.position = "top")


dat_trf <- dat %>%
  group_by(id) %>%
  mutate(rr_method_a = rr_obs[method == "a"],
         diff_rr = rr_obs - rr_method_a) %>%
  filter(method != "a")

p_val <-
  var.test(dat_trf %>%
             filter(method == "b") %>%
             .$diff_rr,
           dat_trf %>%
             filter(method == "c") %>%
             .$diff_rr
           )$p.value

ggplot(dat_trf, aes(x = rr_method_a, y = diff_rr, color = method)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(legend.position = "top")


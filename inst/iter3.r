# evaluating sim results
library(dplyr)
library(splines)
library(ggplot2)

design_mat_trf <- readRDS(here::here("data", "design_mat_trf.rds")) %>%
  mutate( accuracy_diff = abs(accuracy_b - accuracy_c)) %>%
  mutate( rand = runif(n()) ) %>%
  filter(accuracy_diff <= 2,
         nsub >= 20,
         nsub <= 250,
         !(p_succ == 1 & rand > 0.25) # subset 'almost sure' cases
         )


mod <- glm( cbind(n_succ, n_tot - n_succ) ~
              ns( nsub, df = 1)
            + ns(accuracy_diff, df = 5)
            #+ ns(nsub, df = 1) : ns(accuracy_diff, df = 1)
            ,data = design_mat_trf
            ,family = binomial)


summary(mod)

mod %>% effects::predictorEffects() %>% plot()

# dharma diag
library(DHARMa)
simulateResiduals(mod, plot = TRUE)

pr <- expand.grid(
  nsub = seq(20, 200, length.out = 100),
  bias_b = c(1),
  bias_c = c(1),
  accuracy_diff = c(0.5, 1, 1.5)
)

pr$pr <- predict(mod, newdata = pr, type = "response")

pr %>%
  ggplot(aes(x = nsub, y = pr, color = factor(accuracy_diff)
             #,group = paste0(accuracy_b,accuracy_c)
             )) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  geom_hline(yintercept = 0.8, color = "salmon4", linetype = "dashed") +
  geom_hline(yintercept = 0.9, color = "salmon4", linetype = "dashed") +
  scale_y_continuous(limits = c(0.6, 1)) +
  labs(title = "Power analysis",
       x = "Number of subjects",
       y = "Power") +
  theme(legend.position = "top")

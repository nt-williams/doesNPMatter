library(ggplot2)
library(data.table)
library(here)

gcomp <- readRDS(here("data", "extracted", "gcom_DGP_5_uniform_20.rds"))
ptmle <- readRDS(here("data", "extracted", "ptmle_DGP_5_uniform_20.rds"))
tmle <- readRDS(here("data", "extracted", "tmle_DGP_5_uniform_20.rds"))

ggplot() +
  geom_line(
    data = gcomp,
    aes(
      x = abs(estimator_bias),
      y = 1 - ..y..,
      color = "blue"
    ),
    stat = 'ecdf',
    alpha = 0.5
  ) +
  geom_line(
    data = ptmle,
    aes(
      x = abs(estimator_bias),
      y = 1 - ..y..,
      color = "red"
    ),
    stat = 'ecdf',
    alpha = 0.5
  ) +
  geom_line(
    data = tmle,
    aes(
      x = abs(estimator_bias),
      y = 1 - ..y..,
      color = "orange"
    ),
    stat = 'ecdf',
    alpha = 0.5
  ) +
  geom_line(
    data = gcomp, 
    aes(
      x = abs(confounding_bias), 
      y = 1 - ..y..,
      color = "black"
    ), 
    stat = "ecdf"
  ) + 
  scale_color_identity(
    breaks = c("blue", "red", "orange", "black"),
    labels = c("G-comp", "(P)TMLE", "TMLE", "naive"),
    guide = "legend"
  )

ggplot() +
  geom_point(data = gcomp,
             aes(x = confounding_bias,
                 y = estimator_bias,
                 color = "blue"),
             alpha = 0.4) + 
  geom_point(data = ptmle, 
             aes(x = confounding_bias, 
                 y = estimator_bias, 
                 color = "red"), 
             alpha = 0.4) + 
  geom_point(data = tmle, 
             aes(x = confounding_bias, 
                 y = estimator_bias, 
                 color = "orange"), 
             alpha = 0.4) + 
  geom_abline(col = "black") + 
  labs(color = "Model") + 
  scale_color_identity(breaks = c("blue", "red", "orange"),
                       labels = c("G-comp", "(P)TMLE", "TMLE"),
                       guide = "legend")

ggplot() + 
  geom_point(data = gcomp, 
             aes(x = confounding_bias, 
                 y = truth))

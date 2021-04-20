ggplot(o4_0) +
  geom_line(aes(x = abs(rel_bias_tmle), y = 1 - ..y.., color = "blue"), stat = 'ecdf', 
            alpha = 0.65) +
  geom_line(aes(x = abs(rel_bias_param), y = 1 - ..y.., color = "red"), stat = 'ecdf', 
            alpha = 0.65) + 
  geom_line(aes(x = abs(rel_bias_tmlep), y = 1 - ..y.., color = "darkgreen"), stat = 'ecdf', 
            alpha = 0.65) + 
  geom_line(aes(x = abs(rel_bias_tmlecf), y = 1 - ..y.., color = "orange"), stat = 'ecdf', 
            alpha = 0.65) + 
  facet_grid(cols = vars(n)) + 
  labs(x = "|est - true| / max(|est|, |true|)",
       y = NULL, 
       color = NULL) + 
  scale_color_identity(breaks = c("blue", "red", "darkgreen", "orange"),
                       labels = c("TMLE+DA", "G-comp.", "TMLE+GLM", "TMLE(CF)"),
                       guide = "legend") + 
  scale_x_continuous(limits = c(0, 0.15))

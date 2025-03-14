
ex <- tibble(outcome = c(0, 1, 6) , 
       prob = c(.8, 1, .2) , 
       opt = as.factor(c("Long-term winner: Better expected value", "Short-term winner: Better frequent outcome", "Long-term winner: Better expected value")))

ex %>% 
  ggplot(aes(x=outcome, y=prob, fill=opt)) + 
  geom_bar(stat="identity", color = "black") + 
  labs(x = "Outcome",
       y = "Probability", 
       fill = "") +
  scale_fill_scico_d(palette = "managua" , begin = .1, end = .9) + 
  scale_x_continuous(breaks = 0:6) + 
  scale_y_continuous(breaks = seq(0,1, .2)) +
  theme_pubr(base_size = 20) + 
  theme( legend.position = c(.95, .95),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6)
    )


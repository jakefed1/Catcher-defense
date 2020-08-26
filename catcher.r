library(tidyverse)
poptime_new <- read_csv(file = "poptime_new.csv")
poptime_new
poptime_new <-
  poptime_new %>%
  arrange(pop_2b_sba)
poptime_new
poptime_new
poptime_new <-
  poptime_new %>%
  mutate(z_pop = standardize(pop_2b_sba))
poptime_new
summary(poptime_new)
catcher_framing_new <- read_csv(file = "catcher_framing_new.csv")
catcher_framing_new
catcher_framing_new <-
  catcher_framing_new %>%
  arrange(desc(runs_extra_strikes)) %>%
  mutate(z_framing = standardize(runs_extra_strikes))
catcher_framing_new
catcher_defense <-
   catcher_framing_new %>%
  inner_join(poptime_new, by="catcher")
catcher_defense
catcher_defense <-
  catcher_defense %>%
  mutate(z_pop = (z_pop * -1))
catcher_defense <-
  catcher_defense %>%
  mutate(framing_and_pop = (z_framing + z_pop))
catcher_defense
catcher_defense <-
  catcher_defense %>%
  mutate(index = ((framing_and_pop)/2))
catcher_defense
catcher_defense <-
  catcher_defense %>%
  select(catcher, year, n_called_pitches, strike_rate, runs_extra_strikes, z_framing, maxeff_arm_2b_3b_sba, exchange_2b_3b_sba, pop_2b_sba_count, pop_2b_sba, z_pop, index) %>%
  arrange(desc(index))
catcher_defense
pop_frame_fit = lm(data = catcher_defense, z_framing ~ z_pop)
pop_frame_new <-
ggplot(data = catcher_defense) +
     geom_point(mapping = aes(x = z_pop, y = z_framing), col = "springgreen4") +
  geom_abline(intercept = 0.03598, slope = 0.03308, color = "indianred1") +
  labs(
    x="Pop time (standardized)",
    y = "framing runs saved (standardized)",
    title = "Catcher pop time vs framing z scores",
    subtitle = "2019",
    caption = "Data from Baseball Savant") +
  theme_bw()
pop_frame_new

pop_frame_new
summarize(catcher_defense, correlation = cor(z_pop, z_framing))
summary(pop_frame)
summary(pop_frame_fit)

select(catcher_defense, catcher, index)

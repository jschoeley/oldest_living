#################################################
# THE AGE OF THE OLDEST LIVING PERSON 1955-2017 #
#################################################

# Init --------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Input -------------------------------------------------------------------

# oldest living by year
old <-
  read_csv("./data/grg_oldest_living_person_by_year.csv") %>%
  mutate(date_of_accession = dob +
           years(age_at_accession_y) +
           days(age_at_accession_d))

# Plot --------------------------------------------------------------------

old %>%
  ggplot() +
  geom_smooth(method = "lm", se = FALSE, color = "#FFB5AF",
              aes(x = date_of_accession,
                  y = age_at_accession_y*365.25+age_at_accession_d)) +
  geom_segment(aes(
    x = date_of_accession, xend = dod,
    y = age_at_accession_y*365.25+age_at_accession_d,
    yend = age_at_death_y*365.25+age_at_death_d, color = sex)
  ) +
  geom_point(aes(
    x = dod, y = age_at_death_y*365.25+age_at_death_d, color = sex), size = 0.2
  ) +
  scale_y_continuous("Age", breaks = c(seq(105, 120, 5), 122)*365.25,
                     limits = c(105,123)*365.25,
                     minor_breaks = seq(105, 125, 1)*365.25,
                     labels = function (x) x%/%365.25, expand = c(0,0)) +
  scale_x_date("Year",
               limits = parse_date(c(1955, 2018), format = "%Y"),
               breaks = parse_date(seq(1955, 2015, 5), format = "%Y"),
               minor_breaks = parse_date(seq(1955, 2018, 1), format = "%Y"),
               date_labels = c(1955, paste0("'", seq(60, 95, 5)), 2000,
                               "'05", "'10", "'15"),
               expand = c(0.01,0)) +
  scale_color_manual(guide = FALSE, values = c("#000000" ,"#00B3FF")) +
  coord_fixed() +
  theme_minimal()

ggsave("./out/the_oldest_living.svg",
       width = unit(14, "cm"), height = unit(5, "cm"),
       scale = 0.5)

# Linear regression on the age of accession -------------------------------

# mean annual increase of accession age
old <-
  old %>%
  mutate(date_of_accession_centered_y = (date_of_accession - min(date_of_accession, na.rm = TRUE)) / 365.25,
         age_at_accession_centered_y = (age_at_accession_y*365.25 + age_at_accession_d) / 365.25)

model_fit <- lm(data = old, formula = age_at_accession_centered_y ~ date_of_accession_centered_y)
coef(model_fit)[2]*365.25
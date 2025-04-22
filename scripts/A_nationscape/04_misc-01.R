
# - generational imprints ---------------------------------------------------- #
# - nationscape | analyze ---------------------------------------------------- #

### note: this script
###       (a) provides alternative analyses for the descriptive change figure.

rm(list = ls()) # clean-up

pacman::p_load(
  fixest,
  ggfixest,
  srvyr,
  MatchIt,
  purrr,
  furrr,
  splines,
  hrbrthemes,
  tidyverse,
  patchwork,
  marginaleffects,
  modelsummary)

# --- helpers
my_oka <-
  c("#82B2C0",
    "#F6C7B3",
    "#eebd64")
us_oka <- 
  c("#013364",
    "#cbcaca",
    "#cc0000")
theme_set(theme_ipsum_rc(
  grid = "XY",
  axis_title_size = 12,
  plot_margin =
    margin(10, 10, 10, 10)
))
`%nin%` = Negate(`%in%`)

# --- datafile
d <- readRDS("./data/output/nationscape.rds")

# --- survey
d_survey <- srvyr::as_survey(d, weights = wt)

# ---------------------------------------------------------------------------- #
# PART 1: UNWEIGHTED --------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- calculate

## summarize
sum <- d |>
  group_by(pid, agegr, time) |>
  summarize(y = mean(police),
            n = n(),
            y_se = sd(police) / sqrt(n)) |>
  ungroup() |> 
  select(-n)

## baselines
pre <- d |>
  filter(time < 0) |>
  group_by(pid, agegr) |>
  summarize(b = mean(police, na.rm = TRUE)) |>
  select(pid, agegr, b) |> 
  ungroup()

# --- plot estimates

sum |>
  left_join(pre, by = c("agegr", "pid")) |>
  mutate(timing = ifelse(time < 0, 
                         "Before", 
                         "After")) |>
  ggplot(aes(
    x = time,
    y = y,
    ymin = y - 1.96 * y_se,
    ymax = y + 1.96 * y_se,
    color = timing
  )) +
  facet_grid(pid ~ agegr, scales = "fixed") +
  scale_x_continuous(limits = c(-12, 8),
                     breaks = c(-12, -8, -4, 0, 4, 8)) +
  scale_y_continuous(limits = c(.04, .77),
                     breaks = seq(from = .1, to = .7, by = .2)) +
  labs(x = "Time Before and After the Event", 
       y = "Unfavorable Attitudes Toward the Police") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = .4) +
  geom_hline(aes(yintercept = b), 
             linetype = "dashed", 
             linewidth = .4) +
  geom_pointrange(size = .4, alpha = .75) +
  scale_color_manual(values = c(my_oka[1], my_oka[2])) +
  theme(legend.position = "none")
ggsave(
  filename = "./output/A_nationscape/p01-A.png",
  width = 12,
  height = 6,
  units = "in",
  dpi = 500,
  bg = "white"
)

rm(sum, pre) # clean-up

# ---------------------------------------------------------------------------- #
# PART 2: DK DROPPED --------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- calculate

## summarize
sum <- d_survey |>
  filter(police != 0.5) |> 
  mutate(police = case_when(
    police == 0 ~ 0,
    police == 1/4 ~ 1/3,
    police == 3/4 ~ 2/3,
    police == 1 ~ 1,
  )) |> 
  group_by(pid, agegr, time) |>
  summarize(y = srvyr::survey_mean(police)) |>
  ungroup()

## baselines
pre <- d_survey |>
  filter(time < 0) |>
  filter(police != 0.5) |> 
  mutate(police = case_when(
    police == 0 ~ 0,
    police == 1/4 ~ 1/3,
    police == 3/4 ~ 2/3,
    police == 1 ~ 1,
  )) |>
  group_by(pid, agegr) |>
  summarize(b = srvyr::survey_mean(police)) |>
  select(pid, agegr, b) |> 
  ungroup()

# --- plot estimates

sum |>
  left_join(pre, by = c("agegr", "pid")) |>
  mutate(timing = ifelse(time < 0, 
                         "Before", 
                         "After")) |>
  ggplot(aes(
    x = time,
    y = y,
    ymin = y - 1.96 * y_se,
    ymax = y + 1.96 * y_se,
    color = timing
  )) +
  facet_grid(pid ~ agegr, scales = "fixed") +
  scale_x_continuous(limits = c(-12, 8),
                     breaks = c(-12, -8, -4, 0, 4, 8)) +
  scale_y_continuous(limits = c(.04, .77),
                     breaks = seq(from = .1, to = .7, by = .2)) +
  labs(x = "Time Before and After the Event", 
       y = "Unfavorable Attitudes Toward the Police") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = .4) +
  geom_hline(aes(yintercept = b), 
             linetype = "dashed", 
             linewidth = .4) +
  geom_pointrange(size = .4, alpha = .75) +
  scale_color_manual(values = c(my_oka[1], my_oka[2])) +
  theme(legend.position = "none")
ggsave(
  filename = "./output/A_nationscape/p01-B.png",
  width = 12,
  height = 6,
  units = "in",
  dpi = 500,
  bg = "white"
)

rm(sum, pre) # clean-up

# ---------------------------------------------------------------------------- #
# PART 3: BINARY ------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- calculate

## summarize
sum <- d_survey |>
  mutate(police01 = ifelse(police >= 0.75, 1, 0)) |> 
  group_by(pid, agegr, time) |>
  summarize(y = srvyr::survey_mean(police01)) |>
  ungroup()

## baselines
pre <- d_survey |>
  filter(time < 0) |>
  mutate(police01 = ifelse(police >= 0.75, 1, 0)) |> 
  group_by(pid, agegr) |>
  summarize(b = srvyr::survey_mean(police01)) |>
  select(pid, agegr, b) |> 
  ungroup()

# --- plot estimates

sum |>
  left_join(pre, by = c("agegr", "pid")) |>
  mutate(timing = ifelse(time < 0, 
                         "Before", 
                         "After")) |>
  ggplot(aes(
    x = time,
    y = y,
    ymin = y - 1.96 * y_se,
    ymax = y + 1.96 * y_se,
    color = timing
  )) +
  facet_grid(pid ~ agegr, scales = "fixed") +
  scale_x_continuous(limits = c(-12, 8),
                     breaks = c(-12, -8, -4, 0, 4, 8)) +
  scale_y_continuous(limits = c(.01, .82),
                     breaks = seq(from = .1, to = .7, by = .2)) +
  labs(x = "Time Before and After the Event", 
       y = "Unfavorable Attitudes Toward the Police") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = .4) +
  geom_hline(aes(yintercept = b), 
             linetype = "dashed", 
             linewidth = .4) +
  geom_pointrange(size = .4, alpha = .75) +
  scale_color_manual(values = c(my_oka[1], my_oka[2])) +
  theme(legend.position = "none")
ggsave(
  filename = "./output/A_nationscape/p01-C.png",
  width = 12,
  height = 6,
  units = "in",
  dpi = 500,
  bg = "white"
)

rm(sum, pre) # clean-up

# ---------------------------------------------------------------------------- #
# PART 4: NO WEEK AGGREGATIONS ----------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- calculate

## summarize
sum <- d_survey |>
  mutate(wave = wave - 46) |> ## center
  group_by(pid, agegr, wave) |>
  summarize(y = srvyr::survey_mean(police)) |>
  ungroup()

## baselines
pre <- d_survey |>
  filter(time < 0) |>
  group_by(pid, agegr) |>
  summarize(b = srvyr::survey_mean(police)) |>
  select(pid, agegr, b) |> 
  ungroup()

# --- plot estimates

sum |>
  mutate(time = wave) |> 
  left_join(pre, by = c("agegr", "pid")) |>
  mutate(timing = ifelse(time < 0, 
                         "Before", 
                         "After")) |>
  ggplot(aes(
    x = time,
    y = y,
    ymin = y - 1.96 * y_se,
    ymax = y + 1.96 * y_se,
    color = timing
  )) +
  facet_grid(pid ~ agegr, scales = "fixed") +
  # scale_x_continuous(limits = c(-12, 8),
  #                    breaks = c(-12, -8, -4, 0, 4, 8)) +
  scale_y_continuous(limits = c(0, .9),
                     breaks = seq(from = .1, to = .7, by = .2)) +
  labs(x = "Time Before and After the Event", 
       y = "Unfavorable Attitudes Toward the Police") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = .4) +
  geom_hline(aes(yintercept = b), 
             linetype = "dashed", 
             linewidth = .4) +
  geom_pointrange(size = .1, alpha = .25) +
  geom_smooth(aes(x = time,
                  y = y,
                  group = timing,
                  col = timing),
              method = "lm",
              formula = y ~ x,
              se = TRUE) +
  scale_color_manual(values = c(my_oka[1], my_oka[2])) +
  theme(legend.position = "none")
ggsave(
  filename = "./output/A_nationscape/p01-D.png",
  width = 12,
  height = 6,
  units = "in",
  dpi = 500,
  bg = "white"
)

rm(sum, pre) # clean-up

# ---------------------------------------------------------------------------- #
# PART 5: IDEOLOGY ----------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- calculate

## summarize
sum <- d_survey |>
  group_by(ideo, agegr, time) |>
  summarize(y = srvyr::survey_mean(police)) |>
  ungroup()

## baselines
pre <- d_survey |>
  filter(time < 0) |>
  group_by(ideo, agegr) |>
  summarize(b = srvyr::survey_mean(police)) |>
  select(ideo, agegr, b) |> 
  ungroup()

# --- plot estimates

sum |>
  left_join(pre, by = c("agegr", "ideo")) |>
  mutate(timing = ifelse(time < 0, 
                         "Before", 
                         "After")) |>
  ggplot(aes(
    x = time,
    y = y,
    ymin = y - 1.96 * y_se,
    ymax = y + 1.96 * y_se,
    color = timing
  )) +
  facet_grid(ideo ~ agegr, scales = "fixed") +
  scale_x_continuous(limits = c(-12, 8),
                     breaks = c(-12, -8, -4, 0, 4, 8)) +
  scale_y_continuous(limits = c(.01, .92),
                     breaks = seq(from = 0, to = 1, by = .2)) +
  labs(x = "Time Before and After the Event", 
       y = "Unfavorable Attitudes Toward the Police") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = .4) +
  geom_hline(aes(yintercept = b), 
             linetype = "dashed", 
             linewidth = .4) +
  geom_pointrange(size = .4, alpha = .75) +
  scale_color_manual(values = c(my_oka[1], my_oka[2])) +
  theme(legend.position = "none")
ggsave(
  filename = "./output/A_nationscape/p01-E.png",
  width = 12,
  height = 6,
  units = "in",
  dpi = 500,
  bg = "white"
)

rm(sum, pre) # clean-up

# ---------------------------------------------------------------------------- #

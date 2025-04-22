
# - generational imprints ---------------------------------------------------- #
# - misc --------------------------------------------------------------------- #

### note: this script
###       (a) plots the theoretical trends across three generative models,
###       (b) provides a summary list of all data files and expectations,
###       (c) plots a Directed Acyclic Graph for the central expectations.

rm(list = ls()) # clean-up

pacman::p_load(
  tidyverse,
  patchwork,
  hrbrthemes,
  tinytable,
  DiagrammeR)

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
  grid = "Yy",
  axis_title_size = 12,
  plot_margin =
    margin(10, 10, 10, 10)
))
`%nin%` = Negate(`%in%`)
decay_function <-
  function(step,
           total_steps = 50,
           p = 0.5) {
    return(1 - (step / (total_steps - 1))^p)
  }

# ---------------------------------------------------------------------------- #
# PART 1: TRENDS ------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- dataframes
dA <- tibble(
  t = 1:100,
  g = "Old",
  y_0 = c(rep(0, 51), rep(0.3, 49)),
  y_1 = c(rep(0, 51), decay_function(
    c(1:49), total_steps = 49, p = 0.1
  )),
  y_2 = c(rep(0, 51), decay_function(
    c(1:49), total_steps = 49, p = 0.1
  ))
)
dY <- tibble(
  t = 1:100,
  g = "Young",
  y_0 = c(rep(0, 51), rep(0.3, 49)),
  y_1 = c(rep(0, 51), decay_function(
    c(1:49), total_steps = 49, p = 0.1
  )),
  y_2 = c(rep(0, 51), rep(0.3, 49)),
)

# --- combine
d <- bind_rows(dA, dY) |>
  pivot_longer(cols = c("y_0", 
                        "y_1", 
                        "y_2")) |>
  mutate(name = factor(
    name,
    levels = c("y_0", 
               "y_1", 
               "y_2"),
    labels = c(
      "Persistent Updating",
      "Transient Shock",
      "Differential Response"
    )
  )) |> 
  mutate(post = if_else(t >= 51, "yes", "no"))

# --- plot
p1 <- d |>
  ggplot(aes(x = t, y = value)) +
  facet_grid(g ~ name) +
  geom_hline(yintercept = 0,
             linewidth = 0.25,
             alpha = 0.75,
             linetype = "dashed") +
  geom_vline(xintercept = 50,
             linewidth = 0.25,
             alpha = 0.75,
             linetype = "dashed") +
  scale_y_continuous(limits = c(-0.4,
                                 0.4)) +
  scale_x_continuous(limits = c(0, 100)) +
  geom_path(linewidth = 0.5,
            lineend = "round",
            col = "black") +
  labs(x = "Time",
       y = "Attitude",
       col = "") +
  theme(legend.position = "none")

# ---------------------------------------------------------------------------- #
# PART 2: TRENDS, by GROUP --------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- dataframes
dA1 <- tibble(
  t = 1:100,
  g = "Old",
  h = "Group 1",
  y_0 = c(rep(0.25, 51), rep(0.50, 49)),
  y_1 = c(rep(0.25, 51), decay_function(
    c(1:49), total_steps = 49, p = 0.1
  ) + 0.25),
  y_2 = c(rep(0.25, 51), decay_function(
    c(1:49), total_steps = 49, p = 0.1
  ) + 0.25)
)
dA2 <- tibble(
  t = 1:100,
  g = "Old",
  h = "Group 2",
  y_0 = c(rep(-0.25, 100)),
  y_1 = c(rep(-0.25, 100)),
  y_2 = c(rep(-0.25, 100))
)
dY1 <- tibble(
  t = 1:100,
  g = "Young",
  h = "Group 1",
  y_0 = c(rep(0.25, 51), rep(0.50, 49)),
  y_1 = c(rep(0.25, 51), decay_function(
    c(1:49), total_steps = 49, p = 0.1
  ) + 0.25),
  y_2 = c(rep(0.25, 51), rep(0.50, 49))
)
dY2 <- tibble(
  t = 1:100,
  g = "Young",
  h = "Group 2",
  y_0 = c(rep(-0.25, 100)),
  y_1 = c(rep(-0.25, 100)),
  y_2 = c(rep(-0.25, 100))
)

# --- combine
d <- bind_rows(dA1, 
               dA2,
               dY1,
               dY2) |>
  pivot_longer(cols = c("y_0", 
                        "y_1", 
                        "y_2")) |>
  mutate(name = factor(
    name,
    levels = c("y_0", 
               "y_1", 
               "y_2"),
    labels = c(
      "Persistent Updating",
      "Transient Shock",
      "Differential Response"
    )
  ))

# --- plot
p2 <- d |>
  ggplot(aes(x = t, 
             y = value, 
             col = h)) +
  facet_grid(g ~ name) +
  geom_hline(yintercept = 0,
             linewidth = 0.25,
             alpha = 0.75,
             linetype = "dashed") +
  geom_vline(xintercept = 50,
             linewidth = 0.25,
             alpha = 0.75,
             linetype = "dashed") +
  scale_y_continuous(limits = c(-0.6,
                                0.6)) +
  scale_x_continuous(limits = c(0, 100)) +
  geom_path(linewidth = 0.5,
            lineend = "round") +
  scale_color_manual(values = c(us_oka[1],
                                us_oka[3])) +
  labs(x = "Time",
       y = "Attitude",
       col = "") +
  theme(legend.position = "none")

p1 / p2 +
  plot_annotation(tag_levels = 'A')
ggsave(
  filename = "./output/D_miscellaneous/trends.png",
  width = 8,
  height = 8,
  units = "in",
  dpi = 500,
  bg = "white"
)

# ---------------------------------------------------------------------------- #
# PART 3: TABLES ------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# --- data file
d <- tibble(
  Analyses = c("Nationscape", 
               "CCES", 
               "ANES"),
  `Longitudinal` = c("-", 
                     "-", 
                     "+"),
  `Expectation \n (1)` = c("+", 
                           "+", 
                           "+"),
  `Expectation \n (2)` = c("+", 
                           "+", 
                           "-"),
  `Expectation \n (3A)` = c("+", 
                            "-", 
                            "-"),
  `Expectation \n (3B)` = c("+", 
                            "-", 
                            "-")
)

# --- tinytable
t <- tt(d, 
        width = 1) |>
  style_tt(j = 1:6, align = "lccccc")
saveRDS(t, "./output/D_miscellaneous/table.rds")

# ---------------------------------------------------------------------------- #
# PART 3: DAG ---------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

DAG <- grViz("
digraph DAG {
  rankdir = LR
  node [shape = box, 
        fontname = 'Roboto Condensed',
        style = filled, 
        fillcolor = White]
  
  // ----- columns ------------------------------------
  { rank = same;  I }
  { rank = same;  A  P }
  { rank = same;  IA  IP }
  { rank = same;  U }
  // --------------------------------------------------

  // 3. declare labels
  I     [label = 'Treatment']
  A     [label = 'Age']
  P     [label = 'Partisan ID']
  IA    [label = 'Treatment × Age', fillcolor = '#EFEFEF']
  IP    [label = 'Treatment × Party', fillcolor = '#EFEFEF']
  U     [label = 'Updating']
  
  // 4. causal paths
  A -> P
  I -> P [style = dashed]
  
  I -> IA
  A -> IA
  IA -> U
  
  I -> IP
  P -> IP
  IP -> U
}
")

DAG_txt = DiagrammeRsvg::export_svg(DAG)
rsvg::rsvg_png(
  charToRaw(DAG_txt),
  file   = "./output/D_miscellaneous/DAG.png",
  width  = 6000,
  height = 3000
)

# ---------------------------------------------------------------------------- #

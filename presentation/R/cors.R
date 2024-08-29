library(stdmatern)
library(tidyverse)
library(ggh4x)
library(scales)
library(patchwork)
theme_set(bggjphd::theme_bggj())

# Define colors
exact_color <- "#e41a1c"
circulant_color <- "#969696"
folded_color <- "#4daf4a"

# Define geom_smooth parameters
smooth_params <- list(
  se = 0,
  span = 0.03,
  n = 400,
  linewidth = 0.5
)

dim <- 30
rho <- 0.8
nu <- 0
n_obs <- 8000


exact <- cor(t(rmatern_copula_eigen(n_obs, dim, dim, rho, rho, 0)))[1, ]
circulant <- cor(t(rmatern_copula_circulant(n_obs, dim, dim, rho, rho, 0)))[1, ]
folded <- cor(t(rmatern_copula_folded_full(n_obs, dim, dim, rho, rho, 0)))[1, ]

p1 <- tibble(
  Exact = exact,
  Circulant = circulant,
  Folded = folded
) |>
  mutate(
    index = row_number()
  ) |>
  pivot_longer(c(-index)) |>
  mutate(
    name = fct_relevel(name, "Exact")
  ) |>
  ggplot(aes(index, value, col = name)) +
  do.call(geom_smooth, smooth_params) +
  scale_x_continuous(
    guide = guide_axis_truncated(),
    breaks = breaks_extended(9)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    guide = guide_axis_truncated()
  ) +
  scale_colour_manual(
    values = c(
      exact_color,
      circulant_color,
      folded_color
    )
  ) +
  scale_fill_manual(
    values = c(
      exact_color,
      circulant_color,
      folded_color
    )
  ) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  labs(
    title = "Comparing the first line in the correlation matrices for each method",
    subtitle = "Shown for rho = 0.8 and nu = 0 on a 30x30 grid",
    x = "Column index",
    y = "Correlation",
    col = NULL
  ) +
  theme(
    legend.position = "top"
  )


exact <- cor(t(rmatern_copula_eigen(n_obs, dim, dim, rho, rho, 1)))[1, ]
circulant <- cor(t(rmatern_copula_circulant(n_obs, dim, dim, rho, rho, 1)))[1, ]
folded <- cor(t(rmatern_copula_folded_full(n_obs, dim, dim, rho, rho, 1)))[1, ]

p2 <- tibble(
  Exact = exact,
  Circulant = circulant,
  Folded = folded
) |>
  mutate(
    index = row_number()
  ) |>
  pivot_longer(c(-index)) |>
  mutate(
    name = fct_relevel(name, "Exact")
  ) |>
  ggplot(aes(index, value, col = name)) +
  do.call(geom_smooth, smooth_params) +
  scale_x_continuous(
    guide = guide_axis_truncated(),
    breaks = breaks_extended(9)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    guide = guide_axis_truncated()
  ) +
  scale_colour_manual(
    values = c(
      exact_color,
      circulant_color,
      folded_color
    )
  ) +
  scale_fill_manual(
    values = c(
      exact_color,
      circulant_color,
      folded_color
    )
  ) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  labs(
    title = "Comparing the first line in the correlation matrices for each method",
    subtitle = "Shown for rho = 0.8 and nu = 1 on a 30x30 grid",
    x = "Column index",
    y = "Correlation",
    col = NULL
  ) +
  theme(
    legend.position = "top"
  )


exact <- cor(t(rmatern_copula_eigen(n_obs, dim, dim, rho, rho, 2)))[1, ]
circulant <- cor(t(rmatern_copula_circulant(n_obs, dim, dim, rho, rho, 2)))[1, ]
folded <- cor(t(rmatern_copula_folded_full(n_obs, dim, dim, rho, rho, 2)))[1, ]

p3 <- tibble(
  Exact = exact,
  Circulant = circulant,
  Folded = folded
) |>
  mutate(
    index = row_number()
  ) |>
  pivot_longer(c(-index)) |>
  mutate(
    name = fct_relevel(name, "Exact")
  ) |>
  ggplot(aes(index, value, col = name)) +
  do.call(geom_smooth, smooth_params) +
  scale_x_continuous(
    guide = guide_axis_truncated(),
    breaks = breaks_extended(9)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    guide = guide_axis_truncated()
  ) +
  scale_colour_manual(
    values = c(
      exact_color,
      circulant_color,
      folded_color
    )
  ) +
  scale_fill_manual(
    values = c(
      exact_color,
      circulant_color,
      folded_color
    )
  ) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  labs(
    title = "Comparing the first line in the correlation matrices for each method",
    subtitle = "Shown for rho = 0.8 and nu = 0 on a 30x30 grid",
    x = "Column index",
    y = "Correlation",
    col = NULL
  ) +
  theme(
    legend.position = "top"
  )

(p1 + labs(title = NULL, subtitle = "nu = 0")) +
  (p2 + labs(title = NULL, subtitle = "nu = 1")) +
  (p3 + labs(title = NULL, subtitle = "nu = 2")) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Comparing the first line in the correlation matrices",
    subtitle = "Shown for rho = 0.8 a 30x30 grid",
    theme = theme(
      legend.position = "top"
    )
  )

ggsave(
  filename = here::here("presentation", "images", "cors.png"),
  scale = 1.4,
  width = 8, height = 0.5 * 8
)

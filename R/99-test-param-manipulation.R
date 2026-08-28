library(sdmTMB)
library(ggplot2)
library(patchwork)
library(dplyr)

# For making the counterfactual plot, I need to make a prediction. I don't want to
# have to select a given year, but predict for an average year. Since year_f is fixed,
# I need to manipulate the TMB object. In addition, I want to predict without epsilon.
# Here's a quick test demonstrating the approach

# The approach is to solve for the random effects ONCE using the TRUE b_j (original
# projection onto the new grid), then modify b_j in that already-solved parameter
# vector and call report(). report() doesn't optimise anything, so the random effects
# stay exactly as correctly solved, and only the fixed effect changes.

d <- pcod_2011
d$year_f <- factor(d$year)
years <- levels(d$year_f)
set.seed(1)
year_vals <- setNames(rnorm(length(years)), years)
d$fake_index <- year_vals[as.character(d$year_f)]

mesh <- make_mesh(d, c("X", "Y"), cutoff = 15)
fit <- sdmTMB(density ~ 0 + year_f,
  spatial_varying = ~ 0 + fake_index,
  data = d, mesh = mesh, time = "year",
  family = tweedie(link = "log"),
  spatial = "on", spatiotemporal = "iid", silent = TRUE
)

# Predict on the grid for every fitted year at once
grid_all <- replicate_df(qcs_grid, "year", as.integer(years)) |>
  mutate(
    year_f = factor(year, levels = years),
    fake_index = year_vals[as.character(year_f)]
  )

# Solve ONCE with the true, fitted parameters
tmb_data <- predict(fit, newdata = grid_all, return_tmb_data = TRUE)

obj <- TMB::MakeADFun(
  data = tmb_data, parameters = get_pars(fit),
  map = fit$tmb_map, random = fit$tmb_random,
  DLL = "sdmTMB", silent = TRUE
)

obj$fn(fit$model$par)
par_true <- obj$env$last.par.best

# where are they year effects?
b_idx <- which(names(par_true) == "b_j")
par_true[b_idx]

# Modify b_j (year effects) in the already solved vector, then report() (no fn() which predict does and is the reason we cant use predict, since it will re-optimize random effects given the fixed effects)
par_neutral <- par_true
par_neutral[b_idx] <- 0.1 # set to 1 to more clearly show difference in spatial plot, but later use mean(fit$model$par[b_idx])
par_neutral[b_idx]

# get fitted values for both objects (original and modified)
rep_true <- obj$report(par_true)
rep_neutral <- obj$report(par_neutral)

pred_all <- bind_rows(
  grid_all |> mutate(
    eta = rep_true$proj_eta, 
    eta_fixed = rep_true$proj_fe,
    epsilon_st = rep_true$proj_epsilon_st_A_vec[, 1], 
    type = "original"
  ),
  grid_all |> mutate(
    eta = rep_neutral$proj_eta, 
    eta_fixed = rep_neutral$proj_fe,
    epsilon_st = rep_neutral$proj_epsilon_st_A_vec[, 1],
    type = "neutral"
  )
)

# The fixed effect-only spatial plot should be constant across years, in contrast to og
ggplot(pred_all, aes(X, Y, fill = eta_fixed)) +
  geom_raster() +
  facet_grid(type ~ year) +
  coord_fixed() +
  scale_fill_viridis_c() +
  labs(title = "eta_fixed: collapses across years once b_j is neutralised")

# plot full linear predictor (we still see modified intercept)
ggplot(pred_all, aes(X, Y, fill = eta)) +
  geom_raster() +
  facet_grid(type ~ year) +
  coord_fixed() +
  scale_fill_viridis_c() +
  labs(title = "eta (full): shift now visible, omega_s/epsilon_st untouched")

print(pred_all |> 
        summarise(mean_eta_fixed = mean(eta_fixed), mean_eta = mean(eta), .by = c(type, year)))


# Now we need to subtract epsilon
pred_all <- pred_all |> 
  mutate(eta_no_epsilon = eta - epsilon_st)

p1 <- ggplot(pred_all, aes(X, Y, fill = eta_no_epsilon)) +
  geom_raster() +
  facet_grid(type ~ year) +
  coord_fixed() +
  scale_fill_viridis_c()

p2 <- ggplot(pred_all, aes(X, Y, fill = eta)) +
  geom_raster() +
  facet_grid(type ~ year) +
  coord_fixed() +
  scale_fill_viridis_c()

p1 / p2

# The bottom row should show more variation across years...
# Numeric test: does removing epsilon_st reduce the per-year spread across cells? 
print(
  pred_all |>
    filter(type == "original") |>
    summarise(sd_eta = sd(eta),
              sd_eta_no_epsilon = sd(eta_no_epsilon), .by = year)
)

wide_df <- grid_all |>
  mutate(
    cell_id = paste(X, Y),
    eta = rep_true$proj_eta,
    eta_no_epsilon = rep_true$proj_eta - rep_true$proj_epsilon_st_A_vec[, 1]
  )

cor_with <- wide_df |>
  select(cell_id, year, eta) |>
  tidyr::pivot_wider(names_from = year, values_from = eta) |>
  select(-cell_id) |>
  cor()

cor_without <- wide_df |>
  select(cell_id, year, eta_no_epsilon) |>
  tidyr::pivot_wider(names_from = year, values_from = eta_no_epsilon) |>
  select(-cell_id) |>
  cor()

to_long <- function(m, label) {
  as.data.frame(as.table(m)) |>
    setNames(c("year1", "year2", "correlation")) |>
    mutate(condition = label)
}

cor_all <- bind_rows(
  to_long(cor_with, "with epsilon_st"),
  to_long(cor_without, "without epsilon_st (Thorson epsilon(s,t)=0)")
)

ggplot(cor_all, aes(factor(year1), factor(year2), fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  facet_wrap(~condition) +
  scale_fill_viridis_c(limits = c(min(cor_all$correlation), 1)) +
  labs(
    title = "Year-to-year correlation of spatial pattern -- 'without' should be higher",
    x = NULL, y = NULL
  )

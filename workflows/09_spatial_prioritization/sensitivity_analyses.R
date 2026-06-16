# ============================================================
# SENSITIVITY: boundary penalty sweep
#   How much does the current->future priority shift depend on the
#   boundary (connectivity) penalty? At penalty = 0 connectivity is
#   ignored (dams cannot matter); as penalty rises, the barrier-aware
#   connectivity should reshape priorities more. We solve both scenarios
#   at the comparison target (30%) across a range of penalties and report
#   the shift (reaches/km that change priority status).
# ============================================================

message("\n=== Sensitivity: boundary penalty sweep (", COMPARISON_TARGET * 100, "%) ===")

PENALTY_GRID <- c(0, 0.001, 0.01, 0.03, 0.1, 0.3, 1)

penalty_sweep <- lapply(PENALTY_GRID, function(pen) {

  # solve current & future at this penalty (cost = HFI only)
  solve_pen <- function(bmat) {
    p <- problem(pu_dat, spec_dat, cost_column = "cost_hfi", rij = puvspr_dat) %>%
      add_min_set_objective() %>%
      add_relative_targets(COMPARISON_TARGET) %>%
      add_binary_decisions() %>%
      add_cbc_solver(gap = SOLVER_GAP, threads = N_THREADS, verbose = FALSE)
    # penalty = 0 -> no boundary term at all (avoids degenerate 0-weight call)
    if (pen > 0)
      p <- p %>% add_boundary_penalties(penalty = pen, data = bmat)
    solve(p)
  }

  s_cur <- solve_pen(bmat_current)
  s_fut <- solve_pen(bmat_future)

  sel_cur <- s_cur$id[s_cur$solution_1 == 1]
  sel_fut <- s_fut$id[s_fut$solution_1 == 1]

  cur_only <- setdiff(sel_cur, sel_fut)
  fut_only <- setdiff(sel_fut, sel_cur)
  shared   <- intersect(sel_cur, sel_fut)

  km <- function(ids) sum(pu_dat$length_km[pu_dat$id %in% ids], na.rm = TRUE)

  data.frame(
    penalty        = pen,
    n_current      = length(sel_cur),
    n_future       = length(sel_fut),
    km_current     = round(km(sel_cur), 1),
    km_future      = round(km(sel_fut), 1),
    n_shared       = length(shared),
    n_current_only = length(cur_only),
    n_future_only  = length(fut_only),
    km_shift       = round(km(cur_only) + km(fut_only), 1),   # total km that changes status
    jaccard        = round(length(shared) / length(union(sel_cur, sel_fut)), 3)
  )
}) %>% rbindlist()

message("\n  Boundary-penalty sweep (current vs future, ",
        COMPARISON_TARGET * 100, "% target):")
print(penalty_sweep)
message("\n  Interpretation: km_shift = km changing priority status between",
        " scenarios; jaccard near 1 = scenarios agree (dams barely move",
        " priorities); lower jaccard / higher km_shift = penalty makes the",
        " barrier-aware connectivity bite.")

fwrite(penalty_sweep, "prioritization/boundary_penalty_sweep.csv")
message("  Saved: prioritization/boundary_penalty_sweep.csv")

# quick plot: shift vs penalty
p_pen <- ggplot(penalty_sweep, aes(x = factor(penalty))) +
  geom_col(aes(y = km_shift), fill = "#d73027", alpha = 0.85, width = 0.6) +
  geom_text(aes(y = km_shift, label = km_shift), vjust = -0.4, size = 3) +
  labs(x = "Boundary penalty",
       y = "Priority shift current -> future (km)",
       title = "Sensitivity of priority shift to the boundary penalty",
       subtitle = paste0("Minimum-set, cost = HFI, ", COMPARISON_TARGET * 100,
                         "% target; km changing priority status between scenarios")) +
  theme_bw(base_size = 11) +
  theme(plot.subtitle = element_text(size = 9, colour = "grey40"))

png("prioritization/maps/boundary_penalty_sweep.png",
    width = 7, height = 5, units = "in", res = 200)
print(p_pen); dev.off()
message("  Saved: prioritization/maps/boundary_penalty_sweep.png")



# ============================================================
# SENSITIVITY: optimality gap
#   The gap is the solver's guarantee that the returned solution is within
#   X% of the true minimum cost (gap = (upper-lower)/lower). A looser gap
#   is faster but less precise. We check that the solution is stable across
#   gaps (current scenario, comparison target) and record solve time.
# ============================================================

message("\n=== Sensitivity: optimality gap ===")

GAP_GRID <- c(0.1, 0.05, 0.01, 0.001, 0)   # 0 = prove optimality

gap_sens <- lapply(GAP_GRID, function(g) {
  t0 <- Sys.time()
  s <- problem(pu_dat, spec_dat, cost_column = "cost_hfi", rij = puvspr_dat) %>%
    add_min_set_objective() %>%
    add_relative_targets(COMPARISON_TARGET) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = BOUNDARY_PENALTY, data = bmat_current) %>%
    add_cbc_solver(gap = g, threads = N_THREADS, verbose = FALSE) %>%
    solve()
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  sel  <- s$id[s$solution_1 == 1]
  data.frame(
    gap         = g,
    n_selected  = length(sel),
    selected_km = round(sum(pu_dat$length_km[pu_dat$id %in% sel], na.rm = TRUE), 1),
    total_cost  = round(sum(s$cost[s$solution_1 == 1], na.rm = TRUE), 4),
    solve_secs  = round(secs, 1),
    sel_ids     = I(list(sel))   # keep for Jaccard vs the tightest
  )
}) %>% rbindlist()

# Jaccard of each gap's solution vs the gap=0 (proven-optimal) solution
opt_sel <- gap_sens$sel_ids[[which(gap_sens$gap == 0)]]
gap_sens[, jaccard_vs_opt := sapply(sel_ids, function(s)
  length(intersect(s, opt_sel)) / length(union(s, opt_sel)))]

print(gap_sens[, .(gap, n_selected, selected_km, total_cost, solve_secs, jaccard_vs_opt)])
fwrite(gap_sens[, .(gap, n_selected, selected_km, total_cost, solve_secs, jaccard_vs_opt)],
       "prioritization/sensitivity_gap.csv")

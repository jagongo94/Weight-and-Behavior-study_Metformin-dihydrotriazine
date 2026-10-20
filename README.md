# Weight-and-Behavior-study_Metformin-dihydrotriazine
female mice weight change and Behavioral studies upon treatment of Metformin-dihydrotriazine
# =============================================================================
#  Metformin-conjugate mouse study - figure panels d to i
#  Publication-ready R codebook
# -----------------------------------------------------------------------------
#  INPUT   Figure_panels_d_to_i_RAW_DATA.xlsx   (two tabs, raw values only)
#            tab 1  "Figure d-f  weight study"   55 mice, 12 groups
#            tab 2  "Figure g-i  behaviour"      28 mice, 6 groups
#
#  OUTPUT  one vector file per panel in OUT_DIR, as .svg (editable text),
#          .pdf (for submission) and .png (for drafts), plus a stats log.
#
#  PANELS
#    d  Weight change at week 3 relative to vehicle HFD, 12 groups   n = 55
#    e  Weight trajectory from baseline to week 3                    n = 55
#    f  Fasting glucose vs week-3 weight change                      n = 54
#    g  PCoA biplot, Bray-Curtis, behaviour + metabolic measures     n = 27
#    h  Body weight at week 3 vs distance travelled                  n = 27
#    i  Distance travelled vs resting time                           n = 28
#
#  Every heading, subtitle, axis label and caption is defined once in the FIG
#  block below. Edit text there, never inside the plotting code.
#
#  NOTE ON MOUSE 10RR.  Two mice were given ear tag 10RR (YA-50 Vehicle ND and
#  YA-60 Metformin ND). Only YA-60 has treatment weights, so 10RR's weight and
#  metabolic values cannot be attributed and are blank in the source file.
#  Panels g and h therefore use 27 mice; panel i uses all 28 because it needs
#  only behaviour. This is handled automatically by na.omit on the columns each
#  panel requires - nothing is hard-coded.
#
#  All P values are raw and uncorrected for multiple comparisons.
# =============================================================================

## ---- 0. Packages ------------------------------------------------------------
# install.packages(c("readxl","dplyr","tidyr","ggplot2","vegan","svglite","ggrepel","scales"))
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2)
  library(vegan);  library(svglite); library(ggrepel); library(scales)
})

set.seed(1)   # fixes the PERMANOVA permutations and the point jitter

## ---- 1. Configuration -------------------------------------------------------
DATA_FILE <- "Figure_panels_d_to_i_RAW_DATA.xlsx"
TAB_WEIGHT <- "Figure d-f  weight study"
TAB_BEHAV  <- "Figure g-i  behaviour"
OUT_DIR    <- "figures"
DEVICES    <- c("svg", "pdf", "png")   # drop any you do not need
PNG_DPI    <- 600                      # journals ask for >= 300 dpi
BASE_SIZE  <- 9                        # point size of axis text at final print size
BASE_FAMILY <- ""                      # "" = device default sans; set "Helvetica" or "Arial" if available
SHOW_PANEL_TAG <- TRUE                 # the bold a/b/c letter in the corner
SHOW_SUBTITLE  <- TRUE                 # set FALSE if the journal wants all text in the legend

dir.create(OUT_DIR, showWarnings = FALSE)

## ---- 2. Figure headings -----------------------------------------------------
##  Every piece of panel text lives here. Nothing below this block writes text.
FIG <- list(
  d = list(
    tag      = "d",
    title    = "Weight change after three weeks of treatment",
    subtitle = "Each point is one mouse. Vehicle HFD is set to zero. Student t test against vehicle HFD, uncorrected.",
    x        = NULL,
    y        = "Weight change at week 3, relative to\nvehicle HFD (percentage points)",
    width    = 7.2, height = 4.6),
  e = list(
    tag      = "e",
    title    = "Weight trajectory over the treatment period",
    subtitle = "Bold lines are group means with a 95% confidence interval; thin lines are individual mice.",
    x        = NULL,
    y        = "Weight change from baseline (%)",
    legend   = "Group",
    width    = 5.6, height = 4.3),
  f = list(
    tag      = "f",
    title    = "Fasting glucose tracks weight change",
    subtitle = "Dashed line is an ordinary least-squares fit with a 95% confidence band.",
    x        = "Weight change, pre-treatment to week 3 (%)",
    y        = "Fasting glucose (mg/dL)",
    legend   = "Group",
    width    = 5.2, height = 4.3),
  g = list(
    tag      = "g",
    title    = "Behavioural and metabolic profiles separate by treatment",
    subtitle = "Principal coordinates on Bray-Curtis distances. Shaded areas are group convex hulls; arrows are Spearman correlations of each measure with the two axes.",
    x        = "PCoA 1 (%s%% of variation)",      # %s is filled with the eigenvalue percentage
    y        = "PCoA 2 (%s%% of variation)",
    legend   = "Group",
    width    = 6.6, height = 5.0),
  h = list(
    tag      = "h",
    title    = "Mice that move more weigh less",
    subtitle = "Small symbols are individual mice; large symbols are group means with s.e.m. on both axes.",
    x        = "Distance travelled (cm)",
    y        = "Body weight at week 3 (g)",
    legend   = "Group",
    width    = 5.4, height = 4.4),
  i = list(
    tag      = "i",
    title    = "Resting time is inversely related to distance travelled",
    subtitle = "Small symbols are individual mice; large symbols are group means with s.e.m. on both axes.",
    x        = "Resting time (s)",
    y        = "Distance travelled (cm)",
    legend   = "Group",
    width    = 5.4, height = 4.4)
)

## Text fragments reused across panels
TXT <- list(
  hollow   = "Hollow symbols: < 20 s total exploration",
  other    = "Other groups (mean)",
  nfmt     = function(g, n) sprintf("%s\n(n = %d)", g, n),
  spearman = function(rho, p, n) sprintf("Spearman rho = %.2f, P = %s, n = %d", rho, fmt_p(p), n),
  pearson  = function(lab, r, p, n) sprintf("%s: r = %.2f, P = %s, n = %d", lab, r, fmt_p(p), n),
  permanova = function(r2, p) sprintf("PERMANOVA R2 = %.2f, P = %.4f", r2, p)
)

## ---- 3. Appearance ----------------------------------------------------------
GROUPS6 <- c("Vehicle ND", "Vehicle HFD", "Metformin HFD",
             "Metformin-nonanal", "Metformin-acetaldehyde", "Lac-Phe")
COLS <- c("Vehicle ND" = "#2F6FD0", "Vehicle HFD" = "#E8643A", "Metformin HFD" = "#1FA37A",
          "Metformin-nonanal" = "#E8A10C", "Metformin-acetaldehyde" = "#E0709A",
          "Lac-Phe" = "#1E8A1E")
SHAPES <- c("Vehicle ND" = 21, "Vehicle HFD" = 22, "Metformin HFD" = 24,
            "Metformin-nonanal" = 23, "Metformin-acetaldehyde" = 25, "Lac-Phe" = 21)
GREY <- "#9A9A9A"
FLAGGED <- c("9B", "11N")        # drawn hollow in panels g, h and i
SIG_COL <- "#C62828"             # colour of a significant P value label

theme_pub <- function(base_size = BASE_SIZE, base_family = BASE_FAMILY) {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      plot.title        = element_text(size = base_size + 1.5, face = "bold", hjust = 0,
                                       margin = margin(b = 2)),
      plot.subtitle     = element_text(size = base_size - 0.5, colour = "#555555", hjust = 0,
                                       margin = margin(b = 6)),
      plot.tag          = element_text(size = base_size + 5, face = "bold"),
      plot.tag.position = c(0.005, 0.99),
      axis.line         = element_line(linewidth = 0.4, colour = "black"),
      axis.ticks        = element_line(linewidth = 0.4, colour = "black"),
      axis.text         = element_text(colour = "black"),
      legend.position   = "bottom",
      legend.key.size   = unit(0.9, "lines"),
      legend.text       = element_text(size = base_size - 0.5),
      legend.title      = element_text(size = base_size - 0.5),
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.margin       = margin(6, 8, 4, 6)
    )
}

## P values as the figure prints them
fmt_p <- function(p) ifelse(p < 1e-3, formatC(p, format = "e", digits = 1), sprintf("%.3f", p))

## Spearman P from the t approximation.  R's cor.test switches to an exact or AS89
## p value and warns when ranks are tied; the published panels used the t
## approximation (the SciPy default), so we compute it explicitly and the two
## codebooks agree to the quoted digits.
spearman_test <- function(x, y) {
  k <- !is.na(x) & !is.na(y); x <- x[k]; y <- y[k]; n <- length(x)
  rho <- cor(x, y, method = "spearman")
  tt  <- rho * sqrt((n - 2) / (1 - rho^2))
  list(rho = rho, p = 2 * pt(-abs(tt), n - 2), n = n)
}

## Headings are wrapped to the panel width so nothing is clipped at export.
## ~11 characters per inch at BASE_SIZE; tune CHARS_PER_INCH if you change the base size.
CHARS_PER_INCH <- 13
wrap_to <- function(s, inches) {
  if (is.null(s) || !nzchar(s)) return(s)
  paste(strwrap(s, width = max(20, floor(inches * CHARS_PER_INCH))), collapse = "\n")
}

decorate <- function(p, cfg, xlab = cfg$x, ylab = cfg$y) {
  p + labs(title = wrap_to(cfg$title, cfg$width),
           subtitle = if (SHOW_SUBTITLE) wrap_to(cfg$subtitle, cfg$width) else NULL,
           tag = if (SHOW_PANEL_TAG) cfg$tag else NULL,
           x = xlab, y = ylab)
}

save_panel <- function(p, name, cfg) {
  for (dev in DEVICES) {
    f <- file.path(OUT_DIR, paste0(name, ".", dev))
    if (dev == "svg") {
      ggsave(f, p, width = cfg$width, height = cfg$height, device = svglite::svglite)
    } else if (dev == "pdf") {
      ggsave(f, p, width = cfg$width, height = cfg$height, device = cairo_pdf)
    } else {
      ggsave(f, p, width = cfg$width, height = cfg$height, dpi = PNG_DPI, bg = "white")
    }
  }
  message("  wrote ", name, " (", paste(DEVICES, collapse = ", "), ")")
}

LOG <- character(0)
log_line <- function(...) { s <- paste0(...); LOG <<- c(LOG, s); message(s) }

## ---- 4. Data ----------------------------------------------------------------
wt <- read_excel(DATA_FILE, sheet = TAB_WEIGHT, skip = 4) %>%
  rename(mouse = `Mouse ID`, tag = `Ear tag`, group = Group, diet = Diet,
         pre = `Weight pre-treatment (g)`, w1 = `Weight week 1 (g)`,
         w2 = `Weight week 2 (g)`, w3 = `Weight week 3 (g)`,
         glucose = `Fasting glucose (mg/dL)`) %>%
  mutate(c1 = (w1 / pre - 1) * 100,
         c2 = (w2 / pre - 1) * 100,
         c3 = (w3 / pre - 1) * 100)

VMEAN <- mean(wt$c3[wt$group == "Vehicle HFD"])
wt <- mutate(wt, rel3 = c3 - VMEAN)          # panel d plots this: vehicle HFD at zero

bh <- read_excel(DATA_FILE, sheet = TAB_BEHAV, skip = 4) %>%
  rename(mouse = `Mouse ID (cage + ear tag)`, group = Group,
         resting = `Resting / stationary (s)`, distance = `Distance travelled (cm)`,
         pre = `Weight pre-treatment (g)`, w1 = `Weight week 1 (g)`,
         w3 = `Weight week 3 (g)`, glucose = `Fasting glucose (mg/dL)`,
         ogtt = `OGTT AUC, 0-120 min`) %>%
  mutate(group = factor(group, levels = GROUPS6),
         hollow = mouse %in% FLAGGED,
         cw1 = (w1 / pre - 1) * 100,
         cw3 = (w3 / pre - 1) * 100)

log_line(sprintf("loaded %d mice for panels d-f and %d for panels g-i; vehicle HFD gained %.1f%% by week 3",
                 nrow(wt), nrow(bh), VMEAN))

## ---- 5. Panel d -------------------------------------------------------------
ref <- wt$rel3[wt$group == "Vehicle HFD"]
ord <- wt %>% filter(!group %in% c("Vehicle ND", "Metformin ND", "Vehicle HFD")) %>%
  group_by(group) %>% summarise(m = median(rel3), .groups = "drop") %>% arrange(m) %>% pull(group)
LEVELS_D <- c("Vehicle ND", "Metformin ND", ord, "Vehicle HFD")

stats_d <- wt %>% group_by(group) %>%
  summarise(n = n(), median = median(rel3),
            P = if (first(group) == "Vehicle HFD") NA_real_
                else t.test(rel3, ref, var.equal = TRUE)$p.value,   # Student t test
            top = max(rel3), .groups = "drop") %>%
  mutate(group = factor(group, levels = LEVELS_D),
         label = ifelse(is.na(P), NA, ifelse(P < 0.001, "P<0.001", sprintf("P=%.3f", P))),
         sig = !is.na(P) & P <= 0.05)

wt_d <- mutate(wt, group = factor(group, levels = LEVELS_D))
fills_d <- COLS; fills_d["Metformin HFD"] <- "#7FD1B4"
pal_d <- setNames(ifelse(LEVELS_D %in% names(fills_d), fills_d[LEVELS_D], "#CFCFCF"), LEVELS_D)
pal_pt <- setNames(ifelse(LEVELS_D %in% names(COLS), COLS[LEVELS_D], "#8A8A8A"), LEVELS_D)

p_d <- ggplot(wt_d, aes(group, rel3)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = 2.5, linetype = "dotted", colour = "#666666", linewidth = 0.4) +
  geom_boxplot(aes(fill = group), outlier.shape = NA, width = 0.6, alpha = 0.55,
               colour = "#444444", linewidth = 0.35) +
  geom_jitter(aes(colour = group), width = 0.12, height = 0, size = 1.3, stroke = 0.3) +
  geom_text(data = filter(stats_d, !is.na(label)), aes(y = top + 1.5, label = label, colour = sig),
            angle = 90, hjust = 0, vjust = 0.5, size = 2.5, show.legend = FALSE,
            inherit.aes = TRUE) +
  scale_fill_manual(values = pal_d, guide = "none") +
  scale_colour_manual(values = c(pal_pt, "TRUE" = SIG_COL, "FALSE" = "#555555"), guide = "none") +
  scale_x_discrete(labels = function(g) TXT$nfmt(g, stats_d$n[match(g, stats_d$group)])) +
  coord_cartesian(ylim = c(-48, 34)) +
  theme_pub() + theme(axis.text.x = element_text(angle = 40, hjust = 1))
p_d <- decorate(p_d, FIG$d)
save_panel(p_d, "panel_d_weight_change_vs_vehicle_HFD", FIG$d)
stats_d <- arrange(stats_d, group)
for (i in seq_len(nrow(stats_d))) with(stats_d[i, ],
  log_line(sprintf("  d  %-24s n=%d  median %7.2f  %s", as.character(group), n, median,
                   ifelse(is.na(P), "reference", sprintf("P=%.4f", P)))))

## ---- 6. Panel e -------------------------------------------------------------
TP <- c("Pre-treatment", "Week 1", "Week 2", "Week 3")
traj <- wt %>%
  transmute(mouse, group, `Pre-treatment` = 0, `Week 1` = c1, `Week 2` = c2, `Week 3` = c3) %>%
  pivot_longer(-c(mouse, group), names_to = "time", values_to = "pct") %>%
  mutate(time = factor(time, levels = TP))

CI_GROUPS <- c("Metformin-nonanal", "Metformin-acetaldehyde", "Metformin HFD", "Vehicle HFD")
summ_e <- traj %>% group_by(group, time) %>%
  summarise(n = n(), m = mean(pct),
            ci = ifelse(n > 1, qt(0.975, n - 1) * sd(pct) / sqrt(n), 0), .groups = "drop")

p_e <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "#888888", linewidth = 0.4) +
  geom_line(data = filter(summ_e, !group %in% GROUPS6),
            aes(time, m, group = group), colour = GREY, linewidth = 0.4, alpha = 0.6) +
  geom_line(data = filter(traj, group %in% CI_GROUPS),
            aes(time, pct, group = mouse, colour = group), linewidth = 0.25, alpha = 0.35) +
  geom_ribbon(data = filter(summ_e, group %in% CI_GROUPS),
              aes(time, ymin = m - ci, ymax = m + ci, group = group, fill = group),
              alpha = 0.15) +
  geom_line(data = filter(summ_e, group %in% GROUPS6),
            aes(time, m, group = group, colour = group, linewidth = group %in% CI_GROUPS)) +
  scale_colour_manual(values = COLS, breaks = GROUPS6, name = FIG$e$legend) +
  scale_fill_manual(values = COLS, guide = "none") +
  scale_linewidth_manual(values = c(`TRUE` = 1.1, `FALSE` = 0.7), guide = "none") +
  guides(colour = guide_legend(nrow = 2, override.aes = list(linewidth = 1))) +
  theme_pub()
p_e <- decorate(p_e, FIG$e)
save_panel(p_e, "panel_e_weight_trajectories", FIG$e)

## ---- 7. Panel f -------------------------------------------------------------
wf <- filter(wt, !is.na(glucose))
ct_all <- cor.test(wf$c3, wf$glucose)
wh <- filter(wf, diet == "HFD")
ct_hfd <- cor.test(wh$c3, wh$glucose)
lab_f <- paste(TXT$pearson("All mice", ct_all$estimate, ct_all$p.value, nrow(wf)),
               TXT$pearson("HFD only", ct_hfd$estimate, ct_hfd$p.value, nrow(wh)), sep = "\n")

p_f <- ggplot(wf, aes(c3, glucose)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", linetype = "dashed",
              linewidth = 0.5, fill = "#BBBBBB", alpha = 0.35) +
  geom_point(data = filter(wf, !group %in% GROUPS6), colour = "#777777", fill = "#BDBDBD",
             shape = 21, size = 1.6, stroke = 0.25) +
  geom_point(data = filter(wf, group %in% GROUPS6),
             aes(fill = group, shape = group), colour = "#333333", size = 2, stroke = 0.3) +
  annotate("text", x = Inf, y = -Inf, label = lab_f, hjust = 1.03, vjust = -0.4, size = 2.5,
           colour = "#333333", lineheight = 1.1) +
  scale_fill_manual(values = COLS, breaks = GROUPS6, name = FIG$f$legend) +
  scale_shape_manual(values = SHAPES, breaks = GROUPS6, name = FIG$f$legend) +
  guides(fill = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
  theme_pub()
p_f <- decorate(p_f, FIG$f)
save_panel(p_f, "panel_f_glucose_vs_weight_change", FIG$f)
log_line(sprintf("  f  all mice r=%.2f P=%.4f n=%d | HFD r=%.2f P=%.4f n=%d",
                 ct_all$estimate, ct_all$p.value, nrow(wf),
                 ct_hfd$estimate, ct_hfd$p.value, nrow(wh)))

## ---- 8. Panel g -------------------------------------------------------------
MEAS7 <- c("Exploration (s)", "Circling (s)", "Climbing attempts (s)", "Facing object (s)",
           "Bouts (n)", "Approaches, nose <5 cm (n)", "Sitting on object, excluded (s)")
PCOA_COLS <- c(paste0("Familiar: ", MEAS7), paste0("Novel: ", MEAS7),
               "Time at upper rim (s)", "resting", "distance", "ogtt", "glucose", "cw1", "cw3")

bh_g <- bh[stats::complete.cases(bh[, PCOA_COLS]), ]     # drops 10RR automatically
X <- as.data.frame(bh_g[, PCOA_COLS])
for (cc in c("cw1", "cw3")) X[[cc]] <- X[[cc]] - min(X[[cc]])   # Bray-Curtis needs >= 0
X <- sweep(X, 2, apply(X, 2, max), "/")                         # each measure on its own 0-1 scale

D <- vegdist(X, method = "bray")
pco <- cmdscale(D, k = 2, eig = TRUE)
S <- pco$points
VE <- round(100 * pco$eig[1:2] / sum(pco$eig[pco$eig > 0]))
# eigenvector signs are arbitrary; fix them so the panel always reads the same way
if (cor(S[, 1], bh_g$resting, method = "spearman") < 0) S[, 1] <- -S[, 1]
if (cor(S[, 2], bh_g$`Familiar: Bouts (n)`, method = "spearman") < 0) S[, 2] <- -S[, 2]

perm <- adonis2(D ~ group, data = bh_g, permutations = 9999)
R2 <- perm$R2[1]; PP <- perm$`Pr(>F)`[1]

sc <- bh_g %>% mutate(PC1 = S[, 1], PC2 = S[, 2])
hull <- sc %>% group_by(group) %>% slice(chull(PC1, PC2)) %>% ungroup()
cent <- sc %>% group_by(group) %>% summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")

ARROWS <- tibble::tribble(
  ~col,                           ~label,
  "Familiar: Climbing attempts (s)", "Familiar climbing attempts",
  "Novel: Climbing attempts (s)",    "Novel climbing attempts",
  "Novel: Bouts (n)",                "Novel bouts",
  "distance",                        "Distance travelled",
  "resting",                         "Resting time",
  "cw3",                             "Weight change, week 3",
  "cw1",                             "Weight change, week 1") %>%
  rowwise() %>%
  mutate(x = cor(bh_g[[col]], S[, 1], method = "spearman") * 0.2,
         y = cor(bh_g[[col]], S[, 2], method = "spearman") * 0.2,
         metabolic = col %in% c("cw1", "cw3")) %>% ungroup()

labels_g <- bind_rows(
  transmute(ARROWS, x, y, label, ckey = as.character(metabolic), sz = 2.4),
  transmute(cent, x = PC1, y = PC2, label = as.character(group), ckey = as.character(group), sz = 2.7))

p_g <- ggplot() +
  geom_hline(yintercept = 0, colour = "#BBBBBB", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "#BBBBBB", linewidth = 0.3) +
  geom_polygon(data = hull, aes(PC1, PC2, fill = group, group = group), alpha = 0.12) +
  geom_point(data = filter(sc, !hollow),
             aes(PC1, PC2, fill = group, shape = group), colour = "white", size = 2, stroke = 0.2) +
  geom_point(data = filter(sc, hollow),
             aes(PC1, PC2, colour = group, shape = group), fill = NA, size = 2, stroke = 0.6) +
  geom_segment(data = ARROWS, aes(0, 0, xend = x, yend = y, colour = metabolic),
               arrow = arrow(length = unit(0.055, "in"), type = "closed"),
               linewidth = 0.4, show.legend = FALSE) +
  geom_point(data = cent, aes(PC1, PC2, colour = group), shape = 1, size = 6, stroke = 0.9,
             show.legend = FALSE) +
  # arrow labels and group labels repel each other, so they share one layer
  geom_text_repel(data = labels_g, aes(x, y, label = label, colour = ckey, size = sz),
                  fontface = "bold", show.legend = FALSE, seed = 1,
                  box.padding = 0.45, point.padding = 0.5, max.overlaps = Inf, force = 4,
                  min.segment.length = 0.2, segment.size = 0.2, segment.colour = "#AAAAAA") +
  scale_size_identity() +
  annotate("text", x = -Inf, y = Inf, label = TXT$permanova(R2, PP), hjust = -0.05, vjust = 1.4,
           size = 2.5, colour = "#333333") +
  scale_fill_manual(values = COLS, name = FIG$g$legend) +
  scale_shape_manual(values = SHAPES, name = FIG$g$legend) +
  scale_colour_manual(values = c(COLS, "TRUE" = "#1F5FB4", "FALSE" = "#555555"), guide = "none") +
  guides(fill = guide_legend(nrow = 2, override.aes = list(shape = SHAPES[GROUPS6])),
         shape = guide_legend(nrow = 2)) +
  theme_pub()
p_g <- decorate(p_g, FIG$g,
                xlab = sprintf(FIG$g$x, VE[1]), ylab = sprintf(FIG$g$y, VE[2]))
save_panel(p_g, "panel_g_pcoa_biplot", FIG$g)
log_line(sprintf("  g  PCoA1 %d%%  PCoA2 %d%%  PERMANOVA R2=%.2f P=%.4f  n=%d",
                 VE[1], VE[2], R2, PP, nrow(bh_g)))

## ---- 9. Panels h and i ------------------------------------------------------
scatter_panel <- function(df, xvar, yvar, cfg, name) {
  df <- df[!is.na(df[[xvar]]) & !is.na(df[[yvar]]), ]
  ct <- spearman_test(df[[xvar]], df[[yvar]])
  mu <- df %>% group_by(group) %>%
    summarise(mx = mean(.data[[xvar]]), my = mean(.data[[yvar]]),
              sx = sd(.data[[xvar]]) / sqrt(n()), sy = sd(.data[[yvar]]) / sqrt(n()),
              .groups = "drop")
  p <- ggplot() +
    geom_point(data = filter(df, !hollow),
               aes(.data[[xvar]], .data[[yvar]], colour = group, shape = group),
               size = 1.5, alpha = 0.55, stroke = 0.4) +
    geom_point(data = filter(df, hollow),
               aes(.data[[xvar]], .data[[yvar]], colour = group, shape = group),
               size = 1.8, stroke = 0.6, fill = NA) +
    geom_errorbar(data = mu, aes(x = mx, ymin = my - sy, ymax = my + sy, colour = group),
                  width = 0, linewidth = 0.5, show.legend = FALSE) +
    geom_errorbarh(data = mu, aes(y = my, xmin = mx - sx, xmax = mx + sx, colour = group),
                   height = 0, linewidth = 0.5, show.legend = FALSE) +
    geom_point(data = mu, aes(mx, my, fill = group, shape = group), colour = "black",
               size = 3.4, stroke = 0.4) +
    geom_text_repel(data = mu, aes(mx, my, label = group, colour = group), size = 2.5,
                    fontface = "bold", show.legend = FALSE, seed = 1, point.padding = 0.6) +
    annotate("text", x = -Inf, y = Inf,
             label = TXT$spearman(ct$rho, ct$p, ct$n),
             hjust = -0.05, vjust = 1.5, size = 2.5, colour = "#333333") +
    scale_colour_manual(values = COLS, name = cfg$legend) +
    scale_fill_manual(values = COLS, name = cfg$legend) +
    scale_shape_manual(values = SHAPES, name = cfg$legend) +
    guides(colour = guide_legend(nrow = 2), fill = guide_legend(nrow = 2),
           shape = guide_legend(nrow = 2)) +
    theme_pub()
  p <- decorate(p, cfg)
  save_panel(p, name, cfg)
  log_line(sprintf("  %s  Spearman rho=%.2f P=%s n=%d", cfg$tag, ct$rho,
                   fmt_p(ct$p), ct$n))
  invisible(p)
}

scatter_panel(bh, "distance", "w3", FIG$h, "panel_h_distance_vs_week3_weight")
scatter_panel(bh, "resting", "distance", FIG$i, "panel_i_resting_vs_distance")

## ---- 10. Stats log ----------------------------------------------------------
writeLines(c(paste("Generated", format(Sys.time(), "%Y-%m-%d %H:%M")),
             paste("Source:", DATA_FILE), "", LOG),
           file.path(OUT_DIR, "panel_statistics.txt"))
message("done -> ", normalizePath(OUT_DIR))

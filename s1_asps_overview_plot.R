#===== SECTION 1: Setup ======================================================

library(tidyverse)
library(ggrepel)
library(readxl)
library(svglite)  # ggsave() dispatches to this for .svg output

# edit data/asps_overview_figure_data.xlsx directly (add rows, etc.) and
# rerun this script -- no separate export step needed
data_path <- "data/asps_overview_figure_data.xlsx"


#===== SECTION 2: Load & label data ==========================================

# display_name is a manual lookup because the sheet's snake_case variable
# names ("root_sprout_ratio") don't map 1:1 onto the short axis labels we
# want ("Root/Sprout"). Add new variables here as they show up in the sheet.
display_name <- c(
  root_sprout_ratio = "Root/Sprout",
  seedling_length    = "Seedling Length",
  LCFD               = "LCFD",
  cluster_shade      = "Cluster Shade",
  entropy            = "Entropy",
  max_probability    = "Max Prob."
)

overview_raw <- read_excel(data_path)

# group_lookup is the single source of truth for the 4 point categories:
# dataset_scope/interaction determine which row a point falls into, and the
# label, color, and shape for that row are all defined together here. This
# way there's only one place to edit legend text, and color_manual/
# shape_manual (built below from this same table) can never drift out of
# sync with it, the way two separately-typed named vectors could.
group_lookup <- tribble(
  ~dataset_scope, ~interaction,   ~group_label,
  "full",          "none",        "Full dataset, no interact.",
  "full",          "significant", "Full dataset, sig. interact.",
  "subset",        "none",        "Pairwise/subset, no interact.",
  "subset",        "significant", "Pairwise/subset, sig. interact."
) |>
  mutate(
    group_color = if_else(interaction == "none", "#2E7D32", "#E64A19"),
    # 21/24 (fillable circle/triangle), not 16/17 (solid-only), so that
    # Section 2b can hollow out a point independently of this color+shape
    # legend
    group_shape = if_else(dataset_scope == "full", 21, 24)
  )

overview <- overview_raw |>
  mutate(
    label = paste0(display_name[variable], " (", comparison, ")"),
    # comparison_type collapses to a binary shape: circle for the omnibus
    # test on the full dataset, triangle for anything derived from a subset
    # (pairwise post-hoc contrasts, or limited_dataset ANOVAs run on a
    # subset mirroring earlier experiments).
    dataset_scope = if_else(comparison_type == "full_dataset", "full", "subset")
  ) |>
  left_join(group_lookup, by = c("dataset_scope", "interaction")) |>
  mutate(group = factor(group_label, levels = group_lookup$group_label))


#===== SECTION 2b: Subset-agreement fill (hollow = 2016/2022 disagree) =======
#
# Statistical purpose: a pairwise p-value pooled across both experimental
# years can be "significant" even when the two years individually disagree
# on significance status (see e.g. Maximum Probability's near-reversed
# 2016/2022 pattern, discussed in the manuscript). Hollowing out a point
# whose disagreement_between_subsets == "yes" keeps that instability visible
# on the plot itself, rather than only in the running text. Points with no
# subset breakdown recorded (full-dataset omnibus tests, NA) are treated as
# not flagged and stay filled.
#
# Code-level detail: fill is set to plain white rather than mapped through a
# color scale, and applied via scale_fill_identity() (Section 5) -- this
# keeps it out of the discrete group legend entirely, so hollow/filled reads
# as a plot-wide convention (explained once in the caption) instead of adding
# a second row of legend keys.
overview <- overview |>
  mutate(
    point_fill = if_else(
      replace_na(disagreement_between_subsets == "yes", FALSE),
      "white", group_color
    )
  )


#===== SECTION 3: Rescale effect size onto a common 0-2 axis =================
#
# Statistical purpose: Cohen's d and omega-squared live on totally different
# native scales (d is unbounded, omega2 is bounded [0,1] and typically tiny),
# so they can't share an axis without rescaling. We rescale each metric by
# its own "medium" and "large" benchmark (Cohen, 1988), so that x = 1 always
# means "medium effect" and x = 2 always means "large effect" regardless of
# which metric produced the point. Sign is dropped (effect direction is not
# what this figure is about); only |effect_size| is plotted.
#
# Code-level detail: within each metric, the mapping is piecewise linear:
#   [0, medium]      -> [0, 1]
#   [medium, large]  -> [1, 2]
#   > large          -> capped at 2 (we have no "very large" points yet, but
#                        this keeps future extreme values on-axis instead of
#                        blowing out the scale)
effect_size_benchmarks <- tribble(
  ~effect_size_type, ~medium, ~large,
  "cohens_d",          0.5,     0.8,
  "omega2",            0.06,    0.14,
  # partial_eta2 and pillai aren't in the data yet; they reuse the omega2
  # benchmarks as the closest published convention (Cohen, 1988) until we
  # have metric-specific thresholds.
  "partial_eta2",      0.06,    0.14,
  "pillai",            0.06,    0.14
)

rescale_effect_size <- function(abs_es, medium, large) {
  case_when(
    abs_es <= medium ~ abs_es / medium,
    abs_es <= large  ~ 1 + (abs_es - medium) / (large - medium),
    TRUE             ~ 2
  )
}

overview <- overview |>
  left_join(effect_size_benchmarks, by = "effect_size_type") |>
  mutate(effect_size_rescaled = rescale_effect_size(abs(effect_size), medium, large))


#===== SECTION 4: Prepare significance axis ==================================

# p = 0 in the sheet reflects rounding/underflow in the source software, not
# an actual zero p-value -- -log10(0) is undefined (Inf). We floor it at half
# the smallest non-zero p-value observed elsewhere in the data, which keeps
# that point visibly "most significant" without breaking the axis.
p_floor <- min(overview$p_value[overview$p_value > 0], na.rm = TRUE) / 2

overview <- overview |>
  mutate(
    p_value_floored = if_else(p_value == 0, p_floor, p_value),
    neg_log10_p = -log10(p_value_floored),
    # A couple of labels sit almost exactly on a dashed significance line, so
    # their repel starting position gets a manual nudge to clear it before
    # repel's vertical-only collision avoidance takes over. Add more rows
    # here if other labels end up sitting on a reference line.
    label_nudge_y = case_when(
      # p = 0.0426, just above the p<0.05 line (1.37 vs. 1.30) -- nudged
      # down, putting it on the other side of the line instead of astride it
      label == "Seedling Length (full)"    ~ -0.15,
      # p = 0.0136, just below the p<0.01 line (1.87 vs. 2.00) -- nudged up
      # to clear the line rather than sit flush against it
      label == "Cluster Shade (stan-lac)"  ~  0.25,
      TRUE ~ 0
    )
  )


#===== SECTION 5: Plot ========================================================

# pulled straight from group_lookup (Section 2) so the legend can't drift
# out of sync with the label text defined there
group_colors <- setNames(group_lookup$group_color, group_lookup$group_label)
group_shapes <- setNames(group_lookup$group_shape, group_lookup$group_label)

sig_lines <- tibble(
  y = c(-log10(0.05), -log10(0.01)),
  lbl = c("p<0.05", "p<0.01")
)

p <- ggplot(overview, aes(x = effect_size_rescaled, y = neg_log10_p)) +
  # dashed reference lines: grey40 / linewidth 0.4 matches the house convention
  # used for baseline lines in s5_cress_descriptive_anova.r:2438-2440
  geom_hline(data = sig_lines, aes(yintercept = y), linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = c(1, 2), linetype = "dotted", color = "grey70") +
  # stroke=1 keeps the border visible at the same weight whether the point
  # ends up filled (point_fill = group_color) or hollow (point_fill = white)
  geom_point(aes(color = group, shape = group, fill = point_fill), size = 4, stroke = 1) +
  # Labels sit at a fixed offset to the right of every point (nudge_x), and
  # ggrepel is restricted to vertical-only movement (direction = "y") to
  # resolve collisions -- so a label only moves up/down, and only when
  # another label/point is close enough to actually overlap it. This reads
  # more predictably than free 2D repel, where labels can end up on any side
  # of the point they belong to.
  geom_text_repel(
    aes(label = label),
    size = 3.4, color = "grey20", hjust = 0,
    # nudge_x/nudge_y set each label's *starting* position before repel's
    # collision-avoidance runs; the leader segment still anchors to the
    # true data point (unaffected by these nudges), so this only shifts
    # where the text lands, not what it's pointing at.
    nudge_x = 0.06, nudge_y = overview$label_nudge_y, direction = "y",
    max.overlaps = Inf, box.padding = 0.25,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0
  ) +
  geom_text(
    # right-aligned (hjust=1) just inside the x=2 line, nudged above it
    # (y + 0.05) so the text sits clear of the line instead of crossing it
    data = sig_lines, aes(x = 1.95, y = y + 0.05, label = lbl),
    inherit.aes = FALSE, hjust = 1, vjust = 0, size = 3.4, color = "grey40"
  ) +
  # name left as waiver() (not NULL) here, since NULL suppresses the guide
  # title outright -- the actual title text/position is set via guides()
  # below, using the title slot as a footnote rather than a heading
  scale_color_manual(values = group_colors) +
  scale_shape_manual(values = group_shapes) +
  # point_fill is plotted as-is (white or the group's own color), not mapped
  # through a scale, and guide = "none" keeps it from adding a second legend
  # -- the color+shape legend below is overridden to always show filled
  # swatches, since "hollow" is a per-point flag explained via the legend
  # footnote (title.position = "bottom"), not a fifth legend category
  scale_fill_identity(guide = "none") +
  # color and shape need an *identical* guide_legend() spec here, or ggplot
  # stops merging them into one legend and instead draws two side by side --
  # one properly colored, one falling back to black/white for the un-overridden
  # aesthetic
  guides(
    color = guide_legend(
      title = "Hollow: subset disagreement",
      title.position = "bottom",
      title.hjust = 0,
      override.aes = list(fill = group_lookup$group_color)
    ),
    shape = guide_legend(
      title = "Hollow: subset disagreement",
      title.position = "bottom",
      title.hjust = 0,
      override.aes = list(fill = group_lookup$group_color)
    )
  ) +
  scale_x_continuous(
    limits = c(0, 2.05),  # panel ends right after the x=2 "large" tick
    breaks = seq(0, 2, 0.25),  # tick marks every 0.25 ...
    labels = function(x) ifelse(  # ... but only 0/1/2 get a text label, others blank
      x %in% c(0, 1, 2), c("0", "1 (medium)", "2 (large)")[match(x, c(0, 1, 2))], ""
    )
  ) +
  labs(
    title = "ASPS overview",
    x = "Effect size (rescaled: 0 = none, 1 = medium threshold, 2 = large threshold)",
    y = expression(Significance~(-log[10]~p))
  ) +
  # theme_bw() + this sizing pattern is the house convention used throughout
  # s5_cress_descriptive_anova.r / s7_cress_even_uneven_drift.r line plots
  # (title bold ~15pt, axis.title 14pt, axis.text 13pt, legend 13/12pt)
  theme_bw(base_size = 13) +
  theme(
    # inset into the empty top-right corner of the panel (no data ever
    # reaches high effect size + high significance together) instead of
    # taking up a separate column/row outside the panel
    legend.position = "inside",
    legend.position.inside = c(0.99, 0.99),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "grey40", linewidth = 0.3),
    legend.margin = margin(t = 4, r = 6, b = 4, l = 6),
    # same size/color as legend.text below, just with top margin for
    # separation, so the bottom-positioned title (Section 5, guides()) reads
    # as part of the same legend rather than a visually distinct heading
    legend.title = element_text(size = 3.4 * .pt, margin = margin(t = 4)),
    # 3.4 mm (the geom_text_repel point-label size) converts to ~9.7pt via
    # ggplot2's internal mm-to-pt factor (.pt = 72.27/25.4), so the legend
    # text visually matches the point labels
    legend.text = element_text(size = 3.4 * .pt),
    panel.grid = element_blank(),  # drop gridlines; the dotted x=1/x=2 vlines carry that job instead
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  )

p


#===== SECTION 6: Export ======================================================

ggsave("asps_overview_plot.png", p, width = 20, height = 18, units = "cm", dpi = 300)

# Vector master (manuscript-ready, stays smooth at any zoom) alongside the
# PNG above - same dimensions, no dpi (meaningless for a vector format).
ggsave("asps_overview_plot.svg", p, width = 20, height = 18, units = "cm")

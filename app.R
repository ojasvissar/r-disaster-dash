# ══════════════════════════════════════════════════════════════════════════════
#  Disaster Dash (R)  ·  Individual MDS Assignment
#  Global Disaster Impact & Humanitarian Aid (2018–2024)
#  Author: Ojasv Issar
# ══════════════════════════════════════════════════════════════════════════════

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(lubridate)

# ── Data Loading ───────────────────────────────────────────────────────────────
# Reads the CSV if available; falls back to synthetic data for demo
load_data <- function() {
  csv_path <- file.path("data", "global_disaster_response_2018_2024.csv")
  if (file.exists(csv_path)) {
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    return(df)
  } else {
    # Synthetic fallback — mirrors real column structure
    message("ℹ  CSV not found — generating synthetic demo data.")
    set.seed(42)
    n <- 2000

    countries <- c(
      "Australia", "Bangladesh", "Brazil", "Canada", "Chile", "China",
      "France", "Germany", "Greece", "India", "Indonesia", "Italy",
      "Japan", "Mexico", "Nigeria", "Philippines", "South Africa",
      "Spain", "Turkey", "United States"
    )
    disaster_types <- c(
      "Drought", "Earthquake", "Extreme Heat", "Flood", "Hurricane",
      "Landslide", "Storm Surge", "Tornado", "Volcanic Eruption", "Wildfire"
    )

    loss     <- runif(n, 1e5, 2e7)
    severity <- runif(n, 1, 10)
    data.frame(
      country             = sample(countries, n, replace = TRUE),
      disaster_type       = sample(disaster_types, n, replace = TRUE),
      date                = sample(seq(as.Date("2018-01-01"),
                                       as.Date("2024-12-31"), by = "day"),
                                   n, replace = TRUE),
      economic_loss_usd   = loss,
      aid_amount_usd      = loss * runif(n, 0.02, 0.08),
      casualties          = round(runif(n, 0, 500)),
      severity_index      = severity,
      response_time_hours = runif(n, 1, 60),
      response_efficiency_score = runif(n, 30, 100),
      recovery_days       = round(severity * 8 + runif(n, 0, 40)),
      stringsAsFactors = FALSE
    )
  }
}

df <- load_data()

# ── Lookup Tables ──────────────────────────────────────────────────────────────
COUNTRIES      <- sort(unique(df$country))
DISASTER_TYPES <- sort(unique(df$disaster_type))
DATE_MIN       <- min(df$date)
DATE_MAX       <- max(df$date)

COUNTRY_CODES <- c(
  "Australia" = "AU", "Bangladesh" = "BD", "Brazil" = "BR", "Canada" = "CA",
  "Chile" = "CL", "China" = "CN", "France" = "FR", "Germany" = "DE",
  "Greece" = "GR", "India" = "IN", "Indonesia" = "ID", "Italy" = "IT",
  "Japan" = "JP", "Mexico" = "MX", "Nigeria" = "NG", "Philippines" = "PH",
  "South Africa" = "ZA", "Spain" = "ES", "Turkey" = "TR", "United States" = "US"
)

TYPE_ICONS <- c(
  "Drought" = "sun-plant-wilt", "Earthquake" = "house-crack",
  "Extreme Heat" = "temperature-high", "Flood" = "water",
  "Hurricane" = "hurricane", "Landslide" = "hill-rockslide",
  "Storm Surge" = "house-flood-water", "Tornado" = "tornado",
  "Volcanic Eruption" = "volcano", "Wildfire" = "fire"
)

VIEW_TITLES <- c(
  overview  = "Overview",
  impact    = "Impact",
  response  = "Response & recovery",
  countries = "Countries"
)

SEVERITY_BREAKS <- c(1, 3, 5, 7, 9, 10.001)
SEVERITY_LABELS <- c("1–3", "3–5", "5–7", "7–9", "9–10")

# ── Design Tokens ──────────────────────────────────────────────────────────────
FONT    <- "Lexend, system-ui, sans-serif"
INK     <- "#1f1f1f"
INK_2   <- "#5b5a56"
MUTED   <- "#8d8b85"
GRID    <- "#ecebe6"
AXIS    <- "#cfcdc6"
ORANGE  <- "#f2542d"   # chart slot 1 · brand accent
VIOLET  <- "#7b4fe0"   # chart slot 2
BLUE    <- "#2a8fd0"   # chart slot 3
YELLOW  <- "#ffd866"

# Ordinal ramps (validated light → dark, light end ≥ 2:1 on white)
ORANGE_RAMP <- c("#f7a184", "#f5794f", "#e2522b", "#b93c1b", "#862a11")
BLUE_RAMP   <- c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281")
# Sequential (heatmap): near-surface tint → dark orange
HEAT_SCALE  <- list(
  list(0, "#fff4ee"), list(0.25, "#fbc7b0"), list(0.5, "#f5794f"),
  list(0.75, "#c9431f"), list(1, "#7a240e")
)

AVATAR_BG <- c("#ffd866", "#d9bdfb", "#c4e6f9", "#ffc2ad", "#bfe8cf", "#f5d0e6")

# ── Helpers ────────────────────────────────────────────────────────────────────
fmt_currency <- function(v) {
  s <- ifelse(v < 0, "-", "")
  v <- abs(v)
  ifelse(v >= 1e12, sprintf("%s$%.2fT", s, v / 1e12),
         ifelse(v >= 1e9,  sprintf("%s$%.1fB", s, v / 1e9),
                ifelse(v >= 1e6,  sprintf("%s$%.1fM", s, v / 1e6),
                       ifelse(v >= 1e3,  sprintf("%s$%.1fK", s, v / 1e3),
                              sprintf("%s$%.0f",  s, v)))))
}

fmt_int <- function(v) formatC(round(v), format = "d", big.mark = ",")

country_code <- function(x) {
  code <- unname(COUNTRY_CODES[x])
  ifelse(is.na(code), toupper(substr(x, 1, 2)), code)
}

avatar <- function(country, size = "md") {
  idx <- (match(country, COUNTRIES) - 1) %% length(AVATAR_BG) + 1
  tags$span(class = paste("av", size), title = country,
            style = paste0("background:", AVATAR_BG[idx]),
            country_code(country))
}

avatar_stack <- function(countries, total) {
  shown <- head(countries, 3)
  extra <- total - length(shown)
  tags$div(
    class = "av-stack",
    lapply(shown, avatar),
    if (extra > 0) tags$span(class = "av md more", paste0("+", extra))
  )
}

type_icon <- function(type) {
  name <- unname(TYPE_ICONS[type])
  icon(if (is.na(name)) "triangle-exclamation" else name)
}

severity_band <- function(x) {
  cut(x, breaks = SEVERITY_BREAKS, labels = SEVERITY_LABELS,
      right = FALSE, include.lowest = TRUE)
}

# Country / type summaries used by several charts
summarise_impact <- function(d, ...) {
  d |>
    group_by(...) |>
    summarise(
      events     = n(),
      casualties = sum(casualties, na.rm = TRUE),
      loss       = sum(economic_loss_usd, na.rm = TRUE),
      aid        = sum(aid_amount_usd, na.rm = TRUE),
      response   = median(response_time_hours, na.rm = TRUE),
      .groups    = "drop"
    ) |>
    mutate(coverage = ifelse(loss > 0, aid / loss * 100, NA_real_))
}

METRICS <- list(
  events     = list(label = "Events",          scale = 1,   axis = list()),
  casualties = list(label = "Casualties",      scale = 1,   axis = list()),
  loss       = list(label = "Economic loss",   scale = 1e9, axis = list(tickprefix = "$", ticksuffix = "B", tickformat = ",.2~f")),
  coverage   = list(label = "Aid coverage",    scale = 1,   axis = list(ticksuffix = "%")),
  response   = list(label = "Median response", scale = 1,   axis = list(ticksuffix = "h"))
)

fmt_metric <- function(v, metric) {
  switch(metric,
         events     = fmt_int(v),
         casualties = fmt_int(v),
         loss       = fmt_currency(v),
         coverage   = sprintf("%.1f%%", v),
         response   = sprintf("%.1f h", v))
}

# ── Plot Styling ───────────────────────────────────────────────────────────────
ax <- function(title = "", ...) {
  modifyList(list(
    title     = list(text = title, font = list(size = 11, color = MUTED)),
    tickfont  = list(size = 10, color = MUTED),
    gridcolor = GRID, linecolor = AXIS, zeroline = FALSE,
    automargin = TRUE, fixedrange = TRUE
  ), list(...))
}

style_plot <- function(p, x = ax(), y = ax(), legend = FALSE,
                       top = if (legend) 34 else 8, ...) {
  p |>
    layout(
      font          = list(family = FONT, size = 11, color = INK_2),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      xaxis = x, yaxis = y,
      showlegend = legend,
      legend = list(orientation = "h", x = 0, y = 1.02,
                    xanchor = "left", yanchor = "bottom",
                    font = list(size = 11, color = INK_2)),
      margin = list(l = 4, r = 12, t = top, b = 4),
      hoverlabel = list(bgcolor = INK, bordercolor = INK,
                        font = list(family = FONT, size = 12, color = "#fff")),
      ...
    ) |>
    config(displayModeBar = FALSE, responsive = TRUE)
}

empty_plotly <- function(msg = "No data to display",
                         hint = "Adjust your filters") {
  plot_ly(type = "scatter", mode = "markers") |>
    layout(
      annotations = list(list(
        text      = paste0("<b>", msg, "</b><br>",
                           "<span style='font-size:11px'>", hint, "</span>"),
        xref = "paper", yref = "paper", x = 0.5, y = 0.5,
        showarrow = FALSE,
        font = list(size = 13, color = MUTED, family = FONT)
      )),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      margin = list(l = 0, r = 0, t = 0, b = 0)
    ) |>
    config(displayModeBar = FALSE)
}

# Horizontal ranked bars — one series in ink, the leader highlighted in orange
ranked_bars <- function(grp, label_col, metric, hover_label = NULL) {
  m <- METRICS[[metric]]
  grp <- grp |>
    filter(!is.na(.data[[metric]])) |>
    arrange(.data[[metric]]) |>
    mutate(
      lab   = factor(.data[[label_col]], levels = .data[[label_col]]),
      value = .data[[metric]] / m$scale,
      tip   = paste0("<b>", .data[[label_col]], "</b><br>",
                     m$label, ": ", fmt_metric(.data[[metric]], metric)),
      fill  = ifelse(.data[[metric]] == max(.data[[metric]]), ORANGE, INK)
    )
  top <- tail(grp, 1)

  plot_ly(grp, x = ~value, y = ~lab, type = "bar", orientation = "h",
          marker = list(color = ~fill),
          text = ~tip, hoverinfo = "text", textposition = "none") |>
    style_plot(
      x = do.call(ax, c(list(title = m$label), m$axis)),
      y = ax(showgrid = FALSE, tickfont = list(size = 11, color = INK_2)),
      bargap = 0.5,
      annotations = list(list(
        x = top$value, y = as.character(top$lab), xanchor = "left",
        text = paste0("  ", fmt_metric(top[[metric]], metric)),
        showarrow = FALSE, font = list(size = 11, color = INK)
      ))
    )
}

# ── Custom CSS ─────────────────────────────────────────────────────────────────
CSS <- r"(
:root {
  --backdrop: #bfd3dc; --frame: #2b2b2b; --panel: #f6f5f1; --card: #ffffff;
  --ink: #1f1f1f; --ink-2: #5b5a56; --muted: #8d8b85; --line: #e6e4dd;
  --yellow: #ffd866; --lilac: #d9bdfb; --sky: #c4e6f9;
  --orange: #f2542d; --orange-dk: #d9431e; --gap: 14px;
}
*, *::before, *::after { box-sizing: border-box; }
html, body {
  background: var(--backdrop) !important; color: var(--ink);
  font-family: 'Lexend', system-ui, sans-serif !important;
  -webkit-font-smoothing: antialiased;
}
body { padding: 14px; }
.bslib-page-fill, .container-fluid { padding: 0 !important; }

/* ── SHELL ── */
.shell {
  display: grid; grid-template-columns: 76px minmax(0, 1fr);
  max-width: 1600px; margin: 0 auto; background: var(--frame);
  border-radius: 32px; padding: 14px 14px 14px 0;
  box-shadow: 0 30px 60px -30px rgba(31, 52, 64, 0.45);
}
.rail {
  display: flex; flex-direction: column; align-items: center;
  gap: 10px; padding: 40px 0 6px;
}
.rail-btn {
  width: 42px; height: 42px; border-radius: 12px; border: 0;
  display: grid; place-items: center; font-size: 1.02rem;
  background: transparent; color: #e8e6df; transition: background .15s, color .15s;
  text-decoration: none; cursor: pointer;
}
.rail-btn:hover { background: rgba(255,255,255,.08); color: #fff; }
.rail-btn.active { background: var(--yellow); color: var(--ink); }
.rail-gap { height: 24px; }
.rail-bottom { margin-top: auto; }

.panel {
  background: var(--panel); border-radius: 24px; padding: 18px 24px 22px;
  min-width: 0; display: flex; flex-direction: column;
}

/* ── TOP BAR ── */
.topbar { display: flex; align-items: center; gap: 12px; flex-wrap: wrap; }
.brand { font-size: .86rem; color: var(--ink-2); white-space: nowrap; }
.brand b { color: var(--orange); font-size: 1.35rem; font-weight: 600; letter-spacing: -.3px; margin-left: 4px; }
.brand b span { color: var(--ink); }
.search-wrap { margin-left: auto; width: 320px; position: relative; }
.search-wrap .form-group, .filters .form-group { margin: 0 !important; width: 100% !important; }
.search-wrap .selectize-input {
  border: 1px solid var(--ink) !important; border-radius: 12px !important;
  height: 42px; padding: 9px 48px 6px 14px !important; box-shadow: none !important;
  font-size: .85rem; display: flex !important; flex-wrap: nowrap !important;
  overflow-x: auto; overflow-y: hidden; scrollbar-width: none;
}
.selectize-input > * { flex: none; }
.search-ico {
  position: absolute; right: 5px; top: 5px; width: 32px; height: 32px; z-index: 3;
  background: var(--orange); color: #fff; border-radius: 9px;
  display: grid; place-items: center; pointer-events: none; font-size: .8rem;
}
.selectize-control.multi .selectize-input > div.item {
  background: var(--panel) !important; color: var(--ink) !important;
  border: 0 !important; border-radius: 7px !important; padding: 1px 6px !important;
  font-size: .78rem;
}
.selectize-control.plugin-remove_button .item .remove { border-left: 0 !important; color: var(--muted) !important; }
.selectize-dropdown { border-radius: 12px !important; border: 1px solid var(--ink) !important; font-size: .82rem; overflow: hidden; }
.selectize-dropdown .active { background: var(--panel) !important; color: var(--ink) !important; }

.icon-btn {
  width: 42px; height: 42px; border-radius: 50% !important; border: 1px solid var(--ink) !important;
  background: #fff !important; color: var(--ink) !important; display: grid !important;
  place-items: center; padding: 0 !important; position: relative;
}
.icon-btn .dot {
  position: absolute; top: 9px; right: 11px; width: 9px; height: 9px;
  background: var(--orange); border: 2px solid #fff; border-radius: 50%;
}
.user { display: flex; align-items: center; gap: 10px; }
.user .av { width: 42px; height: 42px; font-size: .85rem; background: var(--orange); color: #fff; box-shadow: none; }
.user b { display: block; font-weight: 500; font-size: .9rem; line-height: 1.2; }
.user small { color: var(--muted); font-size: .74rem; }

/* ── TITLE + CHIPS + FILTERS ── */
.title-row { display: flex; align-items: center; gap: 14px; margin: 14px 0 12px; flex-wrap: wrap; }
.title-row h1 { font-size: 1.75rem; font-weight: 500; letter-spacing: -.6px; margin: 0; white-space: nowrap; }
.chips { display: flex; gap: 8px; flex-wrap: wrap; margin-left: auto; }
.chip {
  border: 1px solid var(--ink); background: #fff; color: var(--ink);
  border-radius: 11px; padding: 6px 14px; font-size: .82rem; font-family: inherit;
  cursor: pointer; transition: background .15s, color .15s; white-space: nowrap;
}
.chip:hover { background: #efede7; }
.chip.active { background: var(--ink); color: #fff; }
.chip-group { display: flex; gap: 6px; flex-wrap: wrap; }
.chip-group .chip { padding: 3px 10px; font-size: .72rem; border-radius: 9px; border-color: var(--line); }
.chip-group .chip.active { border-color: var(--ink); }

.filters {
  display: flex; flex-wrap: wrap; gap: 8px 20px; align-items: center;
  background: #fff; border: 1px solid var(--line);
  border-radius: 16px; padding: 2px 12px 4px 16px; margin-bottom: var(--gap);
}
.f-type, .f-period { display: flex; align-items: center; gap: 12px; min-width: 0; }
.f-type { flex: 1 1 240px; }
.f-period { flex: 1.4 1 300px; gap: 34px; padding-right: 26px; }
.f-type > :last-child, .f-period > :last-child { flex: 1 1 auto; min-width: 0; }
.f-label { font-size: .64rem; color: var(--muted); text-transform: uppercase; letter-spacing: .8px; white-space: nowrap; }
.filters .selectize-input {
  border: 0 !important; box-shadow: none !important; padding: 3px 0 !important;
  height: 30px; min-height: 30px; font-size: .84rem; background: transparent !important;
  display: flex !important; flex-wrap: nowrap !important; align-items: center;
  overflow-x: auto; overflow-y: hidden; scrollbar-width: none;
}
.filters .irs--shiny { height: 38px; }
.irs--shiny .irs-line { top: 22px !important; }
.irs--shiny .irs-bar { top: 22px !important; background: var(--orange) !important; border: 0 !important; }
.irs--shiny .irs-line { background: #ecebe6 !important; border: 0 !important; }
.irs--shiny .irs-handle { border: 2px solid var(--ink) !important; background: #fff !important; box-shadow: none !important; width: 18px; height: 18px; top: 17px; }
.irs--shiny .irs-from:before, .irs--shiny .irs-to:before, .irs--shiny .irs-single:before { border-top-color: var(--ink) !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background: var(--ink) !important; color: #fff !important; font-family: 'Lexend', sans-serif;
  font-size: .66rem; border-radius: 6px; top: 0;
}
.irs--shiny .irs-min, .irs--shiny .irs-max { display: none; }
.irs--shiny .irs-grid { display: none; }
.match { margin-left: auto; font-size: .8rem; color: var(--ink-2); white-space: nowrap; }
.match b { color: var(--ink); font-weight: 500; font-size: 1rem; margin-right: 4px; }
#reset_button {
  background: var(--panel) !important; color: var(--ink) !important; border: 1px solid var(--ink) !important;
  border-radius: 11px !important; font-size: .78rem !important; padding: 5px 13px !important;
}
#reset_button:hover { background: var(--ink) !important; color: #fff !important; }

/* ── VIEW GRIDS ── */
.views { display: flex; flex-direction: column; }
.view { display: grid; grid-template-columns: repeat(12, minmax(0, 1fr)); gap: var(--gap); }
.kpis { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: var(--gap); }
.span-3 { grid-column: span 3; } .span-4 { grid-column: span 4; } .span-5 { grid-column: span 5; }
.span-6 { grid-column: span 6; } .span-7 { grid-column: span 7; } .span-8 { grid-column: span 8; }
.span-12 { grid-column: span 12; }

/* ── KPI (course-style) CARDS ── */
.kcard {
  border: 1px solid var(--ink); border-radius: 18px; padding: 13px 16px 13px;
  display: flex; flex-direction: column; gap: 9px; min-width: 0;
}
.kcard.yellow { background: var(--yellow); --track: #e9bb3c; --ring: var(--yellow); }
.kcard.lilac  { background: var(--lilac);  --track: #b38bf0; --ring: var(--lilac); }
.kcard.sky    { background: var(--sky);    --track: #7fc6e6; --ring: var(--sky); }
.kcard-top { display: flex; justify-content: space-between; align-items: center; gap: 10px; }
.tag { display: inline-block; padding: 4px 10px; border-radius: 8px; font-size: .72rem; border: 1px solid var(--ink); white-space: nowrap; }
.tag.dark { background: var(--ink); color: #fff; }
.tag.yellow { background: var(--yellow); }
.tag.lilac { background: var(--lilac); }
.kcard-ico { font-size: 1.05rem; }
.k-value { font-size: 1.6rem; font-weight: 500; letter-spacing: -.7px; line-height: 1.1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.k-value span { font-size: .95rem; font-weight: 400; letter-spacing: 0; margin-left: 4px; }
.k-progress-row { display: flex; justify-content: space-between; gap: 8px; font-size: .74rem; margin-bottom: 5px; }
.bar { height: 7px; border-radius: 99px; background: var(--track, #e6e4dd); overflow: hidden; }
.bar > span { display: block; height: 100%; background: var(--ink); border-radius: 99px; }
.kcard-foot { display: flex; justify-content: space-between; align-items: center; gap: 8px; }
.kcard-foot small { font-size: .76rem; }

.av {
  width: 30px; height: 30px; border-radius: 50%; display: inline-grid; place-items: center;
  font-size: .64rem; font-weight: 600; color: var(--ink); flex: none; letter-spacing: .3px;
}
.av.sm { width: 26px; height: 26px; font-size: .6rem; }
.av-stack { display: flex; }
.av-stack .av { margin-left: -6px; box-shadow: 0 0 0 2px var(--ring, #fff); }
.av-stack .av:first-child { margin-left: 0; }
.av.more { background: var(--ink); color: #fff; }

.btn-cta {
  background: var(--orange); color: #fff; border: 0; border-radius: 10px;
  padding: 6px 15px; font-size: .82rem; font-family: inherit; cursor: pointer;
  transition: background .15s; text-decoration: none; white-space: nowrap;
}
.btn-cta:hover { background: var(--orange-dk); color: #fff; }
.btn-cta.wide { width: 100%; padding: 11px; font-size: .92rem; border-radius: 12px; }

/* ── TILES ── */
.tile {
  background: #fff; border: 1px solid var(--ink); border-radius: 18px; padding: 14px 18px 10px;
  min-width: 0; min-height: 0; display: flex; flex-direction: column;
}
.tile-h { display: flex; justify-content: space-between; align-items: flex-start; gap: 10px; margin-bottom: 4px; flex-wrap: wrap; flex: none; }
.tile-h h3 { font-size: 1.02rem; font-weight: 500; margin: 0; letter-spacing: -.2px; }
.tile-h p { font-size: .72rem; color: var(--muted); margin: 2px 0 0; }
.tile-link { color: var(--orange); font-size: .82rem; text-decoration: none; cursor: pointer; white-space: nowrap; }
.tile-link:hover { color: var(--orange-dk); text-decoration: underline; }
.tile-body { flex: 1 1 auto; min-height: 0; overflow: hidden; }
.plot-box { position: relative; flex: none; height: 280px; min-height: 0; }
.plot-box > .html-widget { position: absolute !important; inset: 0; width: 100% !important; height: 100% !important; }

/* ── DARK SPOTLIGHT ── */
.spot-wrap { min-width: 0; min-height: 0; }
.spot {
  background: var(--frame); color: #f1efe9; border-radius: 18px; padding: 18px 20px;
  height: 100%; display: flex; flex-direction: column; gap: 11px; min-height: 0; overflow: hidden;
}
.spot .eyebrow { font-size: .8rem; color: #cfcdc5; }
.spot h2 { font-size: 1.3rem; font-weight: 500; line-height: 1.25; margin: 0; letter-spacing: -.3px; }
.spot h2 small { display: block; font-size: .78rem; color: #a9a7a0; font-weight: 400; margin-top: 3px; letter-spacing: 0; }
.spot-stats { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 8px; }
.spot-stats div { background: rgba(255,255,255,.06); border-radius: 11px; padding: 8px 10px; min-width: 0; }
.spot-stats small { display: block; color: #a9a7a0; font-size: .68rem; }
.spot-stats b { font-weight: 500; font-size: 1rem; white-space: nowrap; }
.spot .av-stack { --ring: var(--frame); }
.spot .btn-cta { margin-top: auto; }

/* ── LIST TABLE (lessons style) ── */
.ltable { width: 100%; border-collapse: collapse; font-size: .84rem; }
.ltable th { font-weight: 400; color: var(--muted); font-size: .72rem; padding: 4px 0 6px; text-align: left; }
.ltable td { padding: 7px 0; border-top: 1px solid var(--line); vertical-align: middle; }
.ltable tr:first-child td { border-top: 0; }
.ltable .num { text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; padding-left: 10px; }
.ltable .ev b { font-weight: 500; display: block; white-space: nowrap; }
.ltable .ev small { color: var(--ink-2); font-size: .72rem; white-space: nowrap; }
.ltable .who { display: flex; align-items: center; gap: 8px; white-space: nowrap; }
.sev { display: inline-block; min-width: 36px; text-align: center; border-radius: 8px; padding: 1px 7px; font-size: .76rem; background: var(--panel); }
.sev.hi { background: #ffe1d7; color: #9a2e13; }
.mini-bar { display: inline-block; width: 70px; height: 6px; border-radius: 99px; background: #efede7; margin-right: 8px; vertical-align: middle; overflow: hidden; }
.mini-bar span { display: block; height: 100%; background: #7b4fe0; }
.table-scroll { overflow-x: auto; }

/* ── MODAL ── */
.modal-content { border-radius: 22px !important; border: 1px solid var(--ink) !important; font-family: 'Lexend', sans-serif; }
.modal-header { border-bottom: 1px solid var(--line) !important; }
.modal-footer { border-top: 0 !important; }
.modal-footer .btn { border-radius: 11px; background: var(--ink); color: #fff; border: 0; padding: 7px 18px; }
.modal .ltable td { padding: 9px 0; }
.kv { display: grid; grid-template-columns: repeat(2, 1fr); gap: 10px; }
.kv div { background: var(--panel); border-radius: 12px; padding: 10px 14px; }
.kv small { display: block; color: var(--muted); font-size: .72rem; }

/* plotly hover text uses brand font */
.js-plotly-plot .plotly .hovertext text { font-family: 'Lexend', sans-serif !important; }

/* ── FIT-TO-SCREEN (desktop): every view fills the window, no page scroll ── */
@media (min-width: 1181px) and (min-height: 640px) {
  html, body { height: 100%; overflow: hidden; }
  .shell { height: calc(100vh - 28px); }
  .panel { min-height: 0; overflow: hidden; }
  .views { flex: 1 1 auto; min-height: 0; }
  .views > .tabbable { flex: 1 1 auto; min-height: 0; display: flex; flex-direction: column; }
  .views .tab-content { flex: 1 1 auto; min-height: 0; }
  .views .tab-pane.active { height: 100%; }
  .view { height: 100%; }
  .view.overview { grid-template-rows: auto minmax(0, 1fr); }
  .view.two-rows { grid-template-rows: repeat(2, minmax(0, 1fr)); }
  .view.one-row  { grid-template-rows: minmax(0, 1fr); }
  .plot-box { flex: 1 1 0; height: auto; }
}
@media (min-width: 1181px) and (max-height: 800px) {
  :root { --gap: 12px; }
  .title-row { margin: 10px 0 10px; }
  .title-row h1 { font-size: 1.5rem; }
  .ltable .ev small { display: none; }
  .spot { padding: 14px 16px; gap: 8px; }
  .spot h2 { font-size: 1.12rem; }
  .spot-stack, .spot-tag-opt { display: none; }
}

/* ── RESPONSIVE (stacked, scrolling) ── */
@media (max-width: 1180px) {
  .span-3, .span-4, .span-5, .span-6, .span-7, .span-8 { grid-column: span 12; }
  .spot-wrap { grid-column: span 12; }
}
@media (max-width: 760px) {
  body { padding: 0; }
  .shell { grid-template-columns: 1fr; border-radius: 0; padding: 0; min-height: 100vh; }
  .rail { flex-direction: row; justify-content: center; padding: 10px 8px; gap: 6px; flex-wrap: wrap; }
  .rail-gap { display: none; } .rail-bottom { margin-top: 0; }
  .panel { border-radius: 22px 22px 0 0; padding: 18px 16px 24px; }
  .search-wrap { width: 100%; order: 5; margin-left: 0; }
  .user div { display: none; }
  .brand { margin-right: auto; }
  .chips { margin-left: 0; }
  .kpis { grid-template-columns: 1fr; }
  .title-row h1 { font-size: 1.5rem; }
}
)"

# ── Client-side navigation (rail + chips share one `view` input) ───────────────
JS <- sprintf(r"(
$(function () {
  var titles = %s;
  function setView(v) {
    $('.view-link').removeClass('active').filter('[data-view="' + v + '"]').addClass('active');
    $('#view-title').text(titles[v]);
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('view', v, {priority: 'event'});
    history.replaceState(null, '', '#' + v);
    window.scrollTo({top: 0, behavior: 'smooth'});
  }
  $(document).on('shiny:connected', function () {
    var v = location.hash.slice(1);
    if (titles[v]) setView(v);
  });
  $(document).on('click', '[data-view]', function (e) {
    e.preventDefault();
    setView($(this).attr('data-view'));
  });
  $(document).on('click', '.chip-group .chip', function () {
    var g = $(this).closest('.chip-group');
    g.find('.chip').removeClass('active');
    $(this).addClass('active');
    Shiny.setInputValue(g.data('input'), $(this).data('value'));
  });
});
)", jsonlite::toJSON(as.list(VIEW_TITLES), auto_unbox = TRUE))

# ── UI Components ──────────────────────────────────────────────────────────────
rail_btn <- function(view, ico, label) {
  tags$a(class = paste("rail-btn view-link", if (view == "overview") "active"),
         href = "#", `data-view` = view, title = label, `aria-label` = label,
         icon(ico))
}

chip_group <- function(input_id, choices, selected = choices[[1]]) {
  tags$div(
    class = "chip-group", `data-input` = input_id,
    lapply(names(choices), function(lbl) {
      tags$button(class = paste("chip", if (choices[[lbl]] == selected) "active"),
                  type = "button", `data-value` = choices[[lbl]], lbl)
    })
  )
}

tile <- function(title, ..., subtitle = NULL, action = NULL, class = "") {
  tags$div(
    class = paste("tile", class),
    tags$div(class = "tile-h",
             tags$div(tags$h3(title), if (!is.null(subtitle)) tags$p(subtitle)),
             action),
    ...
  )
}

# Chart area that stretches to fill the rest of its tile
plot_box <- function(id) div(class = "plot-box", plotlyOutput(id, height = "100%"))

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page(
  title = "Disaster Dash",
  theme = bs_theme(
    version   = 5,
    bg        = "#f6f5f1",
    fg        = INK,
    primary   = ORANGE,
    base_font = font_google("Lexend", wght = c(300, 400, 500, 600))
  ),
  tags$head(tags$style(HTML(CSS)), tags$script(HTML(JS))),

  div(
    class = "shell",

    # ── Icon rail ────────────────────────────────────────────────────────────
    tags$nav(
      class = "rail",
      rail_btn("overview",  "table-cells-large", "Overview"),
      rail_btn("impact",    "sack-dollar",       "Impact"),
      rail_btn("response",  "stopwatch",         "Response & recovery"),
      rail_btn("countries", "earth-americas",    "Countries"),
      div(class = "rail-gap"),
      actionLink("help", icon("circle-question"), class = "rail-btn",
                 title = "About this dashboard"),
      actionLink("rail_reset", icon("rotate-left"), class = "rail-btn",
                 title = "Reset filters"),
      tags$a(class = "rail-btn rail-bottom", title = "Source on GitHub",
             href = "https://github.com/ojasvissar", target = "_blank",
             icon("github"))
    ),

    # ── Main panel ───────────────────────────────────────────────────────────
    div(
      class = "panel",

      div(
        class = "topbar",
        div(class = "brand", "Welcome to", tags$b("Disaster", tags$span("Dash"))),
        div(
          class = "search-wrap",
          selectizeInput(
            "countries", label = NULL, choices = COUNTRIES, multiple = TRUE,
            width = "100%",
            options = list(placeholder = "Search countries…",
                           plugins = list("remove_button"))
          ),
          span(class = "search-ico", icon("magnifying-glass"))
        ),
        tags$button(id = "bell", type = "button",
                    class = "btn action-button icon-btn", title = "Critical alerts",
                    icon("bell"), tags$span(class = "dot")),
        div(class = "user",
            span(class = "av", "OI"),
            div(tags$b("Ojasv Issar"), tags$small("@ojasvissar")))
      ),

      div(
        class = "title-row",
        tags$h1(id = "view-title", "Overview"),
        div(
          class = "chips",
          lapply(names(VIEW_TITLES), function(v) {
            tags$button(class = paste("chip view-link", if (v == "overview") "active"),
                        type = "button", `data-view` = v,
                        c(overview = "Overview", impact = "Impact",
                          response = "Response", countries = "Countries")[[v]])
          })
        )
      ),

      div(
        class = "filters",
        div(
          class = "f-type",
          div(class = "f-label", "Disaster type"),
          selectizeInput(
            "disaster_type", label = NULL, choices = DISASTER_TYPES,
            multiple = TRUE, width = "100%",
            options = list(placeholder = "All disaster types",
                           plugins = list("remove_button"))
          )
        ),
        div(
          class = "f-period",
          div(class = "f-label", "Period"),
          sliderInput("date_range", label = NULL, width = "100%",
                      min = DATE_MIN, max = DATE_MAX,
                      value = c(DATE_MIN, DATE_MAX), timeFormat = "%b %Y")
        ),
        uiOutput("match_count", class = "match"),
        actionButton("reset_button", "Reset", icon = icon("rotate-left"))
      ),

      div(
        class = "views",
        navset_hidden(
          id = "views",

          # ── Overview ───────────────────────────────────────────────────────
          nav_panel_hidden(
            "overview",
            div(
              class = "view overview",
              div(class = "kpis span-12", uiOutput("kpi_cards")),
              tile("Disasters over time", class = "span-5",
                   subtitle = "Monthly totals across the current selection",
                   action = chip_group("trend_metric",
                                       c("Events" = "events",
                                         "Casualties" = "casualties",
                                         "Loss" = "loss")),
                   plot_box("p_trend")),
              tile("Most severe events", class = "span-4",
                   action = actionLink("view_all", "View all", class = "tile-link"),
                   div(class = "tile-body", uiOutput("event_list"))),
              div(class = "spot-wrap span-3", uiOutput("spotlight"))
            )
          ),

          # ── Impact ─────────────────────────────────────────────────────────
          nav_panel_hidden(
            "impact",
            div(
              class = "view two-rows",
              tile("Losses vs. aid received", class = "span-8",
                   subtitle = "Quarterly economic loss, split into the share covered by aid",
                   plot_box("p_loss_aid")),
              div(class = "spot-wrap span-4", uiOutput("gap_card")),
              tile("Casualties by type", class = "span-4",
                   subtitle = "Total casualties · highest highlighted",
                   plot_box("p_type")),
              tile("Aid coverage by type", class = "span-4",
                   subtitle = "Aid as % of loss · line marks the overall rate",
                   plot_box("p_coverage")),
              tile("Loss per event by severity", class = "span-4",
                   subtitle = "Median economic loss per event",
                   plot_box("p_sev_loss"))
            )
          ),

          # ── Response & recovery ────────────────────────────────────────────
          nav_panel_hidden(
            "response",
            div(
              class = "view two-rows",
              tile("Severity vs. response time", class = "span-7",
                   subtitle = "Sample of up to 1,500 events with the median response per severity step",
                   plot_box("p_scatter")),
              tile("Response time by disaster type", class = "span-5",
                   subtitle = "Box = middle 50% · whiskers = 5th–95th percentile",
                   plot_box("p_resp_box")),
              tile("Response time trend", class = "span-7",
                   subtitle = "Quarterly median with the interquartile range shaded",
                   plot_box("p_resp_trend")),
              tile("Recovery time by severity", class = "span-5",
                   subtitle = "Average days to recover, by severity band",
                   plot_box("p_recovery"))
            )
          ),

          # ── Countries ──────────────────────────────────────────────────────
          nav_panel_hidden(
            "countries",
            div(
              class = "view one-row",
              tile("Country ranking", class = "span-4",
                   subtitle = "Highest highlighted",
                   action = actionLink("view_table", "Full table", class = "tile-link"),
                   plot_box("p_country_rank")),
              tile("Country × disaster type", class = "span-8",
                   subtitle = "Pick a measure — the ranking and heatmap both follow it",
                   action = chip_group("country_metric",
                                       c("Events" = "events",
                                         "Casualties" = "casualties",
                                         "Loss" = "loss",
                                         "Aid coverage" = "coverage",
                                         "Response" = "response")),
                   plot_box("p_heatmap"))
            )
          )
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Navigation ──────────────────────────────────────────────────────────────
  observeEvent(input$view, nav_select("views", input$view))

  trend_metric   <- reactive(input$trend_metric   %||% "events")
  country_metric <- reactive(input$country_metric %||% "events")

  # ── Reset ───────────────────────────────────────────────────────────────────
  reset_filters <- function() {
    updateSelectizeInput(session, "countries", selected = character(0))
    updateSelectizeInput(session, "disaster_type", selected = character(0))
    updateSliderInput(session, "date_range", value = c(DATE_MIN, DATE_MAX))
  }
  observeEvent(input$reset_button, reset_filters())
  observeEvent(input$rail_reset, reset_filters())

  # ── Reactive Filtered Data (empty selection = everything) ──────────────────
  filtered_df <- reactive({
    d  <- df
    if (length(input$countries))     d <- d[d$country %in% input$countries, ]
    if (length(input$disaster_type)) d <- d[d$disaster_type %in% input$disaster_type, ]
    dr <- input$date_range
    if (!is.null(dr)) d <- d[d$date >= as.Date(dr[1]) & d$date <= as.Date(dr[2]), ]
    d
  })

  # Events sorted most-severe first (ties broken by casualties)
  severe_events <- reactive({
    filtered_df() |> arrange(desc(severity_index), desc(casualties))
  })

  output$match_count <- renderUI({
    span(tags$b(fmt_int(nrow(filtered_df()))), "events")
  })

  # ── Modals ──────────────────────────────────────────────────────────────────
  event_table <- function(d) {
    if (nrow(d) == 0) return(tags$p(class = "text-muted", "No events match the current filters."))
    tags$table(
      class = "ltable",
      tags$thead(tags$tr(tags$th("Event"), tags$th("Country"),
                         tags$th(class = "num", "Severity"),
                         tags$th(class = "num", "Casualties"),
                         tags$th(class = "num", "Loss"))),
      tags$tbody(lapply(seq_len(nrow(d)), function(i) {
        r <- d[i, ]
        tags$tr(
          tags$td(class = "ev",
                  tags$b(sprintf("%02d. %s", i, r$disaster_type)),
                  tags$small(format(r$date, "%d %b %Y"), " · ",
                             round(r$recovery_days), " days to recover")),
          tags$td(div(class = "who", avatar(r$country, "sm"), r$country)),
          tags$td(class = "num",
                  span(class = paste("sev", if (r$severity_index >= 9) "hi"),
                       sprintf("%.1f", r$severity_index))),
          tags$td(class = "num", fmt_int(r$casualties)),
          tags$td(class = "num", fmt_currency(r$economic_loss_usd))
        )
      }))
    )
  }

  observeEvent(input$view_all, {
    showModal(modalDialog(
      title = "Top 25 most severe events", size = "l", easyClose = TRUE,
      footer = modalButton("Close"),
      div(class = "table-scroll", event_table(head(severe_events(), 25)))
    ))
  })

  observeEvent(input$bell, {
    crit <- severe_events() |> filter(severity_index >= 9.5)
    showModal(modalDialog(
      title = sprintf("Critical alerts · %s events at severity ≥ 9.5", fmt_int(nrow(crit))),
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      div(class = "table-scroll", event_table(head(crit, 10)))
    ))
  })

  observeEvent(input$help, {
    showModal(modalDialog(
      title = "About Disaster Dash", easyClose = TRUE, footer = modalButton("Close"),
      tags$p("Explore disaster frequency, economic losses, aid coverage, casualties ",
             "and response times across ", length(COUNTRIES), " countries from ",
             format(DATE_MIN, "%Y"), " to ", format(DATE_MAX, "%Y"), "."),
      tags$p("Use the search box to focus on specific countries, and the filter bar ",
             "to narrow disaster types and the period. Leaving a filter empty includes everything."),
      tags$p(class = "text-muted", style = "font-size:.85rem",
             "Data: global_disaster_response_2018_2024.csv · ", fmt_int(nrow(df)), " events."),
      tags$p(class = "text-muted", style = "font-size:.85rem",
             "Built by Ojasv Issar · UBC MDS 2026 · based on the group project by ",
             "Ojasv Issar, Joel Nicholas Peterson & Claire Saunders.")
    ))
  })

  observeEvent(input$spot_details, {
    r <- head(severe_events(), 1)
    req(nrow(r) == 1)
    showModal(modalDialog(
      title = sprintf("%s in %s", r$disaster_type, r$country), easyClose = TRUE,
      footer = modalButton("Close"),
      tags$p(class = "text-muted", format(r$date, "%A, %d %B %Y")),
      div(class = "kv",
          div(tags$small("Severity index"), sprintf("%.2f / 10", r$severity_index)),
          div(tags$small("Casualties"), fmt_int(r$casualties)),
          div(tags$small("Economic loss"), fmt_currency(r$economic_loss_usd)),
          div(tags$small("Aid received"), fmt_currency(r$aid_amount_usd)),
          div(tags$small("Response time"), sprintf("%.1f hours", r$response_time_hours)),
          div(tags$small("Response efficiency"), sprintf("%.1f / 100", r$response_efficiency_score)),
          div(tags$small("Recovery"), paste(round(r$recovery_days), "days")),
          div(tags$small("Aid coverage"),
              sprintf("%.1f%%", r$aid_amount_usd / r$economic_loss_usd * 100)))
    ))
  })

  # ── Overview: KPI cards ─────────────────────────────────────────────────────
  output$kpi_cards <- renderUI({
    d <- filtered_df()
    n <- nrow(d)
    by_country <- d |> count(country, sort = TRUE)
    loss  <- sum(d$economic_loss_usd)
    aid   <- sum(d$aid_amount_usd)
    cov   <- if (loss > 0) aid / loss else 0
    fast  <- sum(d$response_time_hours <= 12)
    med_r <- if (n > 0) median(d$response_time_hours) else NA
    fastest <- d |> group_by(country) |>
      summarise(r = median(response_time_hours), .groups = "drop") |>
      arrange(r)
    pct <- function(x) sprintf("%.0f%%", 100 * min(max(x, 0), 1))

    progress <- function(label, value_txt, frac) {
      div(div(class = "k-progress-row", span(label), span(value_txt)),
          div(class = "bar", span(style = paste0("width:", pct(frac)))))
    }

    tagList(
      div(class = "kcard yellow",
          div(class = "kcard-top", span(class = "tag dark", "Frequency"),
              span(class = "kcard-ico", icon("bolt"))),
          div(class = "k-value", fmt_int(n), span("disasters recorded")),
          progress("Share of all events", paste(fmt_int(n), "/", fmt_int(nrow(df))),
                   n / nrow(df)),
          div(class = "kcard-foot",
              avatar_stack(by_country$country, nrow(by_country)),
              tags$button(class = "btn-cta", `data-view` = "countries", "Countries"))),

      div(class = "kcard lilac",
          div(class = "kcard-top", span(class = "tag yellow", "Economic"),
              span(class = "kcard-ico", icon("sack-dollar"))),
          div(class = "k-value", fmt_currency(loss), span("in economic losses")),
          progress("Aid coverage", paste(fmt_currency(aid), "received"), cov),
          div(class = "kcard-foot",
              tags$small(sprintf("%.1f%% of losses covered", cov * 100)),
              tags$button(class = "btn-cta", `data-view` = "impact", "Impact"))),

      div(class = "kcard sky",
          div(class = "kcard-top", span(class = "tag lilac", "Response"),
              span(class = "kcard-ico", icon("stopwatch"))),
          div(class = "k-value", if (is.na(med_r)) "–" else sprintf("%.1f h", med_r),
              span("median response")),
          progress("Responded within 12 h", paste(fmt_int(fast), "/", fmt_int(n)),
                   if (n > 0) fast / n else 0),
          div(class = "kcard-foot",
              avatar_stack(fastest$country, nrow(fastest)),
              tags$button(class = "btn-cta", `data-view` = "response", "Response")))
    )
  })

  # ── Overview: Trend ─────────────────────────────────────────────────────────
  output$p_trend <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(empty_plotly())
    metric <- trend_metric()

    ts <- d |>
      mutate(month = floor_date(date, "month")) |>
      group_by(month) |>
      summarise(events     = n(),
                casualties = sum(casualties),
                loss       = sum(economic_loss_usd),
                .groups    = "drop") |>
      mutate(value = .data[[metric]] / METRICS[[metric]]$scale,
             tip   = paste0("<b>", format(month, "%b %Y"), "</b><br>",
                            METRICS[[metric]]$label, ": ",
                            fmt_metric(.data[[metric]], metric)))
    peak <- ts[which.max(ts$value), ]

    plot_ly(ts, x = ~month, y = ~value, type = "scatter", mode = "lines",
            line = list(color = ORANGE, width = 2, shape = "spline", smoothing = 0.5),
            fill = "tozeroy", fillcolor = "rgba(242,84,45,0.10)",
            text = ~tip, hoverinfo = "text") |>
      add_markers(data = peak, x = ~month, y = ~value, inherit = FALSE,
                  marker = list(color = ORANGE, size = 9,
                                line = list(color = "#fff", width = 2)),
                  text = ~tip, hoverinfo = "text") |>
      style_plot(
        x = ax(showgrid = FALSE, showspikes = TRUE, spikemode = "across",
               spikethickness = 1, spikecolor = AXIS, spikedash = "solid",
               spikesnap = "data"),
        y = do.call(ax, c(list(title = METRICS[[metric]]$label, rangemode = "tozero"),
                          METRICS[[metric]]$axis)),
        hovermode = "x",
        annotations = list(list(
          x = peak$month, y = peak$value, yshift = 14,
          text = paste("Peak ·", fmt_metric(peak[[metric]], metric)),
          showarrow = FALSE, font = list(size = 11, color = INK)
        ))
      )
  })

  # ── Overview: Casualties by type ────────────────────────────────────────────
  output$p_type <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(empty_plotly())
    ranked_bars(summarise_impact(d, disaster_type), "disaster_type", "casualties")
  })

  # ── Overview: Event list ────────────────────────────────────────────────────
  output$event_list <- renderUI({
    d <- head(severe_events(), 5)
    if (nrow(d) == 0) return(tags$p(class = "text-muted", "No events match the current filters."))
    tags$table(
      class = "ltable",
      tags$thead(tags$tr(tags$th("Event"), tags$th("Country"),
                         tags$th(class = "num", "Severity"))),
      tags$tbody(lapply(seq_len(nrow(d)), function(i) {
        r <- d[i, ]
        tags$tr(
          tags$td(class = "ev",
                  tags$b(sprintf("%02d. %s", i, r$disaster_type)),
                  tags$small(format(r$date, "%d %b %Y"), " · ",
                             fmt_currency(r$economic_loss_usd))),
          tags$td(div(class = "who", avatar(r$country, "sm"), r$country)),
          tags$td(class = "num",
                  span(class = paste("sev", if (r$severity_index >= 9) "hi"),
                       sprintf("%.1f", r$severity_index)))
        )
      }))
    )
  })

  # ── Overview: Spotlight ─────────────────────────────────────────────────────
  output$spotlight <- renderUI({
    d <- severe_events()
    if (nrow(d) == 0)
      return(div(class = "spot", div(class = "eyebrow", "No events match the current filters.")))
    r <- d[1, ]
    same_type <- d |> filter(disaster_type == r$disaster_type) |> count(country, sort = TRUE)

    div(
      class = "spot",
      div(class = "eyebrow", "Most severe event in your selection"),
      div(span(class = "tag yellow", style = "color:#1f1f1f",
               type_icon(r$disaster_type), " ", r$disaster_type)),
      tags$h2(paste(r$disaster_type, "in", r$country),
              tags$small(format(r$date, "%d %B %Y"))),
      div(class = "spot-stats",
          div(tags$small("Severity"), tags$b(sprintf("%.1f", r$severity_index))),
          div(tags$small("Casualties"), tags$b(fmt_int(r$casualties))),
          div(tags$small("Loss"), tags$b(fmt_currency(r$economic_loss_usd)))),
      div(class = "spot-stack",
          div(class = "eyebrow", style = "margin-bottom:6px",
              sprintf("%s %s events across", fmt_int(sum(same_type$n)),
                      tolower(r$disaster_type))),
          avatar_stack(same_type$country, nrow(same_type))),
      actionButton("spot_details", "More details", class = "btn-cta wide")
    )
  })

  # ── Impact: Loss vs aid ─────────────────────────────────────────────────────
  output$p_loss_aid <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(empty_plotly())

    q <- d |>
      mutate(quarter = floor_date(date, "quarter")) |>
      group_by(quarter) |>
      summarise(loss = sum(economic_loss_usd), aid = sum(aid_amount_usd), .groups = "drop") |>
      mutate(label = paste0(year(quarter), " Q", quarter(quarter)),
             gap   = pmax(loss - aid, 0),
             tip_gap = paste0("<b>", label, "</b><br>Uncovered loss: ", fmt_currency(gap),
                              "<br>Total loss: ", fmt_currency(loss)),
             tip_aid = paste0("<b>", label, "</b><br>Aid received: ", fmt_currency(aid),
                              " (", sprintf("%.1f%%", aid / loss * 100), ")"))

    plot_ly(q, x = ~quarter) |>
      add_bars(y = ~aid / 1e9, name = "Aid received",
               marker = list(color = VIOLET, line = list(color = "#fff", width = 1)),
               text = ~tip_aid, hoverinfo = "text", textposition = "none") |>
      add_bars(y = ~gap / 1e9, name = "Uncovered loss",
               marker = list(color = ORANGE, line = list(color = "#fff", width = 1)),
               text = ~tip_gap, hoverinfo = "text", textposition = "none") |>
      style_plot(
        x = ax(showgrid = FALSE),
        y = ax(title = "Economic loss", tickprefix = "$", ticksuffix = "B", tickformat = ",.1~f"),
        legend = TRUE, barmode = "stack", bargap = 0.3
      )
  })

  output$gap_card <- renderUI({
    d <- filtered_df()
    loss <- sum(d$economic_loss_usd)
    aid  <- sum(d$aid_amount_usd)
    worst <- summarise_impact(d, country) |> arrange(coverage)

    div(
      class = "spot",
      div(class = "eyebrow", "Funding gap in your selection"),
      div(class = "spot-tag-opt",
          span(class = "tag yellow", style = "color:#1f1f1f", icon("scale-unbalanced"), " Aid vs. loss")),
      tags$h2(fmt_currency(loss - aid), tags$small("of losses not covered by aid")),
      div(class = "spot-stats",
          div(tags$small("Total loss"), tags$b(fmt_currency(loss))),
          div(tags$small("Aid"), tags$b(fmt_currency(aid))),
          div(tags$small("Coverage"), tags$b(if (loss > 0) sprintf("%.1f%%", aid / loss * 100) else "–"))),
      if (nrow(worst) > 0)
        div(class = "spot-stack",
            div(class = "eyebrow", style = "margin-bottom:6px", "Lowest aid coverage"),
            avatar_stack(worst$country, nrow(worst))),
      tags$button(class = "btn-cta wide", `data-view` = "countries", "Compare countries")
    )
  })

  # ── Impact: Coverage by type ────────────────────────────────────────────────
  output$p_coverage <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(empty_plotly())
    grp <- summarise_impact(d, disaster_type) |>
      arrange(coverage) |>
      mutate(lab = factor(disaster_type, levels = disaster_type),
             tip = paste0("<b>", disaster_type, "</b><br>Aid coverage: ",
                          sprintf("%.2f%%", coverage), "<br>",
                          fmt_currency(aid), " of ", fmt_currency(loss)))
    overall <- sum(d$aid_amount_usd) / sum(d$economic_loss_usd) * 100

    plot_ly(grp, x = ~coverage, y = ~lab, type = "scatter", mode = "markers",
            marker = list(color = VIOLET, size = 13, line = list(color = "#fff", width = 2)),
            text = ~tip, hoverinfo = "text") |>
      style_plot(
        x = ax(title = "Aid as % of loss", ticksuffix = "%"),
        y = ax(tickfont = list(size = 11, color = INK_2)),
        top = 26,
        shapes = list(list(type = "line", xref = "x", yref = "paper",
                           x0 = overall, x1 = overall, y0 = 0, y1 = 1,
                           line = list(color = INK_2, width = 1))),
        annotations = list(list(x = overall, y = 1, xref = "x", yref = "paper",
                                yanchor = "bottom", showarrow = FALSE,
                                text = sprintf("Overall %.2f%%", overall),
                                font = list(size = 10, color = INK_2)))
      )
  })

  # ── Impact: Loss by severity band ───────────────────────────────────────────
  output$p_sev_loss <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(empty_plotly())
    grp <- d |>
      mutate(band = severity_band(severity_index)) |>
      group_by(band, .drop = FALSE) |>
      summarise(loss = median(economic_loss_usd), n = n(), .groups = "drop") |>
      mutate(tip = paste0("<b>Severity ", band, "</b><br>Median loss: ",
                          fmt_currency(loss), "<br>", fmt_int(n), " events"))

    plot_ly(grp, x = ~band, y = ~loss / 1e6, type = "bar",
            marker = list(color = ORANGE_RAMP[as.integer(grp$band)]),
            text = ~tip, hoverinfo = "text", textposition = "none") |>
      style_plot(
        x = ax(title = "Severity index", showgrid = FALSE, type = "category"),
        y = ax(title = "Median loss per event", tickprefix = "$", ticksuffix = "M"),
        bargap = 0.35
      )
  })

  # ── Response: Scatter ───────────────────────────────────────────────────────
  output$p_scatter <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(empty_plotly())
    samp <- if (nrow(d) > 1500) d[sample.int(nrow(d), 1500), ] else d
    samp <- samp |>
      mutate(tip = paste0("<b>", disaster_type, " · ", country, "</b><br>",
                          format(date, "%d %b %Y"), "<br>Severity: ",
                          sprintf("%.1f", severity_index), "<br>Response: ",
                          sprintf("%.1f h", response_time_hours), "<br>Loss: ",
                          fmt_currency(economic_loss_usd)))
    med <- d |>
      mutate(step = round(severity_index * 2) / 2) |>
      group_by(step) |>
      summarise(r = median(response_time_hours), n = n(), .groups = "drop") |>
      filter(n >= 5)

    plot_ly() |>
      add_markers(data = samp, x = ~severity_index, y = ~response_time_hours,
                  name = "Events (sample)",
                  marker = list(color = "rgba(42,143,208,0.28)", size = 7,
                                line = list(color = "#fff", width = 0.5)),
                  text = ~tip, hoverinfo = "text") |>
      add_lines(data = med, x = ~step, y = ~r, name = "Median response",
                line = list(color = INK, width = 2.5, shape = "spline"),
                text = ~paste0("<b>Severity ", step, "</b><br>Median response: ",
                               sprintf("%.1f h", r)),
                hoverinfo = "text") |>
      style_plot(
        x = ax(title = "Severity index"),
        y = ax(title = "Response time", ticksuffix = "h", rangemode = "tozero"),
        legend = TRUE
      )
  })

  # ── Response: Box plot by type (pre-computed stats) ─────────────────────────
  output$p_resp_box <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(empty_plotly())
    st <- d |>
      group_by(disaster_type) |>
      summarise(lo = quantile(response_time_hours, .05),
                q1 = quantile(response_time_hours, .25),
                md = median(response_time_hours),
                q3 = quantile(response_time_hours, .75),
                hi = quantile(response_time_hours, .95),
                .groups = "drop") |>
      arrange(md)

    plot_ly(type = "box", orientation = "h",
            y = st$disaster_type, q1 = st$q1, median = st$md, q3 = st$q3,
            lowerfence = st$lo, upperfence = st$hi,
            fillcolor = YELLOW, line = list(color = INK, width = 1.3),
            hoverinfo = "x+y") |>
      style_plot(
        x = ax(title = "Response time", ticksuffix = "h"),
        y = ax(showgrid = FALSE, categoryorder = "array",
               categoryarray = st$disaster_type,
               tickfont = list(size = 11, color = INK_2))
      )
  })

  # ── Response: Trend with IQR band ───────────────────────────────────────────
  output$p_resp_trend <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(empty_plotly())
    q <- d |>
      mutate(quarter = floor_date(date, "quarter")) |>
      group_by(quarter) |>
      summarise(q1 = quantile(response_time_hours, .25),
                md = median(response_time_hours),
                q3 = quantile(response_time_hours, .75),
                .groups = "drop") |>
      mutate(tip = paste0("<b>", year(quarter), " Q", quarter(quarter), "</b><br>",
                          "Median: ", sprintf("%.1f h", md), "<br>",
                          "Middle 50%: ", sprintf("%.1f–%.1f h", q1, q3)))

    plot_ly(q, x = ~quarter) |>
      add_lines(y = ~q3, line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") |>
      add_lines(y = ~q1, line = list(width = 0), fill = "tonexty",
                fillcolor = "rgba(42,143,208,0.16)", showlegend = FALSE, hoverinfo = "skip") |>
      add_lines(y = ~md, line = list(color = BLUE, width = 2),
                text = ~tip, hoverinfo = "text", showlegend = FALSE) |>
      style_plot(
        x = ax(showgrid = FALSE),
        y = ax(title = "Response time", ticksuffix = "h", rangemode = "tozero")
      )
  })

  # ── Response: Recovery by severity ──────────────────────────────────────────
  output$p_recovery <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(empty_plotly())
    grp <- d |>
      mutate(band = severity_band(severity_index)) |>
      group_by(band, .drop = FALSE) |>
      summarise(days = mean(recovery_days), n = n(), .groups = "drop") |>
      mutate(tip = paste0("<b>Severity ", band, "</b><br>Avg recovery: ",
                          sprintf("%.0f days", days), "<br>", fmt_int(n), " events"))

    plot_ly(grp, x = ~band, y = ~days, type = "bar",
            marker = list(color = BLUE_RAMP[as.integer(grp$band)]),
            text = ~tip, hoverinfo = "text", textposition = "none") |>
      style_plot(
        x = ax(title = "Severity index", showgrid = FALSE, type = "category"),
        y = ax(title = "Days to recover"),
        bargap = 0.35
      )
  })

  # ── Countries: Ranking ──────────────────────────────────────────────────────
  output$p_country_rank <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(empty_plotly())
    ranked_bars(summarise_impact(d, country), "country", country_metric())
  })

  # ── Countries: Heatmap ──────────────────────────────────────────────────────
  output$p_heatmap <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(empty_plotly())
    metric <- country_metric()
    m <- METRICS[[metric]]

    grid <- summarise_impact(d, country, disaster_type)
    ctry <- summarise_impact(d, country) |> arrange(.data[[metric]]) |> pull(country)
    types <- sort(unique(grid$disaster_type))

    z <- matrix(NA_real_, nrow = length(ctry), ncol = length(types),
                dimnames = list(ctry, types))
    tip <- matrix("", nrow = length(ctry), ncol = length(types))
    z[cbind(match(grid$country, ctry), match(grid$disaster_type, types))] <-
      grid[[metric]] / m$scale
    tip[cbind(match(grid$country, ctry), match(grid$disaster_type, types))] <-
      paste0("<b>", grid$country, " · ", grid$disaster_type, "</b><br>",
             m$label, ": ", fmt_metric(grid[[metric]], metric),
             "<br>", fmt_int(grid$events), " events")

    plot_ly(x = types, y = ctry, z = z, type = "heatmap",
            colorscale = HEAT_SCALE, xgap = 2, ygap = 2,
            text = tip, hoverinfo = "text",
            colorbar = c(list(thickness = 8, outlinewidth = 0, len = 0.6,
                              tickfont = list(size = 10, color = MUTED),
                              title = list(text = m$label,
                                           font = list(size = 10, color = MUTED))),
                         m$axis)) |>
      style_plot(
        x = ax(showgrid = FALSE, tickangle = -35, tickfont = list(size = 10, color = INK_2)),
        y = ax(showgrid = FALSE, tickfont = list(size = 11, color = INK_2))
      )
  })

  # ── Countries: Table ────────────────────────────────────────────────────────
  country_table <- function(d) {
    if (nrow(d) == 0) return(tags$p(class = "text-muted", "No events match the current filters."))
    t <- summarise_impact(d, country) |> arrange(desc(events))
    top_type <- d |> count(country, disaster_type) |>
      group_by(country) |> slice_max(n, n = 1, with_ties = FALSE) |> ungroup()
    t$top_type <- top_type$disaster_type[match(t$country, top_type$country)]
    max_cov <- max(t$coverage, na.rm = TRUE)

    tags$table(
      class = "ltable",
      tags$thead(tags$tr(
        tags$th("Country"), tags$th("Most frequent"),
        tags$th(class = "num", "Events"), tags$th(class = "num", "Casualties"),
        tags$th(class = "num", "Economic loss"), tags$th(class = "num", "Aid coverage"),
        tags$th(class = "num", "Median response")
      )),
      tags$tbody(lapply(seq_len(nrow(t)), function(i) {
        r <- t[i, ]
        tags$tr(
          tags$td(div(class = "who", avatar(r$country, "sm"), r$country)),
          tags$td(type_icon(r$top_type), " ", r$top_type),
          tags$td(class = "num", fmt_int(r$events)),
          tags$td(class = "num", fmt_int(r$casualties)),
          tags$td(class = "num", fmt_currency(r$loss)),
          tags$td(class = "num",
                  span(class = "mini-bar",
                       span(style = sprintf("width:%.0f%%", r$coverage / max_cov * 100))),
                  sprintf("%.2f%%", r$coverage)),
          tags$td(class = "num", sprintf("%.1f h", r$response))
        )
      }))
    )
  }

  observeEvent(input$view_table, {
    showModal(modalDialog(
      title = "All countries in your selection", size = "xl", easyClose = TRUE,
      footer = modalButton("Close"),
      div(class = "table-scroll", country_table(filtered_df()))
    ))
  })
}

# ── Launch ─────────────────────────────────────────────────────────────────────
shinyApp(ui, server)

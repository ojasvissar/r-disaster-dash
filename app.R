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
# Reads the parquet file if available; falls back to synthetic data for demo
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
    
    loss <- runif(n, 1e6, 5e9)
    data.frame(
      country          = sample(countries, n, replace = TRUE),
      disaster_type    = sample(disaster_types, n, replace = TRUE),
      date             = sample(seq(as.Date("2018-01-01"),
                                    as.Date("2024-12-31"), by = "day"),
                                n, replace = TRUE),
      economic_loss_usd  = loss,
      aid_amount_usd     = loss * runif(n, 0.1, 0.7),
      casualties         = round(runif(n, 0, 50000)),
      severity_index     = runif(n, 1, 10),
      response_time_hours = runif(n, 1, 200),
      stringsAsFactors = FALSE
    )
  }
}


df <- load_data()

# ── Lookup Tables ──────────────────────────────────────────────────────────────
COUNTRIES      <- sort(unique(df$country))
DISASTER_TYPES <- sort(unique(df$disaster_type))

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

empty_plotly <- function(msg = "No data to display",
                         hint = "Adjust your filters") {
  plot_ly() |>
    layout(
      annotations = list(list(
        text      = paste0("<b>", msg, "</b><br>",
                           "<span style='font-size:11px'>", hint, "</span>"),
        xref = "paper", yref = "paper", x = 0.5, y = 0.5,
        showarrow = FALSE,
        font = list(size = 13, color = "#94a3b8", family = "Inter")
      )),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      margin = list(l = 0, r = 0, t = 0, b = 0)
    )
}

# ── Design Tokens ──────────────────────────────────────────────────────────────
NAVY    <- "#0b1f3a"
BLUE    <- "#1a56db"
RED     <- "#dc2626"
BG      <- "#eef2f7"
CARD    <- "#ffffff"
BORDER  <- "#dde4ee"
T_PRI   <- "#0f172a"
T_SEC   <- "#64748b"
T_MUTED <- "#94a3b8"

# ── Custom CSS ─────────────────────────────────────────────────────────────────
CSS <- sprintf("
@import url('https://fonts.googleapis.com/css2?family=Syne:wght@700;800&family=Inter:wght@400;500;600;700&display=swap');

*, *::before, *::after { box-sizing: border-box; }
html, body { font-family: 'Inter', system-ui, sans-serif !important; background: %s !important; }

/* ── SIDEBAR LABELS ── */
.sb-label {
  font-size: 0.6rem; font-weight: 700; text-transform: uppercase;
  letter-spacing: 1.1px; color: %s; margin: 14px 0 5px 0;
  display: flex; align-items: center; gap: 6px;
}
.sb-label::after { content: ''; flex: 1; height: 1px; background: %s; }

/* ── RESET BUTTON ── */
#reset_button {
  margin-top: 16px !important; width: 100%% !important;
  background: #fff5f5 !important; color: %s !important;
  border: 1px solid #fee2e2 !important; border-radius: 8px !important;
  font-size: 0.76rem !important; font-weight: 700 !important; padding: 8px !important;
  transition: all 0.2s;
}
#reset_button:hover { background: %s !important; color: #fff !important; }

/* ── KPI VALUE BOXES ── */
.bslib-value-box .value-box-value { font-family: 'Syne', sans-serif !important; }
.bslib-value-box { border-radius: 14px !important; }

/* ── CARD HEADERS ── */
.card-header {
  font-size: 0.65rem !important; font-weight: 700 !important;
  text-transform: uppercase !important; letter-spacing: 1px !important;
  color: %s !important; background: #fff !important;
  border-bottom: 1px solid %s !important;
}

/* ── NAV TABS ── */
.nav-underline .nav-link { font-size: 0.8rem !important; font-weight: 600 !important; }
.nav-underline .nav-link.active { color: %s !important; border-bottom-color: %s !important; }

/* ── FILTER STRIP ── */
.filter-strip {
  background: %s; border-bottom: 1px solid %s;
  padding: 7px 16px; display: flex; align-items: center;
  flex-wrap: wrap; gap: 6px; font-size: 0.7rem;
  border-radius: 0 !important;
}
.fp-label { color: %s; font-weight: 700; text-transform: uppercase;
            letter-spacing: 0.7px; font-size: 0.62rem; }
.fp-pill  { background: rgba(11,31,58,0.07); color: %s;
            border: 1px solid rgba(11,31,58,0.15);
            border-radius: 20px; padding: 3px 10px;
            font-size: 0.68rem; font-weight: 600; }
.fp-sep   { color: %s; }

/* ── FOOTER ── */
.dash-footer {
  text-align: center; color: %s; font-size: 0.7rem;
  padding: 10px; border-top: 1px solid %s; background: %s;
}
.dash-footer a { color: %s; text-decoration: none; font-weight: 600; }
",
NAVY, T_MUTED, BORDER,          # sb-label
RED, RED,                        # reset button
T_SEC, BORDER,                   # card header
NAVY, BLUE,                      # nav tabs
CARD, BORDER,                    # filter strip bg
T_MUTED, NAVY, BORDER,           # fp- classes
T_MUTED, BORDER, CARD, BLUE      # footer
)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = tags$span(
    style = "font-family:'Syne',sans-serif; font-weight:800; letter-spacing:-0.5px;",
    "🌍 Disaster Dash"
  ),
  theme = bs_theme(
    version    = 5,
    bg         = BG,
    fg         = T_PRI,
    primary    = BLUE,
    "navbar-bg" = NAVY,
    base_font  = font_google("Inter"),
    heading_font = font_google("Syne")
  ),
  tags$head(tags$style(CSS)),
  
  # ── Sidebar ────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 242,
    bg    = CARD,
    open  = "desktop",
    
    tags$div(class = "sb-label", "Country"),
    selectizeInput(
      "countries", label = NULL,
      choices  = COUNTRIES,
      selected = c("Brazil", "Bangladesh", "South Africa"),
      multiple = TRUE,
      options  = list(placeholder = "Select countries…",
                      plugins = list("remove_button"))
    ),
    
    tags$div(class = "sb-label", "Disaster Type"),
    selectizeInput(
      "disaster_type", label = NULL,
      choices  = DISASTER_TYPES,
      selected = DISASTER_TYPES,
      multiple = TRUE,
      options  = list(placeholder = "Select types…",
                      plugins = list("remove_button"))
    ),
    
    tags$div(class = "sb-label", "Date Range"),
    dateRangeInput(
      "date_range", label = NULL,
      start = "2018-01-01", end = "2024-12-31",
      min   = "2018-01-01", max = "2024-12-31"
    ),
    
    actionButton("reset_button", "↺  Reset All Filters")
  ),
  
  # ── Main ───────────────────────────────────────────────────────────────────
  div(
    # Active filter strip
    uiOutput("filter_strip"),
    
    navset_underline(
      id = "main_tabs",
      
      # ── Trends Tab ─────────────────────────────────────────────────────────
      nav_panel(
        tags$span("📈 ", tags$span("Trends", style = "color:#f59e0b; font-weight:700;")),
        div(
          style = "display:flex; flex-direction:column; gap:14px; padding:14px;",
          layout_columns(
            col_widths = c(7, 5),
            card(
              card_header("📅  Disasters Over Time"),
              plotlyOutput("trend_line", height = "300px"),
              full_screen = TRUE
            ),
            card(
              card_header("🥧  Casualty Share by Disaster Type"),
              plotlyOutput("pie_casualty", height = "300px"),
              full_screen = TRUE
            )
          ),
          card(
            card_header("🔥  Severity vs. Response Time"),
            plotlyOutput("scatter_sev", height = "300px"),
            full_screen = TRUE
          )
        )
      )
    ),
    
    # Footer
    div(
      class = "dash-footer",
      "Disaster Dash (R)  ·  ",
      tags$a("Ojasv Issar", href = "https://github.com/ojasvissar", target = "_blank"),
      "  ·  UBC MDS 2026  ·  Data 2018–2024"
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Sidebar Reset ───────────────────────────────────────────────────────────
  observeEvent(input$reset_button, {
    updateSelectizeInput(session, "countries",
                         selected = c("Brazil", "Bangladesh", "South Africa"))
    updateSelectizeInput(session, "disaster_type", selected = DISASTER_TYPES)
    updateDateRangeInput(session, "date_range",
                         start = "2018-01-01", end = "2024-12-31")
  })
  
  # ── Reactive Filtered Data ──────────────────────────────────────────────────
  filtered_df <- reactive({
    req(input$countries, input$disaster_type, input$date_range)
    df |>
      filter(
        country      %in% input$countries,
        disaster_type %in% input$disaster_type,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
  })
  
  # ── Active Filter Strip ─────────────────────────────────────────────────────
  output$filter_strip <- renderUI({
    countries <- input$countries %||% character(0)
    disasters <- input$disaster_type %||% character(0)
    dr        <- input$date_range
    
    fmt_sel <- function(sel, full, all_label) {
      if (length(sel) == 0)         return("None")
      if (length(sel) == length(full)) return(all_label)
      if (length(sel) <= 3)         return(paste(sel, collapse = ", "))
      paste0(paste(sel[1:2], collapse = ", "), " +", length(sel) - 2, " more")
    }
    
    pill  <- function(txt) tags$span(class = "fp-pill", txt)
    lbl   <- function(txt) tags$span(class = "fp-label", txt)
    sep   <- tags$span(class = "fp-sep", "·")
    
    div(
      class = "filter-strip",
      lbl("Countries:"),  pill(fmt_sel(countries, COUNTRIES,      "All Countries")), sep,
      lbl("Disasters:"),  pill(fmt_sel(disasters, DISASTER_TYPES, "All Types")),    sep,
      lbl("Dates:"),      pill(paste0(dr[1], " → ", dr[2]))
    )
  })
  
  # ── Trends Tab: Line Chart ───────────────────────────────────────────────────
  output$trend_line <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0)
      return(empty_plotly("No data", "Adjust your filters"))
    
    ts <- data |>
      mutate(year_month = floor_date(date, "month")) |>
      group_by(year_month) |>
      summarise(
        events     = n(),
        total_loss = sum(economic_loss_usd, na.rm = TRUE),
        .groups    = "drop"
      )
    
    plot_ly(ts) |>
      add_trace(
        x    = ~year_month, y = ~events,
        type = "scatter", mode = "lines+markers",
        name = "Event Count",
        line   = list(color = BLUE, width = 2),
        marker = list(color = BLUE, size = 4),
        hovertemplate = "<b>%{x|%b %Y}</b><br>Events: %{y}<extra></extra>"
      ) |>
      layout(
        xaxis = list(title = "", tickfont = list(size = 9, family = "Inter"),
                     showgrid = FALSE),
        yaxis = list(title = "Number of Events",
                     titlefont = list(size = 10, family = "Inter"),
                     tickfont  = list(size = 9,  family = "Inter"),
                     gridcolor = BORDER),
        margin        = list(l = 50, r = 20, t = 10, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        showlegend    = FALSE,
        hoverlabel    = list(bgcolor = "#fff", font_color = T_PRI,
                             font_size = 11, font_family = "Inter")
      )
  })
  
  # ── Trends Tab: Pie Chart ────────────────────────────────────────────────────
  output$pie_casualty <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0)
      return(empty_plotly("No data", "Adjust your filters"))
    
    grp <- data |>
      group_by(disaster_type) |>
      summarise(casualties = sum(casualties, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(casualties))
    
    plot_ly(
      grp,
      labels  = ~disaster_type,
      values  = ~casualties,
      type    = "pie",
      textinfo = "label+percent",
      textfont = list(size = 10, family = "Inter"),
      marker  = list(
        colors = colorRampPalette(c(NAVY, BLUE, "#60a5fa", "#93c5fd"))(nrow(grp)),
        line   = list(color = "#fff", width = 1)
      ),
      hovertemplate = "<b>%{label}</b><br>Casualties: %{value:,}<extra></extra>"
    ) |>
      layout(
        showlegend    = FALSE,
        margin        = list(l = 20, r = 20, t = 10, b = 10),
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # ── Trends Tab: Scatter Plot ─────────────────────────────────────────────────
  output$scatter_sev <- renderPlotly({
    data <- filtered_df()
    if (nrow(data) == 0)
      return(empty_plotly("No data", "Adjust your filters"))
    
    samp <- if (nrow(data) > 800) slice_sample(data, n = 800) else data
    
    samp <- samp |>
      mutate(loss_fmt = fmt_currency(economic_loss_usd))
    
    plot_ly(
      samp,
      x    = ~response_time_hours,
      y    = ~severity_index,
      type = "scatter", mode = "markers",
      color = ~disaster_type,
      marker = list(size = 6, opacity = 0.6,
                    line = list(width = 0)),
      text  = ~paste0("<b>", country, "</b><br>",
                      "Type: ", disaster_type, "<br>",
                      "Severity: ", round(severity_index, 1), "<br>",
                      "Response: ", round(response_time_hours, 0), "h<br>",
                      "Loss: ", loss_fmt),
      hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(
          title    = "Response Time (hours)",
          titlefont = list(size = 10, family = "Inter"),
          tickfont  = list(size = 9,  family = "Inter"),
          gridcolor = BORDER, showgrid = TRUE, zeroline = FALSE
        ),
        yaxis = list(
          title    = "Severity Index",
          titlefont = list(size = 10, family = "Inter"),
          tickfont  = list(size = 9,  family = "Inter"),
          gridcolor = BORDER, showgrid = TRUE, zeroline = FALSE
        ),
        legend = list(
          font  = list(size = 9, family = "Inter"),
          title = list(text = "Disaster Type",
                       font = list(size = 9, family = "Inter"))
        ),
        margin        = list(l = 50, r = 140, t = 10, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        hoverlabel    = list(bgcolor = "#fff", font_color = T_PRI,
                             font_size = 11, font_family = "Inter",
                             bordercolor = NAVY)
      )
  })
}

# ── Launch ─────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
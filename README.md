# 🌍 Disaster Dash (R)

**Global Disaster Impact & Humanitarian Aid · 2018–2024**

An interactive R Shiny dashboard exploring disaster frequency, economic losses, casualty patterns, and response time across 20 countries from 2018 to 2024.

> **Live App:** https://019ce3ab-c7c7-4487-8434-582db5539b78.share.connect.posit.cloud/
> **Original Group Project:** [DSCI-532_2026_18_disasterdash](https://github.com/UBC-MDS/DSCI-532_2026_18_disasterdash)

---

## Features

### Inputs (Sidebar)
| Input | Type | Description |
|---|---|---|
| Country | Multi-select dropdown | Filter by one or more of 20 countries |
| Disaster Type | Multi-select dropdown | Filter by disaster category (Flood, Earthquake, etc.) |
| Date Range | Date range picker | Restrict events to a start and end date |
| Reset Button | Action button | Restores all filters to their defaults |

### Reactive Calc
- `filtered_df()` — a reactive dataframe that applies all sidebar filters at once; every chart reads from it

### Outputs
| Output | Type | Description |
|---|---|---|
| Disasters Over Time | Line chart | Monthly event count across the filtered selection |
| Casualty Share by Disaster Type | Pie chart | Proportion of total casualties per disaster category |
| Severity vs. Response Time | Scatter plot | Each event plotted by severity index and response time, coloured by disaster type |

---

## Requirements

- R ≥ 4.3.0
- Packages:

```r
install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "plotly",
  "lubridate"
))
```

---

## Data

Place the CSV in a `data/` folder next to `app.R`:

```
disaster-dash-r/
├── app.R
├── README.md
└── data/
    └── global_disaster_response_2018_2024.csv
```

If the file is **not present**, the app automatically falls back to **synthetic demo data** (2,000 rows) that mirrors the real column structure, so the app always runs even without the data file.

**Expected columns:**

| Column | Type | Description |
|---|---|---|
| `country` | character | Country name |
| `disaster_type` | character | Category of disaster |
| `date` | date | Event date |
| `economic_loss_usd` | numeric | Economic loss in USD |
| `aid_amount_usd` | numeric | Aid disbursed in USD |
| `casualties` | numeric | Number of casualties |
| `severity_index` | numeric | Severity score (1–10) |
| `response_time_hours` | numeric | Hours until response |

---

## Running Locally

**Step 1 — Clone the repo and navigate into it (in your terminal):**

```bash
git clone https://github.com/<your-username>/disaster-dash-r.git
cd disaster-dash-r
```

**Step 2 — Add the data file to `data/`** (or skip — synthetic data loads automatically)

**Step 3 — Open R (in your terminal):**

```bash
R
```

You will see the `>` prompt, meaning you are now in the R console.

**Step 4 — Install dependencies (one-time only):**

```r
install.packages(c("shiny", "bslib", "dplyr", "plotly", "lubridate"))
```

**Step 5 — Launch the app:**

```r
shiny::runApp("app.R")
```

> **RStudio users:** open `app.R` in RStudio and click the **Run App** button — no terminal needed.


## Author

**Ojasv Issar** · UBC MDS 2026  
Individual assignment based on the group project by Ojasv Issar, Joel Nicholas Peterson & Claire Saunders.

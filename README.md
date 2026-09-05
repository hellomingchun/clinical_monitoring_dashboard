# Clinical Trial Safety & Operations Monitoring Dashboard

[![R](https://img.shields.io/badge/R-%3E%3D%204.0.0-blue.svg)](https://www.r-project.org/)
[![Golem](https://img.shields.io/badge/built%20with-golem-orange.svg)](https://github.com/ThinkR-open/golem)
[![Shiny](https://img.shields.io/badge/Shiny-1.8+-blue.svg)](https://shiny.posit.co/)
[![Tests](https://img.shields.io/badge/Tests-114%20passing-brightgreen.svg)](tests/testthat)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

An interactive, production-ready `{golem}` Shiny web application designed for biostatisticians, clinical data scientists, medical monitors, and clinical trial operations teams. It facilitates real-time tracking of subject journeys, milestone disposition, adverse events (AEs), laboratory hepatotoxicity signals (Hy's Law / DILI), and risk-based site monitoring.

---

## Key Features

### 1. Longitudinal Subject Journey & Milestones
- **CONSORT Disposition Flow Funnel**: Dynamic visualization tracking trial progress across key milestones:
  $$\text{Informed Consent (ICF)} \longrightarrow \text{Screening} \longrightarrow \text{Randomization} \longrightarrow \text{Treatment Initiation} \longrightarrow \text{On Treatment}$$
- **Interactive Swimmer Plots**: Visual timeline of subject exposure with milestone overlays (First Dose, Confirmed Tumor Responses [PR/CR], Grade 3+ Adverse Events, Ongoing Treatment).
- **Rescreening Tracking**: Operational visibility into initial screen failures versus successful rescreens.

### 2. Clinical Safety & CDISC Alignment
- **CDISC ADaM Datasets**: Synthetic data engine generating standards-aligned `ADSL` (Subject Level), `ADAE` (Adverse Events), `ADLB` (Laboratory Analysis), and `ADPD` (Protocol Deviations).
- **Adverse Event Profiling**: Categorized by MedDRA System Organ Class (SOC) and CTCAE Toxicity Grade.
- **Hy's Law Quadrant Analysis**: Multi-parameter scatter plots assessing drug-induced liver injury (DILI) signals ($\text{Peak ALT} \ge 3\times\text{ULN}$ and $\text{Peak Bilirubin} \ge 2\times\text{ULN}$).

### 3. Subject Clinical Profile & Longitudinal Trends
- **Single-Subject Drilldown**: Interactive selector with patient demographic card, individual AE timeline from first dose, and longitudinal multi-analyte lab trend charts.

### 4. Risk-Based Site Monitoring & Traceability
- **Clinical Site Performance**: Target vs. actual enrollment comparison across investigative sites.
- **Key Risk Indicators (KRI)**: Protocol deviation frequencies and open query density tracking per site.
- **Traceable Patient Listings**: Searchable, paginated, and filterable DataTables with one-click filtered CSV exports.

---

## Dashboard Architecture

The dashboard is built following the robust **`{golem}`** framework for production R applications:

```text
clinical_monitoring_dashboard/
├── DESCRIPTION                 # Package metadata and dependencies
├── NAMESPACE                   # Exported and imported symbols
├── app.R                       # Standalone launcher for Shiny Server / Posit Connect
├── R/
│   ├── app_config.R            # Configuration helper functions
│   ├── app_ui.R                # Core dashboard layout and navigation bar
│   ├── app_server.R            # Reactive server logic and interactive outputs
│   ├── fct_synthetic_data.R    # Synthetic CDISC & trial milestone data generator
│   ├── mod_overview.R          # Overview & demographic module
│   ├── mod_ae_safety.R         # Adverse event safety reporting module
│   ├── mod_labs.R              # Laboratory signals & Hy's law module
│   ├── mod_patient_profile.R   # Single-patient clinical journey profile module
│   ├── mod_site_performance.R  # Site performance & risk matrix module
│   └── run_app.R               # Main entry point to launch dashboard
├── tests/
│   ├── testthat.R              # Automated test runner
│   └── testthat/               # 114 unit and integration test assertions
└── dev/
    └── run_dev.R               # Development reload and launch script
```

---

## Getting Started

### Prerequisites
- **R** ($\ge 4.0.0$)
- Standard packages: `shiny`, `bslib`, `bsicons`, `dplyr`, `DT`, `ggplot2`, `plotly`, `scales`, `lubridate`, `golem`, `pkgload`

### Installation

Install directly from GitHub using `remotes` or `devtools`:

```r
# install.packages("remotes")
remotes::install_github("hellomingchun/clinical_monitoring_dashboard")
```

### Launching the Application

#### Option A: Run directly in R / RStudio
```r
library(clinicalmonitoringdashboard)
run_app()
```

#### Option B: Development Mode
```r
# Automatically reloads package dependencies and runs app
source("dev/run_dev.R")
```

#### Option C: Server / Docker / Posit Connect
```r
# Uses standard app.R entrypoint
shiny::runApp("app.R")
```

---

## Running the Automated Test Suite

The package includes a comprehensive test suite using `{testthat}` (Edition 3) and `shiny::testServer`:

```r
# Run all tests
testthat::test_dir("tests/testthat")
```

```text
══ Results ═════════════════════════════════════════════════════════════════════
[ FAIL 0 | WARN 32 | SKIP 0 | PASS 114 ]
```

---

## Author

- **Ming-Chun Chen**
  - Email: [hellomingchun@gmail.com](mailto:hellomingchun@gmail.com)
  - GitHub: [@hellomingchun](https://github.com/hellomingchun)

---

## License

This project is licensed under the [MIT License](LICENSE).

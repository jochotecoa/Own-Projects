# Project Guidelines & Technical Overview

## 📂 Repository Structure Detailed

- **`scripts/covid19/`**: Contains core analysis scripts. 
  - `covid19_yt_style.R`: High-end bar chart race animation.
  - `covid19_vaccination_impact.R`: Correlation analysis of vaccine efficacy.
  - `covid19_excess_mortality.R`: Comparative analysis of excess vs. confirmed deaths.
- **`scripts/apps/`**: Interactive web applications.
  - `covid19-dashboard/`: A Shiny/Plotly application for multi-metric exploration.
- **`scripts/machine-learning/`**: Predictive modeling experiments.
  - `covid19_lstm_forecasting.R`: LSTM recurrent neural networks for case prediction.
- **`results/`**: All output files (images, videos, reports) are consolidated here for visibility.

## 🛠 Reproducibility & Environment
This project utilizes `renv` for R package management. This ensures that every developer or reviewer uses the exact same versions of libraries (ggplot2, Keras, etc.) as the original author.

### To replicate the environment:
1. Open the project in RStudio.
2. Run `renv::restore()`.

## 🧬 Bioinformatics Context
While some analyses focus on public health, the methodologies employed—Time-Series Forecasting, Data Wrangling, and Interactive Visualization—are foundational to bioinformatics. The project demonstrates:
- **Big Data Handling:** Processing multi-million row datasets from OWID.
- **Statistical Rigor:** Implementing smoothing (rolling means), interpolation (na.approx), and correlation analysis.
- **Communication:** Translating complex data into intuitive visual narratives.

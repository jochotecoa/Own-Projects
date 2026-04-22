# Data Science & Bioinformatics Portfolio
## Juan Ochoteco Asensio

This repository serves as a professional portfolio showcasing projects in data analysis, epidemiological modeling, and machine learning. As a bioinformatician, I focus on transforming raw data into actionable insights through rigorous statistical methods, reproducible workflows, and interactive visualizations.

---

## 🚀 Key Projects

### 1. COVID-19 Epidemiological Analysis
A comprehensive suite of tools for analyzing global pandemic trends using data from **Our World in Data (OWID)**.
- **Dynamic Bar Chart Races:** High-quality animations (YouTube-style) depicting the spread of cases per million.
- **Vaccination Impact Modeling:** Statistical analysis of the correlation between vaccination rates and mortality.
- **Excess Mortality Analysis:** Comparison of confirmed deaths vs. absolute excess mortality to understand the pandemic's true impact.
- **Interactive Dashboard:** A **Shiny** application for real-time data exploration and country-specific trend analysis.

### 2. Machine Learning & Predictive Modeling
Implementation of various ML algorithms and deep learning architectures.
- **LSTM Time-Series Forecasting:** Neural networks (Keras/TensorFlow) designed to predict future COVID-19 case trends.
- **Regression & Classification:** Tutorials and custom implementations for predictive analysis.

---

## 📂 Project Structure

- `scripts/`: Modular R and Python logic.
    - `covid19/`: Epidemiological analysis and plotting scripts.
    - `machine-learning/`: Deep learning (LSTM) and traditional ML experiments.
    - `apps/`: Full applications, including the Shiny Interactive Explorer.
    - `utils/`: Shared helper functions and data processing utilities.
- `results/`: Generated visualizations, animations, and analysis reports.
- `data/`: (Placeholder) Local data storage and raw data descriptions.
- `docs/`: Technical documentation and project deep-dives.
- `renv/`: Full environment reproducibility using the R package manager.

---

## 🛠 Tech Stack
- **Language:** R (Primary), Python.
- **Frameworks:** Keras, TensorFlow, Shiny.
- **Visualization:** ggplot2, gganimate, Plotly, ggstream, Viridis.
- **Reproducibility:** renv, Git.

---

## 📈 How to Use
1. **Clone the repository:**
   ```bash
   git clone https://github.com/your-username/own-projects.git
   ```
2. **Restore the environment:**
   Open the `.Rproj` file in RStudio. `renv` should automatically prompt you to restore packages. If not, run:
   ```R
   renv::restore()
   ```
3. **Run the Dashboard:**
   ```R
   shiny::runApp("scripts/apps/covid19-dashboard")
   ```

---

## 📝 License
This project is licensed under the Apache License 2.0.

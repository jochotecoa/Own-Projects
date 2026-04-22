# GEMINI.md - COVID19 & Own-Projects Context

## Project Overview
This project is a repository of personal R projects, tutorials, and experiments by **Juan Ochoteco Asensio**. It focuses on data analysis (specifically COVID-19 data) and machine learning experiments using the R programming language.

**Current Branch:** `master` (Last modified on 2026-02-11).

## Tech Stack
- **Primary Language:** **R**
- **Key Libraries:** `ggplot2`, `gganimate` (for COVID-19 animations), `Keras` (for deep learning).
- **Domain:** Data Analysis, Visualization, and Machine Learning.

## Project Structure
- `scripts/`: Contains the core logic.
  - `covid19/`: Scripts for fetching and analyzing COVID-19 data.
    - `covid19_yt_style.R`: **Recommended.** High-quality "YouTube-style" bar chart race with interpolation, dark theme, and high FPS.
    - `covid19v3.R`: Legacy script using ECDC data.
  - `machine-learning/`: Deep learning experiments (Keras) and tutorial implementations.
  - `utils/`: Shared utility functions in `functions.R`.
- `output/`: Generated results like PNG plots and MP4/GIF animations.
- `Own-Projects.Rproj`: RStudio project configuration.

## Building and Running
Most scripts are intended to be run from the project root using R or RStudio.

### Key Workflows:
- **COVID-19 Analysis:** Run scripts in `scripts/covid19/` to generate animations and plots.
- **Machine Learning:** Run scripts in `scripts/machine-learning/` for deep learning and tutorial experiments.
- **Utilities:** Load `scripts/utils/functions.R` to access shared functions.

## Development Conventions
- **Root Context:** Scripts generally expect the working directory to be the project root.
- **Data Sources:** COVID-19 scripts fetch data from external sources (ECDC).
- **Output:** All generated plots and animations should be directed to the `output/` folder.

## Key Files
- `README.md`: Basic project overview.
- `LICENSE`: Apache 2.0 License.
- `GEMINI.md`: This instructional context file.
- `scripts/utils/functions.R`: Shared utility functions used across multiple scripts.

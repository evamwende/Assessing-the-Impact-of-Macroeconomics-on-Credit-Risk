# Assessing-the-Impact-of-Macroeconomics-on-Credit-Risk

## Overview
This repository contains the code and documentation for a dissertation investigating the impact of macroeconomic factors on credit risk in the Eurozone banking sector from 2015-2024. The study employs panel data regression techniques to analyze how variables like GDP growth, unemployment, inflation, and lending rates affect Non-Performing Loan (NPL) ratios, with special attention to the structural break caused by the COVID-19 pandemic.


## Data Sources
The analysis uses quarterly data from 2015-2024 for 13 Eurozone countries, sourced from:

European Central Bank (ECB) - NPL ratios and lending rates

OECD - GDP growth rates

Eurostat - Unemployment and HICP inflation rates

Countries are grouped into three regions:

Core Economies: Germany, France, Netherlands, Austria, Belgium

Southern Periphery: Spain, Italy, Portugal, Greece

Northern Periphery: Ireland, Estonia, Latvia, Lithuania

## Methodology
The research employs:

Descriptive statistics and correlation analysis

Panel data regression with Fixed Effects models

Structural break analysis using:

Split-sample comparison (pre- vs. post-COVID)

Interaction terms with a COVID dummy variable

Stationarity testing using Levin-Lin-Chu panel unit root tests

Model selection via Hausman test

## Key Findings
Unemployment rate is the most significant predictor of NPL ratios (+2.17% per 1% increase)

COVID-19 caused a structural break in credit risk dynamics

Lending rate-NPL relationship reversed during the pandemic due to policy interventions

Significant regional disparities persist (Southern Periphery NPL ratios 5× Core Economies)

## Code Implementation
The R script performs:

Data preprocessing and panel data construction

Descriptive statistics and visualization

Stationarity testing

Fixed and Random Effects model estimation

Hausman test for model selection

COVID impact analysis through split samples and interaction terms

Results visualization and export


## References
Full academic references are available in the dissertation document. Key influences include:

Beck et al. (2015) on global NPL determinants

Kanapickienė et al. (2022) on European credit risk

Plikas et al. (2024) on COVID-19's impact on NPLs

## License
This project is for academic purposes. Please cite appropriately if using any code or methodology.

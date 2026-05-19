# Assessing the Impact of Macroeconomic Factors on Credit Risk

### A panel data analysis of 13 Eurozone economies, 2015–2024
**MSc Operational Research dissertation · University of Edinburgh · August 2025**
**Author:** Eva Mwende Kiio · [LinkedIn](https://www.linkedin.com/in/eva-mwende-634906198/) · evakiio.m@gmail.com

---

## TL;DR

- **Unemployment is the dominant predictor of credit deterioration.** A 1 percentage point rise in unemployment is associated with a **2.17 pp increase** in the Non-Performing Loan ratio across the Eurozone (p < 0.001).
- **COVID-19 was a structural break, not just a shock.** Extraordinary policy interventions (ECB TLTRO-III, national loan guarantees, job retention schemes) **completely neutralised the normal positive relationship between lending rates and NPLs** during the pandemic.
- **Regional disparities run deep.** Southern Periphery economies carry **5.3× more credit risk** than the Core (NPL ratio 12.6% vs 2.36%) — a finding with direct implications for differentiated macroprudential policy.
- **The extended model explains 73% of NPL variation** (Adjusted R² = 0.734), confirming that macroeconomic conditions — and the policy regimes responding to them — are the primary drivers of credit stability.

---

## Why this matters

Credit risk isn't an abstract banking concept. When NPL ratios rise, banks tighten lending — and the households and small businesses that get cut off first are the ones who can least afford it. The 2015–2024 window captured in this study is unusual: it contains the recovery from the Eurozone debt crisis, the COVID-19 shock, and the most aggressive interest-rate cycle in a generation. That's a real-world laboratory for asking which macroeconomic forces actually determine whether credit stays accessible during crisis.

The findings have practical weight. They quantify what large-scale labour-market interventions (Germany's *Kurzarbeit*, Italy's *Cassa Integrazione*, the EU's €100bn SURE instrument) actually bought in credit-stability terms. They show how unconventional monetary policy can temporarily — but only temporarily — suspend the relationship between borrowing costs and default risk. And they expose the Eurozone's persistent regional vulnerabilities, which a single monetary policy still cannot fully address.

---

## Research questions

1. What is the relationship between key macroeconomic variables and credit risk across 13 Eurozone countries from 2015 to 2024?
2. Did the COVID-19 pandemic cause a structural break in these relationships?
3. How did the magnitude and significance of macroeconomic drivers of credit risk differ in the pre-COVID vs. during/post-COVID period?

---

## Data

| Variable | Description | Source |
|---|---|---|
| **NPL Ratio** (dependent) | Non-Performing Loans as % of total gross loans | European Central Bank |
| GDP Growth | Quarter-on-quarter real GDP growth | OECD |
| Unemployment Rate | Seasonally adjusted unemployment rate | Eurostat |
| HICP Inflation | Quarter-on-quarter HICP change | Eurostat |
| Lending Rate | Cost of borrowing for corporations | European Central Bank |

- **Panel structure:** N = 13 countries × T = 40 quarters = **520 observations** (balanced panel)
- **Countries grouped by region:**
  - *Western Core:* Germany, France, Netherlands, Austria, Belgium
  - *Southern Periphery:* Spain, Italy, Portugal, Greece
  - *Northern Periphery:* Ireland, Estonia, Latvia, Lithuania

---

## Methodology

A multi-step econometric strategy was applied:

1. **Stationarity testing** — Levin-Lin-Chu (LLC) panel unit root tests on all five variables. All series rejected the null of non-stationarity at p < 0.05; no further transformation needed.
2. **Model selection** — Both Fixed Effects (FE) and Random Effects (RE) panel regressions were estimated. A **Hausman test** (p < 0.001) confirmed FE as the correct specification, since unobserved country-specific characteristics (institutional quality, banking-sector history) are correlated with the regressors.
3. **Baseline FE regression** of NPL ratio on the four macroeconomic drivers.
4. **Structural break investigation** via two complementary approaches:
   - **Split-sample analysis:** the FE model re-estimated separately on pre-COVID (2015Q1–2019Q4) and during/post-COVID (2020Q1–2024Q4) sub-samples.
   - **Extended model with interaction terms:** a COVID dummy variable (= 1 for 2020Q2–2021Q4) and its interactions with each macroeconomic driver were added to the full-sample FE model. The significance of these δ coefficients formally tests for changes in the underlying relationships.

**Model specification (extended form):**

```
NPL_it = α_i + β1·GDP_it + β2·Unemployment_it + β3·HICP_it + β4·LendingRate_it
       + δ1·COVID_t
       + δ2·(GDP_it × COVID_t) + δ3·(Unemployment_it × COVID_t)
       + δ4·(HICP_it × COVID_t) + δ5·(LendingRate_it × COVID_t)
       + u_it
```

---

## Key results

### 1. Baseline Fixed Effects model (full sample, 2015–2024)

| Variable | Coefficient | Std. Error | p-value |
|---|---|---|---|
| GDP Growth | -0.091 | 0.055 | 0.101 |
| **Unemployment Rate** | **+2.170** | **0.084** | **< 0.001 \*\*\*** |
| HICP Inflation | -0.044 | 0.046 | 0.345 |
| Lending Rate | -0.237 | 0.131 | 0.073 |

R² = 0.622, n = 520.

Unemployment dominates. A one percentage-point rise in the unemployment rate is associated with a **2.17 pp increase in NPLs** — by far the most economically and statistically significant relationship in the model. This single coefficient is the headline finding: labour-market stability is the bedrock of credit stability.

### 2. The COVID structural break

Splitting the sample and re-running the model surfaced the pandemic's signature: every coefficient changed.

| Variable | Pre-COVID (2015–2019) | Post-COVID (2020–2024) |
|---|---|---|
| GDP Growth | +0.170 \* | **-0.113 \*\*\*** |
| Unemployment | **+1.112 \*\*\*** | **+1.966 \*\*\*** |
| HICP | -0.106 | +0.040 |
| Lending Rate | -0.695 | +0.018 |
| R² | 0.400 | 0.532 |

GDP growth flipped from positive to strongly negative (the theoretically expected sign). Unemployment's effect nearly doubled. The model's explanatory power jumped from 40% to 53%. Macroeconomic forces became *more* dominant in the post-COVID world, not less.

### 3. Extended model with interaction terms (the structural break, formally)

| Variable | Coefficient | p-value |
|---|---|---|
| Unemployment Rate | +1.373 \*\*\* | < 0.001 |
| **Lending Rate** | **+4.364 \*\*\*** | < 0.001 |
| **COVID dummy (δ₁)** | **+7.416 \*\*\*** | < 0.001 |
| **Lending Rate × COVID (δ₅)** | **-4.564 \*\*\*** | < 0.001 |
| Unemployment × COVID | +0.049 (ns) | 0.59 |
| GDP × COVID | -0.174 (ns) | 0.17 |
| HICP × COVID | -0.233 (ns) | 0.25 |

**Adjusted R² jumped from 0.610 to 0.734** with the inclusion of COVID terms.

The headline insight: **the normal positive relationship between lending rates and NPLs was reversed during COVID.** Net effect during the pandemic = β₄ + δ₅ = 4.364 − 4.564 = **−0.20** — effectively zero, slightly inverted.

This is the fingerprint of unconventional monetary policy. ECB TLTRO-III provided banks with cheap funding contingent on continued lending. National guarantee schemes — France's €300bn *Prêts Garantis par l'État*, similar programmes across the bloc — transferred credit risk from banks to states. Together they decoupled lending costs from default behaviour for the duration of the crisis.

---

## What this means in practice

For **risk managers:** static credit-risk models that assume constant relationships between macro variables and defaults will fail during policy-driven regime shifts. Risk dashboards need to track the introduction and withdrawal of major support schemes as first-order variables.

For **macroprudential policy:** unemployment is the lever. Job retention schemes during COVID — funded in part by the EU SURE instrument — were the most effective credit-risk mitigation tool deployed. The 2.17 pp NPL sensitivity to unemployment in the baseline model quantifies the value of those interventions.

For **policy translation more broadly:** the regional disparity (Southern Periphery NPL 5.3× higher than Core) shows the limits of one-size-fits-all regulation. Tools like the Countercyclical Capital Buffer need to be applied with regional awareness, and structural reforms — like Greece's "Hercules" Asset Protection Scheme tied to the EU Recovery and Resilience Facility — remain essential.

---

## Limitations and next steps

- **Scope:** Eurozone-only sample. A natural extension is to compare with non-Eurozone EU members operating different monetary regimes (UK, Sweden, Poland) — which would isolate the role of single-currency membership in shock transmission.
- **Variables:** the model excludes private-sector debt levels and real house prices, both established NPL determinants in the literature (Staehr & Uusküla, 2020). Adding them would test the model's robustness.
- **Dynamics:** Fixed Effects with interaction terms captures contemporaneous relationships but not lagged feedback loops. **Panel Vector Autoregression (Panel VAR)** would let the data speak to questions like "how long after an unemployment shock does the NPL response peak?"
- **Generalisation:** these findings come from a high-income monetary union. Whether the same transmission mechanisms hold in developing economies — where banking systems are smaller, informal employment is larger, and policy buffers are weaker — is a question I'd like to explore next, particularly in the African context where I have direct prior survey-design and policy-analytics experience.

---

## Repository structure

```
.
├── README.md                       # This file
├── Dissertation_Eva_Kiio.pdf       # Full dissertation (22 pages + appendix)
├── Dissertation_code.R             # Complete R analysis pipeline
└── outputs/
    ├── Correlation_Heatmap.png
    ├── Coefficient_Plot_Hybrid_Model.png
    ├── Final_Panel_Data.xlsx
    ├── Descriptive_Statistics.xlsx
    └── Group_Summary_Statistics.xlsx
```

## Tech stack

- **Language:** R 4.x
- **Econometrics:** `plm` (panel linear models, Hausman test, LLC unit root tests)
- **Data:** `tidyverse`, `readxl`, `writexl`, `skimr`
- **Visualisation:** `ggplot2`, `corrplot`
- **Reporting:** `stargazer` (regression tables)

## Reproduce

```r
# Clone the repo
# git clone https://github.com/evamwende/Assessing-the-Impact-of-Macroeconomics-on-Credit-Risk.git

# Install dependencies
install.packages(c("tidyverse", "readxl", "writexl", "skimr",
                   "ggplot2", "corrplot", "plm", "stargazer"))

# Update the file path at the top of Dissertation_code.R to point to All_Data.xlsx
# Run the full script — it generates all tables, charts, and Excel outputs.
```

---

## About me

Eva Mwende Kiio — Data/Research analyst with experience in national survey design (nine counties, Kenya), psychometric validation for national assessment tools, KPEEL project (World Bank partner) and panel data econometrics. MSc Operational Research, University of Edinburgh. Mastercard Foundation Scholar in Climate Justice and Sustainable Future for Africa.

Currently open to roles in M&E, impact analytics and research analysis — UK-based, hybrid or remote.

[LinkedIn](https://www.linkedin.com/in/eva-mwende-634906198/) · [Email](mailto:evakiio.m@gmail.com) 

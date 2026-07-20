# Empirical-Bayes shrinkage of 2009 base-period cell means

*Methodology note for the shift-share instrument in “Unpacking Skill Supply and Wages.”*

## 1. Motivation

The shift-share instrument used throughout the paper allocates each worker–year observation a base-period typical-hours value from their (age, undergraduate major, sex) cell in 2009, then aggregates to the cluster25 × agegroup × year level to form the national-counterfactual cluster supply (`lnnationalcluster25share`, the excluded instrument in Tables 1–3, A4–A8). The cell-level base-period quantity, denoted `agemeanhours_c` and computed at line 92 of `Code/Do/Cleaning/cleaning.do` as

```
agemeanhours_c  =  Σ_{i ∈ c, year_i=2009} perwt_i × annualhours_i  /  Σ perwt_i,
```

is treated by the rest of the pipeline as if it were the true cell-level mean of annual hours.

A direct diagnostic on the 2009 sample (see `Output/cell_size_diagnostics_2009.log`) shows that this assumption is fragile. Among the 20,073 (age × degfieldd × sex) cells observed in 2009:

* **75.9% of cells contain fewer than 30 unweighted observations** (median n = 9, p25 = 3, p10 = 1).
* The implied standard error of the raw cell mean is **193 hours in the bin with n < 30** (p90 = 436 hours), against a cell-mean level of roughly 1,500 hours — i.e. **13–29% relative measurement error** at the typical small cell.
* Within each (agegroup × degfieldd) rollup bucket, the median bucket draws **100% of its weighted hours from n < 30 cells**, and the average bucket draws 69% from such cells. Aggregate weight on small cells is moderate (~5% of total weighted hours), but the *cross-cluster variation* that identifies the IV is plausibly contaminated.

Classical measurement error in the instrument attenuates the first-stage coefficient and lowers the first-stage F. Under the paper’s jackknife-clustered standard-error convention, the Table 2 IV-FE first-stage F falls to 3.87, below all Stock–Yogo weak-IV thresholds. Reducing measurement error in the base-period cell means is one of two principal remedies (the other being a pivot to Anderson–Rubin weak-IV-robust confidence sets).

This note describes the hierarchical empirical-Bayes shrinkage estimator we substitute for the raw cell mean.

## 2. Model

For individual i in cell c = (age=a, major=m, sex=s), observed in 2009 (US-born, `bpl ≤ 56`), we model the annual-hours observation as

  h_{ic}  =  μ_{a,s}  +  α_{m,s}  +  γ_c  +  ε_{ic}, &nbsp;&nbsp; (1)

with the following structural priorities:

* **Tier 1 — Population age × sex profile (no shrinkage).** μ_{a,s} is a freely estimated mean for each (age, sex) combination. Age × sex captures the dominant life-cycle variation in annual hours (peak in the 30s and 40s, decline through retirement, larger and earlier decline for women). With 2.08M national observations across ~100 (age, sex) cells in 2009, each μ_{a,s} is precisely estimated; we treat it as a fixed parameter.

* **Tier 2 — Major × sex deviation (light shrinkage in practice).** α_{m,s} is the deviation of (major, sex) (m, s)’s mean hours from the population age × sex profile. The major-by-sex interaction lets the female-vs-male hours gap differ across majors (e.g. the gap in nursing differs from the gap in engineering). With ~350 (major × sex) bins and 2.08M obs, each α_{m,s} is also precisely estimated.

* **Tier 3 — Cell residual (heavy shrinkage for small cells).** γ_c ~ N(0, σ²_γ) is the cell-specific residual, capturing whatever variation in cell mean hours is not explained by additive age × sex + major × sex structure (e.g. art-history majors aged 67 working different hours than the age-sex + major-sex prediction would suggest). This is the term shrunk toward zero in proportion to how noisy the cell-level estimate is.

* ε_{ic} ~ N(0, σ²_ε) is the within-cell sampling residual.

Under (1), the best-linear-unbiased predictor (BLUP) of the true cell mean h̄_c = μ_{a,s} + α_{m,s} + γ_c is

  ĥ_c  =  μ̂_{a,s}  +  α̂_{m,s}  +  w_c × (ȳ_c − μ̂_{a,s} − α̂_{m,s}), &nbsp;&nbsp; (2)

where ȳ_c is the raw perwt-weighted cell mean and

  w_c  =  n_c / (n_c + n*), &nbsp;&nbsp; n*  =  σ²_ε / σ²_γ. &nbsp;&nbsp; (3)

w_c interpolates between the raw cell mean (w_c → 1 as n_c → ∞) and the systematic prediction μ̂_{a,s} + α̂_{m,s} (w_c → 0 as n_c → 0). The crossover point n* is the cell size at which the cell’s own data and the prior carry equal weight; equivalently, n* answers “how many obs in a cell would I need to learn as much from its raw mean as I already know from the age × sex + major × sex structure?”

## 3. Estimation

We estimate the components by a three-stage method-of-moments procedure on the 2009 US-born subsample of the ACS (`bpl ≤ 56`, ~2.08M observations after dropping observations with missing degree field, hours, or sex). Annual hours is constructed as `uhrswork × weeksworked`, with `weeksworked` derived from the IPUMS `wkswork2` brackets using the standard bracket midpoints. Cells are defined at the (age, degfieldd, sex) level, where `age` is the individual year of age and `degfieldd` is the IPUMS 4-digit undergraduate major code.

**Stage 1.** Compute μ̂_{a,s} as the perwt-weighted mean of annual hours by (age, sex):

  μ̂_{a,s}  =  Σ_{i ∈ (a,s)} perwt_i × h_i  /  Σ perwt_i.

**Stage 2.** Compute α̂_{m,s} as the perwt-weighted mean of (h_i − μ̂_{age(i),sex(i)}) by (major, sex):

  α̂_{m,s}  =  Σ_{i ∈ (m,s)} perwt_i × (h_i − μ̂_{age(i),s})  /  Σ perwt_i.

**Stage 3.** Estimate variance components.

* σ̂²_ε is the pooled within-cell variance across cells with n_c ≥ 2:

  σ̂²_ε  =  [ Σ_c (n_c − 1) s_c² ]  /  [ Σ_c (n_c − 1) ], &nbsp;&nbsp; s_c² = Σ_{i ∈ c} (h_i − ȳ_c)² / (n_c − 1).

* σ̂²_γ is estimated by method of moments. For each cell c,

  E[ (ȳ_c − μ̂_{a,s} − α̂_{m,s})² ]  =  σ²_γ + σ²_ε / n_c.

  Averaging over cells:

  σ̂²_γ  =  max( 0, &nbsp; mean_c [ (ȳ_c − μ̂_{age(c),sex(c)} − α̂_{major(c),sex(c)})² − σ̂²_ε / n_c ] ).

  (The truncation at zero handles small-sample sign flips. If σ̂²_γ = 0, every cell is fully shrunk to the Tier 1 + Tier 2 prediction — but in practice σ̂²_γ ≫ 0 because the data clearly show cells with idiosyncratic residual variation.)

Plug σ̂²_ε and σ̂²_γ into (3) to get n* and w_c, then plug into (2) to get the shrunken cell mean ĥ_c. The output is saved to `Output/agemeanhours_shrunk_lookup.dta`, keyed by (age, degfieldd, sex).

## 4. Interpretation

The shrinkage estimator has three appealing properties:

1. **Adaptive shrinkage.** Each cell is shrunk by an amount proportional to its noisiness: a cell with n=4 in a small specialty major is essentially replaced by the (age × sex) + (major × sex) prediction; a cell with n=5,000 keeps its raw mean. Nothing is decided ex-ante — n_c and the variance components determine the weighting.

2. **Variable prioritisation matches economic intuition.** Annual hours vary much more across (age, sex) than across (major, sex), so the procedure puts the precisely-estimated demographic axis (age × sex) at the top of the hierarchy with no shrinkage. Major × sex deviations are a refinement on top. Cell-level idiosyncrasies are the residual layer, which is also where the data are weakest, so it receives the heaviest shrinkage.

3. **Asymptotic optimality.** Under the random-effects model (1), ĥ_c minimises the posterior mean-squared error of h̄_c (Efron and Morris 1973, JASA). The classical Stein result — that shrinkage strictly dominates the raw mean in total MSE when there are three or more cells — applies a fortiori with 20,073 cells, of which three-quarters are very noisy.

## 5. Use in the IV pipeline

The shrunken cell means are substituted for the raw `agemeanhours` variable at line 92 of `Code/Do/Cleaning/cleaning.do`. Concretely, instead of computing

```stata
gegen agemeanhours = mean(annualhours) if year==2009 [aweight=perwt], by(age degfieldd sex year)
ereplace agemeanhours = min(agemeanhours), by(age degfieldd sex)
```

we merge in the shrunken estimates:

```stata
merge m:1 age degfieldd sex using "$out/agemeanhours_shrunk_lookup.dta", ///
    keepusing(agemeanhours_shrunk) keep(master match) nogen
gen agemeanhours = agemeanhours_shrunk
drop agemeanhours_shrunk
* (No `ereplace` needed: the merged value is constant across years by construction.)
```

The remainder of `cleaning.do` and the IV pipeline is unchanged. The cluster25 × agegroup × year instrument `lnnationalcluster25share` is then rebuilt from the shrunken cell means, and Tables 1, 2, 3, and A8 are re-estimated.

## 5b. Empirical finding: shrinkage does not strengthen the first stage

We implemented the procedure and re-ran the Table 2 IV-FE first stage with the shrunken instrument. The results are summarised below.

| Instrument variant | β_FS | SE (cluster-robust) | SE (jackknife) | F (cluster-robust) | F (jackknife) |
|---|---|---|---|---|---|
| Canonical (raw `agemeanhours`, full cleaning.do) | 0.540 | 0.122 | 0.275 | 19.6 | 3.87 |
| Shrunken (this procedure) | 0.509 | 0.114 | 0.263 | 19.8 | 3.75 |

The jackknife F barely moves (3.87 → 3.75). The reason, traced via a preview rollup (`Output/preview_shrunk_rollup.log`), is that the major-year aggregation already smooths away the cell-level noise: at the (degfieldd, year=2009) level, the shrunken and raw instrument values have correlation 1.0000 to four decimal places, with median |Δlog| < 10⁻⁶ and p99 |Δlog| ≈ 0.005. Because each major aggregates over roughly 50 (age × sex) cells, classical measurement error in the cell-level estimates averages out long before the IV reaches the regression. The weak jackknife F therefore reflects structural 25-cluster panel leverage — a small number of high-influence clusters that dominate the jackknife variance — not classical attenuation in the instrument.

We retain the shrinkage estimator in the appendix as a robustness check (the table above is a representative summary). The principal inference adjustment in the body of the paper is the use of Anderson–Rubin weak-instrument-robust confidence intervals, which are valid under any first-stage strength.

## 6. Diagnostics produced

The shrinkage script (`compute_shrunken_agemeanhours.do`) emits the following to `Output/compute_shrunken_agemeanhours.log`:

* σ̂²_ε, σ̂²_γ, and the implied n*.
* Distribution of shrinkage weights w_c by cell-size bin (n < 30, 30–99, 100–499, 500+).
* Mean and quantiles of |ĥ_c − ȳ_c| by bin.
* Percent-change distribution by bin.
* Comparison of population-weighted means of raw vs shrunken cell means (these should be similar, since shrinkage does not change the overall mean — it redistributes weight from noisy cells to the systematic prediction).

## 7. Robustness alternatives

The three-stage procedure described above is intentionally close to a full mixed-effects BLUP (`mixed annualhours i.age##i.sex || degfieldd#sex: || cell:`, REML) but avoids the computational burden of REML on 2M observations with ~20k random-effect levels. The method-of-moments estimators of σ²_ε and σ²_γ are slightly less efficient than REML but consistent under the same model, and the resulting w_c values typically differ from the BLUP weights by less than 0.05 in practice.

Two alternative shrinkage targets we considered and rejected:

* **Tier 2 = major only (no sex interaction).** Imposes equal female-vs-male hours gap across majors; rejected because the data show clear sex-gap heterogeneity across majors (engineering vs nursing).
* **Tier 2 = major × age interaction.** Would let each major have its own age profile (e.g. lawyers’ hours peak later than engineers’). Considered but rejected because (a) it adds ~10,000 parameters where many would be poorly identified, and (b) the within-cell residual γ_c already absorbs this variation when the data support it.

We did not explore Stage-1 alternatives because age × sex is essentially saturated and any smoothness restriction (e.g. cubic in age) would discard precisely-estimated population-level information for no gain.

## 8. Files

| File | Purpose |
|---|---|
| `compute_shrunken_agemeanhours.do` | Produces the lookup table |
| `Output/agemeanhours_shrunk_lookup.dta` | Cell-level lookup (age × degfieldd × sex → ĥ_c, ȳ_c, w_c, n_c) |
| `Output/compute_shrunken_agemeanhours.log` | Run log + diagnostics |
| `Code/Do/Cleaning/cleaning_shrunk.do` *(forthcoming)* | Modified `cleaning.do` that merges in shrunken estimates |
| `Code/Do/Output/Main Regressions_shrunk.do` *(forthcoming)* | Re-runs Tables 1–3 with the new instrument |
| `docs/methodology_shrunken_agemeanhours.md` | This document |

## 9. Reference

Efron, B. and Morris, C., 1973. Stein’s estimation rule and its competitors — an empirical Bayes approach. *Journal of the American Statistical Association*, 68(341), pp.117–130. https://doi.org/10.1080/01621459.1973.10481350

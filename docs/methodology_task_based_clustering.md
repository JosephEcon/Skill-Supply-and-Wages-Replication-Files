# Task-based clustering of majors (O\*NET work activities)

*Methodology note for an alternative to the occupation-based major clustering in "Unpacking Skill Supply and Wages."*

## 1. Motivation

The paper clusters the 175 undergraduate majors (`degfieldd`) by the **occupations** their graduates work in: `Code/Do/Cleaning/clustering.do` builds a major × occupation-share matrix and `Code/R scripts/weighted clustering.R` runs weighted hierarchical clustering on it (Jensen–Shannon divergence, average / Ward linkage, `members = totalcellsize`), with the main cut at **k = 25** (`weighted_average_linkage_jsd_soc4.dta`).

This note documents an alternative that groups majors by the **work activities their graduates' jobs require**, rather than by occupation identity. Two majors whose graduates do similar *things at work* (analysing data, treating patients, operating machinery) are grouped together even if they sort into nominally different occupations. The motivation is that activity content, not occupation labels, is the economically relevant notion of "skill" the paper is trying to capture, and is more robust to arbitrary occupational boundaries.

## 2. Construction

For each major *m* the score on activity *a* is the **proportion of its graduates whose occupation requires activity *a***. Writing **S** for the major × occupation share matrix (graduate-weight shares, summing to 1 within a major) and **A** for the binary occupation × activity "requires" matrix, the major × activity matrix is simply

```
M = S %*% A ,        M[m,a] = Σ_o S[m,o] · A[o,a] ∈ [0,1].
```

**S** is produced by `Code/Do/Cleaning/degree_soc6_shares.do`, which mirrors the sample, filters, and neuroscience fix of `clustering.do` but keeps the occupation at its **native 6-digit** IPUMS `occsoc` granularity (no SOC truncation) and saves long form. Result: 175 majors × **664** distinct 6-digit occupations; shares sum to exactly 1 within each major.

**A** is produced by `Code/R scripts/onet_activity_prep.R` from the **O\*NET 23.0** database (SOC-2010 based, the last release before O\*NET moved to SOC-2018 — chosen to match the 2009–2019 ACS, of which 9 of 11 years are SOC-2010 coded). Three granularities are built:

| Level | Activities | "Requires" definition | Density |
|------|-----------:|-----------------------|--------:|
| **GWA** Generalized Work Activities | 41 | rated: mean Importance ≥ 3 on the 1–5 scale | 0.561 |
| **IWA** Intermediate Work Activities | 332 | task-derived (occupation requires the IWA if any of its tasks maps to a child DWA) | 0.050 |
| **DWA** Detailed Work Activities | 2,070 | task-derived (`Tasks to DWAs.txt`) | 0.010 |

GWA "requires" uses the conventional Importance ≥ 3 cutoff (exposed as `IMPORTANCE_THRESHOLD` in the prep script). IWA/DWA are inherently binary occupation–task links; the DWA→IWA→GWA hierarchy comes from `DWA Reference.txt` / `IWA Reference.txt`.

### Occupation key alignment

O\*NET-SOC codes (`11-1011.03`) are stripped to a 6-character key (`111011`) to match IPUMS `occsoc` (`str6`, no punctuation). A complication surfaced by the Stata diagnostics: **12.6 % of ACS person-rows carry an IPUMS "X" placeholder** (e.g. `1110XX`) where the detailed SOC is unknown, and many ACS occupations are broad/aggregated codes with no exact O\*NET detailed match. `task clustering.R` therefore matches each ACS occupation to O\*NET by **exact 6-digit match where possible, else by averaging the O\*NET activity profiles of all detailed codes sharing the longest known prefix** (after dropping trailing `X`). Coverage (graduate-weighted):

* **exact 6-digit match: 48.8 %**
* **any match (exact or prefix-aggregated): 99.7 %** — only 4 of 664 distinct occupations unmatched.

Rows of **M** are renormalised over matched occupations, so `M[m,a]` is the proportion of a major's *matched* graduates whose occupation requires *a*.

## 3. Clustering

To stay comparable with the occupation-based pipeline, the same weighted hierarchical machinery is reused (`hclust(..., members = totalcellsize)`, average and Ward.D2 linkage, cuts at k ∈ {20,25,…,100,175}). Per the design choice, distances are **cosine** (activity-profile shape) and **Manhattan** (L1), giving 3 levels × 2 distances × 2 linkages = 12 configurations. Clustering is on the raw proportions in **M** (no standardisation), matching the raw-share choice in the existing script. Cluster assignments are saved as `Data/intermediate/task_weighted_{link}_{dist}_{level}.dta`.

## 4. Internal validation

`Output/task cluster validation {level} {dist} {link}.jpeg` plots weighted average silhouette width (ASWw), Hubert's C, and Hubert's Gamma over k = 2…50; `Output/task_internal_validation.csv` summarises peak-k and cophenetic correlation. Key observations:

* **ASWw is U-shaped for the headline GWA/cosine/average specification**: high at k = 2 (~0.67), a **local trough around k = 20–25 (~0.47)**, then a second peak near k ≈ 37. The 41 generic GWAs do **not** carve majors into ~25 well-separated groups — they favour either a few mega-profiles or many fine cuts. The IWA and DWA representations behave similarly but separate somewhat better at moderate k.
* `manhattan + average` degenerates to a k = 2 optimum for IWA and DWA (classic average-linkage chaining on sparse high-dimensional vectors); cosine is the better-behaved distance here.
* Cophenetic correlation is highest for cosine + average (GWA 0.72, IWA 0.76), indicating the dendrogram preserves pairwise dissimilarities reasonably.
* **Caveat:** ASWw is computed in each representation's own distance space, so its *level* is not comparable to the occupation-based JSD silhouette. It is informative only about where each task representation has its own natural cut, and for comparing dendrogram shapes.

## 5. Comparison with the occupation-based clusters

The decision-relevant comparison is partition agreement against the current main partition (`cluster25` in `weighted_average_linkage_jsd_soc4.dta`). `Output/task_vs_occ_agreement.csv` reports Adjusted Rand Index (ARI) and Normalized Mutual Information (NMI, √-normalised) for every config and k; `Output/task_vs_occ_agreement.png` plots the cosine/average headline.

At the paper's **k = 25**, cosine + average linkage:

| Level | ARI | NMI |
|------|----:|----:|
| GWA | 0.485 | 0.729 |
| IWA | 0.399 | 0.742 |
| DWA | 0.403 | 0.721 |

(GWA cosine + **Ward** gives the highest k = 25 agreement: ARI 0.556, NMI 0.770.) Agreement rises with k — by k = 90–100, IWA/DWA reach ARI ≈ 0.75 and NMI ≈ 0.95.

**Interpretation.** NMI ≈ 0.72–0.77 at k = 25 means the task-based grouping recovers *most* of the information in the occupation-based grouping, but ARI ≈ 0.40–0.56 means a substantial share of majors are reorganised — tasks reveal a related-but-genuinely-different structure rather than a relabelling.

### What changes (headline GWA cosine/average, k = 25)

`Output/task_vs_occ_majors_k25.csv` gives the full per-major mapping. Highlights:

* **Recovered cleanly:** the engineering cluster is reproduced almost exactly (occupation cluster 10 → one task cluster covering all engineering and engineering-technology fields); computing/CS/IT and agriculture also form coherent task clusters.
* **The defining difference — task "funnel" mega-clusters.** The task representation merges majors that share generic activity profiles even when they sort into different occupations:
  * a **science mega-cluster** absorbing biology, biochemistry, chemistry, physical sciences, environmental science, public health (and, less intuitively, criminology and military technologies);
  * a **communicate/analyse mega-cluster** absorbing essentially all of business, the social sciences, humanities, communications, psychology, and the fine arts.
  This happens because high-prevalence GWAs ("Getting Information", "Communicating with Supervisors/Peers", "Making Decisions") are required by most white-collar occupations (GWA density 0.56), so they discriminate weakly. IWA/DWA, being far sparser, blunt this somewhat — consistent with their slightly higher agreement at large k.

## 6. Files

**New code**
* `Code/Do/Cleaning/degree_soc6_shares.do` — builds **S** (major × 6-digit-SOC long shares) + `degree_totalcellsize.dta`.
* `Code/R scripts/onet_activity_prep.R` — builds **A** (`onet_requires_{gwa,iwa,dwa}.dta`) + label maps from O\*NET 23.0.
* `Code/R scripts/task clustering.R` — occupation→O\*NET matching, `M = S·A`, clustering, validation, and comparison.

**Inputs** `Data/Raw/onet/db_23_0_text/` (gitignored), `Data/Raw/acssimple.dta`, `Data/intermediate/weighted_average_linkage_jsd_soc4.dta`.

**Outputs** `Data/intermediate/task_weighted_*.dta`; `Output/task cluster validation *.jpeg` (×12), `task_internal_validation.csv`, `task_vs_occ_agreement.{csv,png}`, `task_vs_occ_crosstab_k25.csv`, `task_vs_occ_majors_k25.csv`.

To reproduce: run `degree_soc6_shares.do`, then `onet_activity_prep.R`, then `task clustering.R` (all standalone; not wired into `Master.do`).

## 7. Caveats

* **SOC vintage.** O\*NET 23.0 is SOC-2010; ACS 2018–2019 use SOC-2018. The prefix-aggregation fallback masks most of the mismatch (99.7 % matched), but exact-match coverage is only 48.8 %, so a meaningful fraction of occupations carry a broad-group-averaged activity profile rather than a detailed one.
* **GWA threshold.** The Importance ≥ 3 "requires" cutoff is a modelling choice; it is centralised in `onet_activity_prep.R` for sensitivity testing.
* **Distance space.** Ward and (especially) average linkage on cosine/Manhattan dissimilarities are heuristics — the same heuristic status as the existing JSD + Ward appendix specs. Cosine is better behaved than Manhattan here.
* The §8–§11 econometric evaluation below covers the first-stage F and wage-elasticity regressions under the occupation baseline and a wide range of task specifications (GWA/IWA/DWA × cosine/Manhattan × average/Ward, across k), plus ORIV.

## 8. Downstream econometric test (IWA manhattan/average, k=26)

The IWA manhattan/average clustering at its natural k=26 was fed through the paper's headline
Table 2 specifications, substituting `cluster26` for the occupation `cluster25` in the cluster-level
supply aggregation, the shift-share instrument, and the SE clustering. The panel, fixed effects
(`cluster175` = major identity), controls and `sqrt_weight` are unchanged — confirmed by identical
sample sizes (IV-FE N = 18,515; IV-FD N = 16,817). Built by `task_iv_build.do` (reusing
`nationalclustersprep.dta`, no micro re-clean) and `task_iv_specs.do`; log `Output/task_iv_specs.log`.

**First-stage strength (identical code, `sqrt_weight`, first-stage F = Kleibergen–Paap rk Wald F):**

| | IV-FE F_cr | IV-FE F_jk | IV-FD F_jk |
|---|---:|---:|---:|
| Occupation k=25 (this code) | 34.3 | 12.6 | — |
| **IWA k=26** | **160.3** | **77.6** | **73.6** |

(The draft's Table 2 reports occupation IV-FE F_cr 19.6 / F_jk 3.9. That figure is **population-weighted**:
it matches `sweep_cluster_count.do` — which uses `[pweight=weight]` — to the decimal at k=25 (19.57 / 3.87,
`Output/sweep_cluster_count.log`). But Table 2's *coefficients* use `[pweight=sqrt_weight]`. So the draft
attaches a population-weighted first-stage F to a square-root-weighted estimator. Under the estimator's own
sqrt weighting the occupation first stage is **34.3 / 12.6**, not 19.6 / 3.9. All numbers in this section,
occupation and IWA alike, use `sqrt_weight`, so the contrast is internally consistent.) IWA k=26 has a **~5–6× stronger and unambiguously
strong-by-Stock–Yogo first stage** — the balanced clusters do not inherit the occupation mega-cluster's
leverage problem. First-stage F across the k-sweep stays very strong: k=24 326, k=25 342, k=26 78,
k=27 82, k=28 94 (jackknife).

**Elasticity (point estimate, jackknife SE, 95% CI):**

| Spec | Occupation k=25 (paper) | IWA k=26 |
|---|---|---|
| FE | 0.051 (.068) | −0.024 (.056) [−0.13, 0.09] |
| IV-FE | 0.122 (.157) | **−0.076 (.077) [−0.23, 0.08]** |
| FD | 0.060 (.078) | −0.019 (.059) [−0.13, 0.10] |
| IV-FD | 0.104 (.121) | −0.055 (.071) [−0.19, 0.09] |

**Reading.** Under the stronger-IV task clustering, the IV-FE elasticity is a **precisely-estimated
near-zero** (point estimate slightly negative, SE roughly halved vs the occupation spec). All four IWA
estimates are small, negative-leaning, and statistically indistinguishable from zero; they are also not
statistically distinguishable from the occupation estimates (overlapping CIs). So the two clusterings
*agree* the own-cluster supply–wage elasticity is not distinguishable from zero — but IWA bounds it much
more tightly and removes the occupation spec's larger (insignificant) positive point estimate, which was
riding on a weaker, higher-variance first stage. This (i) confirms the weak occupation first stage is a
property of the occupation clustering's leverage, not of the data, and (ii) means promoting IWA to a main
specification would sharpen identification but shift the headline point estimate's sign (not its
significance). It strengthens the "task clustering = robustness check" framing: the qualitative
conclusion (no robust positive elasticity) survives, with tighter bounds.

## 9. First-stage F weighting (state of the draft)

**The draft (v3) already reports square-root-weighted first-stage F.** Verified directly against
`Unpacking Skill Supply and Wages JMP Upload v3.docx`: the main table shows **34.3 (IV-FE) / 34.5 (IV-FD)**
and the heterogeneity table **149.0 (below-mean) / 34.3 (above-mean)** — these match the recomputed
sqrt-weighted **cluster-robust (Kleibergen–Paap) F** to the decimal (`fix_fstats_sqrt.do`, log
`Output/fix_fstats_sqrt.log`): T2 IV-FE 34.34, IV-FD 34.46; T3 `stdinitialwage25≤0` 149.89,
`stdinitialwage25>0` 34.35. The population-weighted figures (19.6 / 3.9 etc.) appear **only** in the
`sweep_cluster_count.do` diagnostic (`[pweight=weight]`), not in the manuscript tables.

If one prefers to report the **jackknife** first-stage F (to match the jackknife SEs used for inference),
the sqrt-weighted jackknife values are smaller:

| Table / column | sqrt cluster-robust F (in draft) | sqrt **jackknife** F |
|---|---|---|
| Table 2 IV-FE | 34.3 | 12.6 |
| Table 2 IV-FD | 34.5 | 16.4 |
| Table 3, below mean (`stdinitialwage25 ≤ 0`) | 149.0 | 96.3 |
| Table 3, above mean (`stdinitialwage25 > 0`) | 34.3 | 9.3 |

(Under jackknife the headline IV-FE F is 12.6 and the Table-3 split-sample F's *swap* relative to
cluster-robust — state the subsample condition explicitly. Diagnostics: the weakness is not driven by the
demographic controls — dropping them moves the F by <0.1% — nor curable by cluster-robust SEs or higher k;
it is cluster-structure leverage. `fstat_controls.do`.)

## 10. Robustness across task clusterings

**The elasticity is statistically indistinguishable from zero under every clustering, distance, linkage,
and k examined.** It tilts slightly positive for the occupation specs and slightly negative for the task
specs, but no estimate is significant and the confidence intervals overlap throughout. The substantive
conclusion (no robust own-cluster supply–wage elasticity) is therefore extremely robust.

**Identification (simple IV-FE) is, by contrast, highly spec-dependent.** First-stage strength (jackknife
F, `sqrt_weight`) at each clustering's lead k:

| Clustering (lead k) | IV-FE elasticity (SE) | F_jk | note |
|---|---|---:|---|
| Occupation JSD/avg, k=25 (paper) | +0.122 (.157) | 12.6 | borderline |
| **IWA cosine/avg, k=25** | −0.025 (.079) | **375** | strongest; metric-honest |
| IWA Manhattan/avg, k=26 | −0.076 (.077) | 78 | strong at natural k |
| IWA Manhattan/Ward, k=25 | −0.036 (.055) | 15.5 | usable only k≈20–25; collapses higher |
| DWA cosine/avg, k=25 | +0.014 (.084) | 4.8 | weak |
| SOC4 cosine/avg, k=25 | −0.019 (.049) | 0.7 | collapses (simple IV) |

Key reads (sources: `cosine_avg_sweep.do`, `ward_sweep.do`, `ward_highk_sweep.do`, `soc4_cosine_iv.do`):
- **IWA cosine + average is both the metric-defensible pairing** (cosine is not a proper metric, so average
  linkage — which assumes nothing Euclidean — is appropriate, whereas Ward is not) **and the strongest
  simple-IV first stage** (F_jk ≈ 375 at k=25, low-leverage: F_cr 461 ≈ F_jk 375). For IWA cosine/avg,
  k=25 is also where the first stage peaks.
- **Ward is fragile**: usable only at k≈20–25, F_jk collapses to <2 at k≥30 and does not recover at high k
  (k=60–100 stays F_jk 1–5) — high cluster-leverage (F_cr ≫ F_jk).
- **DWA and cosine-on-the-occupation-matrix are weak** under the simple IV.
- Clustering agreement with the occupation k=25 partition is ARI ≈ 0.4–0.54 / NMI ≈ 0.72–0.77 for the IWA
  specs — about as much as the occupation clustering moves under its own distance/linkage choices.

## 11. ORIV across specifications

ORIV Table 4 (over-identified: split-sample internal/external instrument × cluster-size interaction;
`sqrt_weight`; jackknife inference). ORIV-FE columns:

| Clustering | ORIV-FE internal | ORIV-FE external |
|---|---|---|
| Occupation JSD/avg, k=25 | 0.054 (.107) F 21 | 0.113 (.158) F 23 |
| IWA Manhattan/avg, k=26 | −0.014 (.159) F 15 | −0.181 (.170) F 15 |
| IWA cosine/avg, k=25 | 0.127 (.128) F 14 | 0.002 (.188) F 14 |
| DWA cosine/avg, k=25 | 0.087 (.131) F 14 | 0.031 (.184) F 33 |
| IWA Manhattan/Ward, k=12 | 0.014 (.150) F 10 | −0.166 (.186) F 16 |
| SOC4 cosine/avg, k=25 | 0.012 (.084) F 11 | −0.025 (.118) F 61 |

(Sources: `task_oriv_specs.do`, `cosine_avg_oriv.do`, `ward_k12_oriv.do`, `soc4_cosine_oriv.do`.) All ORIV
elasticities are ~0 and insignificant. Two points: (i) the simple-IV identification ranking does **not**
carry to ORIV — the over-identified F's compress to ~13–23 across clusterings, and the dramatic IWA
cosine/avg simple-IV F (375) is a just-identified artifact; (ii) the **ORIV external (national)
instrument is markedly more robust to the clustering choice than the simple IV** — it stays strong
(F 60–83) even for SOC4 cosine, whose *simple* IV collapses to F_jk 0.7. This argues for leaning on the
ORIV as the identification workhorse when the goal is results that do not hinge on the clustering.

## 12. Recommendation for the paper

Report **one task-based robustness exhibit**: the O\*NET **intermediate-work-activity (IWA) clustering,
cosine distance + average linkage, k = 25**, shown as a column set beside the occupation main —
clustering agreement (NMI ≈ 0.75) plus the four-column estimate table (FE/IV-FE/FD/IV-FD).

Framing (important): present it as **"the skill clusters are not an artifact of occupational
boundaries,"** *not* as independent corroboration. The task matrix is `M = S·A` (the occupation-share
matrix `S` projected onto the O\*NET activity matrix `A`), so the high agreement is partly mechanical — a
strength for a robustness check, but it must be stated. Lead the message on **the elasticity being
unchanged** (a precise near-zero), not on instrument strength (the spectacular simple-IV F is
just-identified-only and does not survive ORIV).

Do **not** report: Ward variants (improper for non-metric cosine; leverage-fragile), DWA (weak ID), or
cosine on the occupation matrix (keep JSD there — it cleanly validates k=25 and is identification-stable).
Pair with the §9 weighting correction to Tables 1–3.

================================================================================
Replication package: "Unpacking Skill Supply and Wages"
Author: Joseph Richardson
================================================================================

OVERVIEW
--------
Code/Master.do reproduces every table and figure in the paper. By default it
runs in outputs-only mode: it re-estimates all exhibits from the saved analysis
datasets in Data/clean and Data/intermediate. A full rebuild from the raw ACS
extract is available via the toggles described below.

One-click use:
  1. Set the global $path near the top of Code/Master.do (Section B) to the
     root of this folder.
  2. Set $rpath (Section B) to your Rterm.exe (R 4.5.x).
  3. If running the clustering stage, also set `mypath` at the top of the four
     R scripts in "Code/R scripts": "weighted clustering.R",
     "cluster_visualizations.R", "onet_activity_prep.R", "task clustering.R".
  4. Run Code/Master.do.

REQUIREMENTS
------------
Stata: written and tested on StataNow 19.5/MP; Stata 17+ should work. All
required packages (estout, outreg2, gtools, winsor2, boottest, coefplot,
ereplace, ftools, require, reghdfe, ivreg2, ivreghdfe, ardl, rsource) are
installed automatically by Section A of Master.do on the first run (set the
`install` toggle to 0 on later runs to skip re-installation).

R: version 4.5.x with packages haven, dplyr, cluster, fastcluster,
WeightedCluster, philentropy, writexl, dendextend, ggplot2, ggraph, igraph,
pheatmap, RColorBrewer, gridExtra. R is only needed for the clustering stage
(cluster=1); the default outputs-only run does not call R.

TOGGLES (Section C of Master.do)
--------------------------------
install  default 1  Install Stata packages (set to 0 after the first run)
cluster  default 0  Major-occupation shares + hierarchical/task clustering in R
                    (produces the cluster files, Tables B1-B3, Figures 2, B1)
clean    default 0  Build the analysis panels from the raw ACS extract
tables   default 1  Main tables   (Tables 1-6, A8)
figures  default 1  Main figures  (Figures 1, 3, 4, 5)
apptab   default 1  Appendix tables  (Tables A1-A7, A9-A11, B4, C1, C2; Figure A1)
appfig   default 1  Appendix figures (Figures B2-B5)

Runtime and disk: the default outputs-only run re-estimates every regression
(including jackknife and wild-bootstrap inference) and takes on the order of a
few hours on a modern laptop. The full rebuild (cluster=1, clean=1) addition-
ally requires the ~7 GB raw ACS extract and writes roughly 59 GB of
intermediate files; expect several additional hours.

Note on figure caching: the Appendix B coefficient plots (Figures B2-B5) cache
each estimated panel as a .gph file in "Intermediate Output". Panels with an
existing .gph are NOT re-estimated. To force full re-estimation, delete
coefplot*.gph, ivplot*.gph and task_*.gph from "Intermediate Output" first.

DATA ACCESS (not included in the repository; place in Data/Raw)
---------------------------------------------------------------
1. acsbase.dta  - IPUMS USA extract, American Community Survey 2009-2019
   (all waves used by cleaning.do). Register at usa.ipums.org and build an
   extract containing the variables referenced in Code/Do/Cleaning/cleaning.do
   (demographics, education incl. detailed degree field DEGFIELDD, occupation
   OCCSOC, earnings, weights).
2. acssimple.dta - a reduced version of the same extract holding the variables
   used by Code/Do/Cleaning/clustering.do and degree_soc6_shares.do
   (year, degfieldd, occ, occsoc, perwt, education).
3. colhs1405.dta - Katz-Murphy replication data from openICPSR project 120694
   (https://www.openicpsr.org/openicpsr/project/120694/version/V2/view).
4. clghsgwg-march-regseries-exp.dta and effunits-exp-byexp-6308.dta -
   Acemoglu and Autor (2011) files, originally from Daron Acemoglu's data
   archive (https://www.dropbox.com/scl/fi/j879xx8mraurjis6wiy0v/tab-08.zip?dl=0).
5. km-cg-rsup-6317.dta - AGK relative wage/supply series (same archive family).
6. Francis/km-cg-rsup-6318.csv and Francis/km-cg-rsup-6323.csv - independently
   constructed CPS series used for Table A8. These are produced by the
   self-contained sub-pipeline in "Code/R scripts/Francis_replication"
   (see its own README and master.R).
7. onet/db_23_0_text/ - the O*NET 23.0 database in text format
   (https://www.onetcenter.org/database.html), used by onet_activity_prep.R
   for the task-based clustering robustness (Figure B5).
8. MinWage/ - effective minimum wage inputs for Tables A9-A11 (built into
   Data/intermediate/mw_union_national.dta by Code/Do/Cleaning/
   mw_union_series.do):
     - state_year_combo.dta, vogel_mw_series_1963_2016.dta (= his
       mw_series.dta) and cps-to-census.dta from the replication package of
       Vogel (2025, QJE), "The Race Between Education, Technology, and the
       Minimum Wage" (doi:10.1093/qje/qjaf014; replication data on Harvard
       Dataverse).
     - state_pop_weights.dta: fixed average ASEC state population shares,
       derived one-off from the Vogel package's cps_00095.dta by replicating
       his Ext_CM_national.do lines 332-345.
     - FRED/STTMINWG??.csv (46 state minimum wage series; AL, LA, MS, SC, TN
       have no state minimum) and FRED/GDPDEF.csv, pulled 2026-07-22 from
       https://fred.stlouisfed.org/graph/fredgraph.csv?id=<SERIES>. These
       extend Vogel's national real effective minimum wage series (validated
       to <0.01 log points against his 1963-2016 series) through 2023.
9. Farber2021/ - union density series for Tables A9-A11, from the replication
   package of Farber, Herbst, Kuziemko and Naidu (2021, QJE), "Unions and
   Inequality over the Twentieth Century" (Harvard Dataverse
   doi:10.7910/DVN/QTDUQ0, which links to the full _ProjectRep2.zip):
   ts_union_ineq.dta (verbatim from the package) and the tidy extract
   union_density_farber2021.dta (their funionAverage headline density plus the
   Gallup and BLS/CPS components). See Farber2021/provenance.txt.

The ACS data are too large to post and the remaining files are not ours to
redistribute, which is why Data/ ships empty.

EXHIBIT -> SCRIPT MAP
---------------------
Figure 1                Code/Do/Output/Figure 1.do            degearnings.png
Figure 2 (dendrogram)   Code/R scripts/cluster_visualizations.R  dendrogram_k25.png
Figure 3                Code/Do/Output/Figure 3.do            figure 3.png
Figures 4, 5            Code/Do/Output/Figures 4 and 5.do     kdensitycollege.png, kdensitycluster25.png
Tables 1, 2, 3          Code/Do/Output/Main Regressions.do    table1/2/3_*.rtf
Table 4                 Code/Do/Output/Obviously Related IV.do    table4_oriv.rtf
Tables 5, 6             Code/Do/Output/RBET replication.do    table5_*.rtf, table6_*.rtf
Table A1                Code/Do/Output/Appendix Regressions.do    tableA1_varying_samples.rtf
Tables A2, A3           Code/Do/Output/Obviously Related IV.do    tableA2_oriv_liml.rtf, tableA3_oriv_just_identified.rtf
Table A4                Code/Do/Output/Appendix Regressions.do    tableA4_wild_bootstrap.rtf
Table A5                Code/Do/Output/Appendix Regressions.do    tableA5_no_controls.rtf
Table A6                Code/Do/Output/Weighting robustness.do    table_weighting_robustness.rtf
Table A7                Code/Do/Output/Table A7 AGK extended.do   tableA7_agk_extended_1963_2019.rtf
Table A8                Code/Do/Output/Francis KM robustness.do   tableA8_francis_2019_*.rtf
Table A9                Code/Do/Output/KM controls robustness.do  tableA9_km_controls_agk2017_{levels,fd}.rtf
Table A10               Code/Do/Output/KM controls robustness.do  tableA10_km_controls_francis2019_{levels,fd}.rtf
Table A11               Code/Do/Output/KM controls robustness.do  tableA11_km_controls_diagnostics.rtf
Figure D4               Code/Do/Output/KM controls robustness.do  irfrelsupinstitutions.png
Figure D5               Code/Do/Output/KM controls robustness.do  irfinstitutions.png
Figure A1               Code/Do/Output/Appendix Regressions.do    coefplotage.jpg
Tables B1, B2, B3       Code/R scripts/weighted clustering.R  clusters_25_jsdsoc4.csv, clusters_25_jsdsoc4_ward.csv, clusters_101_jsdsoc4_09.csv
Table B4                Code/Do/Output/Appendix Regressions.do    tableB4_clusters_2009.rtf
Figure B1               Code/R scripts/weighted clustering.R  cluster validation scores jsd.jpeg (and variants)
Figures B2, B3, B4      Code/Do/Output/Appendix B coefplots.do    coefplots_{average_jsd,ward_jsd,average_manhattan}_combine.png
Figure B5               Code/Do/Output/Task clustering coefplots.do   coefplots_task_iwa_manhattan.png
Tables C1, C2           Code/Do/Output/Acemoglu Autor Replication.do  tableC1_*.rtf, tableC2_*.rtf
Figures D1, D2, D3      Code/Do/Output/RBET replication.do    irfcombine.png, differencesirfquadratic.png, irfrelsup.png

All table/figure outputs are written to Output/; intermediate graph objects to
"Intermediate Output/".

PIPELINE (full rebuild order, handled automatically by Master.do)
-----------------------------------------------------------------
1. clustering.do, degree_soc6_shares.do  - major-occupation share matrices
2. weighted clustering.R                 - hierarchical clustering (SOC-based)
   cluster_visualizations.R              - dendrogram + validation figures
   onet_activity_prep.R, task clustering.R - task-based (O*NET IWA) clustering
3. cleaning.do                           - ACS cleaning; builds analysis panels
   mw_union_series.do                    - national effective minimum wage +
                                           union density series (Tables A9-A11;
                                           also auto-built on demand by
                                           "KM controls robustness.do")
4. Output do-files                       - all tables and figures (see map)

LICENSE
-------
Code is released under the MIT License (see Code/LICENSE).

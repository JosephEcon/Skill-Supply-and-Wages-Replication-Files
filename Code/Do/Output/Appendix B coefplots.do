/*==============================================================================
  Script:   Appendix B coefplots.do
  Paper:    Figures B2, B3, B4 (elasticity vs number of clusters, by clustering
            method: distance metric x linkage, at SOC2/SOC3/SOC4)
  Outputs:  $out/coefplots_average_jsd_combine.png        (Figure B2)
            $out/coefplots_ward_jsd_combine.png           (Figure B3)
            $out/coefplots_average_manhattan_combine.png  (Figure B4)

  Each figure is built by coefplot_core.do, parameterised by $LINK/$DIST/$LAB.
  RESUMABLE: coefplot_core caches each OLS/IV panel as a .gph in
  $interout and skips panels whose .gph already exists — delete the cached
  coefplot*.gph / ivplot*.gph files first to force a full re-estimation.
==============================================================================*/

* Shared setup: paths, globals, helper programs
local master = $master
if `master' != 1 {
    global path "C:/Users/josep/OneDrive/Documents/PhD/Skills and Wages"
}
do "$path/Code/Do/setup_globals.do"

set maxvar 10000

* Figure B2: Jensen-Shannon distance, average linkage
global LINK average_linkage_
global DIST jsd
global LAB  average_jsd
do "$path/Code/Do/Output/coefplot_core.do"

* Figure B3: Jensen-Shannon distance, Ward's linkage
global LINK ward_linkage_
global DIST jsd
global LAB  ward_jsd
do "$path/Code/Do/Output/coefplot_core.do"

* Figure B4: Manhattan distance, average linkage
global LINK average_linkage_
global DIST manhattan
global LAB  average_manhattan
do "$path/Code/Do/Output/coefplot_core.do"

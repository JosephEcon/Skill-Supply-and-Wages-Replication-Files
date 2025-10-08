These are the replication files for "Unpacking Skill Supply and Wages"

I apologise for any messiness, as this replication package have not been fully cleaned up and are only posted in the interests of full transparency.

Master.do in the code folder should run all of the cleaning code and produce all outputs.

Before running, you need to download all ACS waves prior from 2001 from IPUMS and save them as acsbase.dta in the data/raw folder. To run the replication of the Katz and Murphy (1992) approach, you must download "colhs1405.dta" from https://www.openicpsr.org/openicpsr/project/120694/version/V2/view;jsessionid=C3BAF2471A9908D4CF39DB5FA7340F0F and place it in the raw data folder. Meanwhile, to run the replication of a regression from Acemoglu and Autor (2011), you must install "effunits-exp-byexp-6308.dta" and "clghsgwg-march-regseries-exp.dta" into the raw data folder from https://www.dropbox.com/scl/fi/j879xx8mraurjis6wiy0v/tab-08.zip?dl=0&e=1&file_subpath=%2Ftab-08&rlkey=lv4c8czsha05sj54mg8cuzhqk - this was originally accessed from Daron Acemoglu's data archive.

These data are not posted in the replication package because the ACS data are too large to be uploaded to GitHub, while I lack the copyright for the other data.

You must also set your file path in "Master.do" and "weighted clustering.R". Alongside this, you also need to have R installed alongside the packages called in "weighted clustering.R"

The code files ran from master.do are as follows:

clustering.do

Calculates the share of graduates from each major that works in each occupation.

weighted clustering.R

Performs hierarchical clustering of the data produced by the previous file. It also produces Tables A1 and A2, alongside Figure A1.

cleaning.do

Cleans the ACS waves for analysis and merges in the clusters formed in weighted clustering.R

Main Regressions.do

Produces Tables 1-3

Obviously Related IV.do

Produces Tables 4, A5, and A6.

RBET Replication.do

Produces Table 6, performs the statistical tests reported in Table 5, and produces all output in Appendix C.

Different clusters coefplots.do

Produces all figures in Appendix A, except for A1.

Appendix Regressions.do

Produces all tables in Appendix A not referred to directly within this readme file.

Figure 1.do, Figure 3.do, and Figures 4 and 5.do

Self-explanatory.



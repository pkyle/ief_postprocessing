This repository contains all data and code necessary to generate the figures for Kyle et al. 2026, "State or nation, sector or system? How granularity shapes U.S. energy modeling results"

The script "ief_paper_figures_merge.R" uses saved model output data in ief_output.proj, combined with mappings in the mappings folder, and third-party libraries, to produce all committed .png files, named by their placement in the paper.

To re-generate the file "ief_output.proj", the GCAM repository is available at https://github.com/pkyle/gcam-core/tree/gpk/paper/ief using the configuration files:
```
exe/configuration-sets-gcam/config.xml
exe/configuration-sets-gcamusa/config.xml
```
Which are saved as exe/configuration.xml prior to running.

Once the model runs are executed, the command `REQUERY_DATA` can be set to `TRUE`, and the `localDBConn()` filepath updated

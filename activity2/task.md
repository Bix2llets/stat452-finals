# Research questions

-
-
-
-
-
-
-
-
-
-
-
-

# Reproducing the outputs

Only the R sources and the raw dataset (`dataset/heart_disease_risk_2026.csv`)
are committed. The tables, figures, logs and `dataset/cleaned_data.rds` are all
written by the scripts, so they are gitignored rather than committed. To
regenerate everything, run each step from its own directory, in order:

```
cd activity2/step1 && Rscript -e "sink('data_inspection_log.txt', split = TRUE); source('data_processing.R', echo = TRUE, max.deparse.length = 10000); sink()"
cd activity2/step2 && Rscript -e "sink('descriptive_log.txt', split = TRUE); source('descriptive_statistics.R', echo = TRUE, max.deparse.length = 10000); sink()"
```

Step 1 must run first: it writes `dataset/cleaned_data.rds`, which Step 2 reads.
Packages required: `dplyr`, `tidyr`, `ggplot2`.

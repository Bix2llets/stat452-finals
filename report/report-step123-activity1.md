# Review: Activity 1 — Steps 1–3

Review-only pass over `activity1/step1` (`data_processing.R`), `activity1/step2`
(`descriptive_statisitics.R`), and `activity1/step3` (`multi_factor_anova.R`,
`interaction_plots.R`), checked against `reqs/project-requirement-activity1.md`.
No source files in the repo were modified. Claims below were checked two ways:
first statically against `activity1/dataset/ds_salaries.csv` (via `awk`) and the
PDFs already committed under `activity1/step2`; then re-validated by actually
running all four scripts end-to-end with R 4.6.0, against an isolated scratch
copy of `activity1/` (dataset + step1–3), so nothing under `activity1/` itself
was touched. Two of the original static-analysis findings were corrected as a
result — see the "Re-validated with R" section at the end.

## Step 1 — `data_processing.R`

### Confirmed correct
- "No NA values": 0 blank/NA cells found in the raw CSV.
- "42 duplicated entries": verified exactly, after dropping `X`, `salary`,
  `salary_currency`.
- Outlier-clamping rationale: Q3 = 150,000, 1.5×IQR upper bound ≈ 280,911
  (comment says "3e5"), max = 600,000 ("6e5"), mean ≈ 112,298 ("1e5"),
  10/607 ≈ 1.6% of rows clamped (comment says "around 1%"). All consistent.

### Issues
1. **Stale comment on role distribution.** The comment claiming role counts of
   "540 and 56, almost 10 times" does not match what the code produces.
   Recomputing the `role` classification directly from the CSV gives
   Engineering = 248, Research = 225, Analyst = 127 — a fairly balanced 3-way
   split, not a 10:1 imbalance. This reads like leftover reasoning from an
   earlier discipline-based grouping attempt that was abandoned in favor of
   the current Engineer/Scientist/Analyst regex split, and the comment was
   never updated to match.
2. **Ordinal encoding is discarded.** `company_size`, `experience_level`, and
   `leadership` are mapped to integers (S/M/L → 1/2/3, EN/MI/SE/EX → 0/1/2/3)
   and then passed through plain `as.factor()` rather than
   `factor(..., ordered = TRUE)`. This is appropriate for the Factorial ANOVA
   use in step 3 (which needs unordered factors under `contr.sum`), but the
   requirement's "ordinal encoding" is not actually preserved anywhere, and it
   has a concrete downstream cost: every plot and ANOVA contrast name now
   shows bare digits ("1"/"2"/"3", "0"/"1") instead of "S"/"M"/"L" or
   "EN"/"MI"/"SE"/"EX" — confirmed in the Step 2 PDFs below.
3. **Undocumented collapses.** `employment_type → Fulltime/Other` and
   `company_location`/`employee_residence → US/Other` have no inline
   justification at the point of transformation, unlike the outlier-handling
   and job-title steps, which each explain their reasoning in a comment. The
   justification for the location collapse only appears later, in Step 2's
   EDA comments — i.e. after the decision was already made in Step 1.
4. **Data-leakage risk for later steps.** The IQR clamping bounds are computed
   on the full 607-row dataset before any train/test split (planned for
   Step 4). Not a bug in Steps 1–3, but Step 4 will need to avoid letting
   information from the eventual test set influence these bounds.
5. Minor: the `.default` branches for `company_size`, `leadership`, and
   `experience_level` in the recoding `case_when()`s are dead code — no such
   out-of-range values exist in the cleaned data — harmless but unnecessary.

## Step 2 — `descriptive_statisitics.R`

### Confirmed correct
Boxplots for experience level, employment type, company size, leadership, and
role all render, and the qualitative interpretations in the comments are
consistent with what the plots show.

### Issues (verified by opening the committed PDFs)
1. **`salary_by_work_year.pdf` is not interpretable.** The plotting call
   (`aes(salary_in_usd, group = work_year)`) never maps `work_year` to a
   labeled axis. The rendered PDF shows three dodged boxplots, but the y-axis
   only shows the auto-dodge offsets `-0.2, 0.0, 0.2` — there is no
   "2020/2021/2022" label anywhere, so a reader cannot tell which box
   corresponds to which year. Needs `y = as.factor(work_year)` (or a
   `fill =` mapping with a legend) to actually convey the trend it's meant to
   show.
2. **Uninformative factor labels**, confirmed in `salary_by_company_size.pdf`
   (axis shows "1", "2", "3") and `salary_by_leadership_role.pdf` (axis shows
   "0", "1") — a direct consequence of Step 1 issue #2 above.
3. **Reproducibility gap.** `salary_qq.pdf` and `salary_sqrt_qq.pdf` exist in
   the folder, but there is no QQ-plot code anywhere in
   `descriptive_statisitics.R` (no `qqnorm`/`stat_qq`/`geom_qq` calls).
   Likewise, `salary_histogram.pdf` exists with no matching `ggsave()` call in
   the current script — opening it shows it is actually the
   **sqrt-transformed** histogram, duplicating `salary_hisotogram_sqrt.pdf`
   under a differently-spelled filename. All three look like orphaned
   artifacts from an earlier version of the script; re-running the current
   script from scratch will not regenerate them.
4. `step2/task.md` is empty — no contributor record, unlike `step1/task.md`.

## Step 3 — `multi_factor_anova.R` / `interaction_plots.R`

### Confirmed sound practice
- `options(contrasts = c("contr.sum", "contr.poly"))` is correctly set before
  running Type III `Anova()` — a very common oversight to skip, done right
  here.
- The Shapiro-Wilk / Breusch-Pagan → Box-Cox → transform → re-check workflow
  is methodologically appropriate. Re-run in R: for the first model
  (`salary_in_usd ~ company_size * leadership`), the Box-Cox λ that maximizes
  the log-likelihood is 0.57 (95% CI ≈ [0.48, 0.67]) — λ = 0.5 (square root)
  falls inside that CI, so "Box-Cox suggests sqrt" is a statistically
  defensible reading, not just an eyeballed one.
- Restricting the factorial design to two-way models is explicitly justified
  in-line ("Cannot do > 2 factors, since it results in some 0 observation..."),
  which is reasonable given the sparse cross-tabs, and satisfies the
  requirement's own two-factor examples (Experience × Company Size, Experience
  × Job Category).
- Re-running `multi_factor_anova.R` end-to-end (via plain `Rscript`, so
  top-level results auto-print) exactly reproduced every pairwise-contrast
  number hardcoded in the file's comments (e.g. the `leadership0 - leadership1`
  contrasts of -73500/-36953/-62488, the `company_size` contrasts of
  -46072/-34316/11756, etc., across all ~15 models) — the comment blocks are
  genuine, reproducible console output, not fabricated or stale.

### Issues
1. **Real inconsistency between the two files.** `interaction_plots.R`
   computes `table(employee_residence, company_location)` and concludes in a
   comment: *"Thus we will only use one of them as factor."*
   `multi_factor_anova.R` nonetheless fits
   `lm(sqrt(salary_in_usd) ~ employee_residence * company_location)` anyway.
   Cross-tabbing the raw CSV with the same US/Other collapsing rule confirms
   why that comment exists — the off-diagonal cells are almost empty:

   | employee_residence \ company_location | US | Other |
   |---|---|---|
   | US    | 330 | 2  |
   | Other | 25  | 250 |

   An interaction term estimated off a 2-observation cell is not trustworthy,
   so the "no interaction" conclusion drawn for this specific model pair rests
   on much weaker footing than the other pairwise models in the file, and it
   directly contradicts the plan stated in `interaction_plots.R`.

   Re-running the model confirms it does fit without error (n=600 after the
   role-NA filter shrinks the off-diagonal further: US-resident/Other-company
   = 2, Other-resident/US-company = 24), and the actual `Anova(type=3)` output
   is: `employee_residence` p = 4.9e-08 (significant), `company_location`
   p = 0.397 (n.s.), interaction p = 0.213 (n.s.) — matching the script's own
   "no interaction" comment exactly. So the conclusion isn't numerically wrong,
   but it is under-powered for that one cell: R fit it silently with no
   rank-deficiency warning, which is precisely what makes the near-empty cell
   easy to miss without checking the cross-tab directly, as this review did.
2. **Step 3 figures aren't committed as reviewable artifacts (execution-dependent).**
   Unlike Step 2, `interaction_plots.R` never calls `ggsave()` for any of its
   15 interaction plots, and there is no `step3/*.pdf` in the repo. Re-running
   it two ways showed the actual behavior differs by how it's launched:
   - Via plain `Rscript interaction_plots.R` (top-level autoprint like an
     interactive session), all 15 `draw_interaction()` plots *do* get written
     — but all into one generic, anonymously-named `Rplots.pdf` in the working
     directory (confirmed: deleting it and re-running regenerates an identical
     26,549-byte file), not as separate descriptively-named files the way
     Step 2 does.
   - In the more likely real workflow — running line-by-line in RStudio, or
     clicking plain "Source" — the plots only ever land in the Plots pane and
     are not written to disk at all unless manually exported.
   Either way, none of the 15 interaction figures currently exist as a
   reviewable, descriptively-named artifact in the repo; `ggsave()` calls
   (as in Step 2) would fix this regardless of how the script is run.
3. **Large-N test sensitivity isn't stated as a general principle.** With
   n ≈ 600–607, Shapiro-Wilk/Breusch-Pagan will flag statistically significant
   deviations even for practically minor ones. The script already does the
   right thing by repeatedly trusting the diagnostic plot over the test in
   several places ("High W, but low p-value...", "residual line flat contrary
   to the test") — but this reasoning is asserted ad hoc each time rather than
   stated once as a general caveat, which will read as inconsistent to a
   grader unless the report states the large-N principle explicitly.
4. **No correction across the ~15 separate ANOVA models.** Within each model,
   Tukey adjustment is correctly applied to pairwise contrasts, but there is
   no adjustment across the battery of ~15 independent two-way models run in
   the file — worth disclosing as a limitation (family-wise error rate across
   the whole exploratory sweep exceeds the nominal 0.05), not necessarily
   something to change given the exploratory intent.
5. `attach(data)` / `detach(data)` in `interaction_plots.R` — discouraged
   practice in general, though harmless here since `detach()` is called at the
   end of the script.
6. `step3/task.md` is empty — same contributor-record gap as Step 2.
7. **Discovered while re-validating: `source()` silently drops most of the
   ANOVA output.** Running `source("multi_factor_anova.R")` (R's default
   `echo = FALSE, print.eval = FALSE`) produces only 300 lines of output —
   the `table()` cross-tabs and the explicitly `print()`-wrapped
   Shapiro-Wilk/Breusch-Pagan tests show, but every bare top-level
   `Anova(model, type = 3)`, `emmeans(...)`, and `pairs(...)` call is silently
   suppressed. Running the identical file via plain `Rscript
   multi_factor_anova.R` (auto-print behaves like an interactive session)
   produces 752 lines — the full ANOVA tables and pairwise contrasts. This
   matters because RStudio's plain **"Source"** button (as opposed to "Source
   with Echo") uses the same suppressing defaults as `source()`. If anyone on
   the team ran the script that way while writing the report, they would have
   seen the cross-tabs and normality/homoscedasticity tests but none of the
   actual ANOVA/emmeans results — worth flagging to the team even though it
   isn't a code defect.

## Re-validated with R

R 4.6.0 turned out to already be installed on this machine
(`C:\Program Files\R\R-4.6.0`), just not on `PATH`; no new software beyond
three missing R packages (`tidyverse`, `tseries`, `ggfortify`, installed into
a temp library) was needed. All four scripts were run end-to-end, in order,
against an isolated scratch copy of `activity1/` — the real `activity1/`
folder was never touched. Outcomes:

- **Step 1** (`data_processing.R`) ran cleanly, 607 → 600 rows. Confirmed by
  inspecting the resulting data frame: `experience_level: Factor w/ 4 levels
  "0","1","2","3"`, `company_size: Factor w/ 3 levels "1","2","3"`,
  `leadership: Factor w/ 2 levels "0","1"` — the label-loss finding (#2 above)
  is exactly reproduced, not a static-analysis guess.
- **Step 2** (`descriptive_statisitics.R`) ran cleanly and wrote exactly 10
  PDFs (`salary_by_*.pdf` ×8, `salary_hisotogram.pdf`,
  `salary_hisotogram_sqrt.pdf`). `salary_qq.pdf`, `salary_sqrt_qq.pdf`, and
  `salary_histogram.pdf` were **not** among them — the reproducibility gap
  (#3 above) is confirmed by actual execution, not just by grep. The freshly
  regenerated `salary_by_work_year.pdf` and `salary_by_company_size.pdf` show
  the exact same unlabeled axes described above.
- **Step 3** (`multi_factor_anova.R`, `interaction_plots.R`) both ran to
  completion with exit code 0 and no rank-deficiency/singularity warnings —
  including the near-empty-cell `employee_residence × company_location`
  model. Running with full auto-print reproduced every hardcoded contrast
  number in the file's comments exactly, and confirmed the Box-Cox λ = 0.5
  choice is inside the 95% CI. This also surfaced two corrections to the
  original static-analysis pass, folded into issues #2 and #7 above: the
  interaction plots do get rendered when run via plain `Rscript` (just into
  one anonymous `Rplots.pdf`, not committed or descriptively named), and
  `source()` — including RStudio's plain "Source" button — silently drops
  most of the ANOVA/emmeans output that plain `Rscript` or "Source with Echo"
  would show.

## Bottom line

The statistical reasoning and workflow — outlier handling → EDA → Box-Cox-
guided transform → Type III factorial ANOVA with Tukey post-hoc — are sound,
and every numeric claim in the comments was independently reproduced by
actually running the scripts. The concrete problems worth fixing before the
report is finalized:

1. The `employee_residence × company_location` model is built on a
   near-empty cell (2 observations) despite the team's own note not to cross
   those two variables — either drop it or clearly caveat the result; R will
   fit it without complaint, so this is easy to miss.
2. Several PDFs committed under Step 2 (`salary_qq.pdf`, `salary_sqrt_qq.pdf`,
   `salary_histogram.pdf`) cannot be regenerated from the checked-in script,
   and none of Step 3's 15 interaction plots are saved anywhere descriptive —
   add `ggsave()` calls to `interaction_plots.R` and either regenerate or
   remove the three orphaned Step 2 PDFs.
3. Categorical variables lost their human-readable labels in Step 1
   (`as.factor()` on integer recodes), which quietly degrades every plot and
   ANOVA contrast name downstream — worth relabeling levels
   (`factor(x, labels = c(...))`) before Step 2/3 plotting.
4. Make sure whoever finalizes the report runs `multi_factor_anova.R` via
   plain `Rscript` or "Source with Echo" — a plain `source()`/"Source" run
   silently hides the ANOVA and emmeans output the interpretation depends on.

# Step 5: Interpretation of the Activity 2 Models

## Activity 2: Heart Disease Risk Dataset (n = 9,000)

## 1. Regression: what moves the heart's working range

### Model selection result

Backward elimination (k set to the 5 % chi-square critical value) removed
triglycerides, alcohol, daily steps and diet quality — 4 of 19 predictors, kept
together by a block F test of F = 2.02 on 4 and 7,177 df (p = 0.089), so the
simpler model is not detectably worse.

On the 1,800 held-out patients the five fits agree almost exactly:

| Model                            | RMSE (bpm) | MAE (bpm) | R² (test) |
| -------------------------------- | ---------: | --------: | --------: |
| Multiple linear regression       |     15.585 |    12.472 |    0.5637 |
| Backward elimination (4 dropped) |     15.573 |    12.463 |    0.5644 |
| Ridge (alpha = 0)                |     15.596 |    12.511 |    0.5631 |
| LASSO (alpha = 1)                |     15.572 |    12.460 |    0.5644 |
| Elastic net (CV alpha = 0.1)     |     15.572 |    12.464 |    0.5644 |

There are minimal difference between prediction results. the R2adj is also near each other, which mean changing of method does not gain any explanatory power.
In such case, we should choose the easiest one to interpret, which is the reduced multiple linear regression model,

The backward-eliminated (reduced) model is the one interpreted here: on held-out
patients it is indistinguishable from the full model and every other fit, so it
is the smaller, easier-to-read model to report. It retains 15 of 19 predictors
and explains 57.1 % of the training variance (adjusted R² = 0.570), with
residual SE 15.75 bpm; 16 of its 18 coefficients are individually significant at
5 % (non-anginal chest pain and former smoking are kept because their dummy
groups are jointly significant). Holding every other variable fixed, the working
range falls with each of:

| Predictor                         | Effect (bpm) |         t | Interpretation (per unit, others fixed)                  |
| --------------------------------- | ------------ | --------: | -------------------------------------------------------- |
| `exercise_induced_anginaYes`      | −5.69        |     −12.4 | Exercise-induced angina costs 5.7 bpm of working range   |
| `smoker_statusCurrent`            | −5.08        |      −9.7 | Current smokers lose 5.1 bpm vs never-smokers            |
| `chest_pain_typeTypical Angina`   | −3.22        |      −5.2 | Typical angina costs 3.2 bpm vs asymptomatic             |
| `st_depression`                   | −1.97        |      −9.8 | Each mV of ST depression costs 2.0 bpm                   |
| `chest_pain_typeAtypical Angina`  | −1.67        |      −3.2 | Atypical angina costs 1.7 bpm                            |
| `hba1c`                           | −1.41        |      −5.0 | Each % of HbA1c costs 1.4 bpm                            |
| `family_historyYes`               | −1.30        |      −3.1 | Family history costs 1.3 bpm                             |
| `sexMale`                         | −1.11        |      −2.9 | Men have 1.1 bpm less range than women                   |
| **`age`**                         | **−1.08**    | **−66.7** | **Each extra year costs 1.08 bpm — the dominant driver** |
| `bmi`                             | −0.37        |      −7.8 | Each BMI point costs 0.37 bpm                            |
| `smoker_statusFormer`             | −0.72        |      −1.7 | Former smoking costs 0.72 bpm (n.s. on its own)          |
| `chest_pain_typeNon-Anginal Pain` | −0.60        |      −1.3 | Non-anginal pain costs 0.60 bpm (n.s. on its own)        |
| `stress_score`                    | −0.09        |      −7.6 | Each stress point costs 0.09 bpm                         |
| `resting_bp_systolic`             | −0.04        |      −2.8 | Each mmHg costs 0.04 bpm                                 |
| `ldl`                             | −0.04        |      −5.5 | Each mg/dL of LDL costs 0.04 bpm                         |
| `sleep_hours`                     | +0.41        |       2.4 | Each hour of sleep adds 0.41 bpm                         |
| `hdl`                             | +0.05        |       2.8 | Each mg/dL of HDL adds 0.05 bpm                          |
| `exercise_minutes_per_week`       | +0.08        |      23.7 | Each weekly exercise minute adds 0.08 bpm                |

Age is the standout: it alone costs 1.08 bpm of working range per year, with a
t-statistic of −66.7. The message of the signs is coherent — everything that
affect the health badly (angina, smoking, ST depression, high HbA1c, family
history) narrows the working range, while sleep and exercise widen it by enhancing the body strength.

### Assumptions held

- Residual normality chi-square 6.55 on 7 df vs a 14.07 critical value — close
  enough to normal for the t and F tests to be read as they stand.
- No predictor has VIF above 5 (largest: resting blood pressure at 1.36), so the
  coefficients can be read individually.

---

## 2. Logistic regression: what raises the odds of heart disease

The predictors together cut the deviance from 8803 to 3445, so they carry a
large amount of diagnostic information. After backward elimination at the 5 %
level, 18 of 21 predictors stay (triglycerides, alcohol and weekly exercise
minutes are dropped together; block X² = 4.51 on 3 df, p = 0.211). Odds ratios
(OR), reported per unit and per one standard deviation so numerically small
predictors can be compared with a dummy's full 0 → 1 step:

| Variable                         | OR per unit | 95 % CI        | OR per SD | Reading                                                 |
| -------------------------------- | ----------- | -------------- | --------: | ------------------------------------------------------- |
| `max_heart_rate_achieved`        | 0.8805      | [0.873, 0.888] |     0.066 | 21 bpm more peak heart rate multiplies the odds by 0.07 |
| `exercise_induced_anginaYes`     | 9.684       | [7.93, 11.83]  |     9.684 | Angina multiplies the odds by 9.7                       |
| `chest_pain_typeTypical Angina`  | 4.788       | [3.64, 6.30]   |     4.788 | Typical angina multiplies the odds by 4.8               |
| `age`                            | 0.9209      | [0.911, 0.931] |     0.342 | Per SD, age multiplies the odds by 0.34                 |
| `smoker_statusCurrent`           | 2.709       | [2.15, 3.42]   |     2.709 | Current smoking multiplies the odds by 2.7              |
| `st_depression`                  | 2.349       | [2.12, 2.60]   |     2.255 | Each mV multiplies the odds by 2.35                     |
| `chest_pain_typeAtypical Angina` | 2.091       | [1.65, 2.65]   |     2.091 | Atypical angina multiplies the odds by 2.1              |
| `ldl`                            | 1.0197      | [1.016, 1.023] |     1.703 | Per SD of LDL, the odds multiply by 1.70                |
| `sexMale`                        | 1.654       | [1.39, 1.98]   |     1.654 | Men's odds are 1.65× women's                            |
| `family_historyYes`              | 1.574       | [1.30, 1.90]   |     1.574 | Family history multiplies the odds by 1.57              |
| `hba1c`                          | 1.738       | [1.52, 1.98]   |     1.467 | Each % of HbA1c multiplies the odds by 1.74             |
| `smoker_statusFormer`            | 1.463       | [1.20, 1.79]   |     1.463 | Quitting limits the excess to 1.46×                     |
| `bmi`                            | 1.051       | [1.03, 1.07]   |     1.241 | Each BMI point multiplies the odds by 1.05              |
| `diet_quality_score`             | 0.9908      | [0.985, 0.997] |     0.875 | A better diet lowers the odds                           |
| `sleep_hours`                    | 0.908       | [0.841, 0.981] |     0.899 | Each hour of sleep multiplies the odds by 0.91          |
| `daily_steps`                    | 1.000       | [1.000, 1.000] |     0.904 | Per ~2170 steps the odds multiply by 0.90               |

The strongest and most telling effects:

- **The exercise test dominates.** Exercise-induced angina multiplies the odds
  by 9.7 and typical angina multiplies by 4.8, while a higher achieved peak heart rate is
  the strongest protective sign: per one SD (21 bpm) the odds fall to 0.07×. A
  heart that can be pushed hard without pain is a healthy heart.
- **Smoking is modifiable.** Current smoking multiplies the odds by 2.71; being
  a former smoker cuts the excess to 1.46 - the effect is largely reversible.
- **The apparent age effect.** The per-SD OR for age is 0.342, i.e. older
  patients appear _less_ likely to have disease once the other predictors are
  held fixed. This is a conditional effect: on
  its own, age is positively associated with the disease (rates rise from ~10 %
  at 18–36 to ~55 % at 72–90). However, since age and achieved peak heart rate are
  strongly correlated (r = −0.73), age's effect is largely carried through the
  exercise-test variables, and the coefficient left over is a suppression
  effect - among patients with the same test result, the older one is
  "healthier" for their age. The data support this: within every peak-heart-rate
  range the disease rate is lower in the older half.

### Discrimination on held-out patients

At the usual cutoff of 0.5 on the 1,800 held-out patients (531 cases):

| Metric                        |     Value |
| ----------------------------- | --------: |
| Accuracy                      |    0.8994 |
| Sensitivity (found cases)     |    0.8192 |
| Specificity (cleared healthy) |    0.9330 |
| Precision (flagged and ill)   |    0.8365 |
| F1                            |    0.8278 |
| **AUC**                       | **0.955** |

An AUC of 0.955 means that for a random ill/healthy pair the model ranks the
ill patient higher 95.5 % of the time — the model discriminates very well. The
asymmetry at 0.5 is sensitivity (0.819) trailing specificity (0.933): it misses
about 1 in 5 true cases while misflagging fewer than 1 in 15 healthy patients.

### The cutoff testing value

Sweeping the cutoff trades sensitivity against specificity and other metrics:

| cutoff | accuracy | sensitivity | specificity | precision | f1     |
| ------ | -------- | ----------- | ----------- | --------- | ------ |
| 0.1    | 0.7944   | 0.9642      | 0.7234      | 0.5933    | 0.7346 |
| 0.2    | 0.8622   | 0.9322      | 0.8329      | 0.7001    | 0.7997 |
| 0.3    | 0.8839   | 0.8945      | 0.8794      | 0.7564    | 0.8197 |
| 0.4    | 0.8956   | 0.8644      | 0.9086      | 0.7983    | 0.8300 |
| 0.5    | 0.8994   | 0.8192      | 0.9330      | 0.8365    | 0.8278 |
| 0.6    | 0.8956   | 0.7646      | 0.9504      | 0.8657    | 0.8120 |
| 0.7    | 0.8861   | 0.6968      | 0.9653      | 0.8937    | 0.7831 |
| 0.8    | 0.8750   | 0.6271      | 0.9787      | 0.9250    | 0.7475 |
| 0.9    | 0.8517   | 0.5254      | 0.9882      | 0.9490    | 0.6764 |

For disease screening, 0.2 is a sweet spot: It balance between the cost of false negative and other metrics, where given a patient, the probability thta the model correctly predict them as having disease is 93.22%, while keeping the other metrics high

---

## 3. Conclusions and recommendations

1. The regression is a measurement model — it
   predicts how wide a heart's working range is (±15.6 bpm), useful for
   understanding what changes it and how much it changes. The logistic model is a screening model —
   it sorts patients by the odds of disease, and does so with AUC 0.955.
2. The factor that affects the health is consistent in both model when taking the interaction into account.
   Angina, smoking, ST depression, high HbA1c, family history and male sex all narrow the working
   range _and_ raise the odds of disease; sleep, exercise and HDL do the
   opposite. The exercise stress test (angina + achieved peak heart rate) is by
   far the strongest signal in both.
3. Cutoff value should not be defaulted to 0.5. Instead, in such health model, we need to balance the cost of false negative and the other accuracy, precision, f1, etc. metrics. A 0.2 cutoff is a good cutoff for that. Higher cutoff trades sensitivity for specificity and precision.
4. The bad effect on health can be remedied with good habit: Quit smoking, exercise, sleep and diet help reduces the odds. In addition, their effect extends to other health statistic, reducing the odds even more

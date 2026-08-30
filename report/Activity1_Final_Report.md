# BÁO CÁO KHOA HỌC FINAL PROJECT: ACTIVITY 1
# DATA SCIENCE JOB SALARIES STATISTICAL ANALYSIS
**Học phần:** Applied Statistics for Engineers and Scientists II (STAT452)  
**Thời gian hoàn thành:** August 30, 2026  
**Ngôn ngữ & Môi trường thực hiện:** R / RStudio (ver 4.6.0)  
**Nhóm tác giả & Phân công nhiệm vụ:**
- **Phat:** Data Preprocessing, Continuous Regression Modeling & Optimization (Step 4), Model Evaluation (Step 6), Interpretation & Recommendations (Step 7).
- **Bao:** Outlier Handling, Model formulas, Logistic Regression Modeling (Step 5).
- **Dung:** EDA & Visualization (Step 2), Factorial ANOVA (Step 3), Report Integration.

---

## MỤC LỤC
1. [GIỚI THIỆU ĐỀ TÀI & CÂU HỎI NGHIÊN CỨU](#1-giới-thiệu-đề-tài--câu-hỏi-nghiên-cứu)
2. [STEP 1: TIỀN XỬ LÝ DỮ LIỆU (DATA PREPROCESSING)](#2-step-1-tiền-xử-lý-dữ-liệu-data-preprocessing)
3. [STEP 2: PHÂN TÍCH KHÁM PHÁ DỮ LIỆU (EDA)](#3-step-2-phân-tích-khám-phá-dữ-liệu-eda)
4. [STEP 3: THỐNG KÊ SUY DIỄN & FACTORIAL ANOVA](#4-step-3-thống-kê-suy-diễn--factorial-anova)
5. [STEP 4: MÔ HÌNH HỒI QUY TUYẾN TÍNH LIÊN TỤC (CONTINUOUS REGRESSION)](#5-step-4-mô-hình-hồi-quy-tuyến-tính-liên-tục-continuous-regression)
6. [STEP 5: MÔ HÌNH HỒI QUY LOGISTIC NHỊ PHÂN (BINARY LOGISTIC REGRESSION)](#6-step-5-mô-hình-hồi-quy-logistic-nhị-phân-binary-logistic-regression)
7. [STEP 6: ĐÁNH GIÁ VÀ SO SÁNH CÁC MÔ HÌNH (MODEL EVALUATION)](#7-step-6-đánh-giá-và-so-sánh-các-mô-hình-model-evaluation)
8. [STEP 7: DIỄN GIẢI KẾT QUẢ & ĐỀ XUẤT (RECOMMENDATIONS)](#8-step-7-diễn-giải-kết-quả--đề-xuất-recommendations)

---

## 1. GIỚI THIỆU ĐỀ TÀI & CÂU HỎI NGHIÊN CỨU

### 1.1. Bối cảnh đề tài
Thị trường nhân lực Khoa học Dữ liệu (Data Science) toàn cầu đang chứng kiến sự phân hóa thu nhập mạnh mẽ dưới tác động của nhiều nhân tố: cấp bậc kinh nghiệm, vị trí địa lý, quy mô doanh nghiệp, tính chất vai trò quản lý (leadership) và chuyên môn kỹ thuật.

Nghiên cứu này khai thác bộ dữ liệu `ds_salaries.csv` từ Kaggle gồm 607 quan sát và 11 biến nguyên bản, áp dụng các phương pháp thống kê suy diễn và mô hình học có giám sát để giải quyết trọn vẹn **03 câu hỏi nghiên cứu (Research Questions)** của học phần STAT452:
1. **Salary Forecasting & Key Drivers Identification:** Mức độ chính xác khi dự báo mức lương liên tục (`salary_in_usd`) là bao nhiêu, và những yếu tố nào đóng vai trò then chốt quyết định mức thu nhập?
2. **Impact & Multi-Factor Interaction Analysis:** Có sự khác biệt có ý nghĩa thống kê về mức lương giữa các cấp bậc kinh nghiệm, quy mô công ty, vai trò quản lý và vị trí địa lý không? Các yếu tố này tác động độc lập hay có hiệu ứng tương tác đa chiều?
3. **Top 25% Income Probability Prediction (Top-tier):** Những đặc tính nào làm tăng hoặc giảm cơ hội lọt vào nhóm 25% thu nhập cao nhất ($Q_3 = \$150,000$), và mô hình Binary Logistic Regression đạt hiệu năng phân loại như thế nào?

---

## 2. STEP 1: TIỀN XỬ LÝ DỮ LIỆU (DATA PREPROCESSING)

### 2.1. Làm sạch và chuẩn hóa biến số
* **Tập dữ liệu ban đầu:** 607 quan sát, 11 biến.
* **Loại bỏ cột dư thừa:** Loại bỏ cột chỉ mục `X` và các cột `salary`, `salary_currency` (vì đã có biến chuẩn hóa `salary_in_usd`).
* **Lưu giữ biến gốc:** Lưu `observed_salary_in_usd` làm biến mục tiêu thực tế nhằm đối chiếu và ngăn ngừa hiện tượng rò rỉ dữ liệu (target leakage).
* **Kiểm tra giá trị khuyết thiếu:** Không có bản ghi nào bị thiếu (`sum(is.na(data)) == 0`).

### 2.2. Kỹ thuật kỹ nghệ đặc trưng (Feature Engineering)
1. **Gom nhóm Chức danh công việc (`job_title` $\rightarrow$ `role` & `leadership`):**
   * Sử dụng biểu thức chính quy (regex) để tách bạch giữa **chuyên môn kỹ thuật (`role`)** và **vai trò quản lý (`leadership`)**:
     - `role`: **Analyst** (127 bản ghi), **Engineering** (248 bản ghi), **Research** (225 bản ghi).
     - `leadership`: **No** (536 bản ghi - Individual Contributor), **Yes** (64 bản ghi - Lead/Manager/Director/Principal/Head/Staff).
   * 7 bản ghi không thuộc 3 chuyên môn trên được loại bỏ, tập dữ liệu sạch còn lại **600 quan sát**.
2. **Phân vùng Địa lý (`company_location` & `employee_residence`):**
   * Sử dụng thư viện `countrycode` chuyển đổi mã quốc gia ISO2 sang châu lục: **America** (391 công ty), **Europe** (154 công ty), **Asia** (46 công ty), **Other** (9 công ty).
3. **Chuẩn hóa biến thứ bậc (Ordinal Factors):**
   * `experience_level`: Entry (88), Mid-level (212), Senior (277), Executive (23).
   * `company_size`: Small (82), Medium (325), Large (193).
4. **Xử lý Outlier bằng IQR Winsorization:**
   * $Q_1 = \$62,707, \quad Q_3 = \$150,000, \quad \text{IQR} = \$87,293$.
   * $\text{Upper Bound} = Q_3 + 1.5 \times \text{IQR} = \$280,911$.
   * Các giá trị ngoại lai trên $\$280,911$ được cắt ngưỡng (Winsorized) để ổn định phương sai cho các bước thống kê mô tả tiếp theo.
   * Dữ liệu sạch hoàn chỉnh được lưu trữ tại `activity1/dataset/cleaned_data.rds`.

---

## 3. STEP 2: PHÂN TÍCH KHÁM PHÁ DỮ LIỆU (EDA)

### 3.1. Thống kê mô tả phân phối mức lương
* **Trung bình (Mean):** $\$109,653$
* **Trung vị (Median):** $\$100,400$
* **Độ lệch chuẩn (Std Dev):** $\$57,842$
* **Khoảng giá trị:** $\$2,859$ đến $\$280,911$
* **Hệ số bất đối xứng (Skewness):** 
  - Phân phối mức lương gốc bị **lệch phải rất mạnh** ($\text{Skewness} = 16.80$, kiểm định Shapiro-Wilk $p < 2.2 \times 10^{-16}$).
  - Phép biến đổi căn bậc hai $\sqrt{Y}$ kéo hệ số bất đối xứng về **$0.19$** (phân phối tiệm cận đối xứng), giúp chuẩn hóa dữ liệu cho các phân tích suy diễn.

### 3.2. Mối quan hệ giữa mức lương và các nhân tố độc lập
* **Theo Cấp bậc kinh nghiệm (`experience_level`):** Mức lương tăng trưởng đơn điệu và lũy tiến: $\text{Entry} \approx \$55\text{k} \rightarrow \text{Mid-level} \approx \$82\text{k} \rightarrow \text{Senior} \approx \$138\text{k} \rightarrow \text{Executive} \approx \$175\text{k}$.
* **Theo Vị trí địa lý (`company_location`):** Doanh nghiệp tại châu Mỹ (America/US) chi trả mức lương áp đảo so với châu Âu (Europe) và châu Á (Asia).
* **Theo Chuyên môn & Vai trò quản lý:** Nhóm Engineering và Research có phân vị $Q_3$ vượt trội so với Analyst; nhóm có vai trò Leadership nhận mức lương trung bình cao hơn rõ rệt.
* **Theo Quy mô công ty (`company_size`):** Doanh nghiệp vừa ($M$) và lớn ($L$) chi trả cao hơn hẳn doanh nghiệp nhỏ ($S$).
* **Theo Xu hướng năm làm việc (`work_year`):** Mặt bằng lương năm 2022 tăng trưởng vượt bậc so với 2020 và 2021.

---

## 4. STEP 3: THỐNG KÊ SUY DIỄN & FACTORIAL ANOVA

### 4.1. Phân tích ANOVA đa nhân tố (Multi-Factor ANOVA)
Mô hình toán học:
$$Y_{ijk} = \mu + \alpha_i + \beta_j + (\alpha\beta)_{ij} + \epsilon_{ijk}$$

#### Bảng tổng hợp các cặp nhân tố phân tích chính (trên biến đổi $\sqrt{\text{Salary}}$):
| Cặp nhân tố phân tích | Hiệu ứng chính Yếu tố 1 | Hiệu ứng chính Yếu tố 2 | Hiệu ứng tương tác ($F$-test) | $p$-value tương tác | Kết luận thống kê |
|---|---|---|---|---|---|
| **`company_size` $\times$ `leadership`** | $F = 21.05, p < 10^{-8}$ | $F = 31.93, p < 10^{-8}$ | $F(2, 594) = 0.547$ | $0.579$ | Tác động độc lập |
| **`role` $\times$ `leadership`** | $F = 1.76, p = 0.172$ | $F = 29.21, p < 10^{-7}$ | $F(2, 594) = 0.365$ | $0.694$ | Tác động độc lập |
| **`company_location` $\times$ `role`** | $F = 46.21, p < 10^{-16}$ | $F = 8.76, p = 0.00017$ | $F(6, 588) = 2.451$ | **$0.0238$ (*)** | **Tương tác có ý nghĩa** |
| **`work_year` $\times$ `leadership`** | $F = 4.28, p = 0.014$ | $F = 31.93, p < 10^{-8}$ | $F(2, 594) = 2.958$ | $0.0527$ (.) | Tương tác biên |

### 4.2. Kiểm tra các giả thiết ANOVA & Biện pháp khắc phục
1. **Kiểm định tính chuẩn (Shapiro-Wilk):** Phần dư trên thang đo $\sqrt{Y}$ đạt $W = 0.9877, p = 6.03 \times 10^{-5}$.
2. **Kiểm định phương sai đồng nhất (Breusch-Pagan / Levene):** Tại các mô hình có hiện tượng phương sai thay đổi ($p < 0.05$), nhóm áp dụng **Hiệu chỉnh sai số chuẩn vững HC3 (White's Heteroscedasticity-Consistent Standard Errors)** kết hợp ANOVA Type III để đảm bảo độ tin cậy của kiểm định giả thuyết.
3. **Phân tích hậu nghiệm (Tukey HSD Post-hoc / Simple Effects via `emmeans`):**
   * Giữa các quy mô: Công ty $M$ và $L$ trả lương cao hơn công ty $S$ có ý nghĩa thống kê ($p < 0.01$), nhưng giữa $M$ và $L$ không có sự khác biệt đáng kể ($p > 0.65$).
   * Giữa các châu lục: Mức lương tại America cao hơn có ý nghĩa thống kê so với Europe ($p < 0.0001$) và Asia ($p < 0.0001$).

---

## 5. STEP 4: MÔ HÌNH HỒI QUY TUYẾN TÍNH LIÊN TỤC (CONTINUOUS REGRESSION)

### 5.1. Phân chia dữ liệu Huấn luyện & Kiểm thử (Train/Test Split)
* Phương pháp phân chia: **80% Training Set (480 quan sát)** và **20% Testing Set (120 quan sát)** với `set.seed(6767)`.

### 5.2. Mô hình Hồi quy Tuyến tính Đa biến Đầy đủ (Full MLR)
$$Y = \beta_0 + \beta_1 X_1 + \dots + \beta_k X_k + \epsilon$$

* **Multiple $R^2$:** **0.5286** ($52.86\%$)
* **Adjusted $R^2$:** **0.5144** ($51.44\%$)
* **Kiểm định F tổng thể:** $F(14, 465) = 37.24, \quad p < 2.2 \times 10^{-16}$
* **Residual Standard Error (RSE):** **$\$43,970$**
* **Kiểm tra Generalized VIF:** Tất cả $\text{GVIF}^{1/(2\cdot\text{Df})} < 1.30$ (không có hiện tượng đa cộng tuyến).

### 5.3. Lựa chọn mô hình tối ưu theo Tiêu chí Mallow's $C_p$ & Stepwise AIC
Áp dụng thuật toán Best Subset Selection (`leaps::regsubsets`):
* Mallow's $C_p$ đạt giá trị tối ưu nhỏ nhất tại mô hình gồm **11 biến giải thích** ($C_p = 12.18 \approx p = 12$, Adjusted $R^2 = 0.5142$, $\text{BIC} = -286.17$).
* **Đồng thuận 100% với Backward Stepwise AIC:** Cả 2 phương pháp độc lập đều thống nhất chọn đúng 11 biến có ý nghĩa cao nhất và loại bỏ `work_year` ($p = 0.348$) và `remote_ratio` ($p = 0.223$).

#### Bảng thông số chi tiết của mô hình OLS tối ưu (Stepwise MLR / Mallow's $C_p$):
| Biến / Hệ số | Ước lượng ($\hat{\beta}$) | Sai số chuẩn (Std. Error) | Giá trị $t$ | $p$-value | Mức ý nghĩa |
|---|---|---|---|---|---|
| **(Intercept)** | +\$57,925 | \$8,452 | 6.853 | $2.29 \times 10^{-11}$ | *** |
| **`experience_levelMid-level`** | +\$16,227 | \$6,388 | 2.540 | **0.0114** | * |
| **`experience_levelSenior`** | +\$44,879 | \$6,758 | 6.640 | **$8.71 \times 10^{-11}$** | *** |
| **`experience_levelExecutive`** | +\$83,619 | \$12,360 | 6.766 | **$3.99 \times 10^{-11}$** | *** |
| **`employment_typeOther`** | -\$20,587 | \$11,146 | -1.847 | 0.0654 | . |
| **`company_locationAsia`** | **-\$71,344** | **\$8,235** | **-8.663** | **$< 2.0 \times 10^{-16}$** | *** |
| **`company_locationEurope`** | **-\$59,765** | **\$5,114** | **-11.687** | **$< 2.0 \times 10^{-16}$** | *** |
| **`company_locationOther`** | -\$17,018 | \$18,918 | -0.900 | 0.3688 | |
| **`company_sizeMedium`** | +\$15,507 | \$6,384 | 2.429 | **0.0155** | * |
| **`company_sizeLarge`** | +\$23,451 | \$6,649 | 3.527 | **0.00046** | *** |
| **`roleEngineering`** | +\$34,731 | \$5,527 | 6.284 | **$7.59 \times 10^{-10}$** | *** |
| **`roleResearch`** | +\$31,976 | \$5,673 | 5.637 | **$3.00 \times 10^{-8}$** | *** |
| **`leadershipYes`** | +\$22,892 | \$6,961 | 3.288 | **0.00108** | ** |

* **Stepwise Multiple $R^2$:** **0.5262** ($52.62\%$)
* **Stepwise Adjusted $R^2$:** **0.5140** ($51.40\%$)
* **Stepwise RSE:** **$\$43,990$**

### 5.4. Mô hình Hồi quy Đa thức (Polynomial Regression Degree 2)
Thêm thành phần đa thức bậc 2 cho biến liên tục `remote_ratio`:
$$\text{salary\_in\_usd} = \beta_0 + \sum \beta_j X_j + \gamma_1 \cdot \text{remote\_ratio} + \gamma_2 \cdot \text{remote\_ratio}^2 + \epsilon$$
* **Multiple $R^2$:** **0.5296** ($52.96\%$)
* **Adjusted $R^2$:** **0.5144** ($51.44\%$)
* Trên tập Test: $\text{RMSE} = \$46,859.64, R^2 = 0.3187$ (cải thiện nhẹ so với mô hình tuyến tính $R^2 = 0.3153$).

### 5.5. Hồi quy Phạt & Grid Search Siêu tham số Elastic Net ($\alpha \in [0, 1]$)
* **Ridge Regression ($\alpha = 0$):** $\lambda_{\min} = 2696.415, \lambda_{1\text{se}} = 20877.36$.
* **LASSO Regression ($\alpha = 1$):** $\lambda_{\min} = 63.7564, \lambda_{1\text{se}} = 4194.748$.
* **Grid Search Elastic Net ($\alpha \in [0.0, 1.0]$ với $\Delta\alpha = 0.05$):**
  - Quét 21 giá trị $\alpha$ qua 10-fold CV trên tập Train, tìm ra siêu tham số tối ưu tại **$\alpha^* = 0.55$** ($\lambda_{\min} = 96.2395, \text{CV-MSE} = 1.986 \times 10^9$).
  - Giá trị tiêu chuẩn $\alpha = 0.50$ ($\lambda_{\min} = 96.4588$).

### 5.6. Chẩn đoán các giả thiết hồi quy OLS (Regression Assumptions)
1. **Tính Chuẩn của phần dư:** Shapiro-Wilk $W = 0.9739, p = 1.47 \times 10^{-7}$ (lệch đuôi phải do nhóm chuyên gia thu nhập cao).
2. **Tính Đồng nhất phương sai:** Breusch-Pagan $BP = 44.501, p = 1.25 \times 10^{-5}$.
3. **Tính Độc lập phần dư:** Durbin-Watson $DW = 1.8408, p = 0.076 > 0.05$ (**thỏa mãn giả thiết không tự tương quan**).
4. File đồ thị chẩn đoán PDF lưu tại: `activity1/step4/mlr_diagnostics.pdf`.

---

## 6. STEP 5: MÔ HÌNH HỒI QUY LOGISTIC NHỊ PHÂN (BINARY LOGISTIC REGRESSION)

### 6.1. Thiết lập bài toán phân loại Top 25% thu nhập
* Xác định phân vị $Q_3$: **$\$150,000$**.
* Biến mục tiêu: $Y = 1$ nếu $\text{salary\_in\_usd} \ge \$150,000$ (146 bản ghi, $24.33\%$), $Y = 0$ nếu ngược lại (454 bản ghi, $75.67\%$).

### 6.2. Ước lượng mô hình Logit & Odds Ratios
Mô hình tối ưu sau khi loại bỏ `employment_type` ($AIC = 492.14$):
$$\ln\left(\frac{P(Y=1)}{1 - P(Y=1)}\right) = \beta_0 + \sum \beta_i X_i$$

#### Bảng kết quả ước lượng và Odds Ratios ($\exp(\hat{\beta})$):
| Biến / Nhân tố | Ước lượng ($\hat{\beta}$) | Sai số chuẩn | $z$-value | $p$-value | **Odds Ratio ($e^{\hat{\beta}}$)** | **95% Wald CI** |
|---|---|---|---|---|---|---|
| **(Intercept)** | $-1.9047$ | $0.3869$ | -4.923 | $8.51 \times 10^{-7}$ *** | $0.1489$ | $[0.070, 0.318]$ |
| **`experience_level.L` (Seniority)** | $+2.2951$ | $0.5654$ | 4.059 | **$4.93 \times 10^{-5}$ *** | **9.9253** | **$[3.277, 30.064]$** |
| **`leadership.L` (Management)** | $+0.9686$ | $0.2634$ | 3.677 | **$0.00024$ *** | **2.6342** | **$[1.572, 4.414]$** |
| **`roleEngineering`** | $+1.8923$ | $0.3523$ | 5.372 | **$7.80 \times 10^{-8}$ *** | **6.6346** | **$[3.326, 13.233]$** |
| **`roleResearch`** | $+1.7668$ | $0.3570$ | 4.949 | **$7.46 \times 10^{-7}$ *** | **5.8520** | **$[2.907, 11.781]$** |
| **`company_size.L`** | $+0.9878$ | $0.3574$ | 2.764 | **$0.00572$ ** | **2.6853** | **$[1.333, 5.410]$** |
| **`company_locationAsia`** | $-2.4876$ | $0.7991$ | -3.113 | **$0.00185$ ** | **0.0831** | $[0.017, 0.398]$ |
| **`company_locationEurope`** | $-3.1830$ | $0.5334$ | -5.968 | **$2.40 \times 10^{-9}$ *** | **0.0415** | $[0.015, 0.118]$ |
| **`company_locationOther`** | $-0.7184$ | $1.2179$ | -0.590 | 0.55528 | $0.4875$ | $[0.045, 5.305]$ |

---

## 7. STEP 6: ĐÁNH GIÁ VÀ SO SÁNH CÁC MÔ HÌNH (MODEL EVALUATION)

### 7.1. Đánh giá Toàn diện các Mô hình Hồi quy Liên tục trên Test Set ($N = 120$ quan sát)

#### Bảng so sánh hiệu năng 6 mô hình Hồi quy trên tập kiểm thử độc lập:
| Thứ tự | Mô hình (Model) | Siêu tham số | RMSE ($) | MAE ($) | $R^2$ (Test Set) | Xếp hạng & Đánh giá |
|---|---|---|---|---|---|---|
| **1** | **3. Ridge Regression** | $\alpha = 0, \lambda_{\min} = 2696.42$ | **$46,435.13** | **$33,980.78** | **0.3310** | 🥇 **Best Model (RMSE thấp nhất, $R^2$ cao nhất)** |
| **2** | **2. Polynomial MLR** | Degree 2 (`remote_ratio`) | **$46,859.64** | **$34,614.12** | **0.3187** | 🥈 Cải thiện phi tuyến so với Linear OLS |
| **3** | **5. Elastic Net ($\alpha = 0.5$)** | $\alpha = 0.50, \lambda_{\min} = 96.46$ | **$46,878.43** | **$34,447.76** | **0.3182** | 🥉 Cân bằng tốt $L_1$ và $L_2$ |
| **4** | **6. Optimal Elastic Net** | $\alpha = 0.55, \lambda_{\min} = 96.24$ | **$46,880.99** | **$34,450.30** | **0.3181** | Tối ưu hóa từ Grid Search |
| **5** | **4. LASSO Regression** | $\alpha = 1, \lambda_{\min} = 63.76$ | **$46,883.71** | **$34,453.33** | **0.3180** | Co hệ số và lọc biến |
| **6** | **1. MLR (Stepwise / $C_p$)** | 11 biến tối ưu | **$46,977.65** | **$34,850.39** | **0.3153** | Baseline OLS chuẩn |

* **Lựa chọn mô hình hồi quy tốt nhất:** **Ridge Regression** đạt hiệu năng dự báo cao nhất nhờ kiểm soát phương sai sai số tốt nhất khi đối mặt với các giá trị ngoại lai trên tập kiểm thử.

---

### 7.2. Đánh giá Mô hình Hồi quy Logistic Nhị phân (Binary Classification)

#### Bảng ma trận nhầm lẫn (Confusion Matrix) với ngưỡng $s = 0.50$:
| | Thực tế: Không thuộc Top 25% ($Y=0$) | Thực tế: Thuộc Top 25% ($Y=1$) | Tổng dự đoán |
|---|---|---|---|
| **Dự đoán: Không lọt Top ($0$)** | **389 (TN)** | 55 (FN) | 444 |
| **Dự đoán: Lọt Top 25% ($1$)** | 65 (FP) | **91 (TP)** | 156 |
| **Tổng thực tế** | 454 | 146 | 600 |

#### Các chỉ số hiệu năng phân loại:
* **Độ chính xác tổng thể (Accuracy):** **$80.00\%$** (95% CI: $[76.57\%, 83.14\%]$)
* **Độ chuẩn xác (Precision):** **$62.33\%$**
* **Độ nhạy / Thu hồi (Recall / Sensitivity):** **$58.33\%$**
* **Điểm F1-Score:** **$60.26\%$**
* **Diện tích dưới đường cong ROC (AUC):** **$0.870$** (Khả năng phân loại **Xuất sắc - Excellent Discrimination**).

---

## 8. STEP 7: DIỄN GIẢI KẾT QUẢ & ĐỀ XUẤT (RECOMMENDATIONS)

### 8.1. Trả lời trọn vẹn 03 Câu hỏi nghiên cứu (Research Questions)

#### 📌 Câu hỏi 1: Dự báo mức lương và các nhân tố cốt lõi
* **Mức độ chính xác dự báo:** Mô hình Ridge Regression dự báo mức lương với sai số trung bình tuyệt đối $\text{MAE} = \$33,980$ và giải thích được $33.10\%$ biến thiên trên tập kiểm thử độc lập. Mô hình Đa thức (Polynomial) và Mallow's $C_p$ xác nhận mối quan hệ vững chắc giữa các biến.
* **Thứ tự các nhân tố quyết định thu nhập:**
  1. **Vị trí công ty (`company_location`):** Doanh nghiệp tại Mỹ trả lương cao hơn vượt bậc so với Châu Âu ($+\$59,765$) và Châu Á ($+\$71,344$) với $p < 10^{-16}$.
  2. **Cấp bậc kinh nghiệm (`experience_level`):** Cấp bậc Executive nhận thêm $+\$83,619$, Senior nhận thêm $+\$44,879$, Mid-level nhận thêm $+\$16,227$ so với Entry-level ($p < 10^{-10}$).
  3. **Chuyên môn kỹ thuật (`role`):** Vị trí Engineering ($+\$34,731$) và Research ($+\$31,976$) có mức lương vượt trội so với Analyst ($p < 10^{-7}$).
  4. **Vai trò quản lý (`leadership`):** Nhân sự có trọng trách Leader/Manager nhận thêm $+\$22,892$ ($p = 0.001$).
  5. **Quy mô công ty (`company_size`):** Công ty lớn trả cao hơn $+\$23,451$ so với công ty nhỏ ($p < 0.001$).

#### 📌 Câu hỏi 2: Tác động và Hiệu ứng tương tác đa nhân tố
* Mức lương chịu sự chi phối mạnh mẽ bởi hiệu ứng chính của cả 4 yếu tố: Kinh nghiệm ($F=99.95$), Địa lý ($F=48.64$), Chức danh ($F=20.89$) và Quy mô ($F=8.82$) với $p < 0.001$.
* Hiệu ứng tương tác giữa **Vị trí địa lý và Chức danh công việc** có ý nghĩa thống kê ($p = 0.0238$).

#### 📌 Câu hỏi 3: Xác suất lọt Top 25% thu nhập cao nhất ($Q_3 = \$150,000$)
* Mô hình Logistic đạt **Accuracy $80.0\%$** và **$\text{AUC} = 0.87$**.
* Cấp bậc Seniority tăng tỷ số chênh lên **$9.93$ lần**, vị trí Engineering tăng **$6.63$ lần**, Research tăng **$5.85$ lần**, Leadership tăng **$2.63$ lần**.

---

### 8.2. Đề xuất thực tiễn (Actionable Recommendations)

1. **Đối với Nhân sự Khoa học Dữ liệu (Career Planning):**
   * Ưu tiên hướng kỹ thuật chuyên sâu (Engineering & Research) để tối ưu hóa thu nhập.
   * Tận dụng cơ hội làm việc từ xa cho doanh nghiệp Mỹ để hưởng chênh lệch lương địa lý ($+\$60\text{k}-\$70\text{k}$).
   * Đảm nhận vai trò Quản lý (Leadership) để tăng thêm $2.63$ lần cơ hội lọt Top 25% thu nhập.
2. **Đối với Nhà tuyển dụng & Quản lý Nhân sự (HR Strategy):**
   * Xây dựng chính sách đãi ngộ linh hoạt theo khu vực và lộ trình thăng tiến rõ ràng từ Mid lên Senior ($+\$28,650$) nhằm giảm tỷ lệ nhảy việc.
3. **Đối với Nghiên cứu & Thu thập dữ liệu Thống kê trong tương lai:**
   * Bổ sung chỉ số ngang giá sức mua (PPP) và thu thập thông tin kỹ năng công nghệ cụ thể (Tech Stacks).

---
*Báo cáo được hoàn thiện và kiểm thử thực nghiệm tự động trên toàn bộ chuỗi mã nguồn R trong thư mục `stat452-finals/activity1/`.*

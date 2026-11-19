# =============================================================================
# 1.8 Modelo de riesgos proporcionales de Cox y verificación de supuestos
# Objetivo: estimar razones de riesgo ajustadas y revisar el modelo con sus
#           residuos: Schoenfeld (proporcionalidad), martingala (forma
#           funcional), devianza (atípicos) y dfbeta (influencia).
# Datos:    "Breast Cancer Gene Expression Profiles (METABRIC)" (Kaggle),
#           datos/Breast Cancer.csv
# =============================================================================

library(survival)
library(survminer)  # ggcoxzph(), ggcoxfunctional(), ggcoxdiagnostics()

datos <- read.csv("datos/Breast Cancer.csv", na.strings = c("", "NA"))
datos <- subset(datos, Overall.Survival..Months. > 0 & !is.na(Patient.s.Vital.Status))  # 0 meses: sin seguimiento
datos$tiempo <- datos$Overall.Survival..Months.
datos$evento <- as.integer(datos$Patient.s.Vital.Status == "Died of Disease")  # otras causas: censura

# Casos completos en las variables del modelo
datos <- subset(datos, !is.na(HER2.Status) & !is.na(Tumor.Size) & !is.na(Lymph.nodes.examined.positive))

# --- 1. Kaplan-Meier por grupos, como exploración ----------------------------
ggsurvplot(survfit(Surv(tiempo, evento) ~ Type.of.Breast.Surgery, data = datos), data = datos,
           pval = TRUE, xlab = "Meses", legend.title = "Cirugía", ggtheme = theme_bw())

# --- 2. Modelo de Cox ----------------------------------------------------------
cox_1 <- coxph(Surv(tiempo, evento) ~ HER2.Status + Tumor.Size + Lymph.nodes.examined.positive, data = datos)
summary(cox_1)
# exp(coef) es la razón de riesgos: cuántas veces cambia el riesgo instantáneo
# de morir por la enfermedad por unidad de la variable, con las demás fijas.

# --- 3. Proporcionalidad: residuos de Schoenfeld --------------------------------
# H0: el efecto de cada variable es constante en el tiempo
riesgos <- cox.zph(cox_1)
riesgos
ggcoxzph(riesgos)

# --- 4. Forma funcional: residuos de martingala -------------------------------
# Para variables continuas: si el efecto es lineal en el log-riesgo, la curva es plana
ggcoxfunctional(Surv(tiempo, evento) ~ Tumor.Size + log1p(Tumor.Size) +
                  Lymph.nodes.examined.positive, data = datos)

# --- 5. Atípicos e influencia ------------------------------------------------------
# Devianza: positivos grandes = murió antes de lo que predice el modelo;
#           negativos grandes = vivió más de lo esperado
ggcoxdiagnostics(cox_1, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())
# dfbeta: cuánto cambia cada coeficiente al quitar cada observación
ggcoxdiagnostics(cox_1, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())

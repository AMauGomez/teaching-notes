# =============================================================================
# 1.5 Kaplan-Meier · 1.6 modelos paramétricos · 1.7 prueba log-rank
# Objetivo: estimar la función de supervivencia de pacientes con cáncer de mama,
#           compararla con modelos paramétricos y probar si difiere entre grupos.
# Datos:    "Breast Cancer Gene Expression Profiles (METABRIC)" (Kaggle),
#           datos/Breast Cancer.csv
# =============================================================================

library(survival)
library(survminer)  # ggsurvplot()
library(flexsurv)   # modelos paramétricos

datos <- read.csv("datos/Breast Cancer.csv", na.strings = c("", "NA"))

# Tiempo: meses de seguimiento. Evento: muerte por la enfermedad.
# Quienes siguen vivos están censurados. Quienes murieron por otra causa también
# se tratan como censurados aquí; eso supone que esa otra causa no se relaciona
# con el riesgo de morir por cáncer (si no, habría que usar riesgos competitivos).
datos <- subset(datos, Overall.Survival..Months. > 0 & !is.na(Patient.s.Vital.Status))  # 0 meses: sin seguimiento
datos$tiempo <- datos$Overall.Survival..Months.
datos$evento <- as.integer(datos$Patient.s.Vital.Status == "Died of Disease")
table(datos$Patient.s.Vital.Status)

# --- 1. Kaplan-Meier ---------------------------------------------------------
km <- survfit(Surv(tiempo, evento) ~ 1, data = datos)
km                                   # mediana de supervivencia e intervalo
summary(km, times = c(60, 120, 180)) # supervivencia a 5, 10 y 15 años
ggsurvplot(km, data = datos, conf.int = TRUE, risk.table = TRUE,
           xlab = "Meses", ylab = "Probabilidad de supervivencia", ggtheme = theme_bw())

# --- 2. Modelos paramétricos: ¿qué distribución describe los tiempos? --------
param <- lapply(c(exponencial = "exp", weibull = "weibull", lognormal = "lnorm", loglogistica = "llogis"),
                function(d) flexsurvreg(Surv(tiempo, evento) ~ 1, data = datos, dist = d))
sapply(param, AIC)                   # menor AIC = mejor ajuste
plot(param$weibull, xlab = "Meses", ylab = "Supervivencia",
     main = "Kaplan-Meier (negro) y Weibull (rojo)")
lines(param$exponencial, col = "blue", ci = FALSE)
legend("topright", c("Weibull", "Exponencial"), col = c("red", "blue"), lty = 1, bty = "n")

# --- 3. Prueba log-rank: ¿difiere la supervivencia entre grupos? -------------
# Tiene sentido para variables categóricas con pocos grupos y suficientes eventos
survdiff(Surv(tiempo, evento) ~ Cellularity, data = datos)
survdiff(Surv(tiempo, evento) ~ Chemotherapy, data = datos)

# Una variable continuo, como el tamaño del tumor, no se usa directamente:
# se agrupa primero (o se modela con Cox, en 1-8-cox.R)
datos$tamano <- cut(datos$Tumor.Size, c(0, 20, 50, Inf), labels = c("<=20 mm", "21-50 mm", ">50 mm"))
survdiff(Surv(tiempo, evento) ~ tamano, data = datos)

ajuste <- survfit(Surv(tiempo, evento) ~ tamano, data = datos)
ggsurvplot(ajuste, data = datos, pval = TRUE, conf.int = TRUE, risk.table = TRUE,
           surv.median.line = "hv", xlab = "Meses", legend.title = "Tamaño del tumor",
           ggtheme = theme_bw())

# =============================================================================
# 2.1 Panorama de la regresión: por qué graficar antes de ajustar (Datasaurus)
# Objetivo: 13 conjuntos con casi la misma media, varianza, correlación y recta
#           de regresión, pero formas completamente distintas. Se calcula a mano
#           la descomposición SCT = SCR + SCE y el R^2, y se revisan los
#           supuestos de los residuos.
# Datos:    paquete datasauRus (Matejka y Fitzmaurice, 2017); no requiere descarga.
# =============================================================================

library(datasauRus)
library(ggplot2)
library(lmtest)   # bptest(), dwtest()

data("datasaurus_dozen")
datos <- datasaurus_dozen

# --- 1. Estadísticas resumen por conjunto: casi idénticas ---------------------
resumen <- do.call(rbind, lapply(split(datos, datos$dataset), function(d) {
  fit <- lm(y ~ x, data = d)
  y_hat <- fitted(fit)
  sct <- sum((d$y - mean(d$y))^2)        # suma de cuadrados total
  sce <- sum((d$y - y_hat)^2)            # suma de cuadrados del error
  scr <- sum((y_hat - mean(d$y))^2)      # suma de cuadrados de la regresión
  data.frame(conjunto = d$dataset[1], media_x = mean(d$x), media_y = mean(d$y),
             sd_x = sd(d$x), sd_y = sd(d$y), correlacion = cor(d$x, d$y),
             b0 = unname(coef(fit)[1]), b1 = unname(coef(fit)[2]),
             R2_a_mano = scr / sct, R2_lm = summary(fit)$r.squared,
             SCT_igual_SCR_mas_SCE = isTRUE(all.equal(sct, scr + sce)))
}))
rownames(resumen) <- NULL
print(resumen, digits = 3)

# --- 2. Pero las gráficas son completamente distintas -------------------------
ggplot(datos, aes(x, y)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "red", linewidth = 0.5) +
  facet_wrap(~dataset, ncol = 4) +
  theme_minimal() +
  labs(title = "Misma recta de regresión, datos completamente distintos")

# --- 3. Residuos de un conjunto: ¿se cumplen los supuestos? -------------------
away <- subset(datos, dataset == "away")
fit <- lm(y ~ x, data = away)
res <- residuals(fit)

plot(fitted(fit), res, xlab = "Valores ajustados", ylab = "Residuos",
     main = "Residuos contra ajustados (away)"); abline(h = 0, lty = 2)
plot(density(res), main = "Densidad de los residuos (away)")
qqnorm(res); qqline(res, col = "red")

shapiro.test(res)   # normalidad
bptest(fit)         # varianza constante (Breusch-Pagan)
dwtest(fit)         # autocorrelación de orden 1 (Durbin-Watson)
acf(res, main = "ACF de los residuos")
pacf(res, main = "PACF de los residuos")

# Lección: el R^2 y la recta pueden ser idénticos con datos que no se parecen.
# Siempre graficar antes de ajustar y revisar los residuos después.

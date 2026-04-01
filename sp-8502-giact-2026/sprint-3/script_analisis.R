# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : script_analisis.R          ← ENTREGABLE SPRINT 3
# SPRINT  : 3 — Análisis Profundo y Modelado (Semanas 9–12)
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# VERSIÓN : 1.0
# =============================================================================
# PROPÓSITO:
#   Este script es el ENTREGABLE de análisis del Sprint 3.
#   Es la versión INTEGRADA y comentada pedagógicamente que:
#
#   (1) Carga y prepara datos (del Sprint 2)
#   (2) Verifica supuestos OLS sistemáticamente
#   (3) Ajusta el modelo de regresión lineal múltiple (M3)
#   (4) Ajusta el modelo logístico
#   (5) Presenta el análisis cualitativo (codificación temática)
#   (6) Valida con Bootstrap (B=2000)
#   (7) Prueba las hipótesis H1–H3 formalmente
#   (8) Genera todas las figuras y tablas del informe
#
# REPRODUCIBILIDAD:
#   Ejecutar desde línea 1 con R reiniciado.
#   Requiere que Sprint 2 haya producido datos/procesados/base_datos.csv
#   set.seed(8502) garantiza resultados idénticos.
#
# TIEMPO ESTIMADO DE EJECUCIÓN: ~2–3 minutos
# =============================================================================

# ══════════════════════════════════════════════════════════════════════════════
# 0. ENTORNO Y CONFIGURACIÓN
# ══════════════════════════════════════════════════════════════════════════════

# Limpiar workspace
rm(list = ls())
gc()

# Reproducibilidad
set.seed(8502)

# Instalación y carga de paquetes
pkgs <- c("tidyverse","broom","psych","car","lmtest","sandwich",
          "nortest","mice","pROC","ResourceSelection","patchwork",
          "ggcorrplot","tidytext","knitr")

instalar_si_falta <- function(p) {
  if (!require(p, character.only = TRUE, quietly = TRUE))
    install.packages(p, repos = "https://cran.rstudio.com/", quiet = TRUE)
  library(p, character.only = TRUE)
}
invisible(lapply(pkgs, instalar_si_falta))

# Opciones globales
options(scipen = 999, digits = 4)
theme_set(theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", colour = "#2c7fb8"),
        plot.subtitle = element_text(colour = "#636363"),
        plot.caption  = element_text(size = 8, colour = "#969696")))

# Verificar existencia de datos
stopifnot(
  "ERROR: Ejecute Sprint 2 primero (falta base_datos.csv)" =
    file.exists("datos/procesados/base_datos.csv")
)

cat("
╔══════════════════════════════════════════════════════════════╗
║  SP-8502 · Sprint 3 · Script de Análisis (ENTREGABLE)       ║
║  Análisis Profundo y Modelado · Pacífico Central CR         ║
╚══════════════════════════════════════════════════════════════╝
")

# ══════════════════════════════════════════════════════════════════════════════
# 1. CARGA Y PREPARACIÓN DE DATOS
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[1/8] Cargando y preparando datos...\n")

datos_raw <- read_csv("datos/procesados/base_datos.csv",
                      show_col_types = FALSE)

datos <- datos_raw %>%
  mutate(
    comunidad          = factor(comunidad),
    sexo               = factor(sexo),
    metodo             = factor(metodo),
    arte_pesca_principal = factor(arte_pesca_principal),
    percepcion_recurso = factor(percepcion_recurso,
                                levels = c("Muy escaso","Escaso","Regular",
                                           "Abundante","Muy abundante"),
                                ordered = TRUE),
    extension_pesquera = as.numeric(extension_pesquera),
    log_ingreso        = log(ingreso_mensual_usd + 1),
    percepcion_num     = as.numeric(percepcion_recurso)
  )

n <- nrow(datos)
cat("  Dataset:", n, "observaciones ×", ncol(datos), "variables\n")
cat("  % datos completos (vars. modelo):",
    round(mean(complete.cases(datos %>%
                                select(sas_total, log_ingreso, años_experiencia,
                                       distancia_anp_km, extension_pesquera,
                                       percepcion_num))) * 100, 1), "%\n")

# ══════════════════════════════════════════════════════════════════════════════
# 2. IMPUTACIÓN MICE
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[2/8] Imputación MICE (Predictive Mean Matching)...\n")

vars_imp <- c("sas_total","log_ingreso","años_experiencia",
              "distancia_anp_km","extension_pesquera","percepcion_num")

imp <- mice(datos %>% select(all_of(vars_imp)),
            m = 5, method = "pmm", seed = 8502, printFlag = FALSE)

datos_imp <- complete(imp, 1)
datos_completo <- datos %>%
  select(-any_of(vars_imp)) %>%
  bind_cols(datos_imp)

cat("  ✓ Imputación completada. NAs restantes en vars. modelo:",
    sum(is.na(datos_completo %>% select(all_of(vars_imp)))), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# 3. VERIFICACIÓN DE SUPUESTOS OLS
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[3/8] Verificando supuestos OLS (Gauss-Markov)...\n")

# Modelo para diagnóstico
m_diag <- lm(sas_total ~ extension_pesquera + log_ingreso + años_experiencia +
               distancia_anp_km + percepcion_num + metodo,
             data = datos_completo)

# Tests clave
bp   <- lmtest::bptest(m_diag)
sw   <- shapiro.test(residuals(m_diag))
vif  <- car::vif(m_diag)
cook <- cooks.distance(m_diag)
cook_flag <- sum(cook > 4 / n)

supuestos_tabla <- tibble(
  Supuesto = c("Linealidad","Homocedasticidad","Normalidad residuos",
               "No multicolinealidad","Sin outliers influyentes"),
  Test     = c("LOESS R-vs-Fit","Breusch-Pagan","Shapiro-Wilk",
               "VIF (car)","Cook < 4/n"),
  Valor    = c(
    round(cor(fitted(m_diag), residuals(m_diag)), 4),
    round(bp$p.value, 4),
    round(sw$p.value, 4),
    round(max(vif), 2),
    cook_flag
  ),
  Veredicto = c(
    ifelse(abs(cor(fitted(m_diag), residuals(m_diag))) < 0.1, "✓","⚠"),
    ifelse(bp$p.value > 0.05, "✓",
           "⚠ SE robustos HC3"),
    ifelse(sw$p.value > 0.05, "✓", "~ TCL"),
    ifelse(max(vif) < 10, "✓","⚠"),
    ifelse(cook_flag <= 3, "✓","⚠")
  )
)
cat("  Resumen de supuestos:\n")
print(supuestos_tabla)

# Usar SE robustos si hay heterocedasticidad
usar_robustos <- bp$p.value <= 0.05

# Panel de diagnóstico
res_df <- tibble(
  fitted  = fitted(m_diag), residuos = residuals(m_diag),
  std_res = rstandard(m_diag), sqrt_abs = sqrt(abs(rstandard(m_diag))),
  leverage = hatvalues(m_diag), cook_d = cook
)
p1 <- ggplot(res_df, aes(fitted,residuos)) +
  geom_point(alpha=0.5,color="#2c7fb8",size=1.5) +
  geom_hline(yintercept=0,color="#e34a33",linetype="dashed") +
  geom_smooth(method="loess",se=FALSE,color="#31a354",linewidth=0.7) +
  labs(title="Residuos vs Ajustados",x="ŷ",y="e")
p2 <- ggplot(res_df, aes(sample=std_res)) +
  stat_qq(alpha=0.5,color="#2c7fb8",size=1.5) +
  stat_qq_line(color="#e34a33") +
  labs(title="QQ Normal",x="Cuantiles teóricos",y="Cuantiles muestrales")
p3 <- ggplot(res_df, aes(fitted,sqrt_abs)) +
  geom_point(alpha=0.5,color="#2c7fb8",size=1.5) +
  geom_smooth(method="loess",se=FALSE,color="#31a354",linewidth=0.7) +
  labs(title="Scale-Location",x="ŷ",y="√|e|")
p4 <- ggplot(res_df, aes(leverage,std_res)) +
  geom_point(aes(color=cook_d>4/n),size=1.8,alpha=0.7) +
  scale_color_manual(values=c("FALSE"="#2c7fb8","TRUE"="#e34a33"),
                     labels=c("Normal","Influyente"),name="") +
  geom_hline(yintercept=c(-2,2),linetype="dashed",color="grey50") +
  labs(title="Residuos vs Leverage",x="hii",y="e std.")

panel_diag <- (p1|p2)/(p3|p4) +
  plot_annotation(title="Panel Diagnóstico OLS · Sprint 3",
                  caption="SP-8502 · GIACT UCR · 2026")
ggsave("outputs/figuras/S3_panel_diagnostico.png",
       plot=panel_diag, width=11, height=8, dpi=300)
cat("  ✓ Panel diagnóstico guardado.\n")

# ══════════════════════════════════════════════════════════════════════════════
# 4. MODELO OLS — CONSTRUCCIÓN SECUENCIAL (M0–M3)
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[4/8] Ajustando modelos OLS (M0 → M3)...\n")

M0 <- lm(sas_total ~ 1, data = datos_completo)
M1 <- lm(sas_total ~ extension_pesquera, data = datos_completo)
M2 <- lm(sas_total ~ extension_pesquera + log_ingreso + años_experiencia +
            tamaño_familia, data = datos_completo)
M3 <- lm(sas_total ~ extension_pesquera + log_ingreso + años_experiencia +
            tamaño_familia + distancia_anp_km + percepcion_num + metodo,
          data = datos_completo)

comp_tabla <- map_dfr(
  list(M0=M0,M1=M1,M2=M2,M3=M3), ~ {
    s <- summary(.x)
    tibble(k = length(coef(.x))-1,
           R2 = round(s$r.squared,4),
           R2adj = round(s$adj.r.squared,4),
           AIC = round(AIC(.x),2),
           BIC = round(BIC(.x),2),
           RMSE = round(sqrt(mean(residuals(.x)^2)),3))
  }, .id = "Modelo"
)
cat("  Comparación de modelos:\n")
print(comp_tabla)

# Coeficientes con IC 95% (SE robustos si heterocedasticidad)
coef_tabla <- if (usar_robustos) {
  cov_rob <- sandwich::vcovHC(M3, type = "HC3")
  se_rob  <- sqrt(diag(cov_rob))
  tidy(M3) %>%
    mutate(std.error = se_rob[term],
           statistic = estimate / std.error,
           p.value   = 2 * pt(-abs(statistic), df = n - length(coef(M3))),
           conf.low  = estimate - qt(0.975, n - length(coef(M3))) * std.error,
           conf.high = estimate + qt(0.975, n - length(coef(M3))) * std.error,
           SE_tipo   = "Robusto HC3")
} else {
  tidy(M3, conf.int = TRUE) %>% mutate(SE_tipo = "OLS estándar")
}

coef_tabla <- coef_tabla %>%
  mutate(across(c(estimate,std.error,conf.low,conf.high), round, 4),
         p.value = round(p.value, 4),
         sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                          p.value < 0.05 ~ "*", p.value < 0.10 ~ ".",
                          TRUE ~ "ns"))

cat("\n  Coeficientes M3 (", coef_tabla$SE_tipo[1], "):\n")
print(coef_tabla %>% select(term,estimate,std.error,conf.low,conf.high,p.value,sig))

# Forest plot
p_forest <- coef_tabla %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x=estimate,y=term)) +
  geom_vline(xintercept=0,color="grey50",linetype="dashed",linewidth=0.8) +
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high),height=0.3,color="#636363") +
  geom_point(aes(color=p.value<0.05),size=3.5) +
  scale_color_manual(values=c("TRUE"="#2c7fb8","FALSE"="#bdbdbd"),
                     labels=c("p≥0.05","p<0.05"),name="") +
  labs(title="Coeficientes OLS con IC 95% · Modelo M3",
       subtitle=paste0("SAS Total | R²=",round(summary(M3)$r.squared,3),
                       " | n=",n," | SE: ",coef_tabla$SE_tipo[1]),
       x="Efecto en SAS (pts)",y=NULL,caption="SP-8502 · Sprint 3 · 2026")
ggsave("outputs/figuras/S3_coeficientes_OLS.png",
       plot=p_forest, width=9, height=6, dpi=300)

# ══════════════════════════════════════════════════════════════════════════════
# 5. MODELO LOGÍSTICO
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[5/8] Ajustando regresión logística...\n")

logit <- glm(extension_pesquera ~ sas_total + log_ingreso + años_experiencia +
               distancia_anp_km + percepcion_num + comunidad,
             data = datos_completo, family = binomial("logit"))

or_tabla <- tidy(logit, exponentiate=TRUE, conf.int=TRUE) %>%
  mutate(across(c(estimate,conf.low,conf.high),round,3),
         p.value = round(p.value,4),
         sig = case_when(p.value<0.001~"***",p.value<0.01~"**",
                          p.value<0.05~"*",p.value<0.10~".",TRUE~"ns"))

roc_obj <- pROC::roc(datos_completo$extension_pesquera,
                      fitted(logit), quiet=TRUE)
auc_val <- round(pROC::auc(roc_obj), 3)
pseudo_r2 <- round(as.numeric(1 - logLik(logit) /
                                 logLik(glm(extension_pesquera~1,
                                             data=datos_completo,
                                             family=binomial()))), 3)

cat("  AUC-ROC =", auc_val,
    "| Pseudo-R² (McFadden) =", pseudo_r2, "\n")
cat("  Tabla OR (primeras 6 filas):\n")
print(head(or_tabla %>% select(term,estimate,conf.low,conf.high,p.value,sig), 6))

# ══════════════════════════════════════════════════════════════════════════════
# 6. PRUEBA DE HIPÓTESIS H1–H3
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[6/8] Probando hipótesis H1–H3...\n")

r2_m3  <- summary(M3)$r.squared
f_m3   <- summary(M3)$fstatistic
p_f    <- pf(f_m3[1], f_m3[2], f_m3[3], lower.tail=FALSE)

# H2: d de Cohen
g1 <- datos_completo$sas_total[datos_completo$extension_pesquera == 0]
g2 <- datos_completo$sas_total[datos_completo$extension_pesquera == 1]
sd_pool <- sqrt(((length(g1)-1)*var(g1,na.rm=TRUE) +
                   (length(g2)-1)*var(g2,na.rm=TRUE)) /
                  (length(g1)+length(g2)-2))
d_cohen <- (mean(g2,na.rm=TRUE) - mean(g1,na.rm=TRUE)) / sd_pool

# H3: correlación
r_h3 <- cor(datos_completo$sas_total, datos_completo$distancia_anp_km,
             use="complete.obs")
p_h3 <- cor.test(datos_completo$sas_total, datos_completo$distancia_anp_km)$p.value

hipotesis <- tibble(
  H    = c("H1","H2","H3"),
  Pred = c("R²≥0.35","d>0.5","r≥0.40"),
  Obs  = c(round(r2_m3,3), round(d_cohen,3), round(r_h3,3)),
  p    = c(round(p_f,4),   round(t.test(g1,g2)$p.value,4), round(p_h3,4)),
  Res  = c(ifelse(r2_m3>=0.35,"✓ CONFIRMADA","✗ No confirmada"),
            ifelse(abs(d_cohen)>=0.5,"✓ CONFIRMADA","✗ No confirmada"),
            ifelse(abs(r_h3)>=0.40,"✓ CONFIRMADA","✗ No confirmada"))
)

cat("  Resultados de hipótesis:\n")
print(hipotesis)

# ══════════════════════════════════════════════════════════════════════════════
# 7. BOOTSTRAP (B=2000) PARA ROBUSTEZ
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[7/8] Bootstrap B =",B, "réplicas (validación de robustez)...\n")

B <- 2000
boot_r2 <- replicate(B, {
  idx <- sample(1:n, n, replace=TRUE)
  summary(lm(sas_total ~ extension_pesquera + log_ingreso + años_experiencia +
               tamaño_familia + distancia_anp_km + percepcion_num + metodo,
             data = datos_completo[idx, ]))$r.squared
})
boot_d <- replicate(B, {
  idx <- sample(1:n, n, replace=TRUE)
  g1b <- datos_completo$sas_total[datos_completo$extension_pesquera==0][
    sample(sum(datos_completo$extension_pesquera==0),
           sum(datos_completo$extension_pesquera==0), replace=TRUE)]
  g2b <- datos_completo$sas_total[datos_completo$extension_pesquera==1][
    sample(sum(datos_completo$extension_pesquera==1),
           sum(datos_completo$extension_pesquera==1), replace=TRUE)]
  sd_p <- sqrt(((length(g1b)-1)*var(g1b,na.rm=T)+(length(g2b)-1)*var(g2b,na.rm=T))/
                 (length(g1b)+length(g2b)-2))
  (mean(g2b,na.rm=T)-mean(g1b,na.rm=T))/sd_p
})

boot_resumen <- tibble(
  Estadístico = c("R² (M3)","d Cohen (H2)"),
  Media_boot  = c(round(mean(boot_r2),3), round(mean(boot_d),3)),
  SE_boot     = c(round(sd(boot_r2),3),   round(sd(boot_d),3)),
  IC_low      = c(round(quantile(boot_r2,0.025),3), round(quantile(boot_d,0.025),3)),
  IC_high     = c(round(quantile(boot_r2,0.975),3), round(quantile(boot_d,0.975),3)),
  Umbral      = c(0.35, 0.50),
  Robusto     = c(ifelse(quantile(boot_r2,0.025)>=0.35,"✓","~"),
                   ifelse(quantile(boot_d,0.025)>=0.50,"✓","~"))
)
cat("  Bootstrap completado:\n")
print(boot_resumen)

# ══════════════════════════════════════════════════════════════════════════════
# 8. GUARDAR TODOS LOS ENTREGABLES
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[8/8] Guardando resultados...\n")

write_csv(supuestos_tabla, "datos/procesados/supuestos_ols_s3.csv")
write_csv(comp_tabla,      "datos/procesados/comparacion_modelos_s3.csv")
write_csv(coef_tabla,      "datos/procesados/coeficientes_M3_final.csv")
write_csv(or_tabla,        "datos/procesados/OR_logistica_s3.csv")
write_csv(hipotesis,       "datos/procesados/resultados_hipotesis_s3.csv")
write_csv(boot_resumen,    "datos/procesados/bootstrap_resumen_s3.csv")
saveRDS(list(M0=M0,M1=M1,M2=M2,M3=M3,logit=logit),
        "datos/procesados/modelos_finales_s3.rds")

cat("
╔══════════════════════════════════════════════════════════════╗
║  ENTREGABLE SPRINT 3 — COMPLETADO                           ║
╠══════════════════════════════════════════════════════════════╣
║  Archivos generados:                                        ║")
cat("║  ✓ datos/procesados/coeficientes_M3_final.csv           ║\n")
cat("║  ✓ datos/procesados/resultados_hipotesis_s3.csv         ║\n")
cat("║  ✓ datos/procesados/bootstrap_resumen_s3.csv           ║\n")
cat("║  ✓ datos/procesados/modelos_finales_s3.rds             ║\n")
cat("║  ✓ outputs/figuras/S3_panel_diagnostico.png            ║\n")
cat("║  ✓ outputs/figuras/S3_coeficientes_OLS.png             ║\n")
cat(paste0("║  R² M3 = ", round(r2_m3,3),
           " | H1:", hipotesis$Res[1],
           " | H2:", hipotesis$Res[2],
           " | H3:", hipotesis$Res[3], "\n"))
cat("║                                                         ║\n")
cat("║  Siguiente: Generar informe_hallazgos.Rmd              ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")

# ==== 1. Cargar librerías ====
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# ==== 2. Cargar base de datos ====
df <- read_excel("df_marinos_vulnerables_control.xlsx") %>%
  mutate(
    # Asegurar que grupo sea factor con niveles ordenados
    grupo = factor(
      grupo,
      levels = c("Control", "Marino", "Vulnerable")
    ),
    # Asegurar que variables de esfuerzo, recompensa y trabajo sean numéricas
    across(
      c(
        starts_with("SelfRew"),
        starts_with("OtherRew"),
        starts_with("SelfEff"),
        starts_with("OtherEff"),
        starts_with("trabajo_"),
        starts_with("zeros_"),
        starts_with("tasa_fallo_"),
        WorkSelf, WorkOther
      ),
      ~ as.numeric(.)
    )
  )

# ==== 3. Guía estética ====

# Paletas sobrias tipo paper (colorblind-friendly)
pal_beneficiario <- c(Self = "#0072B2", Other = "#D55E00")

pal_grupo <- c(
  "Control"    = "#009E73",
  "Marino"     = "#B3B3B3",
  "Vulnerable" = "#8B5E3C"
)

# Etiquetas de facetas para los grupos
lab_grupo <- c(
  "Control"    = "Control socioeconómico",
  "Marino"     = "Marinos",
  "Vulnerable" = "Vulnerabilidad social"
)

# Tema base para todos los gráficos
theme_paper <- theme_classic(base_size = 12) +
  theme(
    axis.title       = element_text(face = "bold"),
    axis.text        = element_text(color = "black"),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text       = element_text(face = "bold"),
    legend.title     = element_blank(),
    legend.position  = "right",
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# ===========================================================
# GRAFICO 1: Self vs Other por grupo (índice global de trabajo)
# ===========================================================

# Pasar WorkSelf y WorkOther a formato largo
df_overall_long <- df %>%
  select(sub, grupo, WorkSelf, WorkOther) %>%
  pivot_longer(
    cols = c(WorkSelf, WorkOther),
    names_to = "beneficiario",
    values_to = "prop_trabajo"
  ) %>%
  mutate(
    beneficiario = recode(
      beneficiario,
      "WorkSelf"  = "Self",
      "WorkOther" = "Other"
    )
  )

# Resumen por grupo y beneficiario: media y error estándar
df_overall_sum <- df_overall_long %>%
  group_by(grupo, beneficiario) %>%
  summarise(
    mean_prop = mean(prop_trabajo, na.rm = TRUE),
    se_prop   = sd(prop_trabajo, na.rm = TRUE) /
      sqrt(sum(!is.na(prop_trabajo))),
    .groups   = "drop"
  )

# Gráfico 1: barras Self vs Other por grupo
plot_overall <- ggplot(
  df_overall_sum,
  aes(x = beneficiario, y = mean_prop, fill = grupo)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_errorbar(
    aes(ymin = mean_prop - se_prop,
        ymax = mean_prop + se_prop),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.4
  ) +
  scale_fill_manual(values = pal_grupo) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Beneficiario",
    y = "Proporción de ensayos trabajados"
  ) +
  theme_paper

# Vista del gráfico
plot_overall

# Guardar gráfico 1
ggsave(
  "grafico_overall_self_other_por_grupo.png",
  plot = plot_overall,
  width = 8, height = 6, dpi = 300
)

# ===========================================================
# GRAFICO 2: Self vs Other por nivel de esfuerzo y grupo
# ===========================================================

# Pasar SelfEff y OtherEff a formato largo
df_eff_long <- df %>%
  select(
    sub, grupo,
    starts_with("SelfEff"),
    starts_with("OtherEff")
  ) %>%
  pivot_longer(
    cols = c(starts_with("SelfEff"), starts_with("OtherEff")),
    names_to = c("beneficiario", "nivel"),
    names_pattern = "(Self|Other)Eff(\\d+)",
    values_to = "prop_trabajo"
  ) %>%
  mutate(
    beneficiario = if_else(beneficiario == "Self", "Self", "Other"),
    # Mapear índices 1–4 a niveles de esfuerzo
    nivel = recode(
      nivel,
      "1" = "50",
      "2" = "65",
      "3" = "80",
      "4" = "95"
    ),
    nivel = factor(
      nivel,
      levels = c("50", "65", "80", "95"),
      labels = c("50%", "65%", "80%", "95%")
    )
  )

# Resumen por grupo, beneficiario y nivel de esfuerzo
df_eff_sum <- df_eff_long %>%
  group_by(grupo, beneficiario, nivel) %>%
  summarise(
    mean_prop = mean(prop_trabajo, na.rm = TRUE),
    se_prop   = sd(prop_trabajo, na.rm = TRUE) /
      sqrt(sum(!is.na(prop_trabajo))),
    .groups   = "drop"
  )

# Gráfico 2: barras por esfuerzo, Self vs Other, con facetas por grupo
plot_effort <- ggplot(
  df_eff_sum,
  aes(x = nivel, y = mean_prop, fill = beneficiario)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_errorbar(
    aes(ymin = mean_prop - se_prop,
        ymax = mean_prop + se_prop),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.4
  ) +
  facet_wrap(
    ~ grupo,
    labeller = labeller(grupo = lab_grupo)
  ) +
  scale_fill_manual(values = pal_beneficiario) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Nivel de esfuerzo (%)",
    y = "Proporción de ensayos trabajados"
  ) +
  theme_paper +
  theme(
    legend.position = "bottom"
  )

# Vista del gráfico
plot_effort

# Guardar gráfico 2
ggsave(
  "grafico_effort_self_other_por_grupo.png",
  plot = plot_effort,
  width = 8, height = 6, dpi = 300
)

# ===========================================================
# GRAFICO 3: Self vs Other por nivel de recompensa y grupo
# ===========================================================

# Pasar SelfRew y OtherRew a formato largo
df_rew_long <- df %>%
  select(
    sub, grupo,
    starts_with("SelfRew"),
    starts_with("OtherRew")
  ) %>%
  pivot_longer(
    cols = c(starts_with("SelfRew"), starts_with("OtherRew")),
    names_to = c("beneficiario", "nivel"),
    names_pattern = "(Self|Other)Rew(\\d+)",
    values_to = "prop_trabajo"
  ) %>%
  mutate(
    beneficiario = if_else(beneficiario == "Self", "Self", "Other"),
    # Mapear índices 1–3 a niveles de recompensa
    nivel = recode(
      nivel,
      "1" = "2",
      "2" = "6",
      "3" = "10"
    ),
    nivel = factor(
      nivel,
      levels = c("2", "6", "10"),
      labels = c("2 créditos", "6 créditos", "10 créditos")
    )
  )

# Resumen por grupo, beneficiario y nivel de recompensa
df_rew_sum <- df_rew_long %>%
  group_by(grupo, beneficiario, nivel) %>%
  summarise(
    mean_prop = mean(prop_trabajo, na.rm = TRUE),
    se_prop   = sd(prop_trabajo, na.rm = TRUE) /
      sqrt(sum(!is.na(prop_trabajo))),
    .groups   = "drop"
  )

# Gráfico 3: barras por recompensa, Self vs Other, con facetas por grupo
plot_reward <- ggplot(
  df_rew_sum,
  aes(x = nivel, y = mean_prop, fill = beneficiario)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_errorbar(
    aes(ymin = mean_prop - se_prop,
        ymax = mean_prop + se_prop),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.4
  ) +
  facet_wrap(
    ~ grupo,
    labeller = labeller(grupo = lab_grupo)
  ) +
  scale_fill_manual(values = pal_beneficiario) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Nivel de recompensa (créditos)",
    y = "Proporción de ensayos trabajados"
  ) +
  theme_paper +
  theme(
    legend.position = "bottom"
  )

# Vista del gráfico
plot_reward

# Guardar gráfico 3
ggsave(
  "grafico_reward_self_other_por_grupo.png",
  plot = plot_reward,
  width = 8, height = 6, dpi = 300
)


# ===========================================================
# GRAFICO 4: Promedio de DASS21_estres por grupo
# ===========================================================

# Resumen por grupo
df_dass_sum <- df %>%
  group_by(grupo) %>%
  summarise(
    mean_estres = mean(DASS21_estres, na.rm = TRUE),
    se_estres   = sd(DASS21_estres, na.rm = TRUE) /
      sqrt(sum(!is.na(DASS21_estres))),
    .groups = "drop"
  )

# Gráfico
plot_dass <- ggplot(
  df_dass_sum,
  aes(x = grupo, y = mean_estres, fill = grupo)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_estres - se_estres,
      ymax = mean_estres + se_estres
    ),
    width = 0.2,
    linewidth = 0.4
  ) +
  scale_fill_manual(values = pal_grupo) +
  labs(
    x = "Grupo",
    y = "Promedio de estrés (DASS-21)"
  ) +
  theme_paper +
  theme(
    legend.position = "none"
  )

# Vista
plot_dass

# Guardar
ggsave(
  "grafico_dass21_estres_por_grupo.png",
  plot = plot_dass,
  width = 8, height = 6, dpi = 300
)
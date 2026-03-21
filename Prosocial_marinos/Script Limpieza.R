## Prosocial Effort Task ##
# Colaboración con APOLINAV #
# Diego Garrido - José Borquez - Matías Carmach #
# Viña del Mar - 2025 #
# VERSION ADAPTADA


# Import Libraries
library(readxl)
library(readr)
library(tidyverse)


# ===========================
# PARTE 1: Limpieza inicial de la base de datos
# ===========================

# Import dataset
effort_task = read_excel("Conducta_Prosocial_Marinos.xlsx")

# Delete all columns with "prac" on their name
effort_task = effort_task[, !grepl("prac", names(effort_task), ignore.case = TRUE)]

# Delete all columns with "boxes" on their name
effort_task = effort_task[, !grepl("boxes", names(effort_task), ignore.case = TRUE)] 

# Save datafile
write.csv(effort_task, "datos_limpios.csv")

# Read dataset without headers
effort_task = read_csv("datos_limpios.csv", col_names = FALSE)

# Extract two first rows as variables (column names)
header1 = effort_task[1, ] |> unlist(use.names = FALSE)
header2 = effort_task[2, ] |> unlist(use.names = FALSE)

# Create JUST ONE new column names based on specific conditions
new_colnames = mapply(function(col_name, second_row_val) {
  # Identify if the name is exactly "X_choice_screen"
  if (grepl("^\\d+_choice_screen$", col_name)) {
    return(second_row_val)
  } else {
    return(col_name)
  }
}, header1, header2, USE.NAMES = FALSE)

# Read data, omiting two first rows
effort_task = read_csv("datos_limpios.csv", skip = 2, col_names = new_colnames)

# Save datafile
write.csv(effort_task, "datos_limpios.csv")


# ===========================
# PARTE 2: Aplicar mapeo de IDs
# ===========================

# Import data
effort_task = read_csv("datos_limpios.csv")

# Diccionario de reemplazo: nombres → códigos P
mapa_ids <- c(
  "Alexander Surriba" = "P4",
  "Diego Gonzalez" = "P2",
  "Francisco Tagle" = "P9",
  "Vicente Muñoz" = "P7",
  "adam shafiee" = "P5",
  "Juan Francisco González Concha" = "P6", 
  "Franco Tarabla" = "P15",
  "Eduardo Manieu" = "P8",
  "Daniela Severino Salamanca" = "P17",
  "Diego Valdés" = "P19",
  "Felipe Rojas" = "P11",
  "Raimundo Silva Rosenberg" = "P10",
  "Juan Pablo Marro Arenas" = "P13",
  "Fernando Nicolás" = "P14",
  "Danny Galdames" = "P12",
  "Cristian Pineda" = "P16",
  "Catalina Verdejo" = "P1"
)

#SUJETO N6 NO HA REALIZADO LA TAREA

# Reemplazo de los nombres por sus códigos
effort_task <- effort_task %>%
  mutate(ID_check = recode(ID_check, !!!mapa_ids))

# Convert Progress column as numeric
effort_task$Progress = as.numeric(gsub("%", "", effort_task$Progress))

# Replace NA for 0 on SELF & OTHER columns
effort_task = effort_task %>%
  mutate(across(contains("SELF"), ~replace_na(., 0))) %>%
  mutate(across(contains("OTHER"), ~replace_na(., 0)))

# Save file
write.csv(effort_task, "datos_limpios.csv")


# ===========================
# PARTE 3: Procesamiento de datos
# ===========================

# Load Data
datos = read.csv("datos_limpios.csv")

# Delete first column
datos = datos[, -1]

# Extract only the columns of interest
datos = datos %>% select(ID_check,
                         starts_with("SELF"), starts_with("OTHER"),
                         matches("^X\\d+_fail_feedback_timing_Page\\.Submit"),
                         Comp2_Q1, Comp2_Q2,
                         X49_attention_check)

# Attention check (stimulus N°49)
respuestas_4 = sum(datos$X49_attention_check == 4, na.rm = TRUE)
respuestas_na = sum(is.na(datos$X49_attention_check))
cat("\nAttention Check:\n")
cat("- Respondieron correctamente (4):", respuestas_4, "participantes\n")
cat("- No respondieron:", respuestas_na, "participantes\n")
# Respondieron correctamente (4): 10 participantes
# No respondieron: 6 participantes


# Divide condition columns with reward, effort and difficulty 
{
  # Identify the 48 columns of stimulus (SELF.* / OTHER.*)
  stim_cols = names(datos) %>% str_subset("^(SELF|OTHER)\\.")
  
  # Dataset base columns
  datos_base = datos %>% select(ID_check,
                                matches("^X\\d+_fail_feedback_timing_Page\\.Submit"),
                                Comp2_Q1, Comp2_Q2,
                                X49_attention_check)
  
  # Build new 192 columns (48 x 4)
  new_cols = list()
  
  for (i in seq_along(stim_cols)) {
    
    col_name  = stim_cols[i]
    respuestas = datos[[col_name]]
    
    # Extract information from the columns
    meta = str_match(col_name,
                     "^(SELF|OTHER)\\.(\\d+)(easy|hard)(\\d+)")[ , 2:5]
    
    condicion  = meta[1]      # SELF / OTHER
    reward     = meta[2]      # 2 / 6 / 10
    dificultad = meta[3]      # easy / hard
    esfuerzo   = meta[4]      # 50 / 65 / 80 / 95
    
    idx = sprintf("%02d", i)  
    
    # Create new 4 columns with the info
    new_cols[[ paste0("condicion_",  condicion,  "_", idx) ]] <- respuestas
    new_cols[[ paste0("reward_",     reward,     "_", idx) ]] <- respuestas
    new_cols[[ paste0("dificultad_", dificultad, "_", idx) ]] <- respuestas
    new_cols[[ paste0("esfuerzo_",   esfuerzo,   "_", idx) ]] <- respuestas
  }
  
  # Unify everything
  datos = bind_cols(datos_base, as_tibble(new_cols))
}

# Create new columns with Comp Q1 & Q2 results
datos = datos %>%
  mutate(resultados_Comp = case_when(
    Comp2_Q1 == 2 & Comp2_Q2 == 1 ~ 0,
    is.na(Comp2_Q1) | is.na(Comp2_Q2) ~ 0,
    TRUE ~ 1
  )) %>%
  relocate(resultados_Comp, .after = Comp2_Q2)

# Rename every fail_feedback_timing_Page.Submit column
for(i in 1:49) {
  old_name <- paste0("X", i, "_fail_feedback_timing_Page.Submit")
  new_name <- sprintf("fallo_%02d", i)
  
  if(old_name %in% names(datos)) {
    names(datos)[names(datos) == old_name] <- new_name
  }
}

# Reorder columns on specific order (simplified version showing pattern)
datos = datos %>%
  select(
    # First columns
    ID_check, Comp2_Q1, Comp2_Q2, resultados_Comp, X49_attention_check,
    
    # For each trial (1-48), select columns and fail_feedback
    all_of(unlist(lapply(1:48, function(i) {
      trial_num = sprintf("%02d", i)
      c(
        grep(paste0("^condicion_.*_", trial_num, "$"), names(datos), value = TRUE),
        grep(paste0("^reward_.*_", trial_num, "$"), names(datos), value = TRUE),
        grep(paste0("^dificultad_.*_", trial_num, "$"), names(datos), value = TRUE),
        grep(paste0("^esfuerzo_.*_", trial_num, "$"), names(datos), value = TRUE),
        paste0("fallo_", trial_num)
      )
    })))
  )

# Apply transformation to every "fallo" column 
for(i in 1:48) {
  trial_num = sprintf("%02d", i)
  
  col_condicion = grep(paste0("condicion.*_", trial_num, "$"), names(datos), value = TRUE)
  col_fallo = paste0("fallo_", trial_num)
  
  if(length(col_condicion) > 0 && col_fallo %in% names(datos)) {
    
    datos = datos %>%
      mutate(!!col_fallo := case_when(
        .data[[col_condicion]] == 1 & !is.na(.data[[col_fallo]]) ~ 1,
        (.data[[col_condicion]] == 0 | .data[[col_condicion]] == 2) & !is.na(.data[[col_fallo]]) ~ NA_real_,
        is.na(.data[[col_fallo]]) ~ NA_real_,
        TRUE ~ .data[[col_fallo]]
      ))
  }
}

# Save Dataset
write.csv(datos, "datos_clean.csv")


# ===========================
# PARTE 4: Calcular proporciones
# ===========================

# Load data
datos_clean = read_csv("datos_clean.csv")

# Delete first column
datos_clean = datos_clean[, -1]

# Identify columns of interest
self_cols  = grep("^condicion_SELF_",  names(datos_clean), value = TRUE)
other_cols = grep("^condicion_OTHER_", names(datos_clean), value = TRUE)

# Calculate proportions 
datos_clean = datos_clean %>% 
  rowwise() %>% 
  mutate(
    ## Work (solo valor 1)
    trabajo_self  = sum(c_across(all_of(self_cols))  == 1, na.rm = TRUE) /
      length(self_cols)  * 100,
    trabajo_other = sum(c_across(all_of(other_cols)) == 1, na.rm = TRUE) /
      length(other_cols) * 100,
    
    ## Total Work
    trabajo_total = (trabajo_self + trabajo_other) / 2,
    
    ## Omition (valor 0)
    zeros_SELF    = sum(c_across(all_of(self_cols))  == 0, na.rm = TRUE) /
      length(self_cols)  * 100,
    zeros_OTHER   = sum(c_across(all_of(other_cols)) == 0, na.rm = TRUE) /
      length(other_cols) * 100,
    
    ## Total Omitions
    zeros_TOTAL = (zeros_SELF + zeros_OTHER) / 2
  ) %>% 
  ungroup() %>%
  mutate(
    trabajo_self = round(trabajo_self, 2),
    trabajo_other = round(trabajo_other, 2),
    trabajo_total = round(trabajo_total, 2),
    zeros_SELF = round(zeros_SELF, 2),
    zeros_OTHER = round(zeros_OTHER, 2),
    zeros_TOTAL = round(zeros_TOTAL, 2)
  )

# Calculate adjusted proportions (excluding zeros from denominator)
datos_clean = datos_clean %>% 
  rowwise() %>% 
  mutate(
    zeros_count_self = sum(c_across(all_of(self_cols)) == 0, na.rm = TRUE),
    zeros_count_other = sum(c_across(all_of(other_cols)) == 0, na.rm = TRUE),
    
    ones_count_self = sum(c_across(all_of(self_cols)) == 1, na.rm = TRUE),
    ones_count_other = sum(c_across(all_of(other_cols)) == 1, na.rm = TRUE),
    
    denominator_self = length(self_cols) - zeros_count_self,
    denominator_other = length(other_cols) - zeros_count_other,
    
    trabajo_self_ajustado = ifelse(
      denominator_self > 0,
      (ones_count_self / denominator_self) * 100,
      NA_real_
    ),
    
    trabajo_other_ajustado = ifelse(
      denominator_other > 0,
      (ones_count_other / denominator_other) * 100,
      NA_real_
    ),
    
    trabajo_total_ajustado = ifelse(
      (denominator_self + denominator_other) > 0,
      ((ones_count_self + ones_count_other) / (denominator_self + denominator_other)) * 100,
      NA_real_
    )
    
  ) %>% 
  ungroup() %>%
  mutate(
    trabajo_self_ajustado = round(trabajo_self_ajustado, 2),
    trabajo_other_ajustado = round(trabajo_other_ajustado, 2),
    trabajo_total_ajustado = round(trabajo_total_ajustado, 2)
  ) %>%
  select(-zeros_count_self, -zeros_count_other, 
         -ones_count_self, -ones_count_other,
         -denominator_self, -denominator_other)

# Fail Proportion
fallo_cols = grep("^fallo_", names(datos_clean), value = TRUE)[1:48]

datos_clean = datos_clean %>% 
  rowwise() %>% 
  mutate(
    trabajo_self_count = sum(c_across(all_of(self_cols)) == 1, na.rm = TRUE),
    
    fallos_self_count = sum(
      mapply(function(cond_col, fallo_col) {
        cond_val = get(cond_col)
        fallo_val = get(fallo_col)
        return(cond_val == 1 & !is.na(fallo_val) & fallo_val == 1)
      }, 
      self_cols, 
      fallo_cols[1:24])
    ),
    
    tasa_fallo_self = ifelse(
      trabajo_self_count > 0,
      (fallos_self_count / trabajo_self_count) * 100,
      NA_real_
    ),
    
    trabajo_other_count = sum(c_across(all_of(other_cols)) == 1, na.rm = TRUE),
    
    fallos_other_count = sum(
      mapply(function(cond_col, fallo_col) {
        cond_val = get(cond_col)
        fallo_val = get(fallo_col)
        return(cond_val == 1 & !is.na(fallo_val) & fallo_val == 1)
      }, 
      other_cols, 
      fallo_cols[25:48])
    ),
    
    tasa_fallo_other = ifelse(
      trabajo_other_count > 0,
      (fallos_other_count / trabajo_other_count) * 100,
      NA_real_
    ),
    
    trabajo_total_count = trabajo_self_count + trabajo_other_count,
    fallos_total_count = fallos_self_count + fallos_other_count,
    
    tasa_fallo_total = ifelse(
      trabajo_total_count > 0,
      (fallos_total_count / trabajo_total_count) * 100,
      NA_real_
    )
    
  ) %>% 
  ungroup() %>%
  mutate(
    tasa_fallo_self = round(tasa_fallo_self, 2),
    tasa_fallo_other = round(tasa_fallo_other, 2),
    tasa_fallo_total = round(tasa_fallo_total, 2)
  ) %>%
  select(-trabajo_self_count, -trabajo_other_count, -trabajo_total_count,
         -fallos_self_count, -fallos_other_count, -fallos_total_count)

# Save dataset
write.csv(datos_clean, "datos_final.csv")


# ===========================
# PARTE 5: Formato largo
# ===========================

# Load Data Set
datos = read_csv("datos_clean.csv")

# Transform from wide to long format
datos_long <- datos %>%
  select(ID_check,
         matches("^condicion_(SELF|OTHER)_\\d+$"),
         matches("^reward_\\d+_\\d+$"),
         matches("^esfuerzo_\\d+_\\d+$"),
         matches("^fallo_\\d+$")) %>%
  
  rowwise() %>%
  
  summarise(
    ID_check = ID_check,
    trials = list(1:48),
    .groups = 'drop'
  ) %>%
  unnest(trials) %>%
  
  mutate(
    trial_str = sprintf("%02d", trials),
    
    condicion = map2_dbl(ID_check, trial_str, function(id, t) {
      if(as.numeric(t) <= 24) {
        col_name <- paste0("condicion_SELF_", t)
      } else {
        col_name <- paste0("condicion_OTHER_", t)
      }
      
      if(col_name %in% names(datos)) {
        datos[datos$ID_check == id, col_name][[1]]
      } else {
        NA_real_
      }
    }),
    
    decision_value = condicion,
    
    reward_val = map2_dbl(ID_check, trial_str, function(id, t) {
      col_pattern <- paste0("_", t, "$")
      reward_cols <- grep(paste0("^reward_.*", col_pattern), names(datos), value = TRUE)
      
      if(length(reward_cols) > 0) {
        col_name <- reward_cols[1]
        reward_num <- as.numeric(str_extract(col_name, "(?<=reward_)\\d+(?=_)"))
        return(reward_num)
      } else {
        NA_real_
      }
    }),
    
    esfuerzo_val = map2_dbl(ID_check, trial_str, function(id, t) {
      col_pattern <- paste0("_", t, "$")
      esfuerzo_cols <- grep(paste0("^esfuerzo_.*", col_pattern), names(datos), value = TRUE)
      
      if(length(esfuerzo_cols) > 0) {
        col_name <- esfuerzo_cols[1]
        esfuerzo_num <- as.numeric(str_extract(col_name, "(?<=esfuerzo_)\\d+(?=_)"))
        return(esfuerzo_num)
      } else {
        NA_real_
      }
    }),
    
    fallo_val = map2_dbl(ID_check, trial_str, function(id, t) {
      col_name <- paste0("fallo_", t)
      if(col_name %in% names(datos)) {
        datos[datos$ID_check == id, col_name][[1]]
      } else {
        NA_real_
      }
    })
  ) %>%
  
  mutate(
    sub = ID_check,
    trial = trials,
    
    decision = case_when(
      decision_value == 1 ~ 1,  # Trabajar
      decision_value == 2 ~ 0,  # Descansar  
      decision_value == 0 ~ 2,  # Omisión
      TRUE ~ NA_integer_
    ),
    
    reward = case_when(
      reward_val == 2 ~ 1,
      reward_val == 6 ~ 2,
      reward_val == 10 ~ 3,
      TRUE ~ NA_integer_
    ),
    
    effort = case_when(
      esfuerzo_val == 50 ~ 1,
      esfuerzo_val == 65 ~ 2,
      esfuerzo_val == 80 ~ 3,
      esfuerzo_val == 95 ~ 4,
      TRUE ~ NA_integer_
    ),
    
    agent = ifelse(trials <= 24, 0, 1),
    
    success = case_when(
      is.na(fallo_val) ~ 0,
      fallo_val == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  
  select(sub, trial, decision, reward, effort, agent, success) %>%
  arrange(sub, trial)

# Save Data long
write.csv(datos_long, "datos_long.csv", row.names = FALSE)


# ===========================
# PARTE 6: Datos para ANOVA
# ===========================

# Read data
datos_long <- read.csv("datos_long.csv")
datos_clean <- read.csv("datos_final.csv")

# Verify participants with data
participantes_con_datos <- datos_long %>%
  filter(decision != 2) %>%
  pull(sub) %>%
  unique()

participantes_totales <- unique(datos_long$sub)
participantes_solo_omisiones <- setdiff(participantes_totales, participantes_con_datos)

if(length(participantes_solo_omisiones) > 0) {
  cat("\nParticipantes con SOLO omisiones:", participantes_solo_omisiones, "\n")
}

# Select proportions and columns of interest
columnas_adicionales <- datos_clean %>%
  select(ID_check,
         trabajo_self, trabajo_other, trabajo_total,
         zeros_SELF, zeros_OTHER, zeros_TOTAL,
         trabajo_self_ajustado, trabajo_other_ajustado, trabajo_total_ajustado,
         tasa_fallo_self, tasa_fallo_other, tasa_fallo_total)

# Calculate averaging
model_free_proportions <- datos_long %>%
  filter(decision != 2) %>%
  group_by(sub) %>%
  summarise(
    # Proportion for reward (SELF)
    SelfRew1 = mean(decision[agent == 0 & reward == 1] == 1, na.rm = TRUE),
    SelfRew2 = mean(decision[agent == 0 & reward == 2] == 1, na.rm = TRUE),
    SelfRew3 = mean(decision[agent == 0 & reward == 3] == 1, na.rm = TRUE),
    
    # Proportion for reward (OTHER)
    OtherRew1 = mean(decision[agent == 1 & reward == 1] == 1, na.rm = TRUE),
    OtherRew2 = mean(decision[agent == 1 & reward == 2] == 1, na.rm = TRUE),
    OtherRew3 = mean(decision[agent == 1 & reward == 3] == 1, na.rm = TRUE),
    
    # Proportion for effort (SELF)
    SelfEff1 = mean(decision[agent == 0 & effort == 1] == 1, na.rm = TRUE),
    SelfEff2 = mean(decision[agent == 0 & effort == 2] == 1, na.rm = TRUE),
    SelfEff3 = mean(decision[agent == 0 & effort == 3] == 1, na.rm = TRUE),
    SelfEff4 = mean(decision[agent == 0 & effort == 4] == 1, na.rm = TRUE),
    
    # Proportion for effort (OTHER)
    OtherEff1 = mean(decision[agent == 1 & effort == 1] == 1, na.rm = TRUE),
    OtherEff2 = mean(decision[agent == 1 & effort == 2] == 1, na.rm = TRUE),
    OtherEff3 = mean(decision[agent == 1 & effort == 3] == 1, na.rm = TRUE),
    OtherEff4 = mean(decision[agent == 1 & effort == 4] == 1, na.rm = TRUE),
    
    # Total proportion
    WorkSelf = mean(decision[agent == 0] == 1, na.rm = TRUE),
    WorkOther = mean(decision[agent == 1] == 1, na.rm = TRUE),
    
    .groups = 'drop'
  )

# Join datasets
model_free_proportions_v2 <- model_free_proportions %>%
  left_join(columnas_adicionales, by = c("sub" = "ID_check"))

# Round to 4 decimals
model_free_proportions_v2 <- model_free_proportions_v2 %>%
  mutate(across(where(is.numeric), ~round(., 4)))

# Save datasets
write.csv(model_free_proportions, "datos_analisis.csv", row.names = FALSE)
write.csv(model_free_proportions_v2, "datos_analisis_v2.csv", row.names = FALSE)

# Final summary
cat("\n========== RESUMEN FINAL ==========\n")
cat("Archivos generados:\n")
cat("- datos_limpios.csv: Datos con IDs mapeados\n")
cat("- datos_clean.csv: Datos procesados\n")
cat("- datos_final.csv: Datos con proporciones calculadas\n")
cat("- datos_long.csv: Formato largo para análisis\n")
cat("- datos_analisis.csv: Proporciones para ANOVA\n")
cat("- datos_analisis_v2.csv: Proporciones extendidas\n")
cat("\nTotal de participantes procesados:", length(unique(datos_long$sub)), "\n")
cat("====================================\n")
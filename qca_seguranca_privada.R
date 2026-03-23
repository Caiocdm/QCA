############################################################
# QCA – Mudança e Permanência Regulatória
# Autor: Caio Cardoso
# Projeto: Tese de Doutorado – UFMG
############################################################

# ----------------------------------------------------------
# 1. Pacotes e opções gerais
# ----------------------------------------------------------

library(QCA)
library(tidyverse)

options(scipen = 999)  # Evitar notação científica

# ----------------------------------------------------------
# 2. Base de dados QCA
# ----------------------------------------------------------

# Calibração binária:
# 1 = presença da condição
# 0 = ausência da condição

dados <- tibble(
  Caso = c("Uruguai", "Paraguai", "Argentina", "Brasil_1", "Brasil_2"),
  X1_Conflito     = c(0, 0, 1, 1, 0),
  X2_VetoPlayers = c(0, 0, 1, 1, 1),
  X3_TipoMudanca = c(0, 0, 1, 0, 0),
  Y_Mudanca      = c(1, 1, 0, 0, 1)
)

dados_qca <- dados %>%
  column_to_rownames("Caso")

print(dados_qca)

# ----------------------------------------------------------
# 3. TABELA DA VERDADE
# ----------------------------------------------------------

# Mudança regulatória (Y = 1)

tt_y1 <- truthTable(
  data       = dados_qca,
  outcome    = "Y_Mudanca",
  incl.cut   = 0.8,
  show.cases = TRUE
)

print(tt_y1)

# Permanência regulatória (Y = 0)

tt_y0 <- truthTable(
  data       = dados_qca,
  outcome    = "Y_Mudanca",
  neg.out    = TRUE,
  incl.cut   = 0.8,
  show.cases = TRUE
)

print(tt_y0)

# ----------------------------------------------------------
# 4. TESTES DE NECESSIDADE
# ----------------------------------------------------------

# Y = 1
nec_y1 <- superSubset(
  data     = dados_qca,
  outcome  = "Y_Mudanca",
  relation = "necessity"
)

print(nec_y1)

# Y = 0
nec_y0 <- superSubset(
  data     = dados_qca,
  outcome  = "Y_Mudanca",
  neg.out  = TRUE,
  relation = "necessity"
)

print(nec_y0)

# ----------------------------------------------------------
# 5. TESTES DE SUFICIÊNCIA (MINIMIZAÇÃO)
# ----------------------------------------------------------

# Observação:
# Por padrão, o minimize() produz a SOLUÇÃO CONSERVADORA.

# Y = 1
sol_y1 <- minimize(
  input      = tt_y1,
  details    = TRUE,
  show.cases = TRUE
)

print(sol_y1)

# Y = 0
sol_y0 <- minimize(
  input      = tt_y0,
  details    = TRUE,
  show.cases = TRUE
)

print(sol_y0)

# ----------------------------------------------------------
# 6. CONSISTÊNCIA E COBERTURA (SOLUÇÕES)
# ----------------------------------------------------------

cons_cov_y1 <- tibble(
  Resultado    = "Mudança regulatória (Y = 1)",
  Consistencia = sol_y1$overall$incl,
  Cobertura    = sol_y1$overall$cov
)

print(cons_cov_y1)

cons_cov_y0 <- tibble(
  Resultado    = "Permanência regulatória (Y = 0)",
  Consistencia = sol_y0$overall$incl,
  Cobertura    = sol_y0$overall$cov
)

print(cons_cov_y0)

# ----------------------------------------------------------
# 7. REDUÇÃO LÓGICA DAS EXPRESSÕES (INTERPRETAÇÃO)
# ----------------------------------------------------------

# === Y = 1 (Mudança Regulatória) ===
#
# ~X1*~X2*~X3  +  ~X1*X2*~X3  →  Y
# ~X1*~X3*(~X2 + X2) → Y
# ~X1 * ~X3 → Y
#
# Interpretação:
# Ausência de conflito e mudança incremental
# são suficientes para mudança regulatória.

# === Y = 0 (Permanência Regulatória) ===
#
# X1*X2*X3  +  X1*X2*~X3  →  ~Y
# X1*X2*(X3 + ~X3) → ~Y
# X1 * X2 → ~Y
#
# Interpretação:
# Conflito e veto players
# são suficientes para manutenção do status quo.

# ----------------------------------------------------------
# 8. NOTA METODOLÓGICA FINAL
# ----------------------------------------------------------

# - Consistência = 1 reflete coerência perfeita no conjunto analisado,
#   mas deve ser interpretada à luz do N pequeno.
# - As soluções são empiricamente completas neste universo de casos,
#   não generalizações universais.
# - A análise reforça o caráter configuracional e contingente
#   da mudança regulatória.

############################################################
# FIM DO SCRIPT
############################################################

#=========================================================================
# Modelos aditivos generalizados (GAM) na caracterização da dinâmica
# temporal de respostas bioquímicas à ferrugem do cafeeiro
# 70ª RBras - Pôster
#
# Respostas: peroxidase (POX) e peroxidação lipídica
# Cultivares: Arara e Catuaí 144 (inoculadas com Hemileia vastatrix)
# Tempo: 1, 5, 7, 11 e 18 dias após a inoculação
#
# Como usar:
#   1. Coloque os dados em  dados/Estatistica_razao.xlsx
#      (colunas: Cultivar, Tempo, POX, Peroxidacao)
#   2. Abra este script no RStudio e execute (Source).
#   3. As figuras são salvas em  figuras/  e as tabelas em  tabelas/
#=========================================================================

#-------------------------------------------------------------------------
# 0. Pacotes e pastas
#-------------------------------------------------------------------------
library(readxl)
library(mgcv)
library(ggplot2)
library(scales)

dir.create("figuras", showWarnings = FALSE)
dir.create("tabelas", showWarnings = FALSE)

#-------------------------------------------------------------------------
# 1. Dados
#-------------------------------------------------------------------------
#=========================================================================
# Modelos aditivos generalizados (GAM) na caracterização da dinâmica
# temporal de respostas bioquímicas à ferrugem do cafeeiro
# 70ª RBras - Pôster
#
# Respostas: peroxidase (POX) e peroxidação lipídica
# Cultivares: Arara e Catuaí 144 (inoculadas com Hemileia vastatrix)
# Tempo: 1, 5, 7, 11 e 18 dias após a inoculação
#
# Como usar:
#   1. Coloque os dados em  dados/Estatistica_razao.xlsx
#      (colunas: Cultivar, Tempo, POX, Peroxidacao)
#   2. Abra este script no RStudio e execute (Source).
#   3. As figuras são salvas em  figuras/  e as tabelas em  tabelas/
#=========================================================================

#-------------------------------------------------------------------------
# 0. Pacotes e pastas
#-------------------------------------------------------------------------
library(readxl)
library(mgcv)
library(ggplot2)
library(scales)

dir.create("figuras", showWarnings = FALSE)
dir.create("tabelas", showWarnings = FALSE)

#-------------------------------------------------------------------------
# 1. Dados
#-------------------------------------------------------------------------
#=========================================================================
# Modelos aditivos generalizados (GAM) na caracterização da dinâmica
# temporal de respostas bioquímicas à ferrugem do cafeeiro
# 70ª RBras - Pôster
#
# Respostas: peroxidase (POX) e peroxidação lipídica
# Cultivares: Arara e Catuaí 144 (inoculadas com Hemileia vastatrix)
# Tempo: 1, 5, 7, 11 e 18 dias após a inoculação
#
# Como usar:
#   1. Coloque os dados em  dados/Estatistica_razao.xlsx
#      (colunas: Cultivar, Tempo, POX, Peroxidacao)
#   2. Abra este script no RStudio e execute (Source).
#   3. As figuras são salvas em  figuras/  e as tabelas em  tabelas/
#=========================================================================

#-------------------------------------------------------------------------
# 0. Pacotes e pastas
#-------------------------------------------------------------------------
library(readxl)
library(mgcv)
library(ggplot2)
library(scales)

dir.create("figuras", showWarnings = FALSE)
dir.create("tabelas", showWarnings = FALSE)

#-------------------------------------------------------------------------
# 1. Dados
#-------------------------------------------------------------------------
dados <- read_excel("dados/Estatistica_razao.xlsx")

dados$Tempo    <- as.numeric(as.character(dados$Tempo))
dados$Cultivar <- factor(dados$Cultivar)   # referência: Arara

#-------------------------------------------------------------------------
# 2. Modelos GAM
#    Y_i = b0 + b1*Cultivar_i + f_Cultivar_i(Tempo_i) + e_i,  e_i ~ N(0, s2)
#    Gaussiana, ligação identidade, spline cúbica (cr), k = 4, REML
#-------------------------------------------------------------------------
m_POX <- gam(
  POX ~ Cultivar + s(Tempo, by = Cultivar, bs = "cr", k = 4),
  family = gaussian(),
  data   = dados,
  method = "REML"
)

m_Perox <- gam(
  Peroxidacao ~ Cultivar + s(Tempo, by = Cultivar, bs = "cr", k = 4),
  family = gaussian(),
  data   = dados,
  method = "REML"
)

summary(m_POX)
summary(m_Perox)

#-------------------------------------------------------------------------
# 3. Tabelas do pôster
#-------------------------------------------------------------------------

# Tabela 1 - Coeficientes paramétricos
tab_param <- function(modelo, variavel) {
  pt <- summary(modelo)$p.table
  data.frame(
    Variavel   = variavel,
    Parametro  = ifelse(rownames(pt) == "(Intercept)", "Intercepto",
                        sub("^Cultivar", "", rownames(pt))),
    Estimativa = pt[, "Estimate"],
    EP         = pt[, "Std. Error"],
    Valor_p    = pt[, "Pr(>|t|)"],
    row.names  = NULL
  )
}

tabela1 <- rbind(
  tab_param(m_POX,   "Peroxidase"),
  tab_param(m_Perox, "Peroxidação lipídica")
)

# Tabela 2 - Termos suavizados
tab_suave <- function(modelo, variavel) {
  st <- summary(modelo)$s.table
  data.frame(
    Variavel = variavel,
    Cultivar = sub("^s\\(Tempo\\):Cultivar", "", rownames(st)),
    edf      = st[, "edf"],
    F        = st[, "F"],
    Valor_p  = st[, "p-value"],
    row.names = NULL
  )
}

tabela2 <- rbind(
  tab_suave(m_POX,   "Peroxidase"),
  tab_suave(m_Perox, "Peroxidação lipídica")
)

# Tabela 3 - Qualidade do ajuste
tab_ajuste <- function(modelo, variavel) {
  s <- summary(modelo)
  data.frame(
    Variavel      = variavel,
    REML          = as.numeric(modelo$gcv.ubre),
    R2_ajustado   = s$r.sq,
    Deviance_perc = 100 * s$dev.expl
  )
}

tabela3 <- rbind(
  tab_ajuste(m_POX,   "Peroxidase"),
  tab_ajuste(m_Perox, "Peroxidação lipídica")
)

# Valores no console (3 casas decimais, como no pôster)
print(tabela1, digits = 3)
print(tabela2, digits = 3)
print(tabela3, digits = 3)

# Salvar em CSV (vírgula decimal)
write.csv2(tabela1, "tabelas/tabela1_coeficientes.csv", row.names = FALSE)
write.csv2(tabela2, "tabelas/tabela2_termos_suaves.csv", row.names = FALSE)
write.csv2(tabela3, "tabelas/tabela3_ajuste.csv",        row.names = FALSE)

#-------------------------------------------------------------------------
# 4. Figuras do pôster (curvas ajustadas com IC95%)
#-------------------------------------------------------------------------
cores <- c("Arara" = "#000000", "Catuaí 144" = "#1F77B4")

fazer_figura <- function(modelo, titulo, rotulo_y,
                         limites_y, quebras_y, arquivo) {

  novo <- expand.grid(
    Tempo    = seq(min(dados$Tempo), max(dados$Tempo), length.out = 300),
    Cultivar = levels(dados$Cultivar)
  )

  pred <- predict(modelo, newdata = novo, se.fit = TRUE)
  novo$fit <- pred$fit
  novo$LI  <- pred$fit - 1.96 * pred$se.fit
  novo$LS  <- pred$fit + 1.96 * pred$se.fit

  g <- ggplot(novo, aes(Tempo, fit, colour = Cultivar, fill = Cultivar)) +
    geom_ribbon(aes(ymin = LI, ymax = LS), alpha = 0.12, colour = NA) +
    geom_line(linewidth = 1.8) +
    scale_colour_manual(values = cores) +
    scale_fill_manual(values = cores) +
    scale_x_continuous(breaks = c(1, 5, 7, 11, 18)) +
    scale_y_continuous(
      breaks = quebras_y,
      labels = label_number(accuracy = 0.1, decimal.mark = ","),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    coord_cartesian(ylim = limites_y) +
    labs(title = titulo, x = "Dias após a inoculação",
         y = rotulo_y, colour = "Cultivar") +
    theme_classic(base_size = 20) +
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold", size = 22),
      legend.position  = "right",
      legend.direction = "vertical",
      legend.title = element_text(face = "bold", size = 18),
      legend.text  = element_text(size = 18),
      axis.title   = element_text(face = "bold", size = 18),
      axis.title.y = element_text(face = "bold", size = 11),
      axis.text    = element_text(colour = "black")
    ) +
    guides(fill = "none")

  ggsave(file.path("figuras", arquivo), g,
         width = 18, height = 11.5, units = "cm", dpi = 300, bg = "white")
  g
}

# Figura 1 - Peroxidase
fazer_figura(
  m_POX,
  titulo    = "Peroxidase",
  rotulo_y  = "Razão da atividade de peroxidase",
  limites_y = c(0.8, 2.0),
  quebras_y = seq(0.8, 2.0, by = 0.2),
  arquivo   = "GAM_POX_curva_temporal_IC95.png"
)

# Figura 2 - Peroxidação lipídica
fazer_figura(
  m_Perox,
  titulo    = "Peroxidação lipídica",
  rotulo_y  = "Razão de peroxidação lipídica",
  limites_y = c(0.8, 2.4),
  quebras_y = seq(0.8, 2.4, by = 0.4),
  arquivo   = "GAM_Peroxidacao_curva_temporal_IC95.png"
)

#-------------------------------------------------------------------------
# 5. Informações da sessão (reprodutibilidade)
#-------------------------------------------------------------------------
sessionInfo()

dados$Tempo    <- as.numeric(as.character(dados$Tempo))
dados$Cultivar <- factor(dados$Cultivar)   # referência: Arara

#-------------------------------------------------------------------------
# 2. Modelos GAM
#    Y_i = b0 + b1*Cultivar_i + f_Cultivar_i(Tempo_i) + e_i,  e_i ~ N(0, s2)
#    Gaussiana, ligação identidade, spline cúbica (cr), k = 4, REML
#-------------------------------------------------------------------------
m_POX <- gam(
  POX ~ Cultivar + s(Tempo, by = Cultivar, bs = "cr", k = 4),
  family = gaussian(),
  data   = dados,
  method = "REML"
)

m_Perox <- gam(
  Peroxidacao ~ Cultivar + s(Tempo, by = Cultivar, bs = "cr", k = 4),
  family = gaussian(),
  data   = dados,
  method = "REML"
)

summary(m_POX)
summary(m_Perox)

#-------------------------------------------------------------------------
# 3. Tabelas do pôster
#-------------------------------------------------------------------------

# Tabela 1 - Coeficientes paramétricos
tab_param <- function(modelo, variavel) {
  pt <- summary(modelo)$p.table
  data.frame(
    Variavel   = variavel,
    Parametro  = ifelse(rownames(pt) == "(Intercept)", "Intercepto",
                        sub("^Cultivar", "", rownames(pt))),
    Estimativa = pt[, "Estimate"],
    EP         = pt[, "Std. Error"],
    Valor_p    = pt[, "Pr(>|t|)"],
    row.names  = NULL
  )
}

tabela1 <- rbind(
  tab_param(m_POX,   "Peroxidase"),
  tab_param(m_Perox, "Peroxidação lipídica")
)

# Tabela 2 - Termos suavizados
tab_suave <- function(modelo, variavel) {
  st <- summary(modelo)$s.table
  data.frame(
    Variavel = variavel,
    Cultivar = sub("^s\\(Tempo\\):Cultivar", "", rownames(st)),
    edf      = st[, "edf"],
    F        = st[, "F"],
    Valor_p  = st[, "p-value"],
    row.names = NULL
  )
}

tabela2 <- rbind(
  tab_suave(m_POX,   "Peroxidase"),
  tab_suave(m_Perox, "Peroxidação lipídica")
)

# Tabela 3 - Qualidade do ajuste
tab_ajuste <- function(modelo, variavel) {
  s <- summary(modelo)
  data.frame(
    Variavel      = variavel,
    REML          = as.numeric(modelo$gcv.ubre),
    R2_ajustado   = s$r.sq,
    Deviance_perc = 100 * s$dev.expl
  )
}

tabela3 <- rbind(
  tab_ajuste(m_POX,   "Peroxidase"),
  tab_ajuste(m_Perox, "Peroxidação lipídica")
)

# Valores no console (3 casas decimais, como no pôster)
print(tabela1, digits = 3)
print(tabela2, digits = 3)
print(tabela3, digits = 3)

# Salvar em CSV (vírgula decimal)
write.csv2(tabela1, "tabelas/tabela1_coeficientes.csv", row.names = FALSE)
write.csv2(tabela2, "tabelas/tabela2_termos_suaves.csv", row.names = FALSE)
write.csv2(tabela3, "tabelas/tabela3_ajuste.csv",        row.names = FALSE)

#-------------------------------------------------------------------------
# 4. Figuras do pôster (curvas ajustadas com IC95%)
#-------------------------------------------------------------------------
cores <- c("Arara" = "#000000", "Catuaí 144" = "#1F77B4")

fazer_figura <- function(modelo, titulo, rotulo_y,
                         limites_y, quebras_y, arquivo) {

  novo <- expand.grid(
    Tempo    = seq(min(dados$Tempo), max(dados$Tempo), length.out = 300),
    Cultivar = levels(dados$Cultivar)
  )

  pred <- predict(modelo, newdata = novo, se.fit = TRUE)
  novo$fit <- pred$fit
  novo$LI  <- pred$fit - 1.96 * pred$se.fit
  novo$LS  <- pred$fit + 1.96 * pred$se.fit

  g <- ggplot(novo, aes(Tempo, fit, colour = Cultivar, fill = Cultivar)) +
    geom_ribbon(aes(ymin = LI, ymax = LS), alpha = 0.12, colour = NA) +
    geom_line(linewidth = 1.8) +
    scale_colour_manual(values = cores) +
    scale_fill_manual(values = cores) +
    scale_x_continuous(breaks = c(1, 5, 7, 11, 18)) +
    scale_y_continuous(
      breaks = quebras_y,
      labels = label_number(accuracy = 0.1, decimal.mark = ","),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    coord_cartesian(ylim = limites_y) +
    labs(title = titulo, x = "Dias após a inoculação",
         y = rotulo_y, colour = "Cultivar") +
    theme_classic(base_size = 20) +
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold", size = 22),
      legend.position  = "right",
      legend.direction = "vertical",
      legend.title = element_text(face = "bold", size = 18),
      legend.text  = element_text(size = 18),
      axis.title   = element_text(face = "bold", size = 18),
      axis.title.y = element_text(face = "bold", size = 11),
      axis.text    = element_text(colour = "black")
    ) +
    guides(fill = "none")

  ggsave(file.path("figuras", arquivo), g,
         width = 18, height = 11.5, units = "cm", dpi = 300, bg = "white")
  g
}

# Figura 1 - Peroxidase
fazer_figura(
  m_POX,
  titulo    = "Peroxidase",
  rotulo_y  = "Razão da atividade de peroxidase",
  limites_y = c(0.8, 2.0),
  quebras_y = seq(0.8, 2.0, by = 0.2),
  arquivo   = "GAM_POX_curva_temporal_IC95.png"
)

# Figura 2 - Peroxidação lipídica
fazer_figura(
  m_Perox,
  titulo    = "Peroxidação lipídica",
  rotulo_y  = "Razão de peroxidação lipídica",
  limites_y = c(0.8, 2.4),
  quebras_y = seq(0.8, 2.4, by = 0.4),
  arquivo   = "GAM_Peroxidacao_curva_temporal_IC95.png"
)

#-------------------------------------------------------------------------
# 5. Informações da sessão (reprodutibilidade)
#-------------------------------------------------------------------------
sessionInfo()

dados$Tempo    <- as.numeric(as.character(dados$Tempo))
dados$Cultivar <- factor(dados$Cultivar)   # referência: Arara

#-------------------------------------------------------------------------
# 2. Modelos GAM
#    Y_i = b0 + b1*Cultivar_i + f_Cultivar_i(Tempo_i) + e_i,  e_i ~ N(0, s2)
#    Gaussiana, ligação identidade, spline cúbica (cr), k = 4, REML
#-------------------------------------------------------------------------
m_POX <- gam(
  POX ~ Cultivar + s(Tempo, by = Cultivar, bs = "cr", k = 4),
  family = gaussian(),
  data   = dados,
  method = "REML"
)

m_Perox <- gam(
  Peroxidacao ~ Cultivar + s(Tempo, by = Cultivar, bs = "cr", k = 4),
  family = gaussian(),
  data   = dados,
  method = "REML"
)

summary(m_POX)
summary(m_Perox)

#-------------------------------------------------------------------------
# 3. Tabelas do pôster
#-------------------------------------------------------------------------

# Tabela 1 - Coeficientes paramétricos
tab_param <- function(modelo, variavel) {
  pt <- summary(modelo)$p.table
  data.frame(
    Variavel   = variavel,
    Parametro  = ifelse(rownames(pt) == "(Intercept)", "Intercepto",
                        sub("^Cultivar", "", rownames(pt))),
    Estimativa = pt[, "Estimate"],
    EP         = pt[, "Std. Error"],
    Valor_p    = pt[, "Pr(>|t|)"],
    row.names  = NULL
  )
}

tabela1 <- rbind(
  tab_param(m_POX,   "Peroxidase"),
  tab_param(m_Perox, "Peroxidação lipídica")
)

# Tabela 2 - Termos suavizados
tab_suave <- function(modelo, variavel) {
  st <- summary(modelo)$s.table
  data.frame(
    Variavel = variavel,
    Cultivar = sub("^s\\(Tempo\\):Cultivar", "", rownames(st)),
    edf      = st[, "edf"],
    F        = st[, "F"],
    Valor_p  = st[, "p-value"],
    row.names = NULL
  )
}

tabela2 <- rbind(
  tab_suave(m_POX,   "Peroxidase"),
  tab_suave(m_Perox, "Peroxidação lipídica")
)

# Tabela 3 - Qualidade do ajuste
tab_ajuste <- function(modelo, variavel) {
  s <- summary(modelo)
  data.frame(
    Variavel      = variavel,
    REML          = as.numeric(modelo$gcv.ubre),
    R2_ajustado   = s$r.sq,
    Deviance_perc = 100 * s$dev.expl
  )
}

tabela3 <- rbind(
  tab_ajuste(m_POX,   "Peroxidase"),
  tab_ajuste(m_Perox, "Peroxidação lipídica")
)

# Valores no console (3 casas decimais, como no pôster)
print(tabela1, digits = 3)
print(tabela2, digits = 3)
print(tabela3, digits = 3)

# Salvar em CSV (vírgula decimal)
write.csv2(tabela1, "tabelas/tabela1_coeficientes.csv", row.names = FALSE)
write.csv2(tabela2, "tabelas/tabela2_termos_suaves.csv", row.names = FALSE)
write.csv2(tabela3, "tabelas/tabela3_ajuste.csv",        row.names = FALSE)

#-------------------------------------------------------------------------
# 4. Figuras do pôster (curvas ajustadas com IC95%)
#-------------------------------------------------------------------------
cores <- c("Arara" = "#000000", "Catuaí 144" = "#1F77B4")

fazer_figura <- function(modelo, titulo, rotulo_y,
                         limites_y, quebras_y, arquivo) {
  
  novo <- expand.grid(
    Tempo    = seq(min(dados$Tempo), max(dados$Tempo), length.out = 300),
    Cultivar = levels(dados$Cultivar)
  )
  
  pred <- predict(modelo, newdata = novo, se.fit = TRUE)
  novo$fit <- pred$fit
  novo$LI  <- pred$fit - 1.96 * pred$se.fit
  novo$LS  <- pred$fit + 1.96 * pred$se.fit
  
  g <- ggplot(novo, aes(Tempo, fit, colour = Cultivar, fill = Cultivar)) +
    geom_ribbon(aes(ymin = LI, ymax = LS), alpha = 0.12, colour = NA) +
    geom_line(linewidth = 1.8) +
    scale_colour_manual(values = cores) +
    scale_fill_manual(values = cores) +
    scale_x_continuous(breaks = c(1, 5, 7, 11, 18)) +
    scale_y_continuous(
      breaks = quebras_y,
      labels = label_number(accuracy = 0.1, decimal.mark = ","),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    coord_cartesian(ylim = limites_y) +
    labs(title = titulo, x = "Dias após a inoculação",
         y = rotulo_y, colour = "Cultivar") +
    theme_classic(base_size = 20) +
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold", size = 22),
      legend.position  = "right",
      legend.direction = "vertical",
      legend.title = element_text(face = "bold", size = 18),
      legend.text  = element_text(size = 18),
      axis.title   = element_text(face = "bold", size = 18),
      axis.title.y = element_text(face = "bold", size = 11),
      axis.text    = element_text(colour = "black")
    ) +
    guides(fill = "none")
  
  ggsave(file.path("figuras", arquivo), g,
         width = 18, height = 11.5, units = "cm", dpi = 300, bg = "white")
  g
}

# Figura 1 - Peroxidase
fazer_figura(
  m_POX,
  titulo    = "Peroxidase",
  rotulo_y  = "Razão da atividade de peroxidase",
  limites_y = c(0.8, 2.0),
  quebras_y = seq(0.8, 2.0, by = 0.2),
  arquivo   = "GAM_POX_curva_temporal_IC95.png"
)

# Figura 2 - Peroxidação lipídica
fazer_figura(
  m_Perox,
  titulo    = "Peroxidação lipídica",
  rotulo_y  = "Razão de peroxidação lipídica",
  limites_y = c(0.8, 2.4),
  quebras_y = seq(0.8, 2.4, by = 0.4),
  arquivo   = "GAM_Peroxidacao_curva_temporal_IC95.png"
)

#-------------------------------------------------------------------------
# 5. Informações da sessão (reprodutibilidade)
#-------------------------------------------------------------------------
sessionInfo()
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
#   2. Abra o projeto no RStudio (ou use setwd() na pasta do repositório)
#      e execute o script (Source).
#   3. As figuras são salvas em  figuras/  e as tabelas em  tabelas/
#
# Estrutura do repositório:
#   SCRIPT_70RBRAS.R
#   dados/Estatistica_razao.xlsx
#   figuras/   (criada pelo script)
#   tabelas/   (criada pelo script)
#=========================================================================

#-------------------------------------------------------------------------
# 0. Pacotes e pastas
#-------------------------------------------------------------------------
# install.packages(c("readxl", "mgcv", "dplyr", "broom", "ggplot2", "scales"))

library(readxl)    # leitura do Excel
library(mgcv)      # ajuste dos GAM
library(dplyr)     # manipulação das tabelas
library(broom)     # extrai coeficientes dos modelos em formato de tabela
library(ggplot2)   # figuras
library(scales)    # formatação dos eixos

dir.create("figuras", showWarnings = FALSE)
dir.create("tabelas", showWarnings = FALSE)

#-------------------------------------------------------------------------
# 1. Dados (caminho relativo à pasta do projeto)
#-------------------------------------------------------------------------
dados <- read_excel("Estatística razão.xlsx")


dados <- dados |>
  mutate(Tempo    = as.numeric(as.character(Tempo)),
         Cultivar = factor(Cultivar))        # referência: Arara

#-------------------------------------------------------------------------
# 2. Modelos GAM
#    Y_i = b0 + b1*Cultivar_i + f_Cultivar_i(Tempo_i) + e_i,  e_i ~ N(0, s2)
#    Gaussiana, ligação identidade, spline cúbica (cr), k = 4, REML
#-------------------------------------------------------------------------
m_POX <- gam(POX ~ Cultivar + s(Tempo, by = Cultivar, bs = "cr", k = 4),
             family = gaussian(), data = dados, method = "REML")

m_Perox <- gam(Peroxidacao ~ Cultivar + s(Tempo, by = Cultivar, bs = "cr", k = 4),
               family = gaussian(), data = dados, method = "REML")

summary(m_POX)
summary(m_Perox)

#-------------------------------------------------------------------------
# 3. Tabelas do pôster
#-------------------------------------------------------------------------

# Tabela 1 - Coeficientes paramétricos
tabela1 <- bind_rows(
  "Peroxidase"           = tidy(m_POX,   parametric = TRUE),
  "Peroxidação lipídica" = tidy(m_Perox, parametric = TRUE),
  .id = "Variavel"
) |>
  transmute(Variavel,
            Parametro   = if_else(term == "(Intercept)", "Intercepto",
                                  sub("^Cultivar", "", term)),
            Estimativa  = estimate,
            Erro_padrao = std.error,
            Valor_p     = p.value)

# Tabela 2 - Termos suavizados
tabela2 <- bind_rows(
  "Peroxidase"           = tidy(m_POX),
  "Peroxidação lipídica" = tidy(m_Perox),
  .id = "Variavel"
) |>
  transmute(Variavel,
            Termo_suave = sub("Cultivar", "", term),   # ex.: s(Tempo):Arara
            edf,
            F           = statistic,
            Valor_p     = p.value)

# Tabela 3 - Qualidade do ajuste
tabela3 <- data.frame(
  Variavel      = c("Peroxidase", "Peroxidação lipídica"),
  R2_ajustado   = c(summary(m_POX)$r.sq,   summary(m_Perox)$r.sq),
  Deviance_perc = 100 * c(summary(m_POX)$dev.expl, summary(m_Perox)$dev.expl)
)

# Valores no console
print(tabela1, digits = 3)
print(tabela2, digits = 3)
print(tabela3, digits = 3)

# Salvar em CSV (vírgula decimal)
write.csv2(tabela1, "tabelas/tabela1_coeficientes.csv",  row.names = FALSE)
write.csv2(tabela2, "tabelas/tabela2_termos_suaves.csv", row.names = FALSE)
write.csv2(tabela3, "tabelas/tabela3_ajuste.csv",        row.names = FALSE)

#-------------------------------------------------------------------------
# 4. Predições com IC 95% (curvas ajustadas)
#-------------------------------------------------------------------------
grade <- expand.grid(
  Tempo    = seq(min(dados$Tempo), max(dados$Tempo), length.out = 300),
  Cultivar = levels(dados$Cultivar)
)

pred_POX   <- predict(m_POX,   newdata = grade, se.fit = TRUE)
pred_Perox <- predict(m_Perox, newdata = grade, se.fit = TRUE)

curva_POX <- grade |>
  mutate(fit = pred_POX$fit,
         LI  = fit - 1.96 * pred_POX$se.fit,
         LS  = fit + 1.96 * pred_POX$se.fit)

curva_Perox <- grade |>
  mutate(fit = pred_Perox$fit,
         LI  = fit - 1.96 * pred_Perox$se.fit,
         LS  = fit + 1.96 * pred_Perox$se.fit)

#-------------------------------------------------------------------------
# 5. Figuras do pôster
#    Camadas e tema comuns às duas figuras ficam num só objeto
#-------------------------------------------------------------------------
cores <- c("Arara" = "#000000", "Catuaí 144" = "#1F77B4")

estilo_poster <- list(
  geom_ribbon(aes(ymin = LI, ymax = LS), alpha = 0.12, colour = NA),
  geom_line(linewidth = 1.8),
  scale_colour_manual(values = cores),
  scale_fill_manual(values = cores),
  scale_x_continuous(breaks = c(1, 5, 7, 11, 18)),
  theme_classic(base_size = 14),
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 18),
    legend.position  = "right",
    legend.direction = "vertical",
    legend.box.spacing = unit(0, "pt"),         # distância entre o gráfico e a legenda
    legend.margin      = margin(0, 0, 0, 0),    # margem interna da legenda
    legend.box.margin  = margin(0, 0, 0, 0),    # margem externa da caixa da legenda
    legend.title     = element_text(face = "bold", size = 14),
    legend.text      = element_text(size = 14),
    axis.title       = element_text(face = "bold", size = 14),
    axis.title.y     = element_text(face = "bold", size = 13),
    axis.text        = element_text(colour = "black", size = 11)
  ),
  guides(fill = "none")
)

# Figura 1 - Peroxidase
fig_POX <- ggplot(curva_POX, aes(Tempo, fit, colour = Cultivar, fill = Cultivar)) +
  estilo_poster +
  scale_y_continuous(breaks = seq(0.8, 2.0, by = 0.2),
                     labels = label_number(accuracy = 0.1, decimal.mark = ","),
                     expand = expansion(mult = c(0.02, 0.02))) +
  coord_cartesian(ylim = c(0.8, 2.0)) +
  labs(title = "Peroxidase",                         # apague esta linha para tirar o título
       x = "Dias após a inoculação",
       y = "Razão da atividade de peroxidase",
       colour = "Cultivar")

# Figura 2 - Peroxidação lipídica
fig_Perox <- ggplot(curva_Perox, aes(Tempo, fit, colour = Cultivar, fill = Cultivar)) +
  estilo_poster +
  scale_y_continuous(breaks = seq(0.8, 2.4, by = 0.2),
                     labels = label_number(accuracy = 0.1, decimal.mark = ","),
                     expand = expansion(mult = c(0.02, 0.02))) +
  coord_cartesian(ylim = c(0.8, 2.4)) +
  labs(title = "Peroxidação lipídica",               # apague esta linha para tirar o título
       x = "Dias após a inoculação",
       y = "Razão de peroxidação lipídica",
       colour = "Cultivar")

fig_POX
fig_Perox

ggsave("figuras/GAM_POX_curva_temporal_IC95.png", fig_POX,
       width = 18, height = 11.5, units = "cm", dpi = 300, bg = "white")
ggsave("figuras/GAM_Peroxidacao_curva_temporal_IC95.png", fig_Perox,
       width = 18, height = 11.5, units = "cm", dpi = 300, bg = "white")

#-------------------------------------------------------------------------
# 6. Informações da sessão (reprodutibilidade)
#-------------------------------------------------------------------------
sessionInfo()

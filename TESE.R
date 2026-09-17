#tese Ana Bernardo

rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(extRemes)
library(evir)
library(tidyr)
library(POT)
library(patchwork)
library(numDeriv)
library(fitdistrplus)
library(actuar)
library(purrr)
library(statmod)
library(MASS)
library(ismev)
library(mgcv)
library(nlme)
library(mev)
library(coda)
library(tibble)
library(evd)
library(goftest)
library(scales)

parte1 <- read.csv("C:/Users/fatim/Downloads/parte1.csv")
parte2 <- read.csv("C:/Users/fatim/Downloads/parte2.csv")

dados <- rbind(parte1, parte2)

head(dados)
summary(dados)

#Análise inicial + limpeza e tratamento dos dados

dados <- dados %>%
  mutate(
    Paid_Claims = as.numeric(Paid_Claims),
    Incurred_Claims = as.numeric(Incurred_Claims),
    Case_Reserve = as.numeric(Case_Reserve)
  )

dados

#VER CADA LOB INDIVIDUALMENTE

dados_por_lob <- dados %>%
  group_by(LOB_SII_1) %>%
  group_split(.keep = TRUE)

nomes_lob <- dados %>%
  group_by(LOB_SII_1) %>%
  group_keys() %>%
  pull(LOB_SII_1)

names(dados_por_lob) <- nomes_lob

names(dados_por_lob)

dados <- dados %>%
  mutate(
    LOB_SII_1 = sub("^[0-9]+_", "", LOB_SII_1),
    LOB_SII_1 = gsub("_", " ", LOB_SII_1)
  )

#n de observaçoes por LOB
tabela_lob <- dados %>%
  group_by(LOB_SII_1) %>%
  summarise(
    n_observacoes = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n_observacoes))

tabela_lob

#claim_id unico por LOB
tabela_lob_claims <- dados %>%
  group_by(LOB_SII_1) %>%
  summarise(
    n_claims = n_distinct(Claim_ID),
    .groups = "drop"
  ) %>%
  arrange(desc(n_claims))

print(tabela_lob_claims, n=Inf)

sum(tabela_lob_claims$n_claims)

#NOTA: Há claim_id iguais para LOBs diferentes porque o mesmo sinistro pode causar danos correspondentes a várias LOBs

dados_agrupados <- dados %>%
  group_by(Claim_ID, LOB_SII_1) %>%
  summarise(
    Accident_Date = first(Accident_Date),
    Paid_Claims = sum(Paid_Claims, na.rm = TRUE),
    Incurred_Claims = sum(Incurred_Claims, na.rm = TRUE),
    Case_Reserve = sum(Case_Reserve, na.rm = TRUE),
    .groups = "drop"
  )

dados_agrupados
summary(dados_agrupados)

#numero de sinistros por LOB

num <- ggplot(tabela_lob_claims, aes(x = reorder(LOB_SII_1, n_claims), y = n_claims, fill = n_claims)) +
  geom_col() +
  geom_text(
    aes(label = comma(n_claims)),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_continuous(
    labels = comma
  ) +
  labs(
    title = "Number of claims by LOB",
    x = "LOB",
    y = "n",
    fill = "Number of claims"
  ) +
  theme_minimal()

num

# ggsave(
#   filename = "C:/Users/fatim/Downloads/numero_sinistros_LOB.pdf",
#   plot = num,
#   width = 8,
#   height = 6
# )

##Dados por CLAIM_ID

#nºde sinistros unicos
n_distinct(dados$Claim_ID)

dados_agrupados_claim <- dados %>%
  group_by(Claim_ID) %>%
  summarise(
    Accident_Date = first(Accident_Date),
    Accident_Year = year(as.Date(Accident_Date)),
    .groups = "drop"
  )


claims_por_ano <- dados_agrupados_claim %>%
  group_by(Accident_Year) %>%
  summarise(
    n_claims = n(),
    .groups = "drop"
  )

#sinistros por anos

ggplot(claims_por_ano, aes(x = Accident_Year, y = n_claims, fill = n_claims)) +
  geom_col() +
  geom_text(
    aes(label = n_claims),
    vjust = -0.3,
    size = 3
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Number of claims by accident year",
    x = "Accident year",
    y = "n",
    fill = "Number of claims"
  ) +
  theme_minimal()


datas <- ggplot(claims_por_ano, aes(x = Accident_Year, y = n_claims)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Number of claims by accident year",
    x = "Accident year",
    y = "n"
  ) +
  theme_minimal()

datas

# ggsave(
#   filename = "C:/Users/fatim/Downloads/datas.pdf",
#   plot = datas,
#   width = 8,
#   height = 6
# )

#sinistros temporais por LOB so para as que têm mais sinistros

claims_por_ano_lob <- dados_agrupados %>%
  mutate(
    Accident_Date = as.Date(Accident_Date),
    Accident_Year = year(Accident_Date)
  ) %>%
  group_by(LOB_SII_1, Accident_Year) %>%
  summarise(
    n_claims = n(),
    .groups = "drop"
  )

lobs_principais <- tabela_lob_claims %>%
  slice_max(n_claims, n = 10) %>%
  pull(LOB_SII_1)

claims_por_ano_lob_top <- claims_por_ano_lob %>%
  filter(LOB_SII_1 %in% lobs_principais)


datas_LOB <- ggplot(
  claims_por_ano_lob_top,
  aes(x = Accident_Year, y = n_claims, group = LOB_SII_1)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(
    ~ LOB_SII_1,
    scales = "free_y",
    axes = "all_x",
    axis.labels = "all_x"
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2030, by = 10)
  ) +
  labs(
    title = "Number of claims by accident year for the main LOBs",
    x = "Accident year",
    y = "n"
  ) +
  theme_minimal()

datas_LOB

# ggsave(
#   filename = "C:/Users/fatim/Downloads/datas_LOB.pdf",
#   plot = datas_LOB,
#   width = 8,
#   height = 6
# )

#dados so a partir de 2011

dados_agrupados_2011 <- dados_agrupados %>%
  mutate(
    Accident_Date = as.Date(Accident_Date),
    Accident_Year = year(Accident_Date)
  ) %>%
  filter(Accident_Year >= 2011)

nrow(dados_agrupados)
nrow(dados_agrupados_2011)

# Tabela com estatísticas e quantis de Incurred Claims para as principais LOBs, a partir de 2011

estatisticas_incurred_lob_principais <- dados_agrupados_2011 %>%
  filter(LOB_SII_1 %in% lobs_principais) %>%
  group_by(LOB_SII_1) %>%
  summarise(
    n_claims = n(),
    total_incurred = sum(Incurred_Claims, na.rm = TRUE),
    mean_incurred = mean(Incurred_Claims, na.rm = TRUE),
    median_incurred = median(Incurred_Claims, na.rm = TRUE),
    min_incurred = min(Incurred_Claims, na.rm = TRUE),
    q75 = quantile(Incurred_Claims, 0.75, na.rm = TRUE),
    q90 = quantile(Incurred_Claims, 0.90, na.rm = TRUE),
    q95 = quantile(Incurred_Claims, 0.95, na.rm = TRUE),
    q99 = quantile(Incurred_Claims, 0.99, na.rm = TRUE),
    max_incurred = max(Incurred_Claims, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(max_incurred))

print(estatisticas_incurred_lob_principais, n = Inf, width = Inf)

#boxplot apenas com valores positivos
dados_evt_2011 <- dados_agrupados_2011 %>%
  filter(
    Incurred_Claims > 0,
    LOB_SII_1 %in% lobs_principais
  )

boxplot= ggplot(
  dados_evt_2011,
  aes(
    x = reorder(LOB_SII_1, Incurred_Claims, median, na.rm = TRUE),
    y = Incurred_Claims
  )
) +
  geom_boxplot(fill = "steelblue", outlier.alpha = 0.4) +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Distribution of incurred claims by LOB",
    x = "LOB",
    y = "Incurred claims (log scale)"
  ) +
  theme_minimal()

boxplot

# ggsave(
#   filename = "C:/Users/fatim/Downloads/boxplot.pdf",
#   plot = boxplot,
#   width = 8,
#   height = 6
# )

#excessos
excessos_lob_principais <- dados_agrupados_2011 %>%
  filter(
    Incurred_Claims > 0,
    LOB_SII_1 %in% lobs_principais
  ) %>%
  group_by(LOB_SII_1) %>%
  summarise(
    n_claims = n(),
    u90 = quantile(Incurred_Claims, 0.90, na.rm = TRUE),
    n_exc_90 = sum(Incurred_Claims > u90, na.rm = TRUE),
    u95 = quantile(Incurred_Claims, 0.95, na.rm = TRUE),
    n_exc_95 = sum(Incurred_Claims > u95, na.rm = TRUE),
    u975 = quantile(Incurred_Claims, 0.975, na.rm = TRUE),
    n_exc_975 = sum(Incurred_Claims > u975, na.rm = TRUE),
    u99 = quantile(Incurred_Claims, 0.99, na.rm = TRUE),
    n_exc_99 = sum(Incurred_Claims > u99, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_claims))

print(excessos_lob_principais, n = Inf, width = Inf)

######################### LOBs escolhidas para aplicação dos métodos ############################

#Auto Danos a terceiros
lob_042_MTPL_Material_Damage <- dados_agrupados_2011 %>%
  filter(
    LOB_SII_1 == "MTPL Material Damage",
    Incurred_Claims > 0
  )

hist042= ggplot(
  lob_042_MTPL_Material_Damage %>% filter(Incurred_Claims > 0),
  aes(x = Incurred_Claims)
) +
  geom_histogram(bins = 80, fill = "steelblue", color = "black") +
  scale_x_log10(
    labels = label_number(big.mark = ",")
  ) +
  labs(
    title = "MTPL Material Damage",
    x = "Incurred Claims (log scale)",
    y = "Frequency"
  ) +
  theme_minimal()

hist042

# ggsave(
#   filename = "C:/Users/fatim/Downloads/hist042.pdf",
#   plot = hist042,
#   width = 8,
#   height = 6
# )

ggplot(
  lob_042_MTPL_Material_Damage %>% filter(Incurred_Claims > 0),
  aes(x = Incurred_Claims)
) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  scale_x_log10() +
  labs(
    title = "Density of incurred claims - 042_MTPL_Material_Damage",
    x = "Incurred Claims (log scale)",
    y = "Density"
  ) +
  theme_minimal()

#saude
lob_011_Health <- dados_agrupados_2011 %>%
  filter(
    LOB_SII_1 == "Health",
    Incurred_Claims > 0
  )

hist011= ggplot(
  lob_011_Health %>% filter(Incurred_Claims > 0),
  aes(x = Incurred_Claims)
) +
  geom_histogram(bins = 80, fill = "steelblue", color = "black") +
  scale_x_log10(
    labels = label_number(big.mark = ",")
  ) +
  labs(
    title = "Health",
    x = "Incurred Claims (log scale)",
    y = "Frequency"
  ) +
  theme_minimal()

hist011

# ggsave(
#   filename = "C:/Users/fatim/Downloads/hist011.pdf",
#   plot = hist011,
#   width = 8,
#   height = 6
# )

ggplot(
  lob_011_Health %>% filter(Incurred_Claims > 0),
  aes(x = Incurred_Claims)
) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  scale_x_log10() +
  labs(
    title = "Density of incurred claims - 011_Health",
    x = "Incurred Claims (log scale)",
    y = "Density"
  ) +
  theme_minimal()

#danos corporais outros
lob_041_MTPL_Bodily_Injury <- dados_agrupados_2011 %>%
  filter(
    LOB_SII_1 == "MTPL Bodily Injury",
    Incurred_Claims > 0
  )

hist041=ggplot(
  lob_041_MTPL_Bodily_Injury %>% filter(Incurred_Claims > 0),
  aes(x = Incurred_Claims)
) +
  geom_histogram(bins = 80, fill = "steelblue", color = "black") +
  scale_x_log10(
    labels = label_number(big.mark = ",")
  ) +
  labs(
    title = "MTPL Bodily Injury",
    x = "Incurred Claims (log scale)",
    y = "Frequency"
  ) +
  theme_minimal()

hist041

# ggsave(
#   filename = "C:/Users/fatim/Downloads/hist041.pdf",
#   plot = hist041,
#   width = 8,
#   height = 6
# )

ggplot(
  lob_041_MTPL_Bodily_Injury %>% filter(Incurred_Claims > 0),
  aes(x = Incurred_Claims)
) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  scale_x_log10() +
  labs(
    title = "Density of incurred claims - 041_MTPL_Bodily_Injury",
    x = "Incurred Claims (log scale)",
    y = "Density"
  ) +
  theme_minimal()


#BLOCK MAXIMA

#anual

#DANOS CORPORAIS TERCEIROS
max_anuais_052 <- lob_041_MTPL_Bodily_Injury %>%
  mutate(
    Accident_Date = as.Date(Accident_Date),
    Accident_Year = year(Accident_Date)
  ) %>%
  group_by(Accident_Year) %>%
  summarise(
    n_claims = n(),
    max_incurred = max(Incurred_Claims, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Accident_Year)

max_anuais_052

#SAUDE
max_anuais_011 <- lob_011_Health %>%
  mutate(
    Accident_Date = as.Date(Accident_Date),
    Accident_Year = year(Accident_Date)
  ) %>%
  group_by(Accident_Year) %>%
  summarise(
    n_claims = n(),
    max_incurred = max(Incurred_Claims, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Accident_Year)

max_anuais_011

#DANOS MATERIAIS TERCEIROS
max_anuais_042 <- lob_042_MTPL_Material_Damage %>%
  mutate(
    Accident_Date = as.Date(Accident_Date),
    Accident_Year = year(Accident_Date)
  ) %>%
  group_by(Accident_Year) %>%
  summarise(
    n_claims = n(),
    max_incurred = max(Incurred_Claims, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Accident_Year)

max_anuais_042

#Ajuste GEV

fit_gev_052 <- fevd(
  x = max_anuais_052$max_incurred,
  type = "GEV",
  method = "MLE"
)

summary(fit_gev_052)
plot(fit_gev_052)

#guardar
# pdf(
#   file = "C:/Users/fatim/Downloads/anual1.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot(fit_gev_052)
# 
# dev.off()

fit_gev_011 <- fevd(
  x = max_anuais_011$max_incurred,
  type = "GEV",
  method = "MLE"
)

summary(fit_gev_011)
plot(fit_gev_011)

# pdf(
#   file = "C:/Users/fatim/Downloads/anual2.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot(fit_gev_011)
# 
# dev.off()

fit_gev_042 <- fevd(
  x = max_anuais_042$max_incurred,
  type = "GEV",
  method = "MLE"
)

summary(fit_gev_042)
plot(fit_gev_042)

# pdf(
#   file = "C:/Users/fatim/Downloads/anual3.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot(fit_gev_042)
# 
# dev.off()~

#trimestral
max_trimestrais_052 <- lob_041_MTPL_Bodily_Injury %>%
  mutate(
    Accident_Date = as.Date(Accident_Date),
    Accident_Year = year(Accident_Date),
    Accident_Quarter = quarter(Accident_Date),
    Period = paste0(Accident_Year, "_Q", Accident_Quarter)
  ) %>%
  group_by(Period, Accident_Year, Accident_Quarter) %>%
  summarise(
    max_incurred = max(Incurred_Claims, na.rm = TRUE),
    n_claims_period = n(),
    .groups = "drop"
  ) %>%
  arrange(Accident_Year, Accident_Quarter)

max_trimestrais_052

print(max_trimestrais_052, n=Inf)

max_trimestrais_011 <- lob_011_Health %>%
  mutate(
    Accident_Date = as.Date(Accident_Date),
    Accident_Year = year(Accident_Date),
    Accident_Quarter = quarter(Accident_Date),
    Period = paste0(Accident_Year, "_Q", Accident_Quarter)
  ) %>%
  group_by(Period, Accident_Year, Accident_Quarter) %>%
  summarise(
    max_incurred = max(Incurred_Claims, na.rm = TRUE),
    n_claims_period = n(),
    .groups = "drop"
  ) %>%
  arrange(Accident_Year, Accident_Quarter)

max_trimestrais_011

print(max_trimestrais_011, n=Inf)

max_trimestrais_042 <- lob_042_MTPL_Material_Damage %>%
  mutate(
    Accident_Date = as.Date(Accident_Date),
    Accident_Year = year(Accident_Date),
    Accident_Quarter = quarter(Accident_Date),
    Period = paste0(Accident_Year, "_Q", Accident_Quarter)
  ) %>%
  group_by(Period, Accident_Year, Accident_Quarter) %>%
  summarise(
    max_incurred = max(Incurred_Claims, na.rm = TRUE),
    n_claims_period = n(),
    .groups = "drop"
  ) %>%
  arrange(Accident_Year, Accident_Quarter)

max_trimestrais_042

print(max_trimestrais_042, n=Inf)

fit_gev_052_tri <- fevd(
  x = max_trimestrais_052$max_incurred,
  type = "GEV",
  method = "MLE"
)

summary(fit_gev_052_tri)
plot(fit_gev_052_tri)

# pdf(
#   file = "C:/Users/fatim/Downloads/tri1.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot(fit_gev_052_tri)
# 
# dev.off()

fit_gev_011_tri <- fevd(
  x = max_trimestrais_011$max_incurred,
  type = "GEV",
  method = "MLE"
)

summary(fit_gev_011_tri)
plot(fit_gev_011_tri)

# pdf(
#   file = "C:/Users/fatim/Downloads/tri2.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot(fit_gev_011_tri)
# 
# dev.off()

fit_gev_042_tri <- fevd(
  x = max_trimestrais_042$max_incurred,
  type = "GEV",
  method = "MLE"
)

summary(fit_gev_042_tri)
plot(fit_gev_042_tri)

# pdf(
#   file = "C:/Users/fatim/Downloads/tri3.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot(fit_gev_042_tri)
# 
# dev.off()

#niveis de retorno

# Períodos de retorno em anos
return_period_years <- c(2, 5, 10, 15, 20)

rl_052_ann <- return.level(
  fit_gev_052,
  return.period = return_period_years
)

rl_011_ann <- return.level(
  fit_gev_011,
  return.period = return_period_years
)

rl_042_ann <- return.level(
  fit_gev_042,
  return.period = return_period_years
)

return_levels_annual <- data.frame(
  Return_Period_Years = return_period_years,
  MTPL_Bodily_Injury = as.numeric(rl_052_ann),
  MTPL_Material_Damage = as.numeric(rl_042_ann),
  Health = as.numeric(rl_011_ann)
)

return_levels_annual

# QUARTERLY MAXIMA

return_period_quarters <- return_period_years * 4

rl_052_tri <- return.level(
  fit_gev_052_tri,
  return.period = return_period_quarters
)

rl_011_tri <- return.level(
  fit_gev_011_tri,
  return.period = return_period_quarters
)

rl_042_tri <- return.level(
  fit_gev_042_tri,
  return.period = return_period_quarters
)

return_levels_quarterly <- data.frame(
  Return_Period_Years = return_period_years,
  MTPL_Bodily_Injury = as.numeric(rl_052_tri),
  MTPL_Material_Damage = as.numeric(rl_042_tri),
  Health = as.numeric(rl_011_tri)
)

return_levels_quarterly

#POT

dados_pot_042 <- lob_042_MTPL_Material_Damage %>%
  filter(Incurred_Claims > 0)
x_042 <- dados_pot_042$Incurred_Claims


dados_pot_011 <- lob_011_Health %>%
  filter(Incurred_Claims > 0)
x_011 <- dados_pot_011$Incurred_Claims


dados_pot_041 <- lob_041_MTPL_Bodily_Injury %>%
  filter(Incurred_Claims > 0)
x_041 <- dados_pot_041$Incurred_Claims

#quantis
q_041 <- round(quantile(x_041, probs = c(0.90, 0.94, 0.95, 0.96, 0.975, 0.99), na.rm = TRUE), 2); q_041
q_011 <- quantile(x_011, probs = c(0.90, 0.95, 0.96, 0.975, 0.99), na.rm = TRUE);q_011
q_042 <- round(quantile(x_042, probs = c(0.90, 0.95,0.96, 0.975, 0.99), na.rm = TRUE),2);q_042

#MRL

mrlplot(x_041, main = "MRL Plot - MTPL Bodily Injury") #library(POT)

# pdf(
#   "C:/Users/fatim/Downloads/MRL1.pdf",
#   width = 8,
#   height = 6
# )
# 
# mrlplot(
#   x_041,
#   main = "MRL Plot - MTPL Bodily Injury",
#   xlab = "u",
#   ylab = "e(u)",
#   xaxt = "n",
#   yaxt = "n"
# )
# legend(
#   "topleft",
#   legend = c("Média empírica dos excessos", "Intervalo de confiança 95%"),
#   col = c("black", "grey"),
#   lty = c(1, 1),
#   lwd = c(2, 1),
#   bty = "n"
# )
# 
# axis(1, at = axTicks(1), labels = scales::comma(axTicks(1)))
# axis(2, at = axTicks(2), labels = scales::comma(axTicks(2)))
# 
# dev.off()

analise_mrl_numerica <- function(x, probs = seq(0.80, 0.995, by = 0.005)) {
  
  x <- as.numeric(x)
  x <- x[is.finite(x) & x > 0]
  
  thresholds <- as.numeric(quantile(x, probs = probs, na.rm = TRUE))
  
  res <- data.frame()
  
  for (i in seq_along(thresholds)) {
    
    u <- thresholds[i]
    excessos <- x[x > u] - u
    n_exc <- length(excessos)
    
    if (n_exc < 5) next
    
    mean_exc <- mean(excessos)
    sd_exc <- sd(excessos)
    se_exc <- sd_exc / sqrt(n_exc)
    
    res <- rbind(
      res,
      data.frame(
        prob_threshold = probs[i],
        threshold = u,
        n_excessos = n_exc,
        mean_excess = mean_exc,
        se = se_exc,
        ci_inf = mean_exc - 1.96 * se_exc,
        ci_sup = mean_exc + 1.96 * se_exc
      )
    )
  }
  
  res$variacao_mean_excess <- c(NA, diff(res$mean_excess))
  res$variacao_relativa <- c(NA, diff(res$mean_excess) / head(res$mean_excess, -1))
  
  return(res)
}

tab_mrl_041 <- analise_mrl_numerica(x_041)
#View(tab_mrl_041)

# write.csv(
#   tab_mrl_041,
#   file = "tabela_mrl_041.csv",
#   row.names = FALSE
# )

#Health
mrlplot(x_011, main = "MRL Plot - Health")

# pdf(
#   "C:/Users/fatim/Downloads/MRL2.pdf",
#   width = 8,
#   height = 6
# )
# 
# mrlplot(
#   x_011,
#   main = "MRL Plot - Health",
#   xlab = "u",
#   ylab = "e(u)",
#   xaxt = "n",
#   yaxt = "n"
# )
# legend(
#   "topright",
#   legend = c("Média empírica dos excessos", "Intervalo de confiança 95%"),
#   col = c("black", "grey"),
#   lty = c(1, 1),
#   lwd = c(2, 1),
#   bty = "n"
# )
# 
# axis(1, at = axTicks(1), labels = scales::comma(axTicks(1)))
# axis(2, at = axTicks(2), labels = scales::comma(axTicks(2)))
# 
# dev.off()

tab_mrl_011 <- analise_mrl_numerica(x_011)
#View(tab_mrl_011)

# write.csv(
#   tab_mrl_011,
#   file = "tabela_mrl_011.csv",
#   row.names = FALSE
# )

#Material Damage
mrlplot(x_042, main = "MRL Plot - MTPL Material Damage")

# pdf(
#   "C:/Users/fatim/Downloads/MRL3.pdf",
#   width = 8,
#   height = 6
# )
# 
# mrlplot(
#   x_042,
#   main = "MRL Plot - MTPL Material Damage",
#   xlab = "u",
#   ylab = "e(u)",
#   xaxt = "n",
#   yaxt = "n"
# )
# legend(
#   "topleft",
#   legend = c("Média empírica dos excessos", "Intervalo de confiança 95%"),
#   col = c("black", "grey"),
#   lty = c(1, 1),
#   lwd = c(2, 1),
#   bty = "n"
# )
# 
# axis(1, at = axTicks(1), labels = scales::comma(axTicks(1)))
# axis(2, at = axTicks(2), labels = scales::comma(axTicks(2)))
# 
# dev.off()

tab_mrl_042 <- analise_mrl_numerica(x_042)
#View(tab_mrl_042)

# write.csv(
#   tab_mrl_042,
#   file = "tabela_mrl_042.csv",
#   row.names = FALSE
# )

#stability threshold

#MLE
threshold_stability_probs <- function(dados_lob, nome_lob,
                                      probs = seq(0.8, 0.98, by = 0.005),
                                      min_excessos = 30) {
  
  x <- dados_lob %>%
    filter(Incurred_Claims > 0) %>%
    pull(Incurred_Claims)
  
  thresholds <- quantile(x, probs = probs, na.rm = TRUE)
  
  resultados <- lapply(seq_along(thresholds), function(i) {
    
    u <- as.numeric(thresholds[i])
    p <- probs[i]
    n_exc <- sum(x > u, na.rm = TRUE)
    
    if (n_exc < min_excessos) {
      return(NULL)
    }
    
    fit <- tryCatch(
      fevd(
        x = x,
        threshold = u,
        type = "GP",
        method = "MLE"
      ), #library(extRemes)
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      return(NULL)
    }
    
    par <- fit$results$par
    
    sigma <- par["scale"]
    xi <- par["shape"]
    sigma_star <- sigma - xi * u
    
    data.frame(
      LOB = nome_lob,
      prob_threshold = p,
      threshold = u,
      n_excessos = n_exc,
      scale = sigma,
      shape = xi,
      sigma_star = sigma_star
    )
  })
  
  bind_rows(resultados)
}

stab_probs_041 <- threshold_stability_probs(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL_Bodily_Injury",
  probs = seq(0.8, 0.98, by = 0.005),
  min_excessos = 10
)

stab_probs_041

stab_probs_011 <- threshold_stability_probs(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  probs = seq(0.8, 0.98, by = 0.005),
  min_excessos = 10
)

stab_probs_011


stab_probs_042 <- threshold_stability_probs(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "042_MTPL_Material_Damage",
  probs = seq(0.70, 0.98, by = 0.01),
  min_excessos = 10
)

stab_probs_042

threshold_stability_probs_evd <- function(dados_lob, nome_lob,
                                          probs = seq(0.70, 0.98, by = 0.01),
                                          min_excessos = 10) {
  
  x <- dados_lob %>%
    filter(
      !is.na(Incurred_Claims),
      is.finite(Incurred_Claims),
      Incurred_Claims > 0
    ) %>%
    pull(Incurred_Claims)
  
  thresholds <- as.numeric(quantile(x, probs = probs, na.rm = TRUE))
  
  resultados <- lapply(seq_along(thresholds), function(i) {
    
    u <- thresholds[i]
    p <- probs[i]
    n_exc <- sum(x > u, na.rm = TRUE)
    
    if (n_exc < min_excessos) {
      return(NULL)
    }
    
    fit <- tryCatch(
      evd::fpot(
        x = x,
        threshold = u,
        model = "gpd",
        std.err = FALSE
      ),
      error = function(e) {
        message("Erro no threshold ", round(u, 2), ": ", e$message)
        return(NULL)
      },
      warning = function(w) {
        message("Aviso no threshold ", round(u, 2), ": ", w$message)
        invokeRestart("muffleWarning")
      }
    )
    
    if (is.null(fit)) {
      return(NULL)
    }
    
    par <- fit$estimate
    
    sigma <- as.numeric(par["scale"])
    xi <- as.numeric(par["shape"])
    
    sigma_star <- sigma - xi * u
    
    data.frame(
      LOB = nome_lob,
      method = "MLE_evd",
      prob_threshold = p,
      threshold = u,
      n_excessos = n_exc,
      scale = sigma,
      shape = xi,
      sigma_star = sigma_star
    )
  })
  
  bind_rows(resultados)
}

##MLE, MOM, PWM
threshold_stability_probs_pot <- function(dados_lob, nome_lob,
                                          probs = seq(0.70, 0.98, by = 0.01),
                                          est = "mle",
                                          min_excessos = 10) {
  
  x <- dados_lob %>%
    filter(Incurred_Claims > 0) %>%
    pull(Incurred_Claims)
  
  thresholds <- quantile(x, probs = probs, na.rm = TRUE)
  
  resultados <- lapply(seq_along(thresholds), function(i) {
    
    u <- as.numeric(thresholds[i])
    p <- probs[i]
    n_exc <- sum(x > u, na.rm = TRUE)
    
    if (n_exc < min_excessos) {
      return(NULL)
    }
    
    fit <- tryCatch(
      POT::fitgpd(
        data = x,
        threshold = u,
        est = est
      ),
      error = function(e) {
        message("Erro no threshold ", round(u, 2), 
                " com método ", est, ": ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(fit)) {
      return(NULL)
    }
    
    sigma <- fit$param["scale"]
    xi <- fit$param["shape"]
    sigma_star <- sigma - xi * u
    
    data.frame(
      LOB = nome_lob,
      method = est,
      prob_threshold = p,
      threshold = u,
      n_excessos = n_exc,
      scale = as.numeric(sigma),
      shape = as.numeric(xi),
      sigma_star = as.numeric(sigma_star)
    )
  })
  
  bind_rows(resultados)
}

stab_mle_041 <- threshold_stability_probs_evd(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL_Bodily_Injury",
  probs = seq(0.80, 0.98, by = 0.005),
  min_excessos = 10
)

stab_mle_041


stab_mom_041 <- threshold_stability_probs_pot(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "041_MTPL_Bodily_Injury",
  probs = seq(0.8, 0.98, by = 0.005),
  est = "moments",
  min_excessos = 10
)

stab_mom_041

stab_pwm_041 <- threshold_stability_probs_pot(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "041_MTPL_Bodily_Injury",
  probs = seq(0.8, 0.98, by = 0.005),
  est = "pwmu",
  min_excessos = 10
)

stab_pwm_041

stab_all_041 <- bind_rows(
  stab_mle_041,
  stab_mom_041,
  stab_pwm_041
)

stab_all_041

compar1 <- ggplot(stab_all_041, aes(x = threshold, y = shape, 
                                    colour = method, linetype = method)) +
  geom_line() +
  geom_point(size = 1.2) +
  scale_x_continuous(
    labels = comma
  ) +
  labs(
    title = "Threshold stability plot for shape parameter",
    subtitle = "MTPL Bodily Injury",
    x = expression(u),
    y = expression(hat(xi)(u)),
    colour = "Estimation method",
    linetype = "Estimation method"
  ) +
  scale_colour_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  scale_linetype_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  theme_minimal()

compar1

# ggsave(
#   filename = "C:/Users/fatim/Downloads/threshold_stability_comparaçao1.pdf",
#   plot = compar1,
#   width = 8,
#   height = 6
# )

compar2 <- ggplot(stab_all_041, aes(x = threshold, y = sigma_star, color = method, linetype = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    labels = comma
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Threshold stability plot for modified scale parameter",
    subtitle = "MTPL Bodily Injury",
    x = "u",
    y = expression(hat(sigma)^"*"),
    colour = "Estimation method",
    linetype = "Estimation method"
  ) +
  scale_colour_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  scale_linetype_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  theme_minimal()

compar2

# ggsave(
#   filename = "C:/Users/fatim/Downloads/threshold_stability_comparaçao2.pdf",
#   plot = compar2,
#   width = 8,
#   height = 6
# )

#health
stab_mle_011 <- threshold_stability_probs_evd(
  dados_lob = lob_011_Health,
  nome_lob = "011_Health",
  probs = seq(0.80, 0.98, by = 0.005),
  min_excessos = 10
)

stab_mle_011

stab_mom_011 <- threshold_stability_probs_pot(
  dados_lob = lob_011_Health,
  nome_lob = "011_Health",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "moments",
  min_excessos = 10
)

stab_mom_011

stab_pwm_011 <- threshold_stability_probs_pot(
  dados_lob = lob_011_Health,
  nome_lob = "011_Health",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "pwmu",
  min_excessos = 10
)

stab_pwm_011

stab_all_011 <- bind_rows(
  stab_mle_011,
  stab_mom_011,
  stab_pwm_011
)

stab_all_011

compar3 <- ggplot(stab_all_011, aes(x = threshold, y = shape, color = method, linetype = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    labels = comma
  ) +
  labs(
    title = "Threshold stability plot for shape parameter",
    subtitle = "Health",
    x = "u",
    y = expression(hat(xi)),
    colour = "Estimation method",
    linetype = "Estimation method"
  ) +
  scale_colour_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  scale_linetype_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  theme_minimal()

compar3

# ggsave(
#   filename = "C:/Users/fatim/Downloads/threshold_stability_comparaçao3.pdf",
#   plot = compar3,
#   width = 8,
#   height = 6
# )

compar4 <- ggplot(stab_all_011, aes(x = threshold, y = sigma_star, color = method, linetype = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    labels = comma
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Threshold stability plot for modified scale parameter",
    subtitle = "Health",
    x = "u",
    y = expression(hat(sigma)^"*"),
    colour = "Estimation method",
    linetype = "Estimation method"
  ) +
  scale_colour_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  scale_linetype_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  theme_minimal()

compar4

# ggsave(
#   filename = "C:/Users/fatim/Downloads/threshold_stability_comparaçao4.pdf",
#   plot = compar4,
#   width = 8,
#   height = 6
# )


#material damage
stab_mle_052 <- threshold_stability_probs_evd(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL_Material_Damage",
  probs = seq(0.80, 0.98, by = 0.005),
  min_excessos = 10
)

stab_mle_052

stab_mom_052 <- threshold_stability_probs_pot(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL_Material_Damage",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "moments",
  min_excessos = 10
)

stab_mom_052

stab_pwm_052 <- threshold_stability_probs_pot(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL_Material_Damage",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "pwmu",
  min_excessos = 10
)

stab_pwm_052

stab_all_052 <- bind_rows(
  stab_mle_052,
  stab_mom_052,
  stab_pwm_052
)

stab_all_052

compar5 <- ggplot(stab_all_052, aes(x = threshold, y = shape, color = method, linetype = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    labels = comma
  ) +
  labs(
    title = "Threshold stability plot for shape parameter",
    subtitle = "MTPL Material Damage",
    x = "u",
    y = expression(hat(xi)),
    colour = "Estimation method",
    linetype = "Estimation method"
  ) +
  scale_colour_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  scale_linetype_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  theme_minimal()

compar5

# ggsave(
#   filename = "C:/Users/fatim/Downloads/threshold_stability_comparaçao5.pdf",
#   plot = compar5,
#   width = 8,
#   height = 6
# )

compar6 <- ggplot(stab_all_052, aes(x = threshold, y = sigma_star, color = method, linetype = method)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    labels = comma
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Threshold stability plot for modified scale parameter",
    subtitle = "MTPL Material Damage",
    x = "u",
    y = expression(hat(sigma)^"*"),
    colour = "Estimation method",
    linetype = "Estimation method"
  ) +
  scale_colour_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  scale_linetype_discrete(
    labels = c(
      "MLE_evd" = "MLE",
      "moments" = "MOM",
      "pwmu" = "PWM"
    )
  ) +
  theme_minimal()

compar6

# ggsave(
#   filename = "C:/Users/fatim/Downloads/threshold_stability_comparaçao6.pdf",
#   plot = compar6,
#   width = 8,
#   height = 6
# )

#QQ plot 
probs_041 <- seq(0.80, 0.98, by = 0.005)

#Bodily Injury

probs_041_1 <- seq(0.80, 0.89, by = 0.005)

par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))

for (p in probs_041_1) {
  
  u_041 <- quantile(
    lob_041_MTPL_Bodily_Injury$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_041 <- fevd(
    x = lob_041_MTPL_Bodily_Injury$Incurred_Claims,
    threshold = u_041,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_041,
    type = "qq",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_041, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots1_parte1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(4, 5), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# for (p in probs_041_1) {
#   
#   u_041 <- quantile(
#     lob_041_MTPL_Bodily_Injury$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_041 <- fevd(
#     x = lob_041_MTPL_Bodily_Injury$Incurred_Claims,
#     threshold = u_041,
#     type = "GP",
#     method = "MLE"
#   )
#   
#   plot(
#     fit_gpd_041,
#     type = "qq",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_041, 0))
#     )
#   )
# }
# 
# mtext(
#   "QQ plots - MTPL Bodily Injury - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

probs_041_2 <- seq(0.895, 0.98, by = 0.005)

par(mfrow = c(3, 6), mar = c(4, 4, 3, 1))

for (p in probs_041_2) {
  
  u_041 <- quantile(
    lob_041_MTPL_Bodily_Injury$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_041 <- fevd(
    x = lob_041_MTPL_Bodily_Injury$Incurred_Claims,
    threshold = u_041,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_041,
    type = "qq",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_041, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots1_parte2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(3, 6), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# for (p in probs_041_2) {
#   
#   u_041 <- quantile(
#     lob_041_MTPL_Bodily_Injury$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_041 <- fevd(
#     x = lob_041_MTPL_Bodily_Injury$Incurred_Claims,
#     threshold = u_041,
#     type = "GP",
#     method = "MLE"
#   )
#   
#   plot(
#     fit_gpd_041,
#     type = "qq",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_041, 0))
#     )
#   )
# }
# 
# mtext(
#   "QQ plots - MTPL Bodily Injury - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

#tabela
qq_diagnostic_table <- function(dados_lob, nome_lob,
                                probs = seq(0.80, 0.98, by = 0.005),
                                est = "mle",
                                min_excessos = 30) {
  
  x <- dados_lob %>%
    filter(
      !is.na(Incurred_Claims),
      is.finite(Incurred_Claims),
      Incurred_Claims > 0
    ) %>%
    pull(Incurred_Claims)
  
  thresholds <- as.numeric(quantile(x, probs = probs, na.rm = TRUE))
  
  resultados <- lapply(seq_along(thresholds), function(i) {
    
    u <- thresholds[i]
    p <- probs[i]
    
    excessos <- sort(x[x > u] - u)
    n_exc <- length(excessos)
    
    if (n_exc < min_excessos) return(NULL)
    
    fit <- tryCatch(
      POT::fitgpd(
        data = x,
        threshold = u,
        est = est
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit)) return(NULL)
    
    sigma <- as.numeric(fit$param["scale"])
    xi <- as.numeric(fit$param["shape"])
    
    pp <- ppoints(n_exc)
    
    q_model <- if (abs(xi) < 1e-8) {
      -sigma * log(1 - pp)
    } else {
      sigma / xi * ((1 - pp)^(-xi) - 1)
    }
    
    # Diagnóstico sobre os excessos
    residuals <- excessos - q_model
    
    rmse_qq <- sqrt(mean(residuals^2))
    mae_qq <- mean(abs(residuals))
    max_abs_dev <- max(abs(residuals))
    
    # Correlação entre quantis empíricos e teóricos
    corr_qq <- cor(excessos, q_model)
    
    data.frame(
      LOB = nome_lob,
      method = est,
      prob_threshold = p,
      threshold = u,
      n_excessos = n_exc,
      scale = sigma,
      shape = xi,
      RMSE_QQ = rmse_qq,
      MAE_QQ = mae_qq,
      MaxAbsDev_QQ = max_abs_dev,
      Corr_QQ = corr_qq
    )
  })
  
  bind_rows(resultados)
}

qq_tab_041_mle <- qq_diagnostic_table(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "mle",
  min_excessos = 30
)

#View(qq_tab_041_mle)

# write.csv(
#   qq_tab_041_mle,
#   "qq_diagnostic_table_041_mle.csv",
#   row.names = FALSE
# )

###Material Damage

#MLE
par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))

for (p in probs_041_1) {
  
  u_042 <- quantile(
    lob_042_MTPL_Material_Damage$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_042 <- fevd(
    x = lob_042_MTPL_Material_Damage$Incurred_Claims,
    threshold = u_042,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_042,
    type = "qq",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_042, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots2_parte1_mle.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(4, 5), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# 
# for (p in probs_041_1) {
#   
#   u_042 <- quantile(
#     lob_042_MTPL_Material_Damage$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_042 <- fevd(
#     x = lob_042_MTPL_Material_Damage$Incurred_Claims,
#     threshold = u_042,
#     type = "GP",
#     method = "MLE"
#   )
#   
#   plot(
#     fit_gpd_042,
#     type = "qq",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_042, 0))
#     )
#   )
# }
# 
# mtext(
#   "QQ plots - MTPL Material Damage - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

par(mfrow = c(3, 6), mar = c(4, 4, 3, 1))

for (p in probs_041_2) {
  
  u_042 <- quantile(
    lob_042_MTPL_Material_Damage$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_042 <- fevd(
    x = lob_042_MTPL_Material_Damage$Incurred_Claims,
    threshold = u_042,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_042,
    type = "qq",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_042, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots2_parte2_mle.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(3, 6), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# 
# for (p in probs_041_2) {
#   
#   u_042 <- quantile(
#     lob_042_MTPL_Material_Damage$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_042 <- fevd(
#     x = lob_042_MTPL_Material_Damage$Incurred_Claims,
#     threshold = u_042,
#     type = "GP",
#     method = "MLE"
#   )
#   
#   plot(
#     fit_gpd_042,
#     type = "qq",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_042, 0))
#     )
#   )
# }
# 
# mtext(
#   "QQ plots - MTPL Material Damage - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

qq_tab_042_mle <- qq_diagnostic_table(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "mle",
  min_excessos = 30
)

# View(qq_tab_042_mle)

# write.csv(
#   qq_tab_042_mle,
#   "qq_diagnostic_table_042_mle.csv",
#   row.names = FALSE
# )

#MOM
par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))

for (p in probs_041_1) {
  
  u_042 <- quantile(
    lob_042_MTPL_Material_Damage$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_042_mom <- POT::fitgpd(
    data = lob_042_MTPL_Material_Damage$Incurred_Claims,
    threshold = u_042,
    est = "moments"
  )
  
  excessos <- sort(lob_042_MTPL_Material_Damage$Incurred_Claims[
    lob_042_MTPL_Material_Damage$Incurred_Claims > u_042
  ] - u_042)
  
  n_exc <- length(excessos)
  probs_emp <- ppoints(n_exc)
  
  sigma_hat <- fit_gpd_042_mom$param["scale"]
  xi_hat <- fit_gpd_042_mom$param["shape"]
  
  if (abs(xi_hat) < 1e-8) {
    q_theo <- -sigma_hat * log(1 - probs_emp)
  } else {
    q_theo <- sigma_hat / xi_hat * ((1 - probs_emp)^(-xi_hat) - 1)
  }
  
  plot(
    q_theo,
    excessos,
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_042, 0))
    ),
    xlab = "Theoretical quantiles",
    ylab = "Empirical excesses",
    pch = 16,
    cex = 0.6
  )
  
  abline(0, 1, lty = 2)
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots2_parte1_mom.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(4, 5), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# for (p in probs_041_1) {
#   
#   u_042 <- quantile(
#     lob_042_MTPL_Material_Damage$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_042_mom <- POT::fitgpd(
#     data = lob_042_MTPL_Material_Damage$Incurred_Claims,
#     threshold = u_042,
#     est = "moments"
#   )
#   
#   excessos <- sort(lob_042_MTPL_Material_Damage$Incurred_Claims[
#     lob_042_MTPL_Material_Damage$Incurred_Claims > u_042
#   ] - u_042)
#   
#   n_exc <- length(excessos)
#   probs_emp <- ppoints(n_exc)
#   
#   sigma_hat <- fit_gpd_042_mom$param["scale"]
#   xi_hat <- fit_gpd_042_mom$param["shape"]
#   
#   if (abs(xi_hat) < 1e-8) {
#     q_theo <- -sigma_hat * log(1 - probs_emp)
#   } else {
#     q_theo <- sigma_hat / xi_hat * ((1 - probs_emp)^(-xi_hat) - 1)
#   }
#   
#   plot(
#     q_theo,
#     excessos,
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_042, 0))
#     ),
#     xlab = "Theoretical quantiles",
#     ylab = "Empirical excesses",
#     pch = 16,
#     cex = 0.6
#   )
#   
#   abline(0, 1, lty = 2)
# }
# 
# mtext(
#   "QQ plots - MTPL Material Damage - GP estimated by MOM",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# dev.off()

par(mfrow = c(3, 6), mar = c(4, 4, 3, 1))

for (p in probs_041_2) {
  
  u_042 <- quantile(
    lob_042_MTPL_Material_Damage$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_042_mom <- POT::fitgpd(
    data = lob_042_MTPL_Material_Damage$Incurred_Claims,
    threshold = u_042,
    est = "moments"
  )
  
  excessos <- sort(lob_042_MTPL_Material_Damage$Incurred_Claims[
    lob_042_MTPL_Material_Damage$Incurred_Claims > u_042
  ] - u_042)
  
  n_exc <- length(excessos)
  probs_emp <- ppoints(n_exc)
  
  sigma_hat <- fit_gpd_042_mom$param["scale"]
  xi_hat <- fit_gpd_042_mom$param["shape"]
  
  if (abs(xi_hat) < 1e-8) {
    q_theo <- -sigma_hat * log(1 - probs_emp)
  } else {
    q_theo <- sigma_hat / xi_hat * ((1 - probs_emp)^(-xi_hat) - 1)
  }
  
  plot(
    q_theo,
    excessos,
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_042, 0))
    ),
    xlab = "Theoretical quantiles",
    ylab = "Empirical excesses",
    pch = 16,
    cex = 0.6
  )
  
  abline(0, 1, lty = 2)
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots2_parte2_mom.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(3, 6), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# for (p in probs_041_2) {
#   
#   u_042 <- quantile(
#     lob_042_MTPL_Material_Damage$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_042_mom <- POT::fitgpd(
#     data = lob_042_MTPL_Material_Damage$Incurred_Claims,
#     threshold = u_042,
#     est = "moments"
#   )
#   
#   excessos <- sort(lob_042_MTPL_Material_Damage$Incurred_Claims[
#     lob_042_MTPL_Material_Damage$Incurred_Claims > u_042
#   ] - u_042)
#   
#   n_exc <- length(excessos)
#   probs_emp <- ppoints(n_exc)
#   
#   sigma_hat <- fit_gpd_042_mom$param["scale"]
#   xi_hat <- fit_gpd_042_mom$param["shape"]
#   
#   if (abs(xi_hat) < 1e-8) {
#     q_theo <- -sigma_hat * log(1 - probs_emp)
#   } else {
#     q_theo <- sigma_hat / xi_hat * ((1 - probs_emp)^(-xi_hat) - 1)
#   }
#   
#   plot(
#     q_theo,
#     excessos,
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_042, 0))
#     ),
#     xlab = "Theoretical quantiles",
#     ylab = "Empirical excesses",
#     pch = 16,
#     cex = 0.6
#   )
#   
#   abline(0, 1, lty = 2)
# }
# 
# mtext(
#   "QQ plots - MTPL Material Damage - GP estimated by MOM",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# dev.off()

qq_tab_042_mom <- qq_diagnostic_table(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "moments",
  min_excessos = 30
)

#View(qq_tab_042_mom)

# write.csv(
#   qq_tab_042_mom,
#   "qq_diagnostic_table_042_mom.csv",
#   row.names = FALSE
# )

#Health

##MOM
par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))

for (p in probs_041_1) {
  
  u_011 <- quantile(
    lob_011_Health$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_011 <- POT::fitgpd(
    data = lob_011_Health$Incurred_Claims,
    threshold = u_011,
    est = "moments"
  )
  
  sigma <- fit_gpd_011$param["scale"]
  xi <- fit_gpd_011$param["shape"]
  
  excessos <- sort(
    lob_011_Health$Incurred_Claims[
      lob_011_Health$Incurred_Claims > u_011
    ] - u_011
  )
  
  n <- length(excessos)
  p_emp <- ppoints(n)
  
  q_teoricos <- if (abs(xi) < 1e-8) {
    -sigma * log(1 - p_emp)
  } else {
    (sigma / xi) * ((1 - p_emp)^(-xi) - 1)
  }
  
  plot(
    q_teoricos,
    excessos,
    xlab = "Theoretical GPD quantiles",
    ylab = "Empirical excess quantiles",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_011, 0))
    ),
    pch = 1
  )
  
  abline(0, 1, lty = 2)
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots3_parte1_mom.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(4, 5), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# for (p in probs_041_1) {
#   
#   u_011 <- quantile(
#     lob_011_Health$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_011 <- POT::fitgpd(
#     data = lob_011_Health$Incurred_Claims,
#     threshold = u_011,
#     est = "moments"
#   )
#   
#   sigma <- fit_gpd_011$param["scale"]
#   xi <- fit_gpd_011$param["shape"]
#   
#   excessos <- sort(
#     lob_011_Health$Incurred_Claims[
#       lob_011_Health$Incurred_Claims > u_011
#     ] - u_011
#   )
#   
#   n <- length(excessos)
#   p_emp <- ppoints(n)
#   
#   q_teoricos <- if (abs(xi) < 1e-8) {
#     -sigma * log(1 - p_emp)
#   } else {
#     (sigma / xi) * ((1 - p_emp)^(-xi) - 1)
#   }
#   
#   plot(
#     q_teoricos,
#     excessos,
#     xlab = "Theoretical GPD quantiles",
#     ylab = "Empirical excess quantiles",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_011, 0))
#     ),
#     pch = 1
#   )
#   
#   abline(0, 1, lty = 2)
# }
# 
# mtext(
#   "QQ plots - Health - GP estimated by MOM",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

par(mfrow = c(3, 6), mar = c(4, 4, 3, 1))

for (p in probs_041_2) {
  
  u_011 <- quantile(
    lob_011_Health$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_011 <- POT::fitgpd(
    data = lob_011_Health$Incurred_Claims,
    threshold = u_011,
    est = "moments"
  )
  
  sigma <- fit_gpd_011$param["scale"]
  xi <- fit_gpd_011$param["shape"]
  
  excessos <- sort(
    lob_011_Health$Incurred_Claims[
      lob_011_Health$Incurred_Claims > u_011
    ] - u_011
  )
  
  n <- length(excessos)
  p_emp <- ppoints(n)
  
  q_teoricos <- if (abs(xi) < 1e-8) {
    -sigma * log(1 - p_emp)
  } else {
    (sigma / xi) * ((1 - p_emp)^(-xi) - 1)
  }
  
  plot(
    q_teoricos,
    excessos,
    xlab = "Theoretical GPD quantiles",
    ylab = "Empirical excess quantiles",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_011, 0))
    ),
    pch = 1
  )
  
  abline(0, 1, lty = 2)
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots3_parte2_mom.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(3, 6), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# for (p in probs_041_2) {
#   
#   u_011 <- quantile(
#     lob_011_Health$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_011 <- POT::fitgpd(
#     data = lob_011_Health$Incurred_Claims,
#     threshold = u_011,
#     est = "moments"
#   )
#   
#   sigma <- fit_gpd_011$param["scale"]
#   xi <- fit_gpd_011$param["shape"]
#   
#   excessos <- sort(
#     lob_011_Health$Incurred_Claims[
#       lob_011_Health$Incurred_Claims > u_011
#     ] - u_011
#   )
#   
#   n <- length(excessos)
#   p_emp <- ppoints(n)
#   
#   q_teoricos <- if (abs(xi) < 1e-8) {
#     -sigma * log(1 - p_emp)
#   } else {
#     (sigma / xi) * ((1 - p_emp)^(-xi) - 1)
#   }
#   
#   plot(
#     q_teoricos,
#     excessos,
#     xlab = "Theoretical GPD quantiles",
#     ylab = "Empirical excess quantiles",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_011, 0))
#     ),
#     pch = 1
#   )
#   
#   abline(0, 1, lty = 2)
# }
# 
# mtext(
#   "QQ plots - Health - GP estimated by MOM",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

qq_tab_011_mom <- qq_diagnostic_table(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "moments",
  min_excessos = 30
)

#View(qq_tab_011_mom)

# write.csv(
#   qq_tab_011_mom,
#   "qq_diagnostic_table_011_mom.csv",
#   row.names = FALSE
# )

#MLE
par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))

for (p in probs_041_1) {
  
  u_011 <- quantile(
    lob_011_Health$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_011 <- fevd(
    x = lob_011_Health$Incurred_Claims,
    threshold = u_011,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_011,
    type = "qq",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_011, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots3_parte1_mle.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(4, 5), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# for (p in probs_041_1) {
#   
#   u_011 <- quantile(
#     lob_011_Health$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_011 <- fevd(
#     x = lob_011_Health$Incurred_Claims,
#     threshold = u_011,
#     type = "GP",
#     method = "MLE"
#   )
#   
#   plot(
#     fit_gpd_011,
#     type = "qq",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_011, 0))
#     )
#   )
# }
# 
# mtext(
#   "QQ plots - Health - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

par(mfrow = c(3, 6), mar = c(4, 4, 3, 1))

for (p in probs_041_2) {
  
  u_011 <- quantile(
    lob_011_Health$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_011 <- fevd(
    x = lob_011_Health$Incurred_Claims,
    threshold = u_011,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_011,
    type = "qq",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_011, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots3_parte2_mle.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(3, 6), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# 
# for (p in probs_041_2) {
#   
#   u_011 <- quantile(
#     lob_011_Health$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_011 <- fevd(
#     x = lob_011_Health$Incurred_Claims,
#     threshold = u_011,
#     type = "GP",
#     method = "MLE"
#   )
#   
#   plot(
#     fit_gpd_011,
#     type = "qq",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_011, 0))
#     )
#   )
# }
# 
# mtext(
#   "QQ plots - Health - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

qq_tab_011_mle <- qq_diagnostic_table(
  dados_lob = lob_011_Health,
  nome_lob = "MTPL Health",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "mle",
  min_excessos = 30
)

# View(qq_tab_011_mle)

# write.csv(
#   qq_tab_011_mle,
#   "qq_diagnostic_table_011_mle.csv",
#   row.names = FALSE
# )

#probability plot
par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))

for (p in probs_041_1) {
  
  u_041 <- quantile(
    lob_041_MTPL_Bodily_Injury$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_041 <- fevd(
    x = lob_041_MTPL_Bodily_Injury$Incurred_Claims,
    threshold = u_041,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_041,
    type = "probprob",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_041, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/probplot1_parte1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(4, 5), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# for (p in probs_041_1) {
# 
#   u_041 <- quantile(
#     lob_041_MTPL_Bodily_Injury$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
# 
#   fit_gpd_041 <- fevd(
#     x = lob_041_MTPL_Bodily_Injury$Incurred_Claims,
#     threshold = u_041,
#     type = "GP",
#     method = "MLE"
#   )
# 
#   plot(
#     fit_gpd_041,
#     type = "probprob",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_041, 0))
#     )
#   )
# }
# 
# mtext(
#   "Probability plots - MTPL Bodily Injury - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

par(mfrow = c(3, 6), mar = c(4, 4, 3, 1))

for (p in probs_041_2) {
  
  u_041 <- quantile(
    lob_041_MTPL_Bodily_Injury$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_041 <- fevd(
    x = lob_041_MTPL_Bodily_Injury$Incurred_Claims,
    threshold = u_041,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_041,
    type = "probprob",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_041, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/probplot1_parte2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(3, 6), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
# 
# for (p in probs_041_2) {
#   
#   u_041 <- quantile(
#     lob_041_MTPL_Bodily_Injury$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_041 <- fevd(
#     x = lob_041_MTPL_Bodily_Injury$Incurred_Claims,
#     threshold = u_041,
#     type = "GP",
#     method = "MLE"
#   )
#   
#   plot(
#     fit_gpd_041,
#     type = "probprob",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_041, 0))
#     )
#   )
# }
# 
# mtext(
#   "Probability plots - MTPL Bodily Injury - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

prob_diagnostic_table <- function(dados_lob,
                                  nome_lob,
                                  claim_var = "Incurred_Claims",
                                  probs = seq(0.80, 0.98, by = 0.005),
                                  est = c("mle", "mom"),
                                  min_excessos = 30) {
  
  est <- match.arg(est)
  
  x <- dados_lob %>%
    dplyr::filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    ) %>%
    dplyr::pull(.data[[claim_var]])
  
  x <- sort(as.numeric(x))
  n <- length(x)
  
  resultados <- list()
  
  for (p_u in probs) {
    
    u <- as.numeric(stats::quantile(x, probs = p_u, na.rm = TRUE, type = 7))
    
    excessos <- x[x > u] - u
    excessos <- sort(excessos)
    n_u <- length(excessos)
    
    if (n_u < min_excessos) next
    
    # -----------------------------
    # Estimação dos parâmetros
    # -----------------------------
    
    if (est == "mle") {
      
      fit <- tryCatch(
        ismev::gpd.fit(
          xdat = excessos,
          threshold = 0,
          show = FALSE
        ),
        error = function(e) NULL
      )
      
      if (is.null(fit)) next
      
      sigma <- as.numeric(fit$mle[1])
      xi    <- as.numeric(fit$mle[2])
    }
    
    if (est == "mom") {
      
      m <- mean(excessos)
      v <- stats::var(excessos)
      
      if (!is.finite(m) || !is.finite(v) || m <= 0 || v <= 0) next
      
      xi <- 0.5 * (1 - (m^2 / v))
      sigma <- m * (1 - xi)
    }
    
    if (!is.finite(sigma) || !is.finite(xi) || sigma <= 0) next
    
    # -----------------------------
    # Probabilidades empíricas
    # -----------------------------
    
    emp_prob <- stats::ppoints(n_u)
    
    # -----------------------------
    # Probabilidades do modelo GPD
    # -----------------------------
    
    if (abs(xi) < 1e-8) {
      
      model_prob <- 1 - exp(-excessos / sigma)
      
    } else {
      
      inside <- 1 + xi * excessos / sigma
      
      valid_support <- is.finite(inside) & inside > 0
      
      excessos_valid <- excessos[valid_support]
      emp_prob_valid <- emp_prob[valid_support]
      inside_valid <- inside[valid_support]
      
      model_prob <- 1 - inside_valid^(-1 / xi)
      
      emp_prob <- emp_prob_valid
      excessos <- excessos_valid
    }
    
    valid <- is.finite(emp_prob) &
      is.finite(model_prob) &
      model_prob >= 0 &
      model_prob <= 1
    
    emp_prob <- emp_prob[valid]
    model_prob <- model_prob[valid]
    
    if (length(emp_prob) < min_excessos) next
    
    # -----------------------------
    # Diagnósticos
    # -----------------------------
    
    dif <- model_prob - emp_prob
    
    rmse <- sqrt(mean(dif^2))
    mae <- mean(abs(dif))
    max_abs_dev <- max(abs(dif))
    corr_prob <- suppressWarnings(stats::cor(emp_prob, model_prob))
    
    resultados[[length(resultados) + 1]] <- data.frame(
      LOB = nome_lob,
      est = est,
      p_u = p_u,
      u = u,
      n_u = n_u,
      xi = xi,
      sigma = sigma,
      RMSE_prob = rmse,
      MAE_prob = mae,
      Max_abs_dev_prob = max_abs_dev,
      Corr_prob = corr_prob
    )
  }
  
  dplyr::bind_rows(resultados)
}

prob_tab_041 <- prob_diagnostic_table(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "mle",
  min_excessos = 30
)

#View(prob_tab_041)

# write.csv(
#   prob_tab_041,
#   "pp_diagnostic_table_041.csv",
#   row.names = FALSE
# )

#Material Damage

#MLE
par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))

for (p in probs_041_1) {
  
  u_042 <- quantile(
    lob_042_MTPL_Material_Damage$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_041 <- fevd(
    x = lob_042_MTPL_Material_Damage$Incurred_Claims,
    threshold = u_042,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_042,
    type = "probprob",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_042, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/probplot2_parte1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))
# 
# for (p in probs_041_1) {
# 
#   u_042 <- quantile(
#     lob_042_MTPL_Material_Damage$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
# 
#   fit_gpd_041 <- fevd(
#     x = lob_042_MTPL_Material_Damage$Incurred_Claims,
#     threshold = u_042,
#     type = "GP",
#     method = "MLE"
#   )
# 
#   plot(
#     fit_gpd_042,
#     type = "probprob",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_042, 0))
#     )
#   )
# }
# 
# mtext(
#   "Probability plots - MTPL Material Damage - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))

for (p in probs_041_2) {
  
  u_042 <- quantile(
    lob_042_MTPL_Material_Damage$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_041 <- fevd(
    x = lob_042_MTPL_Material_Damage$Incurred_Claims,
    threshold = u_042,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_042,
    type = "probprob",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_042, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/probplot2_parte2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))
# 
# for (p in probs_041_2) {
#   
#   u_042 <- quantile(
#     lob_042_MTPL_Material_Damage$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_041 <- fevd(
#     x = lob_042_MTPL_Material_Damage$Incurred_Claims,
#     threshold = u_042,
#     type = "GP",
#     method = "MLE"
#   )
#   
#   plot(
#     fit_gpd_042,
#     type = "probprob",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_042, 0))
#     )
#   )
# }
# 
# mtext(
#   "Probability plots - MTPL Material Damage - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

prob_tab_042 <- prob_diagnostic_table(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "mle",
  min_excessos = 30
)

#View(prob_tab_042)

# write.csv(
#   prob_tab_042,
#   "pp_diagnostic_table_042.csv",
#   row.names = FALSE
# )

#Health

#MLE
par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))

for (p in probs_041_1) {
  
  u_011 <- quantile(
    lob_011_Health$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_011 <- fevd(
    x = lob_011_Health$Incurred_Claims,
    threshold = u_011,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_011,
    type = "probprob",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_011, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/probplot3_parte1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))
# 
# for (p in probs_041_1) {
# 
#   u_011 <- quantile(
#     lob_011_Health$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
# 
#   fit_gpd_011 <- fevd(
#     x = lob_011_Health$Incurred_Claims,
#     threshold = u_011,
#     type = "GP",
#     method = "MLE"
#   )
# 
#   plot(
#     fit_gpd_011,
#     type = "probprob",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_011, 0))
#     )
#   )
# }
# 
# mtext(
#   "Probability plots - Health - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))

for (p in probs_041_2) {
  
  u_011 <- quantile(
    lob_011_Health$Incurred_Claims,
    p,
    na.rm = TRUE
  )
  
  fit_gpd_011 <- fevd(
    x = lob_011_Health$Incurred_Claims,
    threshold = u_011,
    type = "GP",
    method = "MLE"
  )
  
  plot(
    fit_gpd_011,
    type = "probprob",
    main = paste0(
      "q = ", p,
      "\nu = ", scales::comma(round(u_011, 0))
    )
  )
}

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/probplot3_parte2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(mfrow = c(4, 5), mar = c(4, 4, 3, 1))
# 
# for (p in probs_041_2) {
#   
#   u_011 <- quantile(
#     lob_011_Health$Incurred_Claims,
#     p,
#     na.rm = TRUE
#   )
#   
#   fit_gpd_011 <- fevd(
#     x = lob_011_Health$Incurred_Claims,
#     threshold = u_011,
#     type = "GP",
#     method = "MLE"
#   )
#   
#   plot(
#     fit_gpd_011,
#     type = "probprob",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_011, 0))
#     )
#   )
# }
# 
# mtext(
#   "Probability plots - Health - GP estimated by MLE",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

prob_tab_011 <- prob_diagnostic_table(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  probs = seq(0.80, 0.98, by = 0.005),
  est = "mle",
  min_excessos = 30
)
#View(prob_tab_042)

# write.csv(
#   prob_tab_011,
#   "pp_diagnostic_table_011.csv",
#   row.names = FALSE
# )

#return level plot
# return_levels_from_stability <- function(
#     stab,
#     dados_lob,
#     return_periods = c(2, 5, 10, 15, 20)
# ) {
#   
#   n <- nrow(dados_lob)
#   
#   # Número médio de sinistros por ano
#   n_y <- dados_lob %>%
#     dplyr::count(Accident_Year) %>%
#     dplyr::summarise(n_y = mean(n)) %>%
#     dplyr::pull(n_y)
#   
#   resultados <- stab %>%
#     dplyr::select(
#       prob_threshold,
#       threshold,
#       n_excessos,
#       scale,
#       shape
#     ) %>%
#     tidyr::crossing(
#       return_period = return_periods
#     ) %>%
#     dplyr::mutate(
#       phi_u = n_excessos / n,
#       m = return_period * n_y,
#       
#       return_level = dplyr::if_else(
#         abs(shape) < 1e-8,
#         
#         threshold +
#           scale * log(m * phi_u),
#         
#         threshold +
#           (scale / shape) *
#           ((m * phi_u)^shape - 1)
#       )
#     )
#   
#   return(resultados)
# }
# 
# rl_041 <- return_levels_from_stability(
#   stab_mle_041,
#   lob_041_MTPL_Bodily_Injury
# )
# 
# #View(rl_041)
# 
# # write.csv(
# #   rl_041,
# #   "returnlevel_diagnostic_table_041.csv",
# #   row.names = FALSE
# # )
# 
# rl_041_1 <- rl_041 %>%
#   dplyr::filter(
#     round(prob_threshold, 3) %in% round(probs_041_1, 3)
#   ) %>%
#   dplyr::mutate(
#     painel = paste0(
#       "q = ", sprintf("%.3f", prob_threshold),
#       "\nu = ", scales::comma(round(threshold))
#     )
#   )
# 
# graf_rl_041_1 <- ggplot(
#   rl_041_1,
#   aes(x = return_period, y = return_level)
# ) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 1.5) +
#   facet_wrap(
#     ~ painel,
#     ncol = 5,
#     scales = "fixed"
#   ) +
#   scale_x_continuous(
#     breaks = c(2, 5, 10, 15, 20)
#   ) +
#   scale_y_continuous(
#     labels = scales::comma
#   ) +
#   labs(
#     title = "Return level plots - MTPL Bodily Injury",
#     x = "Return period (years)",
#     y = "Return level"
#   ) +
#   theme_minimal()
# 
# graf_rl_041_1
# 
# # ggsave(
# #   filename = "C:/Users/fatim/Downloads/returnlevel1_parte1.pdf",
# #   plot = graf_rl_041_1,
# #   width = 8,
# #   height = 6
# # )
# 
# rl_041_2 <- rl_041 %>%
#   dplyr::filter(
#     round(prob_threshold, 3) %in% round(probs_041_2, 3)
#   ) %>%
#   dplyr::mutate(
#     painel = paste0(
#       "q = ", sprintf("%.3f", prob_threshold),
#       "\nu = ", scales::comma(round(threshold))
#     )
#   )
# 
# graf_rl_041_2 <- ggplot(
#   rl_041_2,
#   aes(x = return_period, y = return_level)
# ) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 1.5) +
#   facet_wrap(
#     ~ painel,
#     ncol = 5,
#     scales = "fixed"
#   ) +
#   scale_x_continuous(
#     breaks = c(2, 5, 10, 15, 20)
#   ) +
#   scale_y_continuous(
#     labels = scales::comma
#   ) +
#   labs(
#     title = "Return level plots - MTPL Bodily Injury",
#     x = "Return period (years)",
#     y = "Return level"
#   ) +
#   theme_minimal()
# 
# graf_rl_041_2
# 
# # ggsave(
# #   filename = "C:/Users/fatim/Downloads/returnlevel1_parte2.pdf",
# #   plot = graf_rl_041_2,
# #   width = 8,
# #   height = 6
# # )
# 
# #material damage
# 
# rl_042 <- return_levels_from_stability(
#   stab_mle_052,
#   lob_042_MTPL_Material_Damage
# )
# 
# # write.csv(
# #   rl_042,
# #   "returnlevel_diagnostic_table_042.csv",
# #   row.names = FALSE
# # )
# 
# rl_042_1 <- rl_042 %>%
#   dplyr::filter(
#     round(prob_threshold, 3) %in% round(probs_041_1, 3)
#   ) %>%
#   dplyr::mutate(
#     painel = paste0(
#       "q = ", sprintf("%.3f", prob_threshold),
#       "\nu = ", scales::comma(round(threshold))
#     )
#   )
# 
# graf_rl_042_1 <- ggplot(
#   rl_042_1,
#   aes(x = return_period, y = return_level)
# ) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 1.5) +
#   facet_wrap(
#     ~ painel,
#     ncol = 5,
#     scales = "fixed"
#   ) +
#   scale_x_continuous(
#     breaks = c(2, 5, 10, 15, 20)
#   ) +
#   scale_y_continuous(
#     labels = scales::comma
#   ) +
#   labs(
#     title = "Return level plots - MTPL Material Damage",
#     x = "Return period (years)",
#     y = "Return level"
#   ) +
#   theme_minimal()
# 
# graf_rl_042_1
# 
# # ggsave(
# #   filename = "C:/Users/fatim/Downloads/returnlevel2_parte1.pdf",
# #   plot = graf_rl_042_1,
# #   width = 8,
# #   height = 6
# # )
# 
# rl_042_2 <- rl_042 %>%
#   dplyr::filter(
#     round(prob_threshold, 3) %in% round(probs_041_2, 3)
#   ) %>%
#   dplyr::mutate(
#     painel = paste0(
#       "q = ", sprintf("%.3f", prob_threshold),
#       "\nu = ", scales::comma(round(threshold))
#     )
#   )
# 
# graf_rl_042_2 <- ggplot(
#   rl_042_2,
#   aes(x = return_period, y = return_level)
# ) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 1.5) +
#   facet_wrap(
#     ~ painel,
#     ncol = 5,
#     scales = "fixed"
#   ) +
#   scale_x_continuous(
#     breaks = c(2, 5, 10, 15, 20)
#   ) +
#   scale_y_continuous(
#     labels = scales::comma
#   ) +
#   labs(
#     title = "Return level plots - MTPL Material Damage",
#     x = "Return period (years)",
#     y = "Return level"
#   ) +
#   theme_minimal()
# 
# graf_rl_042_2
# 
# # ggsave(
# #   filename = "C:/Users/fatim/Downloads/returnlevel2_parte2.pdf",
# #   plot = graf_rl_042_2,
# #   width = 8,
# #   height = 6
# # )
# 
# 
# #health
# rl_011 <- return_levels_from_stability(
#   stab_mle_011,
#   lob_011_Health
# )
# 
# # write.csv(
# #   rl_011,
# #   "returnlevel_diagnostic_table_011.csv",
# #   row.names = FALSE
# # )
# 
# rl_011_1 <- rl_011 %>%
#   dplyr::filter(
#     round(prob_threshold, 3) %in% round(probs_041_1, 3)
#   ) %>%
#   dplyr::mutate(
#     painel = paste0(
#       "q = ", sprintf("%.3f", prob_threshold),
#       "\nu = ", scales::comma(round(threshold))
#     )
#   )
# 
# graf_rl_011_1 <- ggplot(
#   rl_011_1,
#   aes(x = return_period, y = return_level)
# ) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 1.5) +
#   facet_wrap(
#     ~ painel,
#     ncol = 5,
#     scales = "fixed"
#   ) +
#   scale_x_continuous(
#     breaks = c(2, 5, 10, 15, 20)
#   ) +
#   scale_y_continuous(
#     labels = scales::comma
#   ) +
#   labs(
#     title = "Return level plots - Health",
#     x = "Return period (years)",
#     y = "Return level"
#   ) +
#   theme_minimal()
# 
# graf_rl_011_1
# 
# # ggsave(
# #   filename = "C:/Users/fatim/Downloads/returnlevel3_parte1.pdf",
# #   plot = graf_rl_011_1,
# #   width = 8,
# #   height = 6
# # )
# 
# rl_011_2 <- rl_011 %>%
#   dplyr::filter(
#     round(prob_threshold, 3) %in% round(probs_041_2, 3)
#   ) %>%
#   dplyr::mutate(
#     painel = paste0(
#       "q = ", sprintf("%.3f", prob_threshold),
#       "\nu = ", scales::comma(round(threshold))
#     )
#   )
# 
# graf_rl_011_2 <- ggplot(
#   rl_011_2,
#   aes(x = return_period, y = return_level)
# ) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 1.5) +
#   facet_wrap(
#     ~ painel,
#     ncol = 5,
#     scales = "fixed"
#   ) +
#   scale_x_continuous(
#     breaks = c(2, 5, 10, 15, 20)
#   ) +
#   scale_y_continuous(
#     labels = scales::comma
#   ) +
#   labs(
#     title = "Return level plots - Health",
#     x = "Return period (years)",
#     y = "Return level"
#   ) +
#   theme_minimal()
# 
# graf_rl_011_2
# 
# # ggsave(
# #   filename = "C:/Users/fatim/Downloads/returnlevel3_parte2.pdf",
# #   plot = graf_rl_011_2,
# #   width = 8,
# #   height = 6
# # )
#retirar

##NOVO##
return_levels_from_stability <- function(
    stab,
    dados_lob,
    return_periods = seq(2, 20, length.out = 200)
) {
  
  # Se Accident_Year não existir, criá-lo
  if (!"Accident_Year" %in% names(dados_lob)) {
    
    dados_lob <- dados_lob %>%
      dplyr::mutate(
        Accident_Year = lubridate::year(as.Date(Accident_Date))
      )
  }
  
  # Número de anos observados
  n_years <- dplyr::n_distinct(dados_lob$Accident_Year)
  
  resultados <- stab %>%
    dplyr::select(
      prob_threshold,
      threshold,
      n_excessos,
      scale,
      shape
    ) %>%
    
    tidyr::crossing(
      return_period = return_periods
    ) %>%
    
    dplyr::mutate(
      
      # número médio de excedências por ano
      lambda_u = n_excessos / n_years,
      
      # return level
      return_level = dplyr::if_else(
        
        abs(shape) < 1e-8,
        
        threshold +
          scale * log(return_period * lambda_u),
        
        threshold +
          (scale / shape) *
          (
            (return_period * lambda_u)^shape - 1
          )
      )
    ) %>%
    
    dplyr::filter(
      is.finite(return_level)
    )
  
  return(resultados)
}


# RETURN LEVELS EMPÍRICOS
empirical_return_levels <- function(dados_lob) {
  
  # Se Accident_Year não existir, criá-lo
  if (!"Accident_Year" %in% names(dados_lob)) {
    
    dados_lob <- dados_lob %>%
      dplyr::mutate(
        Accident_Year = lubridate::year(as.Date(Accident_Date))
      )
  }
  
  # Manter apenas observações válidas
  dados_validos <- dados_lob %>%
    dplyr::filter(
      !is.na(Incurred_Claims),
      is.finite(Incurred_Claims),
      Incurred_Claims > 0
    )
  
  # Número total de observações
  n <- nrow(dados_validos)
  
  # Número de anos observados
  n_years <- dplyr::n_distinct(dados_validos$Accident_Year)
  
  # Número médio de sinistros por ano
  npy <- n / n_years
  
  dados_validos %>%
    
    # O extRemes trabalha com os valores ordenados por ordem crescente
    dplyr::arrange(Incurred_Claims) %>%
    
    dplyr::mutate(
      
      # Posição da observação ordenada
      i = dplyr::row_number(),
      
      # Plotting position utilizada por ppoints(n, a = 0)
      p_emp = i / (n + 1),
      
      # Período de retorno empírico
      return_period_emp = -1 / (npy * log(p_emp)),
      
      # Valor empírico observado
      empirical_level = Incurred_Claims
    ) %>%
    
    # Apenas para apresentar os maiores sinistros primeiro
    dplyr::arrange(dplyr::desc(empirical_level)) %>%
    
    dplyr::select(
      i,
      p_emp,
      return_period_emp,
      empirical_level
    )
}


# PREPARAR OS PAINÉIS DOS THRESHOLDS
prepare_rl_panels <- function(rl, probs) {
  
  rl %>%
    dplyr::filter(
      round(prob_threshold, 3) %in%
        round(probs, 3)
    ) %>%
    
    dplyr::mutate(
      painel = paste0(
        "q = ",
        sprintf("%.3f", prob_threshold),
        "\nu = ",
        scales::comma(round(threshold))
      )
    )
}


# ASSOCIAR OS VALORES EMPÍRICOS A CADA THRESHOLD

prepare_empirical_points <- function(
    rl_panel,
    empirical_data,
    min_return_period = 2,
    max_return_period = 20
) {
  
  thresholds <- rl_panel %>%
    dplyr::distinct(
      prob_threshold,
      threshold,
      painel
    )
  
  tidyr::crossing(
    thresholds,
    empirical_data
  ) %>%
    
    dplyr::filter(
      
      # só observações acima do respetivo threshold
      empirical_level > threshold,
      
      # apenas período representado no gráfico
      return_period_emp >= min_return_period,
      return_period_emp <= max_return_period
    )
}



# FUNÇÃO PARA CONSTRUIR O RETURN LEVEL PLOT

make_return_level_plot <- function(
    theoretical_data,
    empirical_data,
    title_plot
) {
  
  ggplot() +
    
    # --------------------------------------------------------
  # Linha: return levels estimados pela GPD
  # --------------------------------------------------------
  
  geom_line(
    data = theoretical_data,
    aes(
      x = return_period,
      y = return_level
    ),
    linewidth = 0.8
  ) +
    
    
    # --------------------------------------------------------
  # Pontos: valores empíricos
  # --------------------------------------------------------
  
  geom_point(
    data = empirical_data,
    aes(
      x = return_period_emp,
      y = empirical_level
    ),
    shape = 1,
    size = 1.5,
    stroke = 0.7
  ) +
    
    
    facet_wrap(
      ~ painel,
      ncol = 5,
      scales = "fixed",
      axes = "all_x",
      axis.labels = "all_x"
    ) +
    
    
    scale_x_continuous(
      breaks = c(2, 5, 10, 15, 20),
      limits = c(2, 20)
    ) +
    
    
    scale_y_continuous(
      labels = scales::comma
    ) +
    
    
    labs(
      title = title_plot,
      x = "Return period (years)",
      y = "Return level",
      caption = "Solid line: fitted GPD; circles: empirical return levels."
    ) +
    
    
    theme_minimal()
}


# MTPL BODILY INJURY

# Return levels teóricos
rl_041 <- return_levels_from_stability(
  stab = stab_mle_041,
  dados_lob = lob_041_MTPL_Bodily_Injury
)


# Return levels empíricos
emp_041 <- empirical_return_levels(
  lob_041_MTPL_Bodily_Injury
)

# PARTE 1: q = 0.800 - 0.890

rl_041_1 <- prepare_rl_panels(
  rl_041,
  probs_041_1
)

emp_041_1 <- prepare_empirical_points(
  rl_041_1,
  emp_041
)

graf_rl_041_1 <- make_return_level_plot(
  theoretical_data = rl_041_1,
  empirical_data = emp_041_1,
  title_plot = "Return level plots - MTPL Bodily Injury"
)

graf_rl_041_1


# ggsave(
#   filename = "C:/Users/fatim/Downloads/returnlevel1_parte1.pdf",
#   plot = graf_rl_041_1,
#   width = 8,
#   height = 6
# )


# PARTE 2: q = 0.895 - 0.980
rl_041_2 <- prepare_rl_panels(
  rl_041,
  probs_041_2
)

emp_041_2 <- prepare_empirical_points(
  rl_041_2,
  emp_041
)

graf_rl_041_2 <- make_return_level_plot(
  theoretical_data = rl_041_2,
  empirical_data = emp_041_2,
  title_plot = "Return level plots - MTPL Bodily Injury"
)

graf_rl_041_2


# ggsave(
#   filename = "C:/Users/fatim/Downloads/returnlevel1_parte2.pdf",
#   plot = graf_rl_041_2,
#   width = 8,
#   height = 6
# )


# MTPL MATERIAL DAMAGE
rl_042 <- return_levels_from_stability(
  stab = stab_mle_052,
  dados_lob = lob_042_MTPL_Material_Damage
)


emp_042 <- empirical_return_levels(
  lob_042_MTPL_Material_Damage
)


rl_042_1 <- prepare_rl_panels(
  rl_042,
  probs_041_1
)

emp_042_1 <- prepare_empirical_points(
  rl_042_1,
  emp_042
)

graf_rl_042_1 <- make_return_level_plot(
  theoretical_data = rl_042_1,
  empirical_data = emp_042_1,
  title_plot = "Return level plots - MTPL Material Damage"
)

graf_rl_042_1


# ggsave(
#   filename = "C:/Users/fatim/Downloads/returnlevel2_parte1.pdf",
#   plot = graf_rl_042_1,
#   width = 8,
#   height = 6
# )


rl_042_2 <- prepare_rl_panels(
  rl_042,
  probs_041_2
)

emp_042_2 <- prepare_empirical_points(
  rl_042_2,
  emp_042
)

graf_rl_042_2 <- make_return_level_plot(
  theoretical_data = rl_042_2,
  empirical_data = emp_042_2,
  title_plot = "Return level plots - MTPL Material Damage"
)

graf_rl_042_2


# ggsave(
#   filename = "C:/Users/fatim/Downloads/returnlevel2_parte2.pdf",
#   plot = graf_rl_042_2,
#   width = 8,
#   height = 6
# )


# HEALTH

rl_011 <- return_levels_from_stability(
  stab = stab_mle_011,
  dados_lob = lob_011_Health
)


emp_011 <- empirical_return_levels(
  lob_011_Health
)


rl_011_1 <- prepare_rl_panels(
  rl_011,
  probs_041_1
)

emp_011_1 <- prepare_empirical_points(
  rl_011_1,
  emp_011
)

graf_rl_011_1 <- make_return_level_plot(
  theoretical_data = rl_011_1,
  empirical_data = emp_011_1,
  title_plot = "Return level plots - Health"
)

graf_rl_011_1


# ggsave(
#   filename = "C:/Users/fatim/Downloads/returnlevel3_parte1.pdf",
#   plot = graf_rl_011_1,
#   width = 8,
#   height = 6
# )

rl_011_2 <- prepare_rl_panels(
  rl_011,
  probs_041_2
)

emp_011_2 <- prepare_empirical_points(
  rl_011_2,
  emp_011
)

graf_rl_011_2 <- make_return_level_plot(
  theoretical_data = rl_011_2,
  empirical_data = emp_011_2,
  title_plot = "Return level plots - Health"
)

graf_rl_011_2


# ggsave(
#   filename = "C:/Users/fatim/Downloads/returnlevel3_parte2.pdf",
#   plot = graf_rl_011_2,
#   width = 8,
#   height = 6
# )

comparacao_041 <- emp_041_2 %>%
  dplyr::left_join(
    stab_mle_041 %>%
      dplyr::select(
        prob_threshold,
        threshold,
        n_excessos,
        scale,
        shape
      ),
    by = c("prob_threshold", "threshold")
  ) %>%
  
  dplyr::mutate(
    
    n_years = dplyr::n_distinct(
      lob_041_MTPL_Bodily_Injury$Accident_Year
    ),
    
    lambda_u = n_excessos / n_years,
    
    fitted_level = dplyr::if_else(
      abs(shape) < 1e-8,
      
      threshold +
        scale * log(return_period_emp * lambda_u),
      
      threshold +
        (scale / shape) *
        ((return_period_emp * lambda_u)^shape - 1)
    ),
    
    difference = empirical_level - fitted_level
  ) %>%
  
  dplyr::select(
    prob_threshold,
    threshold,
    return_period_emp,
    empirical_level,
    fitted_level,
    difference
  ) %>%
  
  dplyr::mutate(
    across(
      c(
        threshold,
        empirical_level,
        fitted_level,
        difference
      ),
      ~ round(.x, 2)
    ),
    return_period_emp = round(return_period_emp, 2)
  )

#View(comparacao_041)

# write.csv(
#   comparacao_041,
#   "C:/Users/fatim/Downloads/comparacao_return_levels_041.csv",
#   row.names = FALSE
# )

comparacao_042 <- emp_042_2 %>%
  dplyr::left_join(
    stab_mle_052 %>%
      dplyr::select(
        prob_threshold,
        threshold,
        n_excessos,
        scale,
        shape
      ),
    by = c("prob_threshold", "threshold")
  ) %>%
  
  dplyr::mutate(
    
    n_years = dplyr::n_distinct(
      lob_042_MTPL_Material_Damage$Accident_Year
    ),
    
    lambda_u = n_excessos / n_years,
    
    fitted_level = dplyr::if_else(
      abs(shape) < 1e-8,
      
      threshold +
        scale * log(return_period_emp * lambda_u),
      
      threshold +
        (scale / shape) *
        ((return_period_emp * lambda_u)^shape - 1)
    ),
    
    difference = empirical_level - fitted_level
  ) %>%
  
  dplyr::select(
    prob_threshold,
    threshold,
    return_period_emp,
    empirical_level,
    fitted_level,
    difference
  ) %>%
  
  dplyr::mutate(
    across(
      c(
        threshold,
        empirical_level,
        fitted_level,
        difference
      ),
      ~ round(.x, 2)
    ),
    return_period_emp = round(return_period_emp, 2)
  )

#View(comparacao_042)

comparacao_011 <- emp_011_1 %>%
  dplyr::left_join(
    stab_mle_011 %>%
      dplyr::select(
        prob_threshold,
        threshold,
        n_excessos,
        scale,
        shape
      ),
    by = c("prob_threshold", "threshold")
  ) %>%
  
  dplyr::mutate(
    
    n_years = dplyr::n_distinct(
      lob_011_Health$Accident_Year
    ),
    
    lambda_u = n_excessos / n_years,
    
    fitted_level = dplyr::if_else(
      abs(shape) < 1e-8,
      
      threshold +
        scale * log(return_period_emp * lambda_u),
      
      threshold +
        (scale / shape) *
        ((return_period_emp * lambda_u)^shape - 1)
    ),
    
    difference = empirical_level - fitted_level
  ) %>%
  
  dplyr::select(
    prob_threshold,
    threshold,
    return_period_emp,
    empirical_level,
    fitted_level,
    difference
  ) %>%
  
  dplyr::mutate(
    across(
      c(
        threshold,
        empirical_level,
        fitted_level,
        difference
      ),
      ~ round(.x, 2)
    ),
    return_period_emp = round(return_period_emp, 2)
  )

View(comparacao_011)

# write.csv(
#   comparacao_042,
#   "C:/Users/fatim/Downloads/comparacao_return_levels_042.csv",
#   row.names = FALSE
# )
# 
# write.csv(
#   comparacao_011,
#   "C:/Users/fatim/Downloads/comparacao_return_levels_011.csv",
#   row.names = FALSE
# )

#hill plot
hill_plot_lob <- function(dados_lob, nome_lob,
                          k_min = 10,
                          k_max = NULL,
                          probs_ref = NULL,
                          thresholds_ref = NULL) {
  
  x <- dados_lob %>%
    filter(!is.na(Incurred_Claims),
           Incurred_Claims > 0) %>%
    pull(Incurred_Claims)
  
  x_sort <- sort(x, decreasing = TRUE)
  n <- length(x_sort)
  
  if (is.null(k_max)) {
    k_max <- floor(0.30 * n)
  }
  
  k_max <- min(k_max, n - 1)
  
  if (k_min >= k_max) {
    stop("k_min must be smaller than k_max.")
  }
  
  k_vals <- k_min:k_max
  
  hill_vals <- sapply(k_vals, function(k) {
    mean(log(x_sort[1:k])) - log(x_sort[k + 1])
  })
  
  dados_hill <- data.frame(
    k = k_vals,
    hill = hill_vals,
    threshold = x_sort[k_vals + 1],
    prob_threshold = 1 - k_vals / n
  )
  
  p <- ggplot(dados_hill, aes(x = k, y = hill)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 0.7, alpha = 0.6) +
    scale_x_continuous(labels = scales::comma) +
    labs(
      title = paste("Hill plot -", nome_lob),
      x = "Number of upper order statistics (k)",
      y = expression(hat(xi)[Hill])
    ) +
    theme_minimal()
  
  if (!is.null(probs_ref)) {
    thresholds_ref <- quantile(x, probs = probs_ref, na.rm = TRUE)
  }
  
  if (!is.null(thresholds_ref)) {
    
    k_ref <- sapply(thresholds_ref, function(u) {
      sum(x > u, na.rm = TRUE)
    })
    
    ref_data <- data.frame(
      threshold = as.numeric(thresholds_ref),
      k = as.numeric(k_ref)
    )
    
    p <- p +
      geom_vline(
        data = ref_data,
        aes(xintercept = k),
        linetype = "dashed"
      ) +
      geom_text(
        data = ref_data,
        aes(
          x = k,
          y = Inf,
          label = paste0("u = ", scales::comma(round(threshold, 0)))
        ),
        angle = 90,
        vjust = 1.2,
        hjust = 1,
        size = 3
      )
  }
  
  return(
    list(
      plot = p,
      table = dados_hill
    )
  )
}

hill_041 <- hill_plot_lob(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  k_min = 10,
  probs_ref = c(0.914,0.937)
)

hill_041$plot
hill_041$table

#View(hill_041$table)

# write.csv(
#   hill_041$table,
#   "hill_table_041.csv",
#   row.names = FALSE
# )

# ggsave(
#   filename = "C:/Users/fatim/Downloads/hillplot1.pdf",
#   plot = hill_041$plot,
#   width = 8,
#   height = 6
# )

#Health
hill_011 <- hill_plot_lob(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  k_min = 10,
  probs_ref = c(0.8,0.9)
)

hill_011$plot
hill_011$table

# write.csv(
#   hill_011$table,
#   "hill_table_011.csv",
#   row.names = FALSE
# )

# ggsave(
#   filename = "C:/Users/fatim/Downloads/hillplot3.pdf",
#   plot = hill_011$plot,
#   width = 8,
#   height = 6
# )

#Material Damage
hill_042 <- hill_plot_lob(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  k_min = 10,
  probs_ref = c(0.9, 0.944)
)

hill_042$plot
hill_042$table

# write.csv(
#   hill_042$table,
#   "hill_table_042.csv",
#   row.names = FALSE
# )

# ggsave(
#   filename = "C:/Users/fatim/Downloads/hillplot2.pdf",
#   plot = hill_042$plot,
#   width = 8,
#   height = 6
# )

#hill,dekkers e pickands
tail_index_estimators <- function(dados_lob, nome_lob = NULL,
                                  claim_col = Incurred_Claims,
                                  k_min = 10,
                                  k_max = NULL,
                                  by = 10) {

  
  claim_col <- rlang::enquo(claim_col)
  
  x <- dados_lob %>%
    dplyr::filter(
      !is.na(!!claim_col),
      is.finite(!!claim_col),
      !!claim_col > 0
    ) %>%
    dplyr::pull(!!claim_col)
  
  x <- sort(x, decreasing = TRUE)
  n <- length(x)
  
  if (n < 5) {
    stop("Número insuficiente de observações positivas para estimar a cauda.")
  }
  
  if (is.null(k_max)) {
    # Pickands precisa de 4k <= n
    k_max <- floor(n / 4)
  }
  
  if (k_max < k_min) {
    stop("k_max é menor que k_min. Há poucas observações para estes valores de k.")
  }
  
  k_values <- seq(k_min, k_max, by = by)
  
  results <- lapply(k_values, function(k) {
    
    # Threshold associado: X_{k+1:n}, ou seja, observação logo abaixo das top k
    threshold <- x[k + 1]
    
    # -------------------------
    # 1. Hill estimator
    # -------------------------
    hill <- mean(log(x[1:k]) - log(threshold))
    
    # -------------------------
    # 2. Dekkers-Einmahl-de Haan estimator
    # -------------------------
    M1 <- mean(log(x[1:k]) - log(threshold))
    M2 <- mean((log(x[1:k]) - log(threshold))^2)
    
    dekkers <- M1 + 1 - 0.5 * (1 - (M1^2 / M2))^(-1)
    
    # -------------------------
    # 3. Pickands estimator
    # -------------------------
    pickands <- NA_real_
    
    if (4 * k <= n) {
      numerator <- x[k] - x[2 * k]
      denominator <- x[2 * k] - x[4 * k]
      
      if (numerator > 0 && denominator > 0) {
        pickands <- log(numerator / denominator) / log(2)
      }
    }
    
    data.frame(
      LOB = nome_lob,
      k = k,
      threshold = threshold,
      Hill = hill,
      Dekkers = dekkers,
      Pickands = pickands
    )
  })
  
  dplyr::bind_rows(results)
}

plot_tail_estimators <- function(tail_data, nome_lob,
                                 dados_lob = NULL,
                                 probs_ref = NULL,
                                 thresholds_ref = NULL) {
  
  dados_long <- tail_data %>%
    tidyr::pivot_longer(
      cols = c(Hill, Dekkers, Pickands),
      names_to = "Estimator",
      values_to = "xi_hat"
    ) %>%
    filter(!is.na(xi_hat))
  
  p <- ggplot(
    dados_long,
    aes(x = k, y = xi_hat, color = Estimator, linetype = Estimator)
  ) +
    geom_line(linewidth = 0.8) +
    labs(
      title = paste("Tail index estimators -", nome_lob),
      x = "Number of upper order statistics (k)",
      y = expression(hat(xi))
    ) +
    theme_minimal()
  
  # Se forem dadas probabilidades de threshold, calcular os thresholds
  if (!is.null(probs_ref)) {
    
    if (is.null(dados_lob)) {
      stop("Para usar probs_ref, tens de fornecer também dados_lob.")
    }
    
    x <- dados_lob %>%
      filter(
        !is.na(Incurred_Claims),
        is.finite(Incurred_Claims),
        Incurred_Claims > 0
      ) %>%
      pull(Incurred_Claims)
    
    thresholds_ref <- as.numeric(
      quantile(x, probs = probs_ref, na.rm = TRUE)
    )
  }
  
  # Adicionar linhas verticais de referência
  if (!is.null(thresholds_ref)) {
    
    ref_data <- data.frame(
      threshold = as.numeric(thresholds_ref)
    )
    
    # encontrar o k mais próximo na tabela tail_data
    ref_data$k <- sapply(ref_data$threshold, function(u) {
      tail_data$k[which.min(abs(tail_data$threshold - u))]
    })
    
    ref_data$label <- paste0("u = ", round(ref_data$threshold, 0))
    
    p <- p +
      geom_vline(
        data = ref_data,
        aes(xintercept = k),
        linetype = "dashed",
        color = "black",
        inherit.aes = FALSE
      ) +
      geom_text(
        data = ref_data,
        aes(
          x = k,
          y = Inf,
          label = label
        ),
        angle = 90,
        vjust = 1.2,
        hjust = 1,
        size = 3,
        color = "black",
        inherit.aes = FALSE
      )
  }
  
  return(p)
}

tail_041 <- tail_index_estimators(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  k_min = 10,
  by = 10
)

tail_041


tail_011 <- tail_index_estimators(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  k_min = 10,
  by = 10
)

tail_011

tail_042 <- tail_index_estimators(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  k_min = 10,
  by = 10
)


tail_042

plot_tail_estimators <- function(tail_data, nome_lob,
                                 dados_lob = NULL,
                                 probs_ref = NULL,
                                 thresholds_ref = NULL) {
  
  dados_long <- tail_data %>%
    tidyr::pivot_longer(
      cols = c(Hill, Dekkers, Pickands),
      names_to = "Estimator",
      values_to = "xi_hat"
    ) %>%
    filter(!is.na(xi_hat))
  
  p <- ggplot(
    dados_long,
    aes(x = k, y = xi_hat, color = Estimator, linetype = Estimator)
  ) +
    geom_line(linewidth = 0.8) +
    scale_x_continuous(labels = scales::comma) +
    labs(
      title = paste("Tail index estimators -", nome_lob),
      x = "Number of upper order statistics (k)",
      y = expression(hat(xi))
    ) +
    theme_minimal()
  
  if (!is.null(probs_ref)) {
    
    if (is.null(dados_lob)) {
      stop("Para usar probs_ref, tens de fornecer também dados_lob.")
    }
    
    x <- dados_lob %>%
      filter(
        !is.na(Incurred_Claims),
        is.finite(Incurred_Claims),
        Incurred_Claims > 0
      ) %>%
      pull(Incurred_Claims)
    
    thresholds_ref <- as.numeric(
      quantile(x, probs = probs_ref, na.rm = TRUE)
    )
  }
  
  if (!is.null(thresholds_ref)) {
    
    ref_data <- data.frame(
      threshold = as.numeric(thresholds_ref)
    )
    
    ref_data$k <- sapply(ref_data$threshold, function(u) {
      tail_data$k[which.min(abs(tail_data$threshold - u))]
    })
    
    ref_data$label <- paste0(
      "u = ",
      scales::comma(round(ref_data$threshold, 0))
    )
    
    p <- p +
      geom_vline(
        data = ref_data,
        aes(xintercept = k),
        linetype = "dashed",
        color = "black",
        inherit.aes = FALSE
      ) +
      geom_text(
        data = ref_data,
        aes(
          x = k,
          y = Inf,
          label = label
        ),
        angle = 90,
        vjust = 1.2,
        hjust = 1,
        size = 3,
        color = "black",
        inherit.aes = FALSE
      )
  }
  
  return(p)
}

plot_tail_estimators(
  tail_data = tail_041,
  nome_lob = "MTPL Bodily Injury",
  dados_lob = lob_041_MTPL_Bodily_Injury,
  probs_ref = c(0.914, 0.937)
)

# pdf(
#   file = "C:/Users/fatim/Downloads/tail_estimators_041.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot_tail_estimators(
#   tail_data = tail_041,
#   nome_lob = "MTPL Bodily Injury",
#   dados_lob = lob_041_MTPL_Bodily_Injury,
#   probs_ref = c(0.914, 0.937)
# )
# 
# dev.off()

plot_tail_estimators(
  tail_data = tail_011,
  nome_lob = "Health",
  dados_lob = lob_011_Health,
  probs_ref = c(0.800, 0.900)
)

# pdf(
#   file = "C:/Users/fatim/Downloads/tail_estimators_011.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot_tail_estimators(
#   tail_data = tail_011,
#   nome_lob = "Health",
#   dados_lob = lob_011_Health,
#   probs_ref = c(0.800, 0.900)
# )
# 
# dev.off()

plot_tail_estimators(
  tail_data = tail_042,
  nome_lob = "MTPL Material Damage",
  dados_lob = lob_042_MTPL_Material_Damage,
  probs_ref = c(0.90, 0.944)
)

# pdf(
#   file = "C:/Users/fatim/Downloads/tail_estimators_042.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot_tail_estimators(
#   tail_data = tail_042,
#   nome_lob = "MTPL Material Damage",
#   dados_lob = lob_042_MTPL_Material_Damage,
#   probs_ref = c(0.90, 0.944)
# )
# 
# dev.off()

#Pareto quantile plot
pareto_quantile_plot_probs_clean <- function(dados_lob, nome_lob,
                                             probs = c(0.90, 0.91, 0.92),
                                             slope_method = c("hill", "dekkers", "pickands", "lm"),
                                             show_body = FALSE,
                                             ncol = 3) {
  
  slope_method <- match.arg(slope_method)
  
  x <- dados_lob %>%
    filter(
      !is.na(Incurred_Claims),
      is.finite(Incurred_Claims),
      Incurred_Claims > 0
    ) %>%
    pull(Incurred_Claims)
  
  x_sort <- sort(x, decreasing = TRUE)
  n <- length(x_sort)
  
  thresholds <- as.numeric(quantile(x, probs = probs, na.rm = TRUE))
  
  j <- 1:n
  
  dados_pareto_base <- data.frame(
    j = j,
    x_pareto = -log(j / (n + 1)),
    y_log = log(x_sort),
    claim = x_sort
  )
  
  resultados <- lapply(seq_along(thresholds), function(i) {
    
    u <- thresholds[i]
    q <- probs[i]
    k <- sum(x > u, na.rm = TRUE)
    
    if (k < 5 || (k + 1) > n) return(NULL)
    
    x0 <- -log((k + 1) / (n + 1))
    y0 <- log(x_sort[k + 1])
    
    dados_tail <- dados_pareto_base %>%
      filter(j <= k)
    
    log_ratios <- log(x_sort[1:k]) - log(x_sort[k + 1])
    
    a <- NA_real_
    
    if (slope_method == "hill") {
      
      a <- mean(log_ratios)
      
    } else if (slope_method == "dekkers") {
      
      m1 <- mean(log_ratios)
      m2 <- mean(log_ratios^2)
      
      if (is.finite(m1) && is.finite(m2) && m2 > 0 && abs(1 - m1^2 / m2) > 1e-10) {
        a <- m1 + 1 - 0.5 * (1 - m1^2 / m2)^(-1)
      }
      
    } else if (slope_method == "pickands") {
      
      xs <- sort(x, decreasing = TRUE)
      
      if (4 * k <= n) {
        numerator <- xs[k] - xs[2 * k]
        denominator <- xs[2 * k] - xs[4 * k]
        
        if (is.finite(numerator) && is.finite(denominator) &&
            numerator > 0 && denominator > 0) {
          a <- log(numerator / denominator) / log(2)
        }
      }
      
    } else if (slope_method == "lm") {
      
      fit_lm <- lm(y_log ~ x_pareto, data = dados_tail)
      a <- as.numeric(coef(fit_lm)[2])
    }
    
    if (!is.finite(a)) return(NULL)
    
    x_line <- seq(x0, max(dados_tail$x_pareto), length.out = 100)
    
    facet_label <- paste0(
      "q = ", q,
      " | u = ", scales::comma(round(u, 0)),
      " | k = ", scales::comma(k),
      " | slope = ", round(a, 3)
    )
    
    dados_line <- data.frame(
      prob_threshold = q,
      threshold = u,
      facet_label = facet_label,
      k = k,
      slope_method = slope_method,
      slope = a,
      x0 = x0,
      y0 = y0,
      x_pareto = x_line,
      y_log = y0 + a * (x_line - x0)
    )
    
    dados_points <- dados_pareto_base %>%
      mutate(
        prob_threshold = q,
        threshold = u,
        facet_label = facet_label,
        k = k,
        slope_method = slope_method,
        slope = a,
        x0 = x0,
        y0 = y0,
        type = ifelse(j <= k, "tail", "body")
      )
    
    list(points = dados_points, line = dados_line)
  })
  
  resultados <- resultados[!sapply(resultados, is.null)]
  
  dados_points_all <- bind_rows(lapply(resultados, `[[`, "points"))
  dados_line_all <- bind_rows(lapply(resultados, `[[`, "line"))
  
  tabela_thresholds <- dados_line_all %>%
    distinct(prob_threshold, threshold, k, slope_method, slope) %>%
    mutate(
      threshold = round(threshold, 0),
      slope = round(slope, 3)
    )
  
  if (show_body) {
    dados_plot <- dados_points_all
  } else {
    dados_plot <- dados_points_all %>% filter(type == "tail")
  }
  
  grafico <- ggplot() +
    geom_point(
      data = dados_plot,
      aes(x = x_pareto, y = y_log, alpha = type),
      size = 0.8
    ) +
    geom_line(
      data = dados_line_all,
      aes(x = x_pareto, y = y_log),
      linewidth = 0.8,
      linetype = "dashed"
    ) +
    facet_wrap(~ facet_label, ncol = ncol) +
    labs(
      title = paste("Pareto quantile plots -", nome_lob),
      subtitle = paste("Reference slope estimated using", slope_method),
      x = expression(-log(j / (n + 1))),
      y = expression(log(X[n-j+1:n]))
    ) +
    scale_alpha_manual(
      values = c("body" = 0.15, "tail" = 0.9),
      guide = "none"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 8),
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8)
    )
  
  return(
    list(
      plot = grafico,
      table = tabela_thresholds
    )
  )
}

pq_041_clean <- pareto_quantile_plot_probs_clean(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  probs = c(0.90, 0.92, 0.94, 0.96, 0.98),
  slope_method = "hill",
  show_body = FALSE,
  ncol = 3
)

pq_041_clean$plot
pq_041_clean$table

# ggsave(
#   filename = "C:/Users/fatim/Downloads/pareto_plot_041.pdf",
#   plot = pq_041_clean$plot,
#   width = 8,
#   height = 6
# )

# pq_041_dekkers <- pareto_quantile_plot_probs_clean(
#   dados_lob = lob_041_MTPL_Bodily_Injury,
#   nome_lob = "MTPL Bodily Injury",
#   probs = c(0.90, 0.92, 0.94, 0.96, 0.98),
#   slope_method = "dekkers",
#   show_body = FALSE,
#   ncol = 3
# )
# 
# pq_041_dekkers$plot
# pq_041_dekkers$table

pq_011_clean <- pareto_quantile_plot_probs_clean(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  probs = c(0.90, 0.92, 0.94, 0.96, 0.98),
  slope_method = "hill",
  show_body = FALSE,
  ncol = 3
)

pq_011_clean$plot
pq_011_clean$table

# ggsave(
#   filename = "C:/Users/fatim/Downloads/pareto_plot_011.pdf",
#   plot = pq_011_clean$plot,
#   width = 8,
#   height = 6
# )

# pq_011_dekkers <- pareto_quantile_plot_probs_clean(
#   dados_lob = lob_011_Health,
#   nome_lob = "Health",
#   probs = c(0.90, 0.92, 0.94, 0.96, 0.98),
#   slope_method = "dekkers",
#   show_body = FALSE,
#   ncol = 3
# )
# 
# pq_011_dekkers$plot
# pq_011_dekkers$table

pq_042 <- pareto_quantile_plot_probs_clean(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  probs = c(0.90, 0.92, 0.94, 0.96, 0.98),
  slope_method = "hill",
  show_body = FALSE,
  ncol = 3
)

pq_042$plot
pq_042$table

# ggsave(
#   filename = "C:/Users/fatim/Downloads/pareto_plot_042.pdf",
#   plot = pq_042$plot,
#   width = 8,
#   height = 6
# )

# pq_042_dekkers <- pareto_quantile_plot_probs_clean(
#   dados_lob = lob_042_MTPL_Material_Damage,
#   nome_lob = "MTPL Material Damage",
#   probs = c(0.90, 0.92, 0.94, 0.96, 0.98),
#   slope_method = "dekkers",
#   show_body = FALSE,
#   ncol = 3
# )
# 
# pq_042_dekkers$plot
# pq_042_dekkers$table


#Estatisticas anderson, cramer e kolmogorov
#AIC e BIC

pgpd_custom <- function(y, sigma, xi) {
  
  y <- as.numeric(y)
  
  if (
    length(sigma) != 1L ||
    length(xi) != 1L ||
    !is.finite(sigma) ||
    !is.finite(xi) ||
    sigma <= 0
  ) {
    return(rep(NA_real_, length(y)))
  }
  
  F <- rep(NA_real_, length(y))
  
  # A GPD dos excessos tem suporte y >= 0
  F[y < 0] <- 0
  
  validos <- is.finite(y) & y >= 0
  
  if (abs(xi) < 1e-8) {
    
    # Caso limite xi = 0: distribuição exponencial
    F[validos] <- 1 - exp(-y[validos] / sigma)
    
  } else {
    
    z <- 1 + xi * y[validos] / sigma
    
    dentro_suporte <- z > 0
    
    valores <- rep(NA_real_, sum(validos))
    
    valores[dentro_suporte] <-
      1 - z[dentro_suporte]^(-1 / xi)
    
    # Para xi < 0 existe um limite superior.
    # Acima desse limite, a CDF é igual a 1.
    valores[!dentro_suporte] <- 1
    
    F[validos] <- valores
  }
  
  pmin(pmax(F, 0), 1)
}

gpd_loglik <- function(y, sigma, xi) {
  
  y <- y[
    is.finite(y) &
      y >= 0
  ]
  
  if (
    length(y) == 0L ||
    !is.finite(sigma) ||
    !is.finite(xi) ||
    sigma <= 0
  ) {
    return(NA_real_)
  }
  
  if (abs(xi) < 1e-8) {
    
    # Caso exponencial
    return(
      sum(
        -log(sigma) -
          y / sigma
      )
    )
  }
  
  z <- 1 + xi * y / sigma
  
  # Verificação do suporte da GPD
  if (any(z <= 0)) {
    return(NA_real_)
  }
  
  sum(
    -log(sigma) -
      (1 + 1 / xi) * log(z)
  )
}

fit_gpd <- function(excessos, method = c("MLE", "MOM")) {
  
  method <- match.arg(method)
  
  excessos <- excessos[
    is.finite(excessos) &
      excessos >= 0
  ]
  
  if (length(excessos) < 2L) {
    return(
      list(
        xi = NA_real_,
        sigma = NA_real_,
        loglik = NA_real_
      )
    )
  }
  
  # ----------------------------------------------------------
  # Maximum Likelihood Estimation
  # ----------------------------------------------------------
  
  if (method == "MLE") {
    
    ajuste <- tryCatch(
      evir::gpd(
        data = excessos,
        threshold = 0,
        method = "ml"
      ),
      error = function(e) NULL,
      warning = function(w) {
        suppressWarnings(
          tryCatch(
            evir::gpd(
              data = excessos,
              threshold = 0,
              method = "ml"
            ),
            error = function(e) NULL
          )
        )
      }
    )
    
    if (is.null(ajuste)) {
      return(
        list(
          xi = NA_real_,
          sigma = NA_real_,
          loglik = NA_real_
        )
      )
    }
    
    xi_hat <- as.numeric(ajuste$par.ests["xi"])
    sigma_hat <- as.numeric(ajuste$par.ests["beta"])
    
    loglik <- gpd_loglik(
      y = excessos,
      sigma = sigma_hat,
      xi = xi_hat
    )
    
    return(
      list(
        xi = xi_hat,
        sigma = sigma_hat,
        loglik = loglik
      )
    )
  }
  
  # ----------------------------------------------------------
  # Method of Moments
  # ----------------------------------------------------------
  
  m1 <- mean(excessos)
  m2 <- mean(excessos^2)
  
  if (
    !is.finite(m1) ||
    !is.finite(m2) ||
    m1 <= 0 ||
    m2 <= m1^2
  ) {
    return(
      list(
        xi = NA_real_,
        sigma = NA_real_,
        loglik = NA_real_
      )
    )
  }
  
  xi_hat <- 0.5 * (
    1 -
      m1^2 / (m2 - m1^2)
  )
  
  sigma_hat <- 0.5 * m1 * (
    m2 / (m2 - m1^2) + 1
  )
  
  if (
    !is.finite(xi_hat) ||
    !is.finite(sigma_hat) ||
    sigma_hat <= 0
  ) {
    return(
      list(
        xi = xi_hat,
        sigma = NA_real_,
        loglik = NA_real_
      )
    )
  }
  
  list(
    xi = xi_hat,
    sigma = sigma_hat,
    loglik = NA_real_
  )
}

gpd_gof_statistics <- function(excessos, sigma, xi, eps = 1e-10) {
  
  Z <- pgpd_custom(
    y = excessos,
    sigma = sigma,
    xi = xi
  )
  
  Z <- Z[is.finite(Z)]
  
  n <- length(Z)
  
  if (n == 0L) {
    return(
      list(
        KS = NA_real_,
        D_plus = NA_real_,
        D_minus = NA_real_,
        CvM = NA_real_,
        AD = NA_real_
      )
    )
  }
  
  # Evita log(0) nas estatísticas de Anderson-Darling
  Z <- pmin(
    pmax(Z, eps),
    1 - eps
  )
  
  Z_ord <- sort(Z)
  i <- seq_len(n)
  
  # Kolmogorov-Smirnov
  D_plus <- max(
    i / n - Z_ord
  )
  
  D_minus <- max(
    Z_ord - (i - 1) / n
  )
  
  KS <- max(
    D_plus,
    D_minus
  )
  
  # Cramér-von Mises
  CvM <- 1 / (12 * n) +
    sum(
      (
        Z_ord -
          (2 * i - 1) / (2 * n)
      )^2
    )
  
  # Anderson-Darling
  AD <- -n -
    (1 / n) *
    sum(
      (2 * i - 1) *
        (
          log(Z_ord) +
            log(1 - rev(Z_ord))
        )
    )
  
  list(
    KS = as.numeric(KS),
    D_plus = as.numeric(D_plus),
    D_minus = as.numeric(D_minus),
    CvM = as.numeric(CvM),
    AD = as.numeric(AD)
  )
}

empty_gpd_result <- function(nome_lob,
                             method,
                             p_u,
                             threshold,
                             n_excessos,
                             xi = NA_real_,
                             sigma = NA_real_,
                             loglik = NA_real_) {
  
  data.frame(
    LOB = nome_lob,
    method = method,
    prob_threshold = p_u,
    threshold = threshold,
    n_excessos = n_excessos,
    shape = xi,
    scale = sigma,
    Kolmogorov_Smirnov = NA_real_,
    KS_pvalue = NA_real_,
    D_plus = NA_real_,
    D_minus = NA_real_,
    Cramer_von_Mises = NA_real_,
    CvM_pvalue = NA_real_,
    Anderson_Darling = NA_real_,
    AD_pvalue = NA_real_,
    logLik = loglik,
    AIC = NA_real_,
    BIC = NA_real_,
    AIC_por_excesso = NA_real_,
    BIC_por_excesso = NA_real_
  )
}

gof_gpd_by_quantiles <- function(
    dados_lob,
    nome_lob,
    probs = seq(0.80, 0.98, by = 0.005),
    method = c("MLE", "MOM"),
    claim_var = "Incurred_Claims",
    min_excessos = 10,
    eps = 1e-10) {
  
  method <- match.arg(method)
  
  # ----------------------------------------------------------
  # Preparação dos dados
  # ----------------------------------------------------------
  
  x <- dados_lob %>%
    dplyr::filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    ) %>%
    dplyr::pull(.data[[claim_var]]) %>%
    as.numeric()
  
  if (length(x) == 0L) {
    stop("Não existem observações válidas em ", claim_var, ".")
  }
  
  # Evita repetir o mesmo threshold quando existem empates
  tabela_thresholds <- data.frame(
    prob_threshold = probs,
    threshold = as.numeric(
      stats::quantile(
        x,
        probs = probs,
        na.rm = TRUE,
        type = 7
      )
    )
  )
  
  resultados <- lapply(
    seq_len(nrow(tabela_thresholds)),
    function(j) {
      
      p_u <- tabela_thresholds$prob_threshold[j]
      u <- tabela_thresholds$threshold[j]
      
      # Excessos relativamente ao threshold u
      excessos <- x[x > u] - u
      
      excessos <- excessos[
        is.finite(excessos) &
          excessos >= 0
      ]
      
      n_exc <- length(excessos)
      
      # Threshold com poucos excessos
      if (n_exc < min_excessos) {
        return(
          empty_gpd_result(
            nome_lob = nome_lob,
            method = method,
            p_u = p_u,
            threshold = u,
            n_excessos = n_exc
          )
        )
      }
      
      # ------------------------------------------------------
      # Ajustamento da GPD
      # ------------------------------------------------------
      
      fit <- fit_gpd(
        excessos = excessos,
        method = method
      )
      
      xi_hat <- fit$xi
      sigma_hat <- fit$sigma
      loglik <- fit$loglik
      
      ajuste_valido <-
        is.finite(xi_hat) &&
        is.finite(sigma_hat) &&
        sigma_hat > 0 &&
        all(
          1 + xi_hat * excessos / sigma_hat > 0
        )
      
      if (!ajuste_valido) {
        return(
          empty_gpd_result(
            nome_lob = nome_lob,
            method = method,
            p_u = p_u,
            threshold = u,
            n_excessos = n_exc,
            xi = xi_hat,
            sigma = sigma_hat,
            loglik = loglik
          )
        )
      }
      
      # ------------------------------------------------------
      # Estatísticas GOF manuais
      # ------------------------------------------------------
      
      gof_stats <- gpd_gof_statistics(
        excessos = excessos,
        sigma = sigma_hat,
        xi = xi_hat,
        eps = eps
      )
      
      # CDF ajustada utilizada nos testes
      F_gpd <- function(y) {
        pgpd_custom(
          y = y,
          sigma = sigma_hat,
          xi = xi_hat
        )
      }
      
      # ------------------------------------------------------
      # P-values
      # ------------------------------------------------------
      
      ks <- tryCatch(
        suppressWarnings(
          stats::ks.test(
            x = excessos,
            y = F_gpd
          )
        ),
        error = function(e) NULL
      )
      
      cvm <- tryCatch(
        goftest::cvm.test(
          x = excessos,
          null = F_gpd
        ),
        error = function(e) NULL
      )
      
      ad <- tryCatch(
        goftest::ad.test(
          x = excessos,
          null = F_gpd
        ),
        error = function(e) NULL
      )
      
      KS_pvalue <- if (is.null(ks)) {
        NA_real_
      } else {
        as.numeric(ks$p.value)
      }
      
      CvM_pvalue <- if (is.null(cvm)) {
        NA_real_
      } else {
        as.numeric(cvm$p.value)
      }
      
      AD_pvalue <- if (is.null(ad)) {
        NA_real_
      } else {
        as.numeric(ad$p.value)
      }
      
      # ------------------------------------------------------
      # AIC e BIC: apenas para MLE
      # ------------------------------------------------------
      
      if (
        method == "MLE" &&
        is.finite(loglik)
      ) {
        
        n_par <- 2
        
        AIC_val <- -2 * loglik +
          2 * n_par
        
        BIC_val <- -2 * loglik +
          n_par * log(n_exc)
        
        AIC_por_excesso <- AIC_val / n_exc
        BIC_por_excesso <- BIC_val / n_exc
        
      } else {
        
        AIC_val <- NA_real_
        BIC_val <- NA_real_
        AIC_por_excesso <- NA_real_
        BIC_por_excesso <- NA_real_
      }
      
      # ------------------------------------------------------
      # Resultado para o threshold atual
      # ------------------------------------------------------
      
      data.frame(
        LOB = nome_lob,
        method = method,
        prob_threshold = p_u,
        threshold = u,
        n_excessos = n_exc,
        shape = xi_hat,
        scale = sigma_hat,
        Kolmogorov_Smirnov = gof_stats$KS,
        KS_pvalue = KS_pvalue,
        D_plus = gof_stats$D_plus,
        D_minus = gof_stats$D_minus,
        Cramer_von_Mises = gof_stats$CvM,
        CvM_pvalue = CvM_pvalue,
        Anderson_Darling = gof_stats$AD,
        AD_pvalue = AD_pvalue,
        logLik = loglik,
        AIC = AIC_val,
        BIC = BIC_val,
        AIC_por_excesso = AIC_por_excesso,
        BIC_por_excesso = BIC_por_excesso
      )
    }
  )
  
  dplyr::bind_rows(resultados)
}

#MLE
gof_041 <- gof_gpd_by_quantiles(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  probs = seq(0.80, 0.98, by = 0.005),
  method = "MLE",
  min_excessos = 10
)

gof_041

# write.csv(
#   gof_041,
#   file = "tabela_criterios_041_mle.csv",
#   row.names = FALSE
# )

gof_011 <- gof_gpd_by_quantiles(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  probs = seq(0.80, 0.98, by = 0.005),
  method = "MLE",
  min_excessos = 10
)

gof_011

# write.csv(
#   gof_011,
#   file = "tabela_criterios_011_mle.csv",
#   row.names = FALSE
# )


gof_042 <- gof_gpd_by_quantiles(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  probs = seq(0.8, 0.98, by = 0.005),
  method = "MLE",
  min_excessos = 10
)

gof_042

# write.csv(
#   gof_042,
#   file = "tabela_criterios_042_mle.csv",
#   row.names = FALSE
# )


#MOM

gof_011_mom <- gof_gpd_by_quantiles(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  probs = seq(0.80, 0.98, by = 0.005),
  method = "MOM",
  min_excessos = 10
)

gof_011_mom

# write.csv(
#   gof_011_mom,
#   file = "tabela_criterios_011_mom.csv",
#   row.names = FALSE
# )

# MTPL Material Damage

gof_042_mom <- gof_gpd_by_quantiles(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  probs = seq(0.80, 0.98, by = 0.005),
  method = "MOM",
  min_excessos = 10
)

gof_042_mom

# write.csv(
#   gof_042_mom,
#   file = "tabela_criterios_042_mom.csv",
#   row.names = FALSE
# )

#METODOS AUTOMATICOS

#cabras e morales
ppp_normal_leave_one_out <- function(z_obs, z_train, tail = c("right", "left")) {
  
  tail <- match.arg(tail)
  
  z_train <- z_train[is.finite(z_train)] #remoção de valores invalidos
  m <- length(z_train)
  
  if (m < 3) {
    return(NA_real_)
  }
  
  z_bar <- mean(z_train)
  s <- sd(z_train)
  
  if (!is.finite(s) || s <= 0) {
    return(NA_real_)
  }
  
  # Posterior predictive:
  # Z_new | z_train ~ t_{m-1}(z_bar, s * sqrt(1 + 1/m))
  scale_pred <- s * sqrt(1 + 1 / m)
  t_value <- (z_obs - z_bar) / scale_pred
  #calculo da prob
  if (tail == "right") {
    p <- 1 - pt(t_value, df = m - 1)
  } else {
    p <- pt(t_value, df = m - 1)
  }
  
  return(as.numeric(p))
}


cabras_morales_threshold <- function(x,
                                     alpha = 0.05,
                                     log_transform = TRUE,
                                     detectar_duas_caudas = TRUE) {
  
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  n_original <- length(x)
  
  if (n_original < 20) {
    stop("Amostra demasiado pequena.")
  }
  
  # Trabalhar na escala transformada
  if (log_transform) {
    z <- log(x)
  } else {
    z <- x
  }
  
  dados_work <- data.frame(
    id = seq_along(x),
    x_original = x,
    z = z,
    removed = FALSE,
    tail = NA_character_,
    ppp = NA_real_,
    step = NA_integer_
  )
  
  step <- 1
  
  repeat {
    
    dados_active <- dados_work %>%
      filter(!removed)
    
    if (nrow(dados_active) < 5) {
      break
    }
    #alteraçoes que ainda nao foram consideradas outliers
    
    # Menor e maior observação ainda ativa
    idx_min <- dados_active$id[which.min(dados_active$z)]
    idx_max <- dados_active$id[which.max(dados_active$z)]
    
    z_min <- dados_work$z[dados_work$id == idx_min]
    z_max <- dados_work$z[dados_work$id == idx_max]
    
    # PPP para o máximo: remover o máximo da estimação
    z_train_max <- dados_work %>%
      filter(!removed, id != idx_max) %>%
      pull(z)
    
    p_right <- ppp_normal_leave_one_out(
      z_obs = z_max,
      z_train = z_train_max,
      tail = "right"
    )
    
    # PPP para o mínimo: remover o mínimo da estimação
    if (detectar_duas_caudas) {
      
      z_train_min <- dados_work %>%
        filter(!removed, id != idx_min) %>%
        pull(z)
      
      p_left <- ppp_normal_leave_one_out(
        z_obs = z_min,
        z_train = z_train_min,
        tail = "left"
      )
      
    } else {
      p_left <- Inf
    }
    
    # Decisão
    candidato <- data.frame(
      id = c(idx_min, idx_max),
      tail = c("left", "right"),
      ppp = c(p_left, p_right)
    ) %>%
      filter(is.finite(ppp)) %>%
      arrange(ppp)
    
    if (nrow(candidato) == 0) {
      break
    }
    
    # Se o menor ppp for inferior a alpha, remove essa observação
    if (candidato$ppp[1] < alpha) {
      
      id_remove <- candidato$id[1]
      
      dados_work$removed[dados_work$id == id_remove] <- TRUE
      dados_work$tail[dados_work$id == id_remove] <- candidato$tail[1]
      dados_work$ppp[dados_work$id == id_remove] <- candidato$ppp[1]
      dados_work$step[dados_work$id == id_remove] <- step
      
      step <- step + 1
      
    } else {
      break
    }
  }
  
  # Outliers da cauda direita
  right_outliers <- dados_work %>%
    filter(removed, tail == "right") %>%
    arrange(x_original)
  
  if (nrow(right_outliers) == 0) {
    
    threshold <- NA_real_
    threshold_log <- NA_real_
    n_excessos <- 0
    
  } else {
    
    # Threshold = menor outlier da cauda direita
    threshold <- min(right_outliers$x_original)
    threshold_log <- min(right_outliers$z)
    n_excessos <- sum(x > threshold)
  }
  
  resumo <- data.frame(
    n = n_original,
    alpha = alpha,
    log_transform = log_transform,
    threshold = threshold,
    threshold_log = threshold_log,
    n_right_outliers = nrow(right_outliers),
    n_left_outliers = sum(dados_work$removed & dados_work$tail == "left"),
    n_total_outliers = sum(dados_work$removed),
    n_excessos = n_excessos,
    prop_excessos = n_excessos / n_original
  )
  
  return(
    list(
      summary = resumo,
      removed_table = dados_work %>% filter(removed) %>% arrange(step),
      right_outliers = right_outliers,
      full_table = dados_work
    )
  )
}

#Aplicar a uma LOB
cabras_morales_lob <- function(dados_lob,
                               nome_lob,
                               claim_var = "Incurred_Claims",
                               alpha = 0.05,
                               log_transform = TRUE,
                               min_excessos_gpd = 10) {
  
  x <- dados_lob %>%
    filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    ) %>%
    pull(.data[[claim_var]])
  
  res <- cabras_morales_threshold(
    x = x,
    alpha = alpha,
    log_transform = log_transform,
    detectar_duas_caudas = FALSE
  )
  
  resumo <- res$summary %>%
    dplyr::mutate(LOB = nome_lob) %>%
    dplyr::select(
      LOB,
      n,
      alpha,
      log_transform,
      threshold,
      threshold_log,
      n_right_outliers,
      n_left_outliers,
      n_total_outliers,
      n_excessos,
      prop_excessos
    )
  
  u <- resumo$threshold
  
  # Ajuste GPD acima do threshold selecionado
  if (is.na(u) || resumo$n_excessos < min_excessos_gpd) {
    
    fit_gpd <- NULL
    
    gpd_summary <- data.frame(
      LOB = nome_lob,
      threshold = u,
      n_excessos = resumo$n_excessos,
      scale = NA_real_,
      shape = NA_real_,
      convergence = NA_real_
    )
    
  } else {
    
    fit_gpd <- tryCatch(
      POT::fitgpd(
        data = x,
        threshold = u,
        est = "mle"
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit_gpd)) {
      
      gpd_summary <- data.frame(
        LOB = nome_lob,
        threshold = u,
        n_excessos = resumo$n_excessos,
        scale = NA_real_,
        shape = NA_real_,
        convergence = NA_real_
      )
      
    } else {
      
      gpd_summary <- data.frame(
        LOB = nome_lob,
        threshold = u,
        n_excessos = resumo$n_excessos,
        scale = as.numeric(fit_gpd$param["scale"]),
        shape = as.numeric(fit_gpd$param["shape"]),
        convergence = fit_gpd$conv
      )
    }
  }
  
  return(
    list(
      summary = resumo,
      removed_table = res$removed_table,
      right_outliers = res$right_outliers,
      full_table = res$full_table,
      fit_gpd = fit_gpd,
      gpd_summary = gpd_summary
    )
  )
}


cm_041 <- cabras_morales_lob(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  alpha = 0.05,
  log_transform = TRUE
)

cm_011 <- cabras_morales_lob(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  alpha = 0.05,
  log_transform = TRUE
)

cm_042 <- cabras_morales_lob(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  alpha = 0.05,
  log_transform = TRUE
)

summary_cabras_morales <- bind_rows(
  cm_041$summary,
  cm_011$summary,
  cm_042$summary
)

summary_cabras_morales

gpd_cabras_morales <- bind_rows(
  cm_041$gpd_summary,
  cm_011$gpd_summary,
  cm_042$gpd_summary
)

gpd_cabras_morales

cm_041$removed_table
cm_011$removed_table
cm_042$removed_table

cm_041$right_outliers
cm_011$right_outliers
cm_042$right_outliers

plot_cabras_morales <- function(dados_lob, res_cm, nome_lob,
                                claim_var = "Incurred_Claims") {
  
  u <- res_cm$summary$threshold
  
  dados_plot <- dados_lob %>%
    filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    )
  
  ggplot(dados_plot, aes(x = .data[[claim_var]])) +
    geom_histogram(bins = 80, fill = "grey80", color = "grey40") +
    geom_vline(
      xintercept = u,
      linetype = "dashed",
      linewidth = 1
    ) +
    scale_x_log10(labels = scales::comma) +
    labs(
      title = paste(nome_lob),
      subtitle = paste("u =", scales::comma(round(u, 2))),
      x = "Incurred Claims",
      y = "Frequency"
    ) +
    theme_minimal()
}

met1_041 = plot_cabras_morales(
  lob_041_MTPL_Bodily_Injury,
  cm_041,
  "MTPL Bodily Injury"
)
met1_041

# ggsave(
#   filename = "C:/Users/fatim/Downloads/cabras_morales_041.pdf",
#   plot = met1_041,
#   width = 8,
#   height = 6
# )

met1_011 =plot_cabras_morales(
  lob_011_Health,
  cm_011,
  "Health"
)
met1_011

# ggsave(
#   filename = "C:/Users/fatim/Downloads/cabras_morales_011.pdf",
#   plot = met1_011,
#   width = 8,
#   height = 6
# )

met1_042 =plot_cabras_morales(
  lob_042_MTPL_Material_Damage,
  cm_042,
  "MTPL Material Damage"
)
met1_042

# ggsave(
#   filename = "C:/Users/fatim/Downloads/cabras_morales_042.pdf",
#   plot = met1_042,
#   width = 8,
#   height = 6
# )

qqplot_gpd_cabras <- function(dados_lob, res_cm, nome_lob,
                              claim_var = "Incurred_Claims") {
  
  u <- res_cm$summary$threshold
  fit <- res_cm$fit_gpd
  
  if (is.null(fit) || is.na(u)) {
    stop("Não há ajuste GPD válido.")
  }
  
  x <- dados_lob %>%
    filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    ) %>%
    pull(.data[[claim_var]])
  
  excessos <- sort(x[x > u] - u)
  k <- length(excessos)
  
  sigma <- as.numeric(fit$param["scale"])
  xi <- as.numeric(fit$param["shape"])
  
  p_emp <- ppoints(k)
  
  q_theo <- if (abs(xi) < 1e-8) {
    -sigma * log(1 - p_emp)
  } else {
    (sigma / xi) * ((1 - p_emp)^(-xi) - 1)
  }
  
  dados_qq <- data.frame(
    theoretical = q_theo,
    empirical = excessos
  )
  
  ggplot(dados_qq, aes(x = theoretical, y = empirical)) +
    geom_point(alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(
      title = paste("GPD QQ plot - Cabras and Morales -", nome_lob),
      subtitle = paste("u =", round(u, 2), ", k =", k),
      x = "Theoretical GPD excess quantiles",
      y = "Empirical excess quantiles"
    ) +
    theme_minimal()
}

qqplot_gpd_cabras(
  lob_041_MTPL_Bodily_Injury,
  cm_041,
  "MTPL Bodily Injury"
)

qqplot_gpd_cabras(
  lob_011_Health,
  cm_011,
  "Health"
)

qqplot_gpd_cabras(
  lob_042_MTPL_Material_Damage,
  cm_042,
  "MTPL Material Damage"
)

#### TESTAR DISTRIBUIÇOES ### CODIGO TODO

# ------------------------------------------------------------
# Simulação multivariada Normal robusta
# ------------------------------------------------------------

rmvn_safe <- function(n, mean, sigma) {
  
  sigma <- (sigma + t(sigma)) / 2
  
  eig <- eigen(
    sigma,
    symmetric = TRUE
  )
  
  # Evitar problemas numéricos
  eig$values <- pmax(
    eig$values,
    1e-12
  )
  
  A <- eig$vectors %*%
    diag(
      sqrt(eig$values),
      nrow = length(mean)
    )
  
  Z <- matrix(
    rnorm(n * length(mean)),
    nrow = n,
    ncol = length(mean)
  )
  
  draws <- Z %*% t(A)
  
  draws <- sweep(
    draws,
    MARGIN = 2,
    STATS = mean,
    FUN = "+"
  )
  
  draws
}


# ============================================================
# 2. LOGNORMAL
# Posterior predictive exata na escala log
# ============================================================

ppp_normal_leave_one_out <- function(
    z_obs,
    z_train,
    tail = c("right", "left")) {
  
  tail <- match.arg(tail)
  
  z_train <- z_train[
    is.finite(z_train)
  ]
  
  m <- length(z_train)
  
  if (m < 3) {
    return(NA_real_)
  }
  
  z_bar <- mean(z_train)
  s <- sd(z_train)
  
  if (!is.finite(s) || s <= 0) {
    return(NA_real_)
  }
  
  # Posterior predictive:
  #
  # Z_new | z_train
  # ~ t_{m-1}(
  #     location = z_bar,
  #     scale = s * sqrt(1 + 1/m)
  #   )
  
  scale_pred <- s * sqrt(1 + 1 / m)
  
  t_value <- (
    z_obs - z_bar
  ) / scale_pred
  
  if (tail == "right") {
    
    p <- pt(
      t_value,
      df = m - 1,
      lower.tail = FALSE
    )
    
  } else {
    
    p <- pt(
      t_value,
      df = m - 1,
      lower.tail = TRUE
    )
  }
  
  as.numeric(p)
}


# ============================================================
# 3. MLE RÁPIDO PARA GAMMA
# ============================================================

fit_gamma_fast <- function(x) {
  
  x <- x[
    is.finite(x) &
      !is.na(x) &
      x > 0
  ]
  
  n <- length(x)
  
  if (n < 3) {
    return(NULL)
  }
  
  mean_x <- mean(x)
  mean_log_x <- mean(log(x))
  
  target <- log(mean_x) - mean_log_x
  
  # Equação MLE para o parâmetro shape:
  #
  # log(alpha) - digamma(alpha)
  # =
  # log(mean(x)) - mean(log(x))
  
  equation_shape <- function(alpha) {
    
    log(alpha) -
      digamma(alpha) -
      target
  }
  
  lower <- 1e-6
  upper <- 1
  
  while (
    equation_shape(upper) > 0 &&
    upper < 1e6
  ) {
    
    upper <- upper * 2
  }
  
  root_ok <- tryCatch(
    {
      uniroot(
        equation_shape,
        interval = c(lower, upper)
      )$root
    },
    error = function(e) NA_real_
  )
  
  if (!is.finite(root_ok)) {
    return(NULL)
  }
  
  shape <- root_ok
  rate <- shape / mean_x
  
  # Fisher Information para (shape, rate)
  
  I11 <- trigamma(shape)
  I12 <- -1 / rate
  I22 <- shape / rate^2
  
  fisher <- n * matrix(
    c(
      I11, I12,
      I12, I22
    ),
    nrow = 2,
    byrow = TRUE
  )
  
  vcov_theta <- tryCatch(
    solve(fisher),
    error = function(e) NULL
  )
  
  if (is.null(vcov_theta)) {
    return(NULL)
  }
  
  list(
    estimate = c(
      shape = shape,
      rate = rate
    ),
    vcov = vcov_theta
  )
}


# ============================================================
# 4. MLE RÁPIDO PARA WEIBULL
# ============================================================

fit_weibull_fast <- function(x) {
  
  x <- x[
    is.finite(x) &
      !is.na(x) &
      x > 0
  ]
  
  n <- length(x)
  
  if (n < 3) {
    return(NULL)
  }
  
  log_x <- log(x)
  mean_log_x <- mean(log_x)
  
  # ----------------------------------------------------------
  # Equação MLE para o shape
  # ----------------------------------------------------------
  
  equation_shape <- function(k) {
    
    if (!is.finite(k) || k <= 0) {
      return(Inf)
    }
    
    log_weights <- k * log_x
    
    max_log_weight <- max(log_weights)
    
    weights <- exp(
      log_weights -
        max_log_weight
    )
    
    weighted_log_mean <-
      sum(weights * log_x) /
      sum(weights)
    
    1 / k +
      mean_log_x -
      weighted_log_mean
  }
  
  
  lower <- 1e-3
  upper <- 1
  
  f_lower <- equation_shape(lower)
  f_upper <- equation_shape(upper)
  
  while (
    is.finite(f_upper) &&
    f_upper > 0 &&
    upper < 1e3
  ) {
    
    upper <- upper * 2
    
    f_upper <- equation_shape(upper)
  }
  
  
  shape <- tryCatch(
    {
      uniroot(
        equation_shape,
        interval = c(lower, upper)
      )$root
    },
    error = function(e) NA_real_
  )
  
  if (!is.finite(shape)) {
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # MLE para o scale
  #
  # lambda = [mean(x^k)]^(1/k)
  #
  # calculado em log para evitar overflow
  # ----------------------------------------------------------
  
  log_x_shape <- shape * log_x
  
  m <- max(log_x_shape)
  
  log_mean_power <-
    m +
    log(
      mean(
        exp(
          log_x_shape - m
        )
      )
    )
  
  log_scale <- log_mean_power / shape
  
  scale <- exp(log_scale)
  
  if (
    !is.finite(scale) ||
    scale <= 0
  ) {
    
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # Fisher Information aproximada
  # para (shape, scale)
  # ----------------------------------------------------------
  
  euler_gamma <- 0.5772156649015329
  
  A <-
    pi^2 / 6 +
    (1 - euler_gamma)^2
  
  I11 <- A / shape^2
  
  I12 <- (
    euler_gamma - 1
  ) / scale
  
  I22 <- shape^2 / scale^2
  
  fisher <- n * matrix(
    c(
      I11, I12,
      I12, I22
    ),
    nrow = 2,
    byrow = TRUE
  )
  
  vcov_theta <- tryCatch(
    solve(fisher),
    error = function(e) NULL
  )
  
  if (is.null(vcov_theta)) {
    return(NULL)
  }
  
  list(
    estimate = c(
      shape = shape,
      scale = scale
    ),
    vcov = vcov_theta
  )
}


# ============================================================
# 5. POSTERIOR PREDICTIVE PARA GAMMA / WEIBULL
#
# Aproximação de Laplace:
#
# posterior dos parâmetros
# ≈ Normal em torno do MLE
#
# Trabalhamos na escala log dos parâmetros para garantir
# positividade.
# ============================================================

posterior_predictive_positive <- function(
    x_obs,
    x_train,
    body_dist = c("gamma", "weibull"),
    tail = c("right", "left"),
    n_draws = 4000) {
  
  body_dist <- match.arg(body_dist)
  tail <- match.arg(tail)
  
  x_train <- x_train[
    is.finite(x_train) &
      !is.na(x_train) &
      x_train > 0
  ]
  
  if (
    !is.finite(x_obs) ||
    is.na(x_obs) ||
    x_obs <= 0
  ) {
    
    return(NA_real_)
  }
  
  if (length(x_train) < 3) {
    return(NA_real_)
  }
  
  
  # ----------------------------------------------------------
  # Reescalamento apenas por estabilidade numérica
  # ----------------------------------------------------------
  
  scale_factor <- median(x_train)
  
  if (
    !is.finite(scale_factor) ||
    scale_factor <= 0
  ) {
    
    return(NA_real_)
  }
  
  y_train <- x_train / scale_factor
  y_obs <- x_obs / scale_factor
  
  
  # ----------------------------------------------------------
  # Ajuste paramétrico
  # ----------------------------------------------------------
  
  fit <- if (body_dist == "gamma") {
    
    fit_gamma_fast(y_train)
    
  } else {
    
    fit_weibull_fast(y_train)
  }
  
  
  if (is.null(fit)) {
    return(NA_real_)
  }
  
  
  theta_hat <- fit$estimate
  vcov_theta <- fit$vcov
  
  
  # ----------------------------------------------------------
  # Transformar para log-parâmetros
  #
  # eta = log(theta)
  #
  # Delta method:
  #
  # Var(log theta)
  # ≈ J Var(theta) J'
  # ----------------------------------------------------------
  
  eta_hat <- log(theta_hat)
  
  J <- diag(
    1 / theta_hat,
    nrow = length(theta_hat)
  )
  
  vcov_eta <-
    J %*%
    vcov_theta %*%
    J
  
  
  # ----------------------------------------------------------
  # Draws da posterior aproximada
  # ----------------------------------------------------------
  
  eta_draws <- tryCatch(
    {
      rmvn_safe(
        n = n_draws,
        mean = eta_hat,
        sigma = vcov_eta
      )
    },
    error = function(e) NULL
  )
  
  if (is.null(eta_draws)) {
    return(NA_real_)
  }
  
  
  theta_draws <- exp(eta_draws)
  
  
  # Remover possíveis problemas numéricos
  
  valid <- apply(
    theta_draws,
    1,
    function(z) {
      all(
        is.finite(z) &
          z > 0 &
          z < 1e10
      )
    }
  )
  
  theta_draws <- theta_draws[
    valid,
    ,
    drop = FALSE
  ]
  
  if (nrow(theta_draws) < 100) {
    return(NA_real_)
  }
  
  
  # ----------------------------------------------------------
  # Probabilidade preditiva
  # ----------------------------------------------------------
  
  if (body_dist == "gamma") {
    
    shape_draw <- theta_draws[, 1]
    rate_draw <- theta_draws[, 2]
    
    p_draws <- pgamma(
      q = rep(
        y_obs,
        length(shape_draw)
      ),
      shape = shape_draw,
      rate = rate_draw,
      lower.tail = (
        tail == "left"
      )
    )
    
  } else {
    
    shape_draw <- theta_draws[, 1]
    scale_draw <- theta_draws[, 2]
    
    p_draws <- pweibull(
      q = rep(
        y_obs,
        length(shape_draw)
      ),
      shape = shape_draw,
      scale = scale_draw,
      lower.tail = (
        tail == "left"
      )
    )
  }
  
  
  p_draws <- p_draws[
    is.finite(p_draws)
  ]
  
  if (length(p_draws) == 0) {
    return(NA_real_)
  }
  
  
  # Integração da incerteza paramétrica
  #
  # P(X_new >= x_obs | dados)
  # =
  # E_theta [
  #   P(X_new >= x_obs | theta)
  # ]
  
  p <- mean(p_draws)
  
  p <- max(
    min(
      as.numeric(p),
      1
    ),
    0
  )
  
  p
}


# ============================================================
# 6. FUNÇÃO ÚNICA PARA AS TRÊS DISTRIBUIÇÕES
# ============================================================

body_tail_pvalue_leave_one_out <- function(
    x_obs,
    x_train,
    body_dist = c(
      "lognormal",
      "gamma",
      "weibull"
    ),
    tail = c(
      "right",
      "left"
    ),
    n_draws = 4000) {
  
  body_dist <- match.arg(body_dist)
  tail <- match.arg(tail)
  
  
  x_train <- x_train[
    is.finite(x_train) &
      !is.na(x_train) &
      x_train > 0
  ]
  
  
  if (
    !is.finite(x_obs) ||
    is.na(x_obs) ||
    x_obs <= 0
  ) {
    
    return(NA_real_)
  }
  
  
  if (length(x_train) < 3) {
    return(NA_real_)
  }
  
  
  # ----------------------------------------------------------
  # LOGNORMAL
  # ----------------------------------------------------------
  
  if (body_dist == "lognormal") {
    
    return(
      ppp_normal_leave_one_out(
        z_obs = log(x_obs),
        z_train = log(x_train),
        tail = tail
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # GAMMA / WEIBULL
  # ----------------------------------------------------------
  
  posterior_predictive_positive(
    x_obs = x_obs,
    x_train = x_train,
    body_dist = body_dist,
    tail = tail,
    n_draws = n_draws
  )
}


# ============================================================
# 7. PROCEDIMENTO SEQUENCIAL CABRAS & MORALES
# ============================================================

cabras_morales_threshold_dist <- function(
    x,
    alpha = 0.05,
    body_dist = c(
      "lognormal",
      "gamma",
      "weibull"
    ),
    detectar_duas_caudas = FALSE,
    min_active = 5,
    n_draws = 4000,
    restrict_upper_half = TRUE,
    max_removals = Inf,
    verbose = FALSE) {
  
  body_dist <- match.arg(body_dist)
  
  
  x <- x[
    is.finite(x) &
      !is.na(x) &
      x > 0
  ]
  
  
  n_original <- length(x)
  
  if (n_original < 20) {
    stop(
      "Amostra demasiado pequena."
    )
  }
  
  
  dados_work <- data.frame(
    id = seq_along(x),
    x_original = x,
    removed = FALSE,
    tail = NA_character_,
    ppp = NA_real_,
    step = NA_integer_
  )
  
  
  # ----------------------------------------------------------
  # Restrição de Cabras & Morales:
  #
  # [n/2] <= k <= n
  #
  # Portanto o threshold da direita não deve
  # atravessar a metade da amostra.
  # ----------------------------------------------------------
  
  if (restrict_upper_half) {
    
    minimum_allowed_rank <-
      floor(
        n_original / 2
      )
    
    maximum_right_outliers <-
      n_original -
      minimum_allowed_rank +
      1
    
  } else {
    
    maximum_right_outliers <- Inf
  }
  
  
  step <- 1
  
  boundary_hit <- FALSE
  
  
  repeat {
    
    
    dados_active <- dados_work %>%
      filter(!removed)
    
    
    if (
      nrow(dados_active) <
      min_active
    ) {
      
      break
    }
    
    
    if (
      (step - 1) >=
      max_removals
    ) {
      
      warning(
        paste(
          "Número máximo de remoções",
          "atingido para",
          body_dist
        )
      )
      
      break
    }
    
    
    n_right_removed <- sum(
      dados_work$removed &
        dados_work$tail == "right"
    )
    
    
    # --------------------------------------------------------
    # Se atingirmos a metade superior e a última observação
    # nesse limite também foi rejeitada, consideramos que
    # o modelo do corpo está a tentar classificar uma fração
    # excessiva da amostra como extrema.
    # --------------------------------------------------------
    
    if (
      restrict_upper_half &&
      n_right_removed >=
      maximum_right_outliers
    ) {
      
      boundary_hit <- TRUE
      
      break
    }
    
    
    # --------------------------------------------------------
    # Observações extremas atualmente ativas
    # --------------------------------------------------------
    
    idx_min <- dados_active$id[
      which.min(
        dados_active$x_original
      )
    ]
    
    idx_max <- dados_active$id[
      which.max(
        dados_active$x_original
      )
    ]
    
    
    x_min <- dados_work$x_original[
      dados_work$id == idx_min
    ]
    
    x_max <- dados_work$x_original[
      dados_work$id == idx_max
    ]
    
    
    # --------------------------------------------------------
    # TESTE DA CAUDA DIREITA
    #
    # A observação máxima é retirada da estimação.
    # --------------------------------------------------------
    
    x_train_max <- dados_work %>%
      filter(
        !removed,
        id != idx_max
      ) %>%
      pull(x_original)
    
    
    p_right <-
      body_tail_pvalue_leave_one_out(
        x_obs = x_max,
        x_train = x_train_max,
        body_dist = body_dist,
        tail = "right",
        n_draws = n_draws
      )
    
    
    # --------------------------------------------------------
    # TESTE DA CAUDA ESQUERDA
    # --------------------------------------------------------
    
    if (detectar_duas_caudas) {
      
      x_train_min <- dados_work %>%
        filter(
          !removed,
          id != idx_min
        ) %>%
        pull(x_original)
      
      
      p_left <-
        body_tail_pvalue_leave_one_out(
          x_obs = x_min,
          x_train = x_train_min,
          body_dist = body_dist,
          tail = "left",
          n_draws = n_draws
        )
      
    } else {
      
      p_left <- NA_real_
    }
    
    
    # --------------------------------------------------------
    # Escolher o outlier com maior evidência
    # = menor ppp
    # --------------------------------------------------------
    
    if (detectar_duas_caudas) {
      
      candidato <- data.frame(
        id = c(
          idx_min,
          idx_max
        ),
        tail = c(
          "left",
          "right"
        ),
        ppp = c(
          p_left,
          p_right
        )
      ) %>%
        filter(
          is.finite(ppp)
        ) %>%
        arrange(ppp)
      
    } else {
      
      candidato <- data.frame(
        id = idx_max,
        tail = "right",
        ppp = p_right
      ) %>%
        filter(
          is.finite(ppp)
        )
    }
    
    
    if (nrow(candidato) == 0) {
      
      warning(
        paste(
          "Não foi possível calcular",
          "probabilidades válidas para",
          body_dist
        )
      )
      
      break
    }
    
    
    # --------------------------------------------------------
    # Remover se ppp < alpha
    # --------------------------------------------------------
    
    if (
      candidato$ppp[1] <
      alpha
    ) {
      
      id_remove <-
        candidato$id[1]
      
      
      dados_work$removed[
        dados_work$id ==
          id_remove
      ] <- TRUE
      
      
      dados_work$tail[
        dados_work$id ==
          id_remove
      ] <-
        candidato$tail[1]
      
      
      dados_work$ppp[
        dados_work$id ==
          id_remove
      ] <-
        candidato$ppp[1]
      
      
      dados_work$step[
        dados_work$id ==
          id_remove
      ] <-
        step
      
      
      if (verbose) {
        
        message(
          paste0(
            "Step ",
            step,
            " | ",
            body_dist,
            " | x = ",
            round(
              dados_work$x_original[
                dados_work$id ==
                  id_remove
              ],
              2
            ),
            " | tail = ",
            candidato$tail[1],
            " | ppp = ",
            signif(
              candidato$ppp[1],
              4
            )
          )
        )
      }
      
      
      step <- step + 1
      
    } else {
      
      break
    }
  }
  
  
  # =========================================================
  # OUTLIERS DA CAUDA DIREITA
  # =========================================================
  
  right_outliers <- dados_work %>%
    filter(
      removed,
      tail == "right"
    ) %>%
    arrange(x_original)
  
  
  if (
    nrow(right_outliers) ==
    0
  ) {
    
    threshold <- NA_real_
    n_excessos <- 0
    threshold_quantile <- NA_real_
    
  } else {
    
    threshold <-
      min(
        right_outliers$x_original
      )
    
    
    # POT:
    # excessos estritamente superiores a u
    
    n_excessos <-
      sum(
        x > threshold
      )
    
    
    threshold_quantile <-
      mean(
        x <= threshold
      )
  }
  
  
  predictive_method <- switch(
    body_dist,
    
    lognormal =
      paste(
        "Exact Student-t posterior predictive",
        "on log scale"
      ),
    
    gamma =
      paste(
        "Posterior predictive",
        "with Laplace approximation"
      ),
    
    weibull =
      paste(
        "Posterior predictive",
        "with Laplace approximation"
      )
  )
  
  
  resumo <- data.frame(
    n = n_original,
    alpha = alpha,
    body_dist = body_dist,
    predictive_method =
      predictive_method,
    threshold = threshold,
    threshold_quantile =
      threshold_quantile,
    n_right_outliers =
      nrow(right_outliers),
    n_left_outliers =
      sum(
        dados_work$removed &
          dados_work$tail ==
          "left"
      ),
    n_total_outliers =
      sum(
        dados_work$removed
      ),
    n_excessos =
      n_excessos,
    prop_excessos =
      n_excessos /
      n_original,
    boundary_hit =
      boundary_hit
  )
  
  
  list(
    summary = resumo,
    
    removed_table =
      dados_work %>%
      filter(removed) %>%
      arrange(step),
    
    right_outliers =
      right_outliers,
    
    full_table =
      dados_work
  )
}


# APLICAÇÃO LOBS

cabras_morales_lob_dist <- function(
    dados_lob,
    nome_lob,
    claim_var =
      "Incurred_Claims",
    alpha = 0.05,
    body_dist = c(
      "lognormal",
      "gamma",
      "weibull"
    ),
    detectar_duas_caudas = FALSE,
    min_excessos_gpd = 10,
    n_draws = 4000,
    restrict_upper_half = TRUE,
    max_removals = Inf,
    verbose = FALSE) {
  
  body_dist <- match.arg(
    body_dist
  )
  
  
  x <- dados_lob %>%
    filter(
      !is.na(
        .data[[claim_var]]
      ),
      is.finite(
        .data[[claim_var]]
      ),
      .data[[claim_var]] > 0
    ) %>%
    pull(
      .data[[claim_var]]
    )
  
  
  res <-
    cabras_morales_threshold_dist(
      x = x,
      alpha = alpha,
      body_dist = body_dist,
      detectar_duas_caudas =
        detectar_duas_caudas,
      n_draws = n_draws,
      restrict_upper_half =
        restrict_upper_half,
      max_removals =
        max_removals,
      verbose = verbose
    )
  
  
  resumo <- res$summary %>%
    dplyr::mutate(
      LOB = nome_lob
    ) %>%
    dplyr::select(
      LOB,
      dplyr::everything()
    )
  
  
  u <- resumo$threshold
  
  
  # =========================================================
  # AJUSTE GPD
  # =========================================================
  
  if (
    is.na(u) ||
    resumo$n_excessos <
    min_excessos_gpd
  ) {
    
    fit_gpd <- NULL
    
    gpd_summary <- data.frame(
      LOB = nome_lob,
      body_dist = body_dist,
      threshold = u,
      n_excessos =
        resumo$n_excessos,
      scale = NA_real_,
      shape = NA_real_,
      convergence =
        NA_character_
    )
    
  } else {
    
    fit_gpd <- tryCatch(
      {
        POT::fitgpd(
          data = x,
          threshold = u,
          est = "mle"
        )
      },
      error = function(e) NULL
    )
    
    
    if (is.null(fit_gpd)) {
      
      gpd_summary <- data.frame(
        LOB = nome_lob,
        body_dist = body_dist,
        threshold = u,
        n_excessos =
          resumo$n_excessos,
        scale = NA_real_,
        shape = NA_real_,
        convergence =
          "failed"
      )
      
    } else {
      
      gpd_summary <- data.frame(
        LOB = nome_lob,
        body_dist = body_dist,
        threshold = u,
        n_excessos =
          resumo$n_excessos,
        scale =
          as.numeric(
            fit_gpd$param[
              "scale"
            ]
          ),
        shape =
          as.numeric(
            fit_gpd$param[
              "shape"
            ]
          ),
        convergence =
          ifelse(
            fit_gpd$conv == 0,
            "successful",
            paste0(
              "code ",
              fit_gpd$conv
            )
          )
      )
    }
  }
  
  
  list(
    summary =
      resumo,
    
    removed_table =
      res$removed_table,
    
    right_outliers =
      res$right_outliers,
    
    full_table =
      res$full_table,
    
    fit_gpd =
      fit_gpd,
    
    gpd_summary =
      gpd_summary
  )
}


# COMPARAR AS TRÊS DISTRIBUIÇÕES

comparar_distribuicoes_lob <- function(
    dados_lob,
    nome_lob,
    claim_var =
      "Incurred_Claims",
    alpha = 0.05,
    body_dists = c(
      "lognormal",
      "gamma",
      "weibull"
    ),
    detectar_duas_caudas = FALSE,
    min_excessos_gpd = 10,
    n_draws = 4000,
    restrict_upper_half = TRUE,
    max_removals = Inf,
    verbose = FALSE) {
  
  
  resultados <- setNames(
    lapply(
      body_dists,
      function(dist_i) {
        
        cabras_morales_lob_dist(
          dados_lob =
            dados_lob,
          nome_lob =
            nome_lob,
          claim_var =
            claim_var,
          alpha =
            alpha,
          body_dist =
            dist_i,
          detectar_duas_caudas =
            detectar_duas_caudas,
          min_excessos_gpd =
            min_excessos_gpd,
          n_draws =
            n_draws,
          restrict_upper_half =
            restrict_upper_half,
          max_removals =
            max_removals,
          verbose =
            verbose
        )
      }
    ),
    body_dists
  )
  
  
  comparison <- bind_rows(
    lapply(
      resultados,
      function(res_i) {
        
        s <- res_i$summary
        g <- res_i$gpd_summary
        
        data.frame(
          LOB =
            s$LOB,
          body_dist =
            s$body_dist,
          predictive_method =
            s$predictive_method,
          n =
            s$n,
          threshold =
            s$threshold,
          threshold_quantile =
            s$threshold_quantile,
          n_right_outliers =
            s$n_right_outliers,
          n_excessos =
            s$n_excessos,
          prop_excessos =
            s$prop_excessos,
          boundary_hit =
            s$boundary_hit,
          gpd_scale =
            g$scale,
          gpd_shape =
            g$shape,
          gpd_convergence =
            g$convergence
        )
      }
    )
  )
  
  
  list(
    comparison =
      comparison,
    results =
      resultados
  )
}


# AJUSTE DO CORPO PARA DIAGNÓSTICOS

fit_body_for_diagnostics <- function(
    x,
    body_dist = c(
      "lognormal",
      "gamma",
      "weibull"
    )) {
  
  body_dist <- match.arg(
    body_dist
  )
  
  
  x <- x[
    is.finite(x) &
      !is.na(x) &
      x > 0
  ]
  
  
  scale_factor <- median(x)
  
  if (
    !is.finite(scale_factor) ||
    scale_factor <= 0
  ) {
    
    return(NULL)
  }
  
  
  y <- x / scale_factor
  
  
  distr_fitdist <- switch(
    body_dist,
    
    lognormal =
      "lnorm",
    
    gamma =
      "gamma",
    
    weibull =
      "weibull"
  )
  
  
  fit <- tryCatch(
    {
      
      if (
        body_dist ==
        "gamma"
      ) {
        
        media_y <- mean(y)
        var_y <- var(y)
        
        
        start_gamma <- list(
          shape =
            media_y^2 /
            var_y,
          rate =
            media_y /
            var_y
        )
        
        
        suppressWarnings(
          fitdistrplus::fitdist(
            y,
            distr =
              distr_fitdist,
            method =
              "mle",
            start =
              start_gamma
          )
        )
        
      } else {
        
        suppressWarnings(
          fitdistrplus::fitdist(
            y,
            distr =
              distr_fitdist,
            method =
              "mle"
          )
        )
      }
    },
    error = function(e) NULL
  )
  
  
  if (is.null(fit)) {
    return(NULL)
  }
  
  
  list(
    fit =
      fit,
    scale_factor =
      scale_factor,
    body_dist =
      body_dist
  )
}


comparar_ajuste_corpo_comum <- function(
    dados_lob,
    comparison_cm,
    claim_var =
      "Incurred_Claims",
    body_dists = c(
      "lognormal",
      "gamma",
      "weibull"
    ),
    common_threshold = NULL) {
  
  
  x <- dados_lob %>%
    filter(
      !is.na(
        .data[[claim_var]]
      ),
      is.finite(
        .data[[claim_var]]
      ),
      .data[[claim_var]] > 0
    ) %>%
    pull(
      .data[[claim_var]]
    )
  
  
  if (
    is.null(
      common_threshold
    )
  ) {
    
    thresholds_validos <-
      comparison_cm$threshold[
        is.finite(
          comparison_cm$threshold
        )
      ]
    
    
    if (
      length(
        thresholds_validos
      ) ==
      0
    ) {
      
      stop(
        "Nenhum threshold válido foi obtido."
      )
    }
    
    
    common_threshold <-
      min(
        thresholds_validos
      )
  }
  
  
  x_body <- x[
    x <
      common_threshold
  ]
  
  
  fits <- setNames(
    lapply(
      body_dists,
      function(dist_i) {
        
        fit_body_for_diagnostics(
          x =
            x_body,
          body_dist =
            dist_i
        )
      }
    ),
    body_dists
  )
  
  
  tabela <- bind_rows(
    lapply(
      names(fits),
      function(dist_i) {
        
        
        fit_i <-
          fits[[dist_i]]
        
        
        if (is.null(fit_i)) {
          
          return(
            data.frame(
              body_dist =
                dist_i,
              common_threshold =
                common_threshold,
              n_body =
                length(x_body),
              logLik =
                NA_real_,
              AIC =
                NA_real_,
              BIC =
                NA_real_,
              KS =
                NA_real_,
              CvM =
                NA_real_,
              AD =
                NA_real_
            )
          )
        }
        
        
        fit_obj <-
          fit_i$fit
        
        n_par <-
          length(
            fit_obj$estimate
          )
        
        loglik <-
          fit_obj$loglik
        
        
        gof <- tryCatch(
          fitdistrplus::gofstat(
            fit_obj
          ),
          error =
            function(e) NULL
        )
        
        
        data.frame(
          body_dist =
            dist_i,
          
          common_threshold =
            common_threshold,
          
          n_body =
            length(x_body),
          
          logLik =
            loglik,
          
          AIC =
            -2 * loglik +
            2 * n_par,
          
          BIC =
            -2 * loglik +
            log(
              length(x_body)
            ) *
            n_par,
          
          KS =
            if (
              is.null(gof)
            ) {
              NA_real_
            } else {
              unname(
                gof$ks
              )
            },
          
          CvM =
            if (
              is.null(gof)
            ) {
              NA_real_
            } else {
              unname(
                gof$cvm
              )
            },
          
          AD =
            if (
              is.null(gof)
            ) {
              NA_real_
            } else {
              unname(
                gof$ad
              )
            }
        )
      }
    )
  ) %>%
    arrange(AIC)
  
  
  list(
    common_threshold =
      common_threshold,
    
    x_body =
      x_body,
    
    fits =
      fits,
    
    comparison =
      tabela
  )
}


qqplot_corpo_comum <- function(
    diagnostico_corpo,
    body_dist = c(
      "lognormal",
      "gamma",
      "weibull"
    ),
    nome_lob = "") {
  
  
  body_dist <-
    match.arg(
      body_dist
    )
  
  
  fit_i <-
    diagnostico_corpo$fits[[body_dist]]
  
  
  if (is.null(fit_i)) {
    
    stop(
      "O modelo não foi ajustado com sucesso."
    )
  }
  
  
  x_body <-
    sort(
      diagnostico_corpo$x_body
    )
  
  p_emp <-
    ppoints(
      length(x_body)
    )
  
  
  estimates <-
    fit_i$fit$estimate
  
  sf <-
    fit_i$scale_factor
  
  
  q_theo_scaled <- switch(
    body_dist,
    
    
    lognormal =
      qlnorm(
        p_emp,
        meanlog =
          unname(
            estimates[
              "meanlog"
            ]
          ),
        sdlog =
          unname(
            estimates[
              "sdlog"
            ]
          )
      ),
    
    
    gamma =
      qgamma(
        p_emp,
        shape =
          unname(
            estimates[
              "shape"
            ]
          ),
        rate =
          unname(
            estimates[
              "rate"
            ]
          )
      ),
    
    
    weibull =
      qweibull(
        p_emp,
        shape =
          unname(
            estimates[
              "shape"
            ]
          ),
        scale =
          unname(
            estimates[
              "scale"
            ]
          )
      )
  )
  
  
  q_theo <-
    q_theo_scaled *
    sf
  
  
  dados_qq <-
    data.frame(
      theoretical =
        q_theo,
      empirical =
        x_body
    )
  
  
  ggplot(
    dados_qq,
    aes(
      x = theoretical,
      y = empirical
    )
  ) +
    
    geom_point(
      alpha = 0.65
    ) +
    
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype =
        "dashed"
    ) +
    
    scale_x_log10(
      labels =
        scales::comma
    ) +
    
    scale_y_log10(
      labels =
        scales::comma
    ) +
    
    labs(
      title =
        paste(
          "Body QQ plot -",
          nome_lob,
          "-",
          body_dist
        ),
      
      subtitle =
        paste(
          "Common upper limit =",
          scales::comma(
            round(
              diagnostico_corpo$
                common_threshold,
              2
            )
          )
        ),
      
      x =
        "Theoretical quantiles",
      
      y =
        "Empirical quantiles"
    ) +
    
    theme_minimal()
}


# APLICAÇÃO

set.seed(20260808) # Para tornar a aproximação Monte Carlo reproduzível


# HEALTH

cm_health_compare <-
  comparar_distribuicoes_lob(
    dados_lob =
      lob_011_Health,
    
    nome_lob =
      "Health",
    
    alpha =
      0.05,
    
    body_dists =
      c(
        "lognormal",
        "gamma",
        "weibull"
      ),
    
    detectar_duas_caudas =
      FALSE,
    
    n_draws =
      4000,
    
    restrict_upper_half =
      TRUE
  )


cm_health_compare$comparison


health_body_diagnostics <-
  comparar_ajuste_corpo_comum(
    dados_lob =
      lob_011_Health,
    
    comparison_cm =
      cm_health_compare$
      comparison
  )


health_body_diagnostics$
  comparison


qqplot_corpo_comum(
  health_body_diagnostics,
  body_dist =
    "lognormal",
  nome_lob =
    "Health"
)


qqplot_corpo_comum(
  health_body_diagnostics,
  body_dist =
    "gamma",
  nome_lob =
    "Health"
)


qqplot_corpo_comum(
  health_body_diagnostics,
  body_dist =
    "weibull",
  nome_lob =
    "Health"
)


# MTPL BODILY INJURY

cm_bodily_compare <-
  comparar_distribuicoes_lob(
    dados_lob =
      lob_041_MTPL_Bodily_Injury,
    
    nome_lob =
      "MTPL Bodily Injury",
    
    alpha =
      0.05,
    
    body_dists =
      c(
        "lognormal",
        "gamma",
        "weibull"
      ),
    
    detectar_duas_caudas =
      FALSE,
    
    n_draws =
      4000,
    
    restrict_upper_half =
      TRUE
  )


cm_bodily_compare$comparison


bodily_body_diagnostics <-
  comparar_ajuste_corpo_comum(
    dados_lob =
      lob_041_MTPL_Bodily_Injury,
    
    comparison_cm =
      cm_bodily_compare$
      comparison
  )


bodily_body_diagnostics$
  comparison


qqplot_corpo_comum(
  bodily_body_diagnostics,
  body_dist =
    "lognormal",
  nome_lob =
    "MTPL Bodily Injury"
)


qqplot_corpo_comum(
  bodily_body_diagnostics,
  body_dist =
    "gamma",
  nome_lob =
    "MTPL Bodily Injury"
)


qqplot_corpo_comum(
  bodily_body_diagnostics,
  body_dist =
    "weibull",
  nome_lob =
    "MTPL Bodily Injury"
)


# MTPL MATERIAL DAMAGE

cm_material_compare <-
  comparar_distribuicoes_lob(
    dados_lob =
      lob_042_MTPL_Material_Damage,
    
    nome_lob =
      "MTPL Material Damage",
    
    alpha =
      0.05,
    
    body_dists =
      c(
        "lognormal",
        "gamma",
        "weibull"
      ),
    
    detectar_duas_caudas =
      FALSE,
    
    n_draws =
      4000,
    
    restrict_upper_half =
      TRUE
  )


cm_material_compare$comparison


material_body_diagnostics <-
  comparar_ajuste_corpo_comum(
    dados_lob =
      lob_042_MTPL_Material_Damage,
    
    comparison_cm =
      cm_material_compare$
      comparison
  )


material_body_diagnostics$
  comparison


qqplot_corpo_comum(
  material_body_diagnostics,
  body_dist =
    "lognormal",
  nome_lob =
    "MTPL Material Damage"
)


qqplot_corpo_comum(
  material_body_diagnostics,
  body_dist =
    "gamma",
  nome_lob =
    "MTPL Material Damage"
)


qqplot_corpo_comum(
  material_body_diagnostics,
  body_dist =
    "weibull",
  nome_lob =
    "MTPL Material Damage"
)

plot_cabras_morales <- function(
    dados,
    threshold,
    nome_lob,
    claim_var = "Incurred_Claims",
    bins = 80) {
  
  dados_plot <- dados %>%
    dplyr::filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    )
  
  ggplot(
    dados_plot,
    aes(x = .data[[claim_var]])
  ) +
    
    geom_histogram(
      bins = bins,
      fill = "grey75",
      colour = "grey35",
      linewidth = 0.35
    ) +
    
    geom_vline(
      xintercept = threshold,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    
    scale_x_log10(
      labels = scales::comma
    ) +
    
    labs(
      title = nome_lob,
      subtitle = paste0(
        "u = ",
        scales::comma(
          round(threshold, 0)
        )
      ),
      x = "Incurred Claims",
      y = "Frequency"
    ) +
    
    theme_minimal(base_size = 12) +
    
    theme(
      plot.title = element_text(
        face = "plain",
        size = 14
      ),
      plot.subtitle = element_text(
        size = 12
      ),
      panel.grid.minor = element_line(
        linewidth = 0.25
      )
    )
}

p_health_cm <- plot_cabras_morales(
  dados = lob_011_Health,
  threshold = 79020.58,
  nome_lob = "Health"
)

p_health_cm


# ggsave(
#   filename = "C:/Users/fatim/Downloads/cabras_morales_011.pdf",
#   plot = p_health_cm,
#   width = 8,
#   height = 6
# )

p_bodily_cm <- plot_cabras_morales(
  dados = lob_041_MTPL_Bodily_Injury,
  threshold = 565732.99,
  nome_lob = "MTPL Bodily Injury"
)

p_bodily_cm

# ggsave(
#   filename = "C:/Users/fatim/Downloads/cabras_morales_041.pdf",
#   plot = p_bodily_cm,
#   width = 8,
#   height = 6
# )

p_material_cm <- plot_cabras_morales(
  dados = lob_042_MTPL_Material_Damage,
  threshold = 78798.49,
  nome_lob = "MTPL Material Damage"
)

p_material_cm

# ggsave(
#   filename = "C:/Users/fatim/Downloads/cabras_morales_042.pdf",
#   plot = p_material_cm,
#   width = 8,
#   height = 6
# )

###NOVO###

# ------------------------------------------------------------
# P-value preditivo leave-one-out usando T = X_(n)
# ------------------------------------------------------------

pvalue_xmax_leave_one_out <- function(x_obs, x_train, body_dist) {
  
  body_dist <- match.arg(body_dist, c("lognormal", "gamma", "weibull"))
  
  x_train <- x_train[is.finite(x_train) & x_train > 0]
  
  if (length(x_train) < 5) {
    return(NA_real_)
  }
  
  # Reescala apenas para estabilidade numérica
  scale_factor <- median(x_train)
  y_train <- x_train / scale_factor
  y_obs <- x_obs / scale_factor
  
  fit <- tryCatch(
    {
      if (body_dist == "lognormal") {
        
        fitdistrplus::fitdist(y_train, "lnorm")
        
      } else if (body_dist == "gamma") {
        
        fitdistrplus::fitdist(
          y_train,
          "gamma",
          start = list(
            shape = mean(y_train)^2 / stats::var(y_train),
            rate  = mean(y_train) / stats::var(y_train)
          )
        )
        
      } else if (body_dist == "weibull") {
        
        fitdistrplus::fitdist(y_train, "weibull")
      }
    },
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(NA_real_)
  }
  
  est <- fit$estimate
  
  # Probabilidade de cauda direita:
  # p_R = P(X_new >= x_obs | x_train)
  if (body_dist == "lognormal") {
    
    p_right <- stats::plnorm(
      q = y_obs,
      meanlog = est["meanlog"],
      sdlog   = est["sdlog"],
      lower.tail = FALSE
    )
    
  } else if (body_dist == "gamma") {
    
    p_right <- stats::pgamma(
      q = y_obs,
      shape = est["shape"],
      rate  = est["rate"],
      lower.tail = FALSE
    )
    
  } else if (body_dist == "weibull") {
    
    p_right <- stats::pweibull(
      q = y_obs,
      shape = est["shape"],
      scale = est["scale"],
      lower.tail = FALSE
    )
  }
  
  as.numeric(p_right)
}

# ------------------------------------------------------------
# Algoritmo Cabras-Morales usando T = X_(n)
# Apenas cauda direita
# ------------------------------------------------------------

cabras_morales_xmax <- function(
    x,
    alpha = 0.05,
    body_dist = c("lognormal", "gamma", "weibull"),
    min_active = 5,
    restrict_upper_half = TRUE,
    verbose = FALSE
) {
  
  body_dist <- match.arg(body_dist)
  
  x <- x[is.finite(x) & x > 0]
  x <- sort(x)
  
  n_original <- length(x)
  
  dados_work <- data.frame(
    id = seq_along(x),
    x_original = x,
    removed = FALSE,
    p_value = NA_real_,
    step = NA_integer_
  )
  
  if (restrict_upper_half) {
    minimum_allowed_rank <- floor(n_original / 2)
    maximum_right_outliers <- n_original - minimum_allowed_rank + 1
  } else {
    maximum_right_outliers <- Inf
  }
  
  step <- 1
  boundary_hit <- FALSE
  
  repeat {
    
    active_data <- dados_work %>%
      dplyr::filter(!removed)
    
    if (nrow(active_data) < min_active) {
      break
    }
    
    right_removed <- sum(dados_work$removed)
    
    if (right_removed >= maximum_right_outliers) {
      boundary_hit <- TRUE
      break
    }
    
    # Estatística do artigo para cauda direita:
    # T = X_(n), isto é, o máximo corrente
    idx_max <- active_data$id[which.max(active_data$x_original)]
    x_max <- active_data$x_original[which.max(active_data$x_original)]
    
    x_train <- active_data %>%
      dplyr::filter(id != idx_max) %>%
      dplyr::pull(x_original)
    
    p_right <- pvalue_xmax_leave_one_out(
      x_obs = x_max,
      x_train = x_train,
      body_dist = body_dist
    )
    
    if (verbose) {
      cat(
        "Step:", step,
        "| x_max:", x_max,
        "| p-value:", p_right,
        "\n"
      )
    }
    
    if (is.na(p_right) || p_right >= alpha) {
      break
    }
    
    dados_work$removed[dados_work$id == idx_max] <- TRUE
    dados_work$p_value[dados_work$id == idx_max] <- p_right
    dados_work$step[dados_work$id == idx_max] <- step
    
    step <- step + 1
  }
  
  right_outliers <- dados_work %>%
    dplyr::filter(removed) %>%
    dplyr::arrange(x_original)
  
  if (nrow(right_outliers) == 0) {
    threshold <- NA_real_
    n_excessos <- NA_integer_
    threshold_quantile <- NA_real_
  } else {
    threshold <- min(right_outliers$x_original)
    n_excessos <- sum(x > threshold)
    threshold_quantile <- mean(x <= threshold)
  }
  
  summary <- data.frame(
    n = n_original,
    alpha = alpha,
    body_dist = body_dist,
    statistic = "T = X_(n)",
    threshold = threshold,
    threshold_quantile = threshold_quantile,
    n_right_outliers = nrow(right_outliers),
    n_excessos = n_excessos,
    prop_excessos = n_excessos / n_original,
    boundary_hit = boundary_hit
  )
  
  list(
    summary = summary,
    right_outliers = right_outliers,
    full_table = dados_work
  )
}

# ------------------------------------------------------------
# Aplicar por LOB e ajustar GPD aos excessos
# ------------------------------------------------------------

cabras_morales_xmax_lob <- function(
    dados_lob,
    nome_lob,
    claim_var = "Incurred_Claims",
    alpha = 0.05,
    body_dist,
    min_excessos_gpd = 10,
    restrict_upper_half = TRUE,
    verbose = FALSE
) {
  
  x <- dados_lob %>%
    dplyr::filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    ) %>%
    dplyr::pull(.data[[claim_var]])
  
  res <- cabras_morales_xmax(
    x = x,
    alpha = alpha,
    body_dist = body_dist,
    restrict_upper_half = restrict_upper_half,
    verbose = verbose
  )
  
  resumo <- res$summary %>%
    dplyr::mutate(LOB = nome_lob) %>%
    dplyr::select(LOB, dplyr::everything())
  
  u <- resumo$threshold
  k <- resumo$n_excessos
  
  if (is.na(u) || is.na(k) || k < min_excessos_gpd) {
    
    fit_gpd <- NULL
    
    gpd_summary <- data.frame(
      LOB = nome_lob,
      body_dist = body_dist,
      statistic = "T = X_(n)",
      threshold = u,
      n_excessos = k,
      scale = NA_real_,
      shape = NA_real_,
      convergence = NA
    )
    
  } else {
    
    fit_gpd <- tryCatch(
      POT::fitgpd(
        data = x,
        threshold = u,
        est = "mle"
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit_gpd)) {
      
      gpd_summary <- data.frame(
        LOB = nome_lob,
        body_dist = body_dist,
        statistic = "T = X_(n)",
        threshold = u,
        n_excessos = k,
        scale = NA_real_,
        shape = NA_real_,
        convergence = NA
      )
      
    } else {
      
      gpd_summary <- data.frame(
        LOB = nome_lob,
        body_dist = body_dist,
        statistic = "T = X_(n)",
        threshold = u,
        n_excessos = k,
        scale = as.numeric(fit_gpd$param["scale"]),
        shape = as.numeric(fit_gpd$param["shape"]),
        convergence = fit_gpd$convergence
      )
    }
  }
  
  list(
    summary = resumo,
    right_outliers = res$right_outliers,
    full_table = res$full_table,
    fit_gpd = fit_gpd,
    gpd_summary = gpd_summary
  )
}

# ------------------------------------------------------------
# Versão final alinhada com Cabras e Morales:
# T = X_(n) para todas as distribuições
# ------------------------------------------------------------

cm_xmax_bodily <- cabras_morales_xmax_lob(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  claim_var = "Incurred_Claims",
  alpha = 0.05,
  body_dist = "lognormal"
)

cm_xmax_health <- cabras_morales_xmax_lob(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  claim_var = "Incurred_Claims",
  alpha = 0.05,
  body_dist = "gamma"
)

cm_xmax_material <- cabras_morales_xmax_lob(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  claim_var = "Incurred_Claims",
  alpha = 0.05,
  body_dist = "weibull"
)

summary_xmax <- bind_rows(
  cm_xmax_bodily$summary,
  cm_xmax_health$summary,
  cm_xmax_material$summary
)

gpd_xmax <- bind_rows(
  cm_xmax_bodily$gpd_summary,
  cm_xmax_health$gpd_summary,
  cm_xmax_material$gpd_summary
)

summary_xmax
gpd_xmax

### Dupuis 1998 OBRE ###

# Quadratura de Gauss-Legendre em [0,1] usado para calcular integrais
gauss_legendre_01 <- function(n = 200) {
  
  i <- 1:(n - 1)
  beta <- i / sqrt(4 * i^2 - 1)
  
  J <- matrix(0, n, n)
  J[cbind(i, i + 1)] <- beta
  J[cbind(i + 1, i)] <- beta
  
  eig <- eigen(J, symmetric = TRUE)
  
  x <- eig$values
  w <- 2 * eig$vectors[1, ]^2
  
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  list(
    nodes = (x + 1) / 2,
    weights = w / 2
  )
}

# Quantil da GP
qgpd_std <- function(p, sigma, xi) {
  
  if (sigma <= 0) stop("sigma must be positive")
  
  if (abs(xi) < 1e-8) {
    return(-sigma * log(1 - p))
  }
  
  sigma / xi * ((1 - p)^(-xi) - 1)
}

# Score da GP (derivadas)
gpd_score_std <- function(y, sigma, xi) {
  
  if (sigma <= 0) stop("sigma must be positive")
  if (any(1 + xi * y / sigma <= 0)) stop("Invalid GPD support")
  
  r <- y / sigma
  
  if (abs(xi) < 1e-6) {
    
    # Limite quando xi -> 0
    s_sigma <- -1 / sigma + y / sigma^2
    s_xi <- 0.5 * r^2 - r
    
  } else {
    
    z <- 1 + xi * y / sigma
    
    s_sigma <- -1 / sigma +
      ((1 + xi) * y) / (sigma * (sigma + xi * y))
    
    s_xi <- (1 / xi^2) * log(z) -
      ((1 + xi) * y) / (xi * (sigma + xi * y))
  }
  
  cbind(s_sigma, s_xi)
}

# Verificação do suporte
gpd_valid_support <- function(y, sigma, xi) {
  
  if (!is.finite(sigma) || !is.finite(xi)) return(FALSE)
  if (sigma <= 0) return(FALSE)
  if (any(1 + xi * y / sigma <= 0)) return(FALSE)
  
  TRUE
}

# Log-verosimilhança negativa da GPD usada apenas para valores iniciais MLE
gpd_nll_std <- function(par, y) {
  
  sigma <- exp(par[1])
  xi <- par[2]
  
  if (!gpd_valid_support(y, sigma, xi)) return(1e10)
  
  if (abs(xi) < 1e-8) {
    logdens <- -log(sigma) - y / sigma
  } else {
    logdens <- -log(sigma) -
      (1 + 1 / xi) * log(1 + xi * y / sigma)
  }
  
  -sum(logdens)
}

# Estimativa MLE inicial
gpd_mle_start <- function(y) {
  
  par0 <- c(log(mean(y)), 0.1)
  
  opt <- optim(
    par = par0,
    fn = gpd_nll_std,
    y = y,
    method = "Nelder-Mead",
    control = list(maxit = 5000)
  )
  
  c(
    sigma = exp(opt$par[1]),
    xi = opt$par[2]
  )
}

# Raiz quadrada inversa simétrica de matriz positiva definida
inv_sqrt_matrix <- function(M, eps = 1e-10) {
  
  eig <- eigen(M, symmetric = TRUE)
  vals <- pmax(eig$values, eps)
  
  eig$vectors %*%
    diag(1 / sqrt(vals), nrow = length(vals)) %*%
    t(eig$vectors)
}

# Cálculo de a(theta), A(theta), M1 e M2 por integração numérica sob o modelo GP
obre_model_quantities <- function(sigma, xi, c = 4,
                                  n_quad = 200,
                                  inner_tol = 1e-7,
                                  inner_max_iter = 100) {
  
  quad <- gauss_legendre_01(n_quad)
  
  p <- quad$nodes
  ww <- quad$weights
  
  yq <- qgpd_std(p, sigma, xi)
  S <- gpd_score_std(yq, sigma, xi)
  
  # Valores iniciais: a = 0 e A baseado na matriz de informação aproximada
  a <- c(0, 0)
  J <- Reduce(
    "+",
    lapply(seq_along(ww), function(i) {
      ww[i] * tcrossprod(S[i, ])
    })
  )
  
  A <- inv_sqrt_matrix(J)
  
  for (it in 1:inner_max_iter) {
    
    Sc <- sweep(S, 2, a, "-")
    Z <- Sc %*% t(A)
    norm_Z <- sqrt(rowSums(Z^2))
    
    W <- pmin(1, c / norm_Z)
    
    a_new <- colSums(S * (ww * W)) / sum(ww * W)
    
    Sc_new <- sweep(S, 2, a_new, "-")
    
    M2 <- Reduce(
      "+",
      lapply(seq_along(ww), function(i) {
        ww[i] * W[i]^2 * tcrossprod(Sc_new[i, ])
      })
    )
    
    A_new <- inv_sqrt_matrix(M2)
    
    diff <- max(abs(a_new - a)) + max(abs(A_new - A)) #ate estabilizarem
    
    a <- as.numeric(a_new)
    A <- A_new
    
    if (diff < inner_tol) break
  }
  
  Sc <- sweep(S, 2, a, "-") #centrar o score
  Z <- Sc %*% t(A) #padronizar o score
  norm_Z <- sqrt(rowSums(Z^2))
  W <- pmin(1, c / norm_Z) #pesos
  
  M1 <- Reduce(
    "+",
    lapply(seq_along(ww), function(i) {
      ww[i] * W[i] * tcrossprod(Sc[i, ])
    })
  )
  
  M2 <- Reduce(
    "+",
    lapply(seq_along(ww), function(i) {
      ww[i] * W[i]^2 * tcrossprod(Sc[i, ])
    })
  )
  
  list(
    a = a,
    A = A,
    M1 = M1,
    M2 = M2,
    inner_iterations = it
  )
}

# Pesos OBRE para uma amostra
obre_weights_sample <- function(y, sigma, xi, a, A, c = 4) {
  
  S <- gpd_score_std(y, sigma, xi)
  Sc <- sweep(S, 2, a, "-")
  
  Z <- Sc %*% t(A)
  norm_Z <- sqrt(rowSums(Z^2))
  
  W <- pmin(1, c / norm_Z)
  
  list(
    weights = W,
    norm_score = norm_Z,
    score = S,
    centered_score = Sc
  )
}

# Ajuste OBRE-GP
fit_obre_gpd_threshold_dupuis <- function(dados_lob, nome_lob, u,
                                          c = 4,
                                          tol = 1e-6,
                                          max_iter = 100,
                                          min_excessos = 10,
                                          n_quad = 200) {
  
  x <- dados_lob %>%
    filter(
      !is.na(Incurred_Claims),
      is.finite(Incurred_Claims),
      Incurred_Claims > 0
    ) %>%
    pull(Incurred_Claims)
  
  y <- x[x > u] - u
  y <- y[is.finite(y) & y > 0]
  
  k <- length(y)
  
  if (k < min_excessos) return(NULL)
  
  theta <- gpd_mle_start(y)
  sigma <- theta["sigma"]
  xi <- theta["xi"]
  
  converged <- FALSE
  
  for (iter in 1:max_iter) {
    
    if (!gpd_valid_support(y, sigma, xi)) return(NULL)
    
    q_obj <- obre_model_quantities(
      sigma = sigma,
      xi = xi,
      c = c,
      n_quad = n_quad
    )
    
    w_obj <- obre_weights_sample(
      y = y,
      sigma = sigma,
      xi = xi,
      a = q_obj$a,
      A = q_obj$A,
      c = c
    )
    
    psi_bar <- colMeans(w_obj$centered_score * w_obj$weights)
    
    delta <- tryCatch(
      solve(q_obj$M1, psi_bar),
      error = function(e) rep(NA_real_, 2)
    )
    
    if (any(!is.finite(delta))) return(NULL)
    
    # Atualização do Apêndice II:
    # theta_new = theta_old + Delta theta
    step <- 1
    accepted <- FALSE
    
    while (step > 1e-6 && !accepted) {
      
      sigma_new <- sigma + step * delta[1]
      xi_new <- xi + step * delta[2]
      
      if (gpd_valid_support(y, sigma_new, xi_new)) {
        accepted <- TRUE
      } else {
        step <- step / 2
      }
    }
    
    if (!accepted) return(NULL)
    
    rel_change <- max(
      abs(c(sigma_new - sigma, xi_new - xi)) /
        pmax(abs(c(sigma, xi)), 1e-6)
    )
    
    sigma <- sigma_new
    xi <- xi_new
    
    if (rel_change < tol) {
      converged <- TRUE
      break
    }
  }
  
  final_q <- obre_model_quantities(
    sigma = sigma,
    xi = xi,
    c = c,
    n_quad = n_quad
  )
  
  final_w <- obre_weights_sample(
    y = y,
    sigma = sigma,
    xi = xi,
    a = final_q$a,
    A = final_q$A,
    c = c
  )$weights
  
  data.frame(
    LOB = nome_lob,
    threshold = as.numeric(u),
    n_excessos = k,
    c = c,
    scale_OBRE = as.numeric(sigma),
    shape_OBRE = as.numeric(xi),
    shape_Dupuis_k = as.numeric(-xi),
    mean_weight = mean(final_w),
    min_weight = min(final_w),
    prop_weights_below_09 = mean(final_w < 0.90),
    prop_weights_below_08 = mean(final_w < 0.80),
    iterations = iter,
    converged = converged
  )
}

obre_gpd_by_quantiles_dupuis <- function(dados_lob, nome_lob,
                                         probs = seq(0.70, 0.98, by = 0.01),
                                         c = 4,
                                         min_excessos = 10,
                                         n_quad = 200) {
  
  x <- dados_lob %>%
    dplyr::filter(
      !is.na(Incurred_Claims),
      is.finite(Incurred_Claims),
      Incurred_Claims > 0
    ) %>%
    dplyr::pull(Incurred_Claims)
  
  resultados <- lapply(probs, function(p) {
    
    u <- as.numeric(stats::quantile(x, probs = p, na.rm = TRUE))
    
    res <- fit_obre_gpd_threshold_dupuis(
      dados_lob = dados_lob,
      nome_lob = nome_lob,
      u = u,
      c = c,
      min_excessos = min_excessos,
      n_quad = n_quad
    )
    
    if (is.null(res)) return(NULL)
    
    res$prob_threshold <- p
    res
  })
  
  res_final <- dplyr::bind_rows(resultados)
  
  res_final <- res_final[, c(
    "LOB",
    "prob_threshold",
    "threshold",
    "n_excessos",
    "c",
    "scale_OBRE",
    "shape_OBRE",
    "shape_Dupuis_k",
    "mean_weight",
    "min_weight",
    "prop_weights_below_09",
    "prop_weights_below_08",
    "iterations",
    "converged"
  )]
  
  return(res_final)
}

#analise de c
obre_041_2 <- obre_gpd_by_quantiles_dupuis(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  probs = seq(0.80, 0.98, by = 0.005),
  c = 3,
  min_excessos = 10,
  n_quad = 200
)

#obre_041_2

obre_041_3 <- obre_gpd_by_quantiles_dupuis(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  probs = seq(0.8, 0.98, by = 0.005),
  c = 5,
  min_excessos = 10,
  n_quad = 200
)

#obre_041_3

#fica c=4

obre_041 <- obre_gpd_by_quantiles_dupuis(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  probs = seq(0.8, 0.98, by = 0.005),
  c = 4,
  min_excessos = 10,
  n_quad = 200
)

obre_041

# write.csv(
#   obre_041,
#   file = "obre_041.csv",
#   row.names = FALSE
# )

obre_011<- obre_gpd_by_quantiles_dupuis(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  probs = seq(0.80, 0.98, by = 0.005),
  c = 4,
  min_excessos = 10,
  n_quad = 200
)

obre_011

# write.csv(
#   obre_011,
#   file = "obre_011.csv",
#   row.names = FALSE
# )

obre_042 <- obre_gpd_by_quantiles_dupuis(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  probs = seq(0.8, 0.98, by = 0.005),
  c = 4,
  min_excessos = 10,
  n_quad = 200
)

obre_042

#View(obre_042)

# write.csv(
#   obre_042,
#   file = "obre_042.csv",
#   row.names = FALSE
# )

#graficos
obre_all <- bind_rows(
  obre_041 %>% mutate(LOB = "MTPL Bodily Injury"),
  obre_011 %>% mutate(LOB = "Health"),
  obre_042 %>% mutate(LOB = "MTPL Material Damage")
)

obre_shape = ggplot(obre_all, aes(x = prob_threshold, y = shape_OBRE,
                     group = LOB, color = LOB, linetype = LOB)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "OBRE shape estimates by LOB",
    x = "Threshold quantile",
    y = expression(hat(xi)[OBRE]),
    color = "LOB",
    linetype = "LOB"
  ) +
  theme_minimal()

obre_shape

# ggsave(
#   filename = "C:/Users/fatim/Downloads/obre_shape.pdf",
#   plot = obre_shape,
#   width = 8,
#   height = 6
# )

obre_pesos= ggplot(obre_all, aes(x = prob_threshold, y = mean_weight,
                     group = LOB, color = LOB, linetype = LOB)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(
    title = "Average OBRE weights by LOB",
    x = "Threshold quantile",
    y = expression(bar(w)),
    color = "LOB",
    linetype = "LOB"
  ) +
  theme_minimal()

obre_pesos

# ggsave(
#   filename = "C:/Users/fatim/Downloads/obre_pesos.pdf",
#   plot = obre_pesos,
#   width = 8,
#   height = 6
# )

obre_90= ggplot(obre_all, aes(x = prob_threshold, y = prop_weights_below_09,
                     group = LOB, color = LOB, linetype = LOB)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Proportion of OBRE weights below 0.90 by LOB",
    x = "Threshold quantile",
    y = expression(P(w < 0.90)),
    color = "LOB",
    linetype = "LOB"
  ) +
  theme_minimal()

obre_90

# ggsave(
#   filename = "C:/Users/fatim/Downloads/obre_90.pdf",
#   plot = obre_90,
#   width = 8,
#   height = 6
# )

#descriçao do metodo para um threshold
trace_obre_threshold <- function(dados_lob, u,
                                 c = 4,
                                 tol = 1e-6,
                                 max_iter = 100,
                                 n_quad = 200) {
  
  x <- dados_lob %>%
    dplyr::filter(
      !is.na(Incurred_Claims),
      is.finite(Incurred_Claims),
      Incurred_Claims > 0
    ) %>%
    dplyr::pull(Incurred_Claims)
  
  y <- x[x > u] - u
  y <- y[is.finite(y) & y > 0]
  
  theta <- gpd_mle_start(y)
  
  sigma <- as.numeric(theta["sigma"])
  xi <- as.numeric(theta["xi"])
  
  historico <- list()
  
  for (iter in 1:max_iter) {
    
    q_obj <- obre_model_quantities(
      sigma = sigma,
      xi = xi,
      c = c,
      n_quad = n_quad
    )
    
    w_obj <- obre_weights_sample(
      y = y,
      sigma = sigma,
      xi = xi,
      a = q_obj$a,
      A = q_obj$A,
      c = c
    )
    
    psi_bar <- colMeans(
      w_obj$centered_score * w_obj$weights
    )
    
    delta <- solve(q_obj$M1, psi_bar)
    
    step <- 1
    
    repeat {
      
      sigma_new <- sigma + step * delta[1]
      xi_new <- xi + step * delta[2]
      
      if (gpd_valid_support(y, sigma_new, xi_new)) {
        break
      }
      
      step <- step / 2
      
      if (step < 1e-6) {
        stop("Não foi possível obter uma atualização válida.")
      }
    }
    
    rel_change <- max(
      abs(c(sigma_new - sigma, xi_new - xi)) /
        pmax(abs(c(sigma, xi)), 1e-6)
    )
    
    historico[[iter]] <- list(
      iteracao = iter,
      sigma = sigma,
      xi = xi,
      a = q_obj$a,
      A = q_obj$A,
      M1 = q_obj$M1,
      M2 = q_obj$M2,
      psi_bar = psi_bar,
      delta = delta,
      step = step,
      sigma_new = sigma_new,
      xi_new = xi_new,
      mean_weight = mean(w_obj$weights),
      min_weight = min(w_obj$weights),
      rel_change = rel_change
    )
    
    sigma <- sigma_new
    xi <- xi_new
    
    if (rel_change < tol) break
  }
  
  historico
}

u_095 <- as.numeric(
  quantile(
    lob_042_MTPL_Material_Damage$Incurred_Claims,
    probs = 0.95,
    na.rm = TRUE
  )
)

u_098 <- as.numeric(
  quantile(
    lob_042_MTPL_Material_Damage$Incurred_Claims,
    probs = 0.98,
    na.rm = TRUE
  )
)

trace_095 <- trace_obre_threshold(
  dados_lob = lob_042_MTPL_Material_Damage,
  u = u_095,
  c = 4
)

trace_098 <- trace_obre_threshold(
  dados_lob = lob_042_MTPL_Material_Damage,
  u = u_098,
  c = 4
)

trace_095[[1]]

trace_to_table <- function(trace_obj) {
  
  dplyr::bind_rows(
    lapply(trace_obj, function(z) {
      data.frame(
        iteration = z$iteracao,
        sigma_old = z$sigma,
        xi_old = z$xi,
        delta_sigma = z$delta[1],
        delta_xi = z$delta[2],
        sigma_new = z$sigma_new,
        xi_new = z$xi_new,
        mean_weight = z$mean_weight,
        min_weight = z$min_weight,
        relative_change = z$rel_change
      )
    })
  )
}

tabela_095 <- trace_to_table(trace_095)
tabela_098 <- trace_to_table(trace_098)

tabela_095
tabela_098

############################################################
### SIMULAÇÃO DE CALIBRAÇÃO DOS PESOS — DUPUIS (1998)
############################################################


# ==========================================================
# 1. Gerador de observações de uma distribuição GP
# ==========================================================

rgpd_std <- function(n, sigma = 1, xi) {
  
  if (sigma <= 0) {
    stop("sigma must be positive")
  }
  
  u <- runif(n)
  
  qgpd_std(
    p = u,
    sigma = sigma,
    xi = xi
  )
}


# ==========================================================
# 2. Ajuste OBRE diretamente a uma amostra de excessos
#
# Esta função é equivalente ao teu
# fit_obre_gpd_threshold_dupuis(), mas recebe diretamente y.
# É necessária porque, nas simulações, já temos os excessos.
# ==========================================================

fit_obre_gpd_excesses <- function(y,
                                  c = 4,
                                  tol = 1e-6,
                                  max_iter = 100,
                                  n_quad = 200) {
  
  y <- y[
    is.finite(y) &
      y > 0
  ]
  
  k <- length(y)
  
  if (k < 10) {
    return(NULL)
  }
  
  
  # --------------------------------------------------------
  # Valores iniciais por MLE
  # --------------------------------------------------------
  
  theta <- tryCatch(
    gpd_mle_start(y),
    error = function(e) NULL
  )
  
  if (is.null(theta)) {
    return(NULL)
  }
  
  sigma <- as.numeric(theta["sigma"])
  xi <- as.numeric(theta["xi"])
  
  if (!gpd_valid_support(y, sigma, xi)) {
    return(NULL)
  }
  
  converged <- FALSE
  
  
  # --------------------------------------------------------
  # Iteração exterior do OBRE
  # --------------------------------------------------------
  
  for (iter in 1:max_iter) {
    
    if (!gpd_valid_support(y, sigma, xi)) {
      return(NULL)
    }
    
    
    # a(theta), A(theta), M1 e M2
    q_obj <- tryCatch(
      obre_model_quantities(
        sigma = sigma,
        xi = xi,
        c = c,
        n_quad = n_quad
      ),
      error = function(e) NULL
    )
    
    if (is.null(q_obj)) {
      return(NULL)
    }
    
    
    # Pesos para a amostra atual
    w_obj <- tryCatch(
      obre_weights_sample(
        y = y,
        sigma = sigma,
        xi = xi,
        a = q_obj$a,
        A = q_obj$A,
        c = c
      ),
      error = function(e) NULL
    )
    
    if (is.null(w_obj)) {
      return(NULL)
    }
    
    
    # Média da função de estimação
    psi_bar <- colMeans(
      w_obj$centered_score *
        w_obj$weights
    )
    
    
    # Correção dos parâmetros
    delta <- tryCatch(
      solve(
        q_obj$M1,
        psi_bar
      ),
      error = function(e) {
        rep(NA_real_, 2)
      }
    )
    
    if (any(!is.finite(delta))) {
      return(NULL)
    }
    
    
    # ------------------------------------------------------
    # Atualização dos parâmetros com redução do passo
    # caso o suporte da GP seja violado
    # ------------------------------------------------------
    
    step <- 1
    accepted <- FALSE
    
    while (
      step > 1e-6 &&
      !accepted
    ) {
      
      sigma_new <- sigma +
        step * delta[1]
      
      xi_new <- xi +
        step * delta[2]
      
      
      if (
        gpd_valid_support(
          y,
          sigma_new,
          xi_new
        )
      ) {
        
        accepted <- TRUE
        
      } else {
        
        step <- step / 2
      }
    }
    
    
    if (!accepted) {
      return(NULL)
    }
    
    
    # Critério de convergência
    rel_change <- max(
      abs(
        c(
          sigma_new - sigma,
          xi_new - xi
        )
      ) /
        pmax(
          abs(
            c(
              sigma,
              xi
            )
          ),
          1e-6
        )
    )
    
    
    sigma <- sigma_new
    xi <- xi_new
    
    
    if (rel_change < tol) {
      
      converged <- TRUE
      break
    }
  }
  
  
  # --------------------------------------------------------
  # Quantidades finais
  # --------------------------------------------------------
  
  final_q <- tryCatch(
    obre_model_quantities(
      sigma = sigma,
      xi = xi,
      c = c,
      n_quad = n_quad
    ),
    error = function(e) NULL
  )
  
  if (is.null(final_q)) {
    return(NULL)
  }
  
  
  final_w_obj <- tryCatch(
    obre_weights_sample(
      y = y,
      sigma = sigma,
      xi = xi,
      a = final_q$a,
      A = final_q$A,
      c = c
    ),
    error = function(e) NULL
  )
  
  if (is.null(final_w_obj)) {
    return(NULL)
  }
  
  
  list(
    sigma = sigma,
    xi = xi,
    y = y,
    weights = final_w_obj$weights,
    a = final_q$a,
    A = final_q$A,
    M1 = final_q$M1,
    M2 = final_q$M2,
    converged = converged,
    iterations = iter
  )
}


# ==========================================================
# 3. Calibração dos pesos para UM threshold
#
# Procedimento:
#
#   1. ajusta OBRE aos excessos observados;
#   2. obtém xi_hat;
#   3. simula B amostras GP com:
#          n = número de excessos
#          sigma = 1
#          xi = xi_hat
#   4. ajusta OBRE em cada amostra simulada;
#   5. compara os pesos observados com os pesos simulados
#      associados à mesma posição (rank).
# ==========================================================

dupuis_weight_calibration <- function(y,
                                      c = 4,
                                      B = 200,
                                      n_quad = 200,
                                      tol = 1e-6,
                                      max_iter = 100,
                                      seed = 123,
                                      verbose = TRUE) {
  
  set.seed(seed)
  
  
  y <- y[
    is.finite(y) &
      y > 0
  ]
  
  k <- length(y)
  
  
  if (k < 10) {
    stop("Too few excesses.")
  }
  
  
  # --------------------------------------------------------
  # Ajuste OBRE aos dados observados
  # --------------------------------------------------------
  
  fit_obs <- fit_obre_gpd_excesses(
    y = y,
    c = c,
    tol = tol,
    max_iter = max_iter,
    n_quad = n_quad
  )
  
  
  if (is.null(fit_obs)) {
    stop("OBRE fit failed for observed data.")
  }
  
  
  sigma_hat <- fit_obs$sigma
  xi_hat <- fit_obs$xi
  
  
  # --------------------------------------------------------
  # Ordenar os excessos observados
  #
  # Isto é importante:
  # os pesos são comparados segundo o rank da observação,
  # como nas tabelas de Dupuis.
  # --------------------------------------------------------
  
  ord_obs <- order(y)
  
  y_ord <- y[ord_obs]
  w_obs <- fit_obs$weights[ord_obs]
  
  
  # --------------------------------------------------------
  # Matriz onde serão guardados os pesos simulados
  #
  # linha = simulação
  # coluna = rank da observação
  # --------------------------------------------------------
  
  W_sim <- matrix(
    NA_real_,
    nrow = B,
    ncol = k
  )
  
  
  # --------------------------------------------------------
  # Simulações
  # --------------------------------------------------------
  
  b_success <- 0
  attempts <- 0
  
  max_attempts <- 5 * B
  
  
  while (
    b_success < B &&
    attempts < max_attempts
  ) {
    
    attempts <- attempts + 1
    
    
    # ------------------------------------------------------
    # Dupuis fixa sigma = 1 devido à invariância de escala
    # ------------------------------------------------------
    
    y_sim <- rgpd_std(
      n = k,
      sigma = 1,
      xi = xi_hat
    )
    
    
    # ------------------------------------------------------
    # Ajustar novamente OBRE à amostra simulada
    # ------------------------------------------------------
    
    fit_sim <- tryCatch(
      fit_obre_gpd_excesses(
        y = y_sim,
        c = c,
        tol = tol,
        max_iter = max_iter,
        n_quad = n_quad
      ),
      error = function(e) NULL
    )
    
    
    if (is.null(fit_sim)) {
      next
    }
    
    
    if (!fit_sim$converged) {
      next
    }
    
    
    # ------------------------------------------------------
    # Ordenar a amostra simulada e os respetivos pesos
    # ------------------------------------------------------
    
    ord_sim <- order(y_sim)
    
    weights_sim_ordered <-
      fit_sim$weights[ord_sim]
    
    
    b_success <- b_success + 1
    
    W_sim[
      b_success,
    ] <- weights_sim_ordered
    
    
    if (
      verbose &&
      (
        b_success %% 25 == 0 ||
        b_success == B
      )
    ) {
      
      cat(
        "Successful simulations:",
        b_success,
        "/",
        B,
        "\n"
      )
    }
  }
  
  
  # --------------------------------------------------------
  # Verificar quantas simulações foram concluídas
  # --------------------------------------------------------
  
  if (b_success == 0) {
    stop("No successful simulations.")
  }
  
  
  W_sim <- W_sim[
    seq_len(b_success),
    ,
    drop = FALSE
  ]
  
  
  if (
    b_success < B &&
    verbose
  ) {
    
    warning(
      paste(
        "Only",
        b_success,
        "successful simulations out of",
        B
      )
    )
  }
  
  
  # --------------------------------------------------------
  # Distribuição dos pesos simulados por rank
  # --------------------------------------------------------
  
  sim_mean <- colMeans(
    W_sim,
    na.rm = TRUE
  )
  
  
  sim_sd <- apply(
    W_sim,
    2,
    sd,
    na.rm = TRUE
  )
  
  
  sim_q025 <- apply(
    W_sim,
    2,
    quantile,
    probs = 0.025,
    na.rm = TRUE
  )
  
  
  sim_q05 <- apply(
    W_sim,
    2,
    quantile,
    probs = 0.05,
    na.rm = TRUE
  )
  
  
  sim_q50 <- apply(
    W_sim,
    2,
    quantile,
    probs = 0.50,
    na.rm = TRUE
  )
  
  
  sim_q95 <- apply(
    W_sim,
    2,
    quantile,
    probs = 0.95,
    na.rm = TRUE
  )
  
  
  sim_q975 <- apply(
    W_sim,
    2,
    quantile,
    probs = 0.975,
    na.rm = TRUE
  )
  
  
  # --------------------------------------------------------
  # p-value empírico
  #
  # Probabilidade, sob a GP, de obter um peso
  # igual ou inferior ao peso observado naquele rank.
  #
  # p pequeno:
  # downweighting observado é mais severo do que seria
  # normalmente esperado sob a GP.
  # --------------------------------------------------------
  
  empirical_p_value <- sapply(
    seq_len(k),
    function(j) {
      
      mean(
        W_sim[, j] <=
          w_obs[j],
        na.rm = TRUE
      )
    }
  )
  
  
  # --------------------------------------------------------
  # Tabela completa
  # --------------------------------------------------------
  
  calibration_table <- data.frame(
    
    rank = seq_len(k),
    
    excess = y_ord,
    
    observed_weight = w_obs,
    
    simulated_mean_weight = sim_mean,
    
    simulated_sd_weight = sim_sd,
    
    simulated_q025 = sim_q025,
    
    simulated_q05 = sim_q05,
    
    simulated_median = sim_q50,
    
    simulated_q95 = sim_q95,
    
    simulated_q975 = sim_q975,
    
    empirical_p_value = empirical_p_value
  )
  
  
  # --------------------------------------------------------
  # Tabela apenas das observações downweighted
  #
  # Equivalente à lógica das tabelas de Dupuis:
  # mostrar sobretudo os pesos diferentes de 1.
  # --------------------------------------------------------
  
  downweighted_table <-
    calibration_table[
      calibration_table$observed_weight <
        (1 - 1e-10),
      ,
      drop = FALSE
    ]
  
  
  # --------------------------------------------------------
  # Resultado
  # --------------------------------------------------------
  
  list(
    
    n_excesses = k,
    
    sigma_OBRE = sigma_hat,
    
    xi_OBRE = xi_hat,
    
    c = c,
    
    B_requested = B,
    
    B_successful = b_success,
    
    observed_fit = fit_obs,
    
    calibration_table = calibration_table,
    
    downweighted_table = downweighted_table,
    
    simulated_weights = W_sim
  )
}



# ==========================================================
# 4. Aplicação da calibração a VÁRIOS thresholds
#
# Esta função permite analisar automaticamente vários
# quantis candidatos para uma LOB.
# ==========================================================

dupuis_calibrate_threshold_grid <- function(dados_lob,
                                            nome_lob,
                                            probs,
                                            c = 4,
                                            B = 200,
                                            n_quad = 200,
                                            tol = 1e-6,
                                            max_iter = 100,
                                            min_excessos = 10,
                                            seed = 123,
                                            verbose = TRUE) {
  
  x <- dados_lob %>%
    dplyr::filter(
      !is.na(Incurred_Claims),
      is.finite(Incurred_Claims),
      Incurred_Claims > 0
    ) %>%
    dplyr::pull(Incurred_Claims)
  
  
  detalhes <- list()
  resumos <- list()
  
  
  for (j in seq_along(probs)) {
    
    p <- probs[j]
    
    
    u <- as.numeric(
      stats::quantile(
        x,
        probs = p,
        na.rm = TRUE
      )
    )
    
    
    y <- x[x > u] - u
    
    y <- y[
      is.finite(y) &
        y > 0
    ]
    
    
    k <- length(y)
    
    
    if (k < min_excessos) {
      next
    }
    
    
    if (verbose) {
      
      cat(
        "\n-----------------------------------------\n",
        "LOB:", nome_lob, "\n",
        "Threshold quantile:", p, "\n",
        "Threshold:", u, "\n",
        "Number of excesses:", k, "\n",
        "-----------------------------------------\n"
      )
    }
    
    
    cal <- tryCatch(
      dupuis_weight_calibration(
        y = y,
        c = c,
        B = B,
        n_quad = n_quad,
        tol = tol,
        max_iter = max_iter,
        seed = seed + j,
        verbose = verbose
      ),
      error = function(e) {
        
        if (verbose) {
          message(
            "Calibration failed for q = ",
            p,
            ": ",
            e$message
          )
        }
        
        NULL
      }
    )
    
    
    if (is.null(cal)) {
      next
    }
    
    
    # ------------------------------------------------------
    # Guardar detalhe da calibração
    # ------------------------------------------------------
    
    detalhes[[as.character(p)]] <- cal
    
    
    down_tab <- cal$downweighted_table
    
    
    # ------------------------------------------------------
    # Resumo do threshold
    #
    # Atenção:
    # estes valores são diagnósticos.
    # Não existe em Dupuis uma regra universal baseada
    # num único cut-off.
    # ------------------------------------------------------
    
    n_downweighted <- nrow(down_tab)
    
    
    if (n_downweighted > 0) {
      
      min_p_value <-
        min(
          down_tab$empirical_p_value,
          na.rm = TRUE
        )
      
      n_p_below_005 <-
        sum(
          down_tab$empirical_p_value <
            0.05,
          na.rm = TRUE
        )
      
      n_p_below_010 <-
        sum(
          down_tab$empirical_p_value <
            0.10,
          na.rm = TRUE
        )
      
    } else {
      
      min_p_value <- NA_real_
      n_p_below_005 <- 0
      n_p_below_010 <- 0
    }
    
    
    resumos[[length(resumos) + 1]] <-
      data.frame(
        
        LOB = nome_lob,
        
        prob_threshold = p,
        
        threshold = u,
        
        n_excesses = k,
        
        c = c,
        
        scale_OBRE =
          cal$sigma_OBRE,
        
        shape_OBRE =
          cal$xi_OBRE,
        
        mean_weight =
          mean(
            cal$observed_fit$weights
          ),
        
        min_weight =
          min(
            cal$observed_fit$weights
          ),
        
        n_downweighted =
          n_downweighted,
        
        min_empirical_p_value =
          min_p_value,
        
        n_p_below_005 =
          n_p_below_005,
        
        n_p_below_010 =
          n_p_below_010,
        
        B_successful =
          cal$B_successful
      )
  }
  
  
  summary_table <-
    dplyr::bind_rows(
      resumos
    )
  
  
  list(
    summary = summary_table,
    details = detalhes
  )
}


calib_041 <- dupuis_calibrate_threshold_grid(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  
  probs = c(
    0.50,
    0.60,
    0.70,
    0.80,
    seq(0.85, 0.98, by = 0.005)
  ),
  
  c = 4,
  B = 200,
  n_quad = 200,
  seed = 123,
  verbose = TRUE
)

calib_041$summary

calib_011 <- dupuis_calibrate_threshold_grid(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  
  probs = seq(0.85, 0.98, by = 0.005),
  
  c = 4,
  B = 200,
  n_quad = 200,
  seed = 123,
  verbose = TRUE
)

calib_011$summary

calib_042 <- dupuis_calibrate_threshold_grid(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  
  probs = seq(0.85, 0.98, by = 0.005),
  
  c = 4,
  B = 200,
  n_quad = 200,
  seed = 123,
  verbose = TRUE
)

calib_042$summary


calib_all <- dplyr::bind_rows(
  calib_041$summary,
  calib_042$summary,
  calib_011$summary
)

calib_all

ggplot(
  calib_all,
  aes(
    x = prob_threshold,
    y = min_empirical_p_value,
    color = LOB,
    group = LOB
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(
    yintercept = 0.05,
    linetype = "dashed"
  ) +
  labs(
    x = "Threshold quantile",
    y = "Minimum empirical p-value",
    color = "LOB"
  ) +
  theme_minimal()

ggplot(
  calib_all,
  aes(
    x = prob_threshold,
    y = n_p_below_005,
    color = LOB,
    group = LOB
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    x = "Threshold quantile",
    y = "Number of unusually low OBRE weights",
    color = "LOB"
  ) +
  theme_minimal()

calib_all_plot <- calib_all %>%
  dplyr::filter(prob_threshold >= 0.85)

obre_calibration=ggplot(
  calib_all_plot,
  aes(
    x = prob_threshold,
    y = n_p_below_005,
    group = LOB,
    color = LOB
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq(0.85, 0.98, by = 0.02)
  ) +
  labs(
    x = "Threshold quantile",
    y = "Number of unusually low OBRE weights",
    color = "LOB"
  ) +
  theme_minimal()

obre_calibration

# ggsave(
#   filename = "C:/Users/fatim/Downloads/obre_calibracao.pdf",
#   plot = obre_calibration,
#   width = 8,
#   height = 6,
#   device = "pdf"
# )

#Northrop and Coleman
source("C:/Users/fatim/Downloads/NorthropColeman2014.fns")

# MTPL Bodily Injury
probs_041 <- seq(0.80, 0.98, by = 0.005)

u_041 <- as.numeric(quantile(
  lob_041_MTPL_Bodily_Injury$Incurred_Claims,
  probs = probs_041,
  na.rm = TRUE
))

res_nc_041_oficial <- score.fitrange(
  raw.data = lob_041_MTPL_Bodily_Injury$Incurred_Claims,
  u = u_041,
  GP.fit = "Grimshaw",
  do.LRT = TRUE
)

res_041_tabela <- data.frame(
  prob_threshold = probs_041[-length(probs_041)],
  threshold = res_nc_041_oficial$u,
  n_excessos = res_nc_041_oficial$nexc[1:length(res_nc_041_oficial$u)],
  m = res_nc_041_oficial$df + 1,
  LR = res_nc_041_oficial$LRT.test.stats,
  p_value_LR = res_nc_041_oficial$LRT.p.values,
  Score = res_nc_041_oficial$e.test.stats,
  p_value_Score = res_nc_041_oficial$e.p.values
)

res_041_tabela

# write.csv(
#   res_041_tabela,
#   file = "northrop_041.csv",
#   row.names = FALSE
# )

plot(
  res_041_tabela$threshold,
  res_041_tabela$p_value_LR,
  type = "b",
  pch = 16,
  col = "blue",
  lty = 1,
  ylim = c(0, 1),
  xaxt = "n",
  xlab = "u",
  ylab = "p-value",
  main = "Northrop-Coleman test - MTPL Bodily Injury"
)

axis(
  1,
  at = axTicks(1),
  labels = scales::comma(axTicks(1))
)

lines(
  res_041_tabela$threshold,
  res_041_tabela$p_value_Score,
  type = "b",
  pch = 17,
  col = "red",
  lty = 2
)

abline(h = 0.05, lty = 3, col = "gray40")

legend(
  "topleft",
  legend = c("LR test", "Score test", "alpha = 0.05"),
  col = c("blue", "red", "gray40"),
  lty = c(1, 2, 3),
  pch = c(16, 17, NA),
  bty = "n"
)

# pdf(
#   file = "C:/Users/fatim/Downloads/northrop_coleman_041.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot(
#   res_041_tabela$threshold,
#   res_041_tabela$p_value_LR,
#   type = "b",
#   pch = 16,
#   col = "blue",
#   lty = 1,
#   ylim = c(0, 1),
#   xaxt = "n",
#   xlab = "u",
#   ylab = "p-value",
#   main = "Northrop-Coleman test - MTPL Bodily Injury"
# )
# 
# axis(
#   1,
#   at = axTicks(1),
#   labels = scales::comma(axTicks(1))
# )
# 
# lines(
#   res_041_tabela$threshold,
#   res_041_tabela$p_value_Score,
#   type = "b",
#   pch = 17,
#   col = "red",
#   lty = 2
# )
# 
# abline(h = 0.05, lty = 3, col = "gray40")
# 
# legend(
#   "topleft",
#   legend = c("LR test", "Score test", "alpha = 0.05"),
#   col = c("blue", "red", "gray40"),
#   lty = c(1, 2, 3),
#   pch = c(16, 17, NA),
#   bty = "n"
# )
# 
# dev.off()


#health
u_011 <- as.numeric(quantile(
  lob_011_Health$Incurred_Claims,
  probs = probs_041,
  na.rm = TRUE
))

res_nc_011_oficial <- score.fitrange(
  raw.data = lob_011_Health$Incurred_Claims,
  u = u_011,
  GP.fit = "Grimshaw",
  do.LRT = TRUE
)

res_011_tabela <- data.frame(
  prob_threshold = probs_041[-length(probs_041)],
  threshold = res_nc_011_oficial$u,
  n_excessos = res_nc_011_oficial$nexc[1:length(res_nc_011_oficial$u)],
  m = res_nc_011_oficial$df + 1,
  LR = res_nc_011_oficial$LRT.test.stats,
  p_value_LR = res_nc_011_oficial$LRT.p.values,
  Score = res_nc_011_oficial$e.test.stats,
  p_value_Score = res_nc_011_oficial$e.p.values
)

res_011_tabela

# write.csv(
#   res_011_tabela,
#   file = "northrop_011.csv",
#   row.names = FALSE
# )

plot(
  res_011_tabela$threshold,
  res_011_tabela$p_value_LR,
  type = "b",
  pch = 16,
  col = "blue",
  lty = 1,
  ylim = c(0, 1),
  xaxt = "n",
  xlab = "u",
  ylab = "p-value",
  main = "Northrop-Coleman test - Health"
)

axis(
  1,
  at = axTicks(1),
  labels = scales::comma(axTicks(1))
)

lines(
  res_011_tabela$threshold,
  res_011_tabela$p_value_Score,
  type = "b",
  pch = 17,
  col = "red",
  lty = 2
)

abline(h = 0.05, lty = 3, col = "gray40")

legend(
  "topleft",
  legend = c("LR test", "Score test", "alpha = 0.05"),
  col = c("blue", "red", "gray40"),
  lty = c(1, 2, 3),
  pch = c(16, 17, NA),
  bty = "n"
)

# pdf(
#   file = "C:/Users/fatim/Downloads/northrop_coleman_011.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot(
#   res_011_tabela$threshold,
#   res_011_tabela$p_value_LR,
#   type = "b",
#   pch = 16,
#   col = "blue",
#   lty = 1,
#   ylim = c(0, 1),
#   xaxt = "n",
#   xlab = "u",
#   ylab = "p-value",
#   main = "Northrop-Coleman test - Health"
# )
# 
# axis(
#   1,
#   at = axTicks(1),
#   labels = scales::comma(axTicks(1))
# )
# 
# lines(
#   res_011_tabela$threshold,
#   res_011_tabela$p_value_Score,
#   type = "b",
#   pch = 17,
#   col = "red",
#   lty = 2
# )
# 
# abline(h = 0.05, lty = 3, col = "gray40")
# 
# legend(
#   "topleft",
#   legend = c("LR test", "Score test", "alpha = 0.05"),
#   col = c("blue", "red", "gray40"),
#   lty = c(1, 2, 3),
#   pch = c(16, 17, NA),
#   bty = "n"
# )
# 
# dev.off()

#material damage
u_042 <- as.numeric(quantile(
  lob_042_MTPL_Material_Damage$Incurred_Claims,
  probs = probs_041,
  na.rm = TRUE
))

res_nc_042_oficial <- score.fitrange(
  raw.data = lob_042_MTPL_Material_Damage$Incurred_Claims,
  u = u_042,
  GP.fit = "Grimshaw",
  do.LRT = TRUE
)

res_042_tabela <- data.frame(
  prob_threshold = probs_041[-length(probs_041)],
  threshold = res_nc_042_oficial$u,
  n_excessos = res_nc_042_oficial$nexc[1:length(res_nc_042_oficial$u)],
  m = res_nc_042_oficial$df + 1,
  LR = res_nc_042_oficial$LRT.test.stats,
  p_value_LR = res_nc_042_oficial$LRT.p.values,
  Score = res_nc_042_oficial$e.test.stats,
  p_value_Score = res_nc_042_oficial$e.p.values
)

res_042_tabela

# write.csv(
#   res_042_tabela,
#   file = "northrop_042.csv",
#   row.names = FALSE
# )

plot(
  res_042_tabela$threshold,
  res_042_tabela$p_value_LR,
  type = "b",
  pch = 16,
  col = "blue",
  lty = 1,
  ylim = c(0, 1),
  xaxt = "n",
  xlab = "u",
  ylab = "p-value",
  main = "Northrop-Coleman test - MTPL Material Damage"
)

axis(
  1,
  at = axTicks(1),
  labels = scales::comma(axTicks(1))
)

lines(
  res_042_tabela$threshold,
  res_042_tabela$p_value_Score,
  type = "b",
  pch = 17,
  col = "red",
  lty = 2
)

abline(h = 0.05, lty = 3, col = "gray40")

legend(
  "topleft",
  legend = c("LR test", "Score test", "alpha = 0.05"),
  col = c("blue", "red", "gray40"),
  lty = c(1, 2, 3),
  pch = c(16, 17, NA),
  bty = "n"
)

# pdf(
#   file = "C:/Users/fatim/Downloads/northrop_coleman_042.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot(
#   res_042_tabela$threshold,
#   res_042_tabela$p_value_LR,
#   type = "b",
#   pch = 16,
#   col = "blue",
#   lty = 1,
#   ylim = c(0, 1),
#   xaxt = "n",
#   xlab = "u",
#   ylab = "p-value",
#   main = "Northrop-Coleman test - MTPL Material Damage"
# )
# 
# axis(
#   1,
#   at = axTicks(1),
#   labels = scales::comma(axTicks(1))
# )
# 
# lines(
#   res_042_tabela$threshold,
#   res_042_tabela$p_value_Score,
#   type = "b",
#   pch = 17,
#   col = "red",
#   lty = 2
# )
# 
# abline(h = 0.05, lty = 3, col = "gray40")
# 
# legend(
#   "topleft",
#   legend = c("LR test", "Score test", "alpha = 0.05"),
#   col = c("blue", "red", "gray40"),
#   lty = c(1, 2, 3),
#   pch = c(16, 17, NA),
#   bty = "n"
# )
# 
# dev.off()

#wadsworth 2016
source("C:/Users/fatim/Downloads/JointMLEFunctions.r")

#Problema codigo autores
#os sinistros têm valores muito grandes;
#a integração numérica da matriz de informação podia falhar;
#algumas grelhas de thresholds davam matrizes de covariância instáveis ou não positivas definidas.

#Usei o código original do autor, mas precisei de criar uma versão mais robusta porque, com os meus dados, algumas integrações e matrizes de covariância falhavam

#tenta calcular a integração; se der erro, em vez de rebentar o código imediatamente, devolve NA
#E isto pode acontecer facilmente em dados reais quando: ξ está perto de zero, quando o limite superior é infinito, 
#quando os parâmetros estimados são instáveis, ou quando os thresholds são muito altos e há poucos excessos.
E.Info.Mat <- function(theta, u, M) {
  
  if (theta[3] < 0) {
    up <- theta[1] - theta[2] / theta[3]
  } else {
    up <- Inf
  }
  
 # calcular integrações difíceis e evita que erros pequenos de integração parem logo o método 
  safe_integrate <- function(f) {
    tryCatch(
      integrate(
        f,
        lower = u,
        upper = up,
        mu = theta[1],
        sig = theta[2],
        xi = theta[3],
        u = u,
        M = M,
        abs.tol = 1e-8,
        rel.tol = 1e-6,
        subdivisions = 1000,
        stop.on.error = FALSE
      ),
      error = function(e) {
        list(value = NA_real_, abs.error = NA_real_, message = e$message)
      }
    )
  }
  
  a11 <- safe_integrate(i.d2ldmu2)
  a12 <- safe_integrate(i.d2ldmudsig)
  a13 <- safe_integrate(i.d2ldmudxi)
  a22 <- safe_integrate(i.d2ldsig2)
  a23 <- safe_integrate(i.d2ldsigdxi)
  
  if (abs(theta[3]) > 0.0001) {
    a33 <- safe_integrate(i.d2ldxi2)
  } else {
    a33 <- safe_integrate(i.d2ldxi2.xi0)
  }
  
  vals <- c(a11$value, a12$value, a13$value,
            a12$value, a22$value, a23$value,
            a13$value, a23$value, a33$value)
  
  if (any(!is.finite(vals))) {
    stop("Falha na integração da matriz de informação esperada.")
  }
  
  EIM <- -matrix(vals, byrow = TRUE, nrow = 3)
  rownames(EIM) <- colnames(EIM) <- c("mu", "sigma", "xi")
  
  list(
    EIM = EIM,
    Errors = c(a11$abs.error, a12$abs.error, a13$abs.error,
               a12$abs.error, a22$abs.error, a23$abs.error,
               a13$abs.error, a23$abs.error, a33$abs.error)
  )
}

wadsworth_original_robusto <- function(x, k = 20, q1 = 0.80, q2 = 0.97,
                                       nbs = 5000, alpha = 0.05,
                                       scale_factor = 1000) {
  
  x0 <- x[is.finite(x) & x > 0]
  xs <- x0 / scale_factor #um sinistro de 200 000 passa a ser 200. Isto facilita a otimização e a integração numérica
  
  for (kk in seq(k, 8, by = -1)) {
    
    cat("A tentar k =", kk, "\n")
    
    res <- tryCatch(
      NHPP.diag(
        x = xs,
        k = kk,
        q1 = q1,
        q2 = q2,
        nbs = nbs,
        alpha = alpha,
        UseQuantiles = TRUE
      ),
      error = function(e) {
        message("Erro com k = ", kk, ": ", e$message)
        NULL
      }
    )
    
    if (!is.null(res)) {
      res$thresh_original <- res$thresh * scale_factor
      res$MLEall_original <- res$MLEall
      res$MLEall_original[, 1:2] <- res$MLEall_original[, 1:2] * scale_factor
      res$mle.u_original <- res$mle.u
      res$mle.u_original[1:2] <- res$mle.u_original[1:2] * scale_factor
      res$k_final <- kk
      return(res)
    }
  }
  #Esta transformação não muda o parâmetro de forma porque ξ é invariável a mudanças positivas de escala. 
  #Mas muda μ, σ e o threshold. Por isso, depois de obter os resultados, voltas à escala original
  
  stop("Não foi possível ajustar. Tenta aumentar q1 ou reduzir q2.")
}


x_041 <- lob_041_MTPL_Bodily_Injury %>%
  dplyr::filter(Incurred_Claims > 0) %>%
  dplyr::pull(Incurred_Claims)

res_041_wads <- wadsworth_original_robusto(
  x = x_041,
  k = 20,
  q1 = 0.70,
  q2 = 0.97,
  nbs = 5000,
  alpha = 0.05,
  scale_factor = 1000
)

res_041_wads$thresh_original
res_041_wads$pval
res_041_wads$k_final
res_041_wads$mle.u_original


q2_vals <- c(0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97)

set.seed(123)

sens_041_2 <- lapply(q2_vals, function(q1) {
  res <- wadsworth_original_robusto(
    x = x_041,
    k = 20,
    q1 = q1,
    q2 = 0.98,
    nbs = 50000,
    alpha = 0.05,
    scale_factor = 1000
  )
  
  data.frame(
    q1 = q1,
    threshold = res$thresh_original,
    p_value = res$pval,
    k_final = res$k_final,
    xi = res$mle.u_original[3]
  )
}) |> dplyr::bind_rows()

sens_041_2

#guardar
# dir.create("C:/Users/fatim/Downloads/wadsworth_plots_041", showWarnings = FALSE)
# 
# sens_041_2 <- lapply(q2_vals, function(q1) {
#   
#   pdf(
#     file = paste0("C:/Users/fatim/Downloads/wadsworth_plots_041/wadsworth_041_q1_", q1, ".pdf"),
#     width = 10,
#     height = 8
#   )
#   
#   res <- wadsworth_original_robusto(
#     x = x_041,
#     k = 20,
#     q1 = q1,
#     q2 = 0.98,
#     nbs = 50000,
#     alpha = 0.05,
#     scale_factor = 1000
#   )
#   
#   dev.off()
#   
#   data.frame(
#     q1 = q1,
#     threshold = res$thresh_original,
#     p_value = res$pval,
#     k_final = res$k_final,
#     xi = res$mle.u_original[3]
#   )
# }) |> dplyr::bind_rows()


# Health
x_011 <- lob_011_Health %>%
  dplyr::filter(Incurred_Claims > 0) %>%
  dplyr::pull(Incurred_Claims)

res_011_wads <- wadsworth_original_robusto(
  x = x_011,
  k = 20,
  q1 = 0.70,
  q2 = 0.97,
  nbs = 5000,
  alpha = 0.05,
  scale_factor = 1000
)

res_011_wads$thresh_original
res_011_wads$pval
res_011_wads$k_final
res_011_wads$mle.u_original

set.seed(123)

sens_011 <- lapply(q2_vals, function(q1) {
  res <- wadsworth_original_robusto(
    x = x_011,
    k = 20,
    q1 = q1,
    q2 = 0.98,
    nbs = 50000,
    alpha = 0.05,
    scale_factor = 1000
  )
  
  data.frame(
    q1 = q1,
    threshold = res$thresh_original,
    p_value = res$pval,
    k_final = res$k_final,
    xi = res$mle.u_original[3]
  )
}) |> dplyr::bind_rows()

sens_011

# dir.create("C:/Users/fatim/Downloads/wadsworth_plots_011", showWarnings = FALSE)
# 
# sens_011 <- lapply(q2_vals, function(q1) {
#   
#   q_lab <- sprintf("%.2f", q1)
#   
#   pdf(
#     file = paste0("C:/Users/fatim/Downloads/wadsworth_plots_011/wadsworth_011_q1_", q_lab, ".pdf"),
#     width = 10,
#     height = 8
#   )
#   
#   res <- wadsworth_original_robusto(
#     x = x_011,
#     k = 20,
#     q1 = q1,
#     q2 = 0.98,
#     nbs = 50000,
#     alpha = 0.05,
#     scale_factor = 1000
#   )
#   
#   dev.off()
#   
#   data.frame(
#     q1 = q1,
#     threshold = res$thresh_original,
#     p_value = res$pval,
#     k_final = res$k_final,
#     xi = res$mle.u_original[3]
#   )
# }) |> dplyr::bind_rows()

# MTPL Material Damage
x_042 <- lob_042_MTPL_Material_Damage %>%
  dplyr::filter(Incurred_Claims > 0) %>%
  dplyr::pull(Incurred_Claims)

res_042_wads <- wadsworth_original_robusto(
  x = x_042,
  k = 20,
  q1 = 0.70,
  q2 = 0.97,
  nbs = 5000,
  alpha = 0.05,
  scale_factor = 1000
)

res_042_wads$thresh_original
res_042_wads$pval
res_042_wads$k_final
res_042_wads$mle.u_original

set.seed(123)

q1_vals=c(0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.9)

sens_042_2 <- lapply(q1_vals, function(q1) {
  res <- wadsworth_original_robusto(
    x = x_042,
    k = 20,
    q1 = q1,
    q2 = 0.98,
    nbs = 50000,
    alpha = 0.05,
    scale_factor = 1000
  )
  
  data.frame(
    q1 = q1,
    threshold = res$thresh_original,
    p_value = res$pval,
    k_final = res$k_final,
    xi = res$mle.u_original[3]
  )
}) |> dplyr::bind_rows()

sens_042_2

#beirlant1999

beirlant_log_spacings <- function(x, k) {
  
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  x <- sort(x)
  n <- length(x)
  
  if (k >= n) stop("k tem de ser menor que n.")
  if (k < 3) stop("k tem de ser pelo menos 3.")
  
  j <- 1:k
  
  z <- j * (
    log(x[n - j + 1]) -
      log(x[n - j])
  )
  
  return(z)
}

#Ajuste regressao para um dado k
fit_beirlant_k <- function(x, k,
                           rho_min = 0.25,
                           rho_max_abs = 5,
                           prev_b = NULL,
                           prev_rho = NULL,
                           smooth = TRUE) {
  
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  x <- sort(x)
  n <- length(x)
  
  z <- beirlant_log_spacings(x, k)
  j <- 1:k
  p <- j / (k + 1)
  
  hill <- mean(z) #a média dos log-spacings é o estimador de Hill
  
  # Log-verosimilhança negativa
  negloglik <- function(par) {
    
    gamma <- par[1]
    b     <- par[2]
    rho   <- par[3]
    
    mu <- gamma + b * p^(-rho) #media do modelo
    
    if (gamma <= 0) return(1e20) #indice de cauda tem de ser positivo
    if (rho >= -rho_min) return(1e20)
    if (any(!is.finite(mu)) || any(mu <= 0)) return(1e20) #media exponencial positiva
    
    sum(log(mu) + z / mu)
  }
  
  # Limites para gamma, b e rho
  lower_gamma <- 1e-8
  upper_gamma <- max(10 * hill, 1)
  
  # b pode ser positivo ou negativo
  b_bound <- max(10 * hill, 1)
  
  lower_b <- -b_bound
  upper_b <-  b_bound
  
  lower_rho <- -rho_max_abs
  upper_rho <- -rho_min
  
  # Condição de suavidade inspirada no artigo:
  # |b_k| <= 1.1 |b_{k+1}|
  # |rho_k| <= 1.1 |rho_{k+1}|
  if (smooth && !is.null(prev_b) && is.finite(prev_b)) {
    b_bound_smooth <- max(1.1 * abs(prev_b), 1e-6)
    lower_b <- max(lower_b, -b_bound_smooth)
    upper_b <- min(upper_b,  b_bound_smooth)
  }
  
  if (smooth && !is.null(prev_rho) && is.finite(prev_rho)) {
    lower_rho <- max(lower_rho, -1.1 * abs(prev_rho))
  }
  
  lower <- c(lower_gamma, lower_b, lower_rho)
  upper <- c(upper_gamma, upper_b, upper_rho)
  
  # Vários pontos iniciais para evitar máximos locais
  starts <- list(
    c(hill,  0.10 * hill, -0.50),
    c(hill, -0.10 * hill, -0.50),
    c(hill,  0.25 * hill, -1.00),
    c(hill, -0.25 * hill, -1.00),
    c(hill,  0.50 * hill, -0.25),
    c(hill, -0.50 * hill, -0.25)
  )
  
  fits <- lapply(starts, function(st) {
    
    st <- pmin(pmax(st, lower), upper)
    
    tryCatch(
      optim(
        par = st,
        fn = negloglik,
        method = "L-BFGS-B", #permite otimização com restrições
        lower = lower,
        upper = upper,
        control = list(maxit = 2000)
      ),
      error = function(e) NULL
    )
  })
  
  fits <- fits[!sapply(fits, is.null)]
  
  if (length(fits) == 0) {
    return(data.frame(
      k = k,
      threshold = x[n - k],
      Hill = hill,
      gamma_hat = NA_real_,
      b_hat = NA_real_,
      rho_hat = NA_real_,
      AMSE_Hill = NA_real_,
      negloglik = NA_real_,
      convergence = 1
    ))
  }
  
  best <- fits[[which.min(sapply(fits, function(f) f$value))]]
  
  gamma_hat <- best$par[1]
  b_hat     <- best$par[2]
  rho_hat   <- best$par[3]
  
  # AMSE do Hill = gamma^2/k + (b/(1-rho))^2
  AMSE_Hill <- gamma_hat^2 / k + (b_hat / (1 - rho_hat))^2
  
  out <- data.frame(
    k = k,
    threshold = x[n - k],
    Hill = hill,
    gamma_hat = gamma_hat,
    b_hat = b_hat,
    rho_hat = rho_hat,
    AMSE_Hill = AMSE_Hill,
    negloglik = best$value,
    convergence = best$convergence
  )
  
  return(out)
}

#Ajuste para vários valores de k
beirlant_range <- function(x,
                           k_min = 20,
                           k_max = NULL,
                           rho_min = NULL,
                           smooth = TRUE) {
  
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  n <- length(x)
  
  if (is.null(k_max)) {
    k_max <- floor(0.30 * n)
  }
  
  if (is.null(rho_min)) {
    # No artigo:
    # n <= 1000: |rho| >= 0.5
    # n maior: pode relaxar gradualmente, por exemplo 0.25
    rho_min <- ifelse(n <= 1000, 0.5, 0.25)
  }
  
  k_grid <- k_min:k_max
  
  res_list <- vector("list", length(k_grid))
  names(res_list) <- k_grid
  
  prev_b <- NULL
  prev_rho <- NULL
  
  # Para aplicar a suavidade |b_k| <= 1.1 |b_{k+1}|,
  # ajustamos de k_max para k_min.
  for (kk in rev(k_grid)) {
    
    fit <- fit_beirlant_k(
      x = x,
      k = kk,
      rho_min = rho_min,
      prev_b = prev_b,
      prev_rho = prev_rho,
      smooth = smooth
    )
    
    res_list[[as.character(kk)]] <- fit
    
    prev_b <- fit$b_hat
    prev_rho <- fit$rho_hat
  }
  
  res <- bind_rows(res_list) %>%
    arrange(k)
  
  # k que minimiza o AMSE estimado do Hill
  res_aux <- res %>%
    dplyr::filter(!is.na(AMSE_Hill), is.finite(AMSE_Hill))
  
  k_opt <- res_aux$k[which.min(res_aux$AMSE_Hill)]
  
  res <- res %>%
    mutate(
      k_AMSE_opt = k == k_opt
    )
  
  return(res)
}

#LOBs
beirlant_lob <- function(dados_lob,
                         nome_lob,
                         claim_var = "Incurred_Claims",
                         k_min = 20,
                         k_max = NULL,
                         rho_min = NULL,
                         smooth = TRUE) {
  
  x <- dados_lob %>%
    filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    ) %>%
    pull(.data[[claim_var]])
  
  n <- length(x)
  
  res <- beirlant_range(
    x = x,
    k_min = k_min,
    k_max = k_max,
    rho_min = rho_min,
    smooth = smooth
  )
  
  res <- res %>%
    dplyr::mutate(
      LOB = nome_lob,
      n = n,
      n_excessos = k,
      prob_threshold = 1 - k / n
    ) %>%
    dplyr::select(
      LOB,
      n,
      prob_threshold,
      threshold,
      n_excessos,
      k,
      Hill,
      gamma_hat,
      b_hat,
      rho_hat,
      AMSE_Hill,
      convergence,
      k_AMSE_opt
    )
  
  summary <- res %>%
    dplyr::filter(k_AMSE_opt) %>%
    dplyr::select(
      LOB,
      n,
      prob_threshold,
      threshold,
      n_excessos,
      k,
      Hill,
      gamma_hat,
      b_hat,
      rho_hat,
      AMSE_Hill,
      convergence
    )
  
  p_gamma <- ggplot(res, aes(x = k)) +
    geom_line(aes(y = Hill, linetype = "Hill")) +
    geom_line(aes(y = gamma_hat, linetype = "Beirlant ML")) +
    geom_vline(
      xintercept = summary$k,
      linetype = "dashed"
    ) +
    labs(
      title = paste("Beirlant et al. (1999) -", nome_lob),
      x = "k",
      y = expression(hat(gamma)),
      linetype = "Estimador"
    ) +
    theme_minimal()
  
  p_amse <- ggplot(res, aes(x = k, y = AMSE_Hill)) +
    geom_line() +
    geom_vline(
      xintercept = summary$k,
      linetype = "dashed"
    ) +
    labs(
      title = paste("AMSE estimado do Hill -", nome_lob),
      x = "k",
      y = "AMSE estimado"
    ) +
    theme_minimal()
  
  return(list(
    results = res,
    summary = summary,
    plot_gamma = p_gamma,
    plot_amse = p_amse
  ))
}


res_041_beirlant <- beirlant_lob(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  k_min = 400,
  k_max = 550,
  smooth = TRUE
)

res_041_beirlant$summary

res_041_beirlant$plot_gamma
res_041_beirlant$plot_amse

plot_beirlant_gamma <- function(res_obj) {
  
  res <- res_obj$results
  summary <- res_obj$summary
  
  res_long <- res %>%
    pivot_longer(
      cols = c(Hill, gamma_hat),
      names_to = "Estimator",
      values_to = "value"
    ) %>%
    mutate(
      Estimator = recode(
        Estimator,
        Hill = "Hill",
        gamma_hat = "Beirlant ML"
      )
    )
  
  ggplot(res_long, aes(x = k, y = value, color = Estimator)) +
    geom_line(linewidth = 0.9) +
    geom_vline(
      xintercept = summary$k,
      linetype = "dashed",
      color = "black",
      linewidth = 0.8
    ) +
    annotate(
      "text",
      x = summary$k,
      y = Inf,
      label = paste0("k = ", scales::comma(summary$k)),
      angle = 90,
      vjust = 1.3,
      hjust = 1.1,
      size = 3.5
    ) +
    scale_x_continuous(labels = scales::comma) +
    scale_color_manual(
      values = c(
        "Hill" = "#1F77B4",
        "Beirlant ML" = "#D62728"
      )
    ) +
    labs(
      title = paste(summary$LOB),
      subtitle = "Comparison between Hill and bias-corrected ML estimator",
      x = "Number of upper order statistics (k)",
      y = expression(hat(gamma)),
      color = "Estimator"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

plot_beirlant_amse <- function(res_obj) {
  
  res <- res_obj$results
  summary <- res_obj$summary
  
  ggplot(res, aes(x = k, y = AMSE_Hill)) +
    geom_line(color = "#2CA02C", linewidth = 0.9) +
    geom_point(color = "#2CA02C", size = 1.2, alpha = 0.7) +
    geom_vline(
      xintercept = summary$k,
      linetype = "dashed",
      color = "black",
      linewidth = 0.8
    ) +
    annotate(
      "text",
      x = summary$k,
      y = Inf,
      label = paste0("k = ", scales::comma(summary$k)),
      angle = 90,
      vjust = 1.3,
      hjust = 1.1,
      size = 3.5
    ) +
    scale_x_continuous(labels = scales::comma) +
    labs(
      title = paste("Estimated AMSE of the Hill estimator -", summary$LOB),
      x = "Number of upper order statistics (k)",
      y = expression(widehat(AMSE)(k))
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold")
    )
}

p_gamma_041 <- plot_beirlant_gamma(res_041_beirlant)
p_amse_041  <- plot_beirlant_amse(res_041_beirlant)

p_gamma_041
p_amse_041

# ggsave("C:/Users/fatim/Downloads/beirlant_gamma_041.pdf", p_gamma_041, width = 8, height = 5)
# ggsave("C:/Users/fatim/Downloads/beirlant_amse_041.pdf", p_amse_041, width = 8, height = 5)

res_011_beirlant <- beirlant_lob(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  k_min = 650,
  k_max = 1300,
  smooth = TRUE
)

res_011_beirlant$summary

res_011_beirlant$plot_gamma
res_011_beirlant$plot_amse

p_gamma_011 <- plot_beirlant_gamma(res_011_beirlant)
p_amse_011  <- plot_beirlant_amse(res_011_beirlant)

p_gamma_011
p_amse_011

# ggsave("C:/Users/fatim/Downloads/beirlant_gamma_011.pdf", p_gamma_011, width = 8, height = 5)
# ggsave("C:/Users/fatim/Downloads/beirlant_amse_011.pdf", p_amse_011, width = 8, height = 5)

res_042_beirlant <- beirlant_lob(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  k_min = 400,
  k_max = 700,
  smooth = TRUE
)

res_042_beirlant$summary

res_042_beirlant$plot_gamma
res_042_beirlant$plot_amse

p_gamma_042 <- plot_beirlant_gamma(res_042_beirlant)
p_amse_042  <- plot_beirlant_amse(res_042_beirlant)

p_gamma_042
p_amse_042

# ggsave("C:/Users/fatim/Downloads/beirlant_gamma_042.pdf", p_gamma_042, width = 8, height = 5)
# ggsave("C:/Users/fatim/Downloads/beirlant_amse_042.pdf", p_amse_042, width = 8, height = 5)

##Ferreira 

ferreira_bootstrap_k <- function(x, n1 = NULL, r = 200, p_exceed = NULL,
                                 usar_indicador = FALSE, d = 0,
                                 k_min = NULL, k_max = NULL,
                                 seed = 123) {
  
  set.seed(seed)
  
  x <- x[is.finite(x) & x > 0]
  n <- length(x)
  
  if (is.null(n1)) n1 <- floor(n^0.8) #n1
  n2 <- floor(n1^2 / n) #n2
  
  if (is.null(p_exceed)) p_exceed <- 10 / n
  
  rho1 <- function(gamma) {
    1 / (1 - pmin(gamma, 0))
  }
  
  calc_estimators <- function(x_boot, k, p_exceed) {
    
    m <- length(x_boot)
    xs <- sort(x_boot)
    
    if (k >= m) return(NA_real_)
    
    threshold <- xs[m - k]
    tail_vals <- xs[(m - k + 1):m]
    
    log_excess <- log(tail_vals) - log(threshold)
    
    M1 <- mean(log_excess)
    M2 <- mean(log_excess^2)
    M3 <- mean(log_excess^3)
    
    if (!is.finite(M1) || !is.finite(M2) || !is.finite(M3)) return(NA_real_)
    if (M1 <= 0 || M2 <= 0 || M3 <= 0) return(NA_real_)
    
    gamma1 <- M1 + 1 - 0.5 * (1 - (M1^2 / M2))^(-1)
    
    gamma2 <- sqrt(M2 / 2) + 1 -
      (2 / 3) * (1 - (M1 * M2 / M3))^(-1)
    
    if (!is.finite(gamma1) || !is.finite(gamma2)) return(NA_real_)
    if (abs(gamma1) < 1e-8 || abs(gamma2) < 1e-8) return(NA_real_)
    
    a1 <- threshold * M1 / rho1(gamma1)
    a2 <- threshold * M1 / rho1(gamma2)
    
    factor <- k / (m * p_exceed)
    
    if (!is.finite(factor) || factor <= 0) return(NA_real_)
    
    xhat1 <- threshold + a1 * ((factor^gamma1 - 1) / gamma1)
    xhat2 <- threshold + a2 * ((factor^gamma2 - 1) / gamma2)
    
    if (!is.finite(xhat1) || !is.finite(xhat2)) return(NA_real_)
    
    diff <- xhat1 - xhat2
    
    if (usar_indicador) {
      q <- diff^2 * as.numeric(abs(diff) <= k^d)
    } else {
      q <- diff^2
    }
    
    return(q)
  }
  
  estimate_k0 <- function(m) {
    
    if (is.null(k_min)) {
      k_min_m <- max(10, ceiling(log(m)))
    } else {
      k_min_m <- k_min
    }
    
    if (is.null(k_max)) {
      k_max_m <- floor(m / log(m))
    } else {
      k_max_m <- min(k_max, m - 1)
    }
    
    if (k_min_m >= k_max_m) {
      stop("Intervalo de k inválido.")
    }
    
    k_grid <- k_min_m:k_max_m
    
    q_mat <- matrix(NA_real_, nrow = length(k_grid), ncol = r)
    
    #bootstrap
    for (s in 1:r) {
      x_boot <- sample(x, size = m, replace = TRUE)
      
      for (j in seq_along(k_grid)) {
        q_mat[j, s] <- calc_estimators(x_boot, k_grid[j], p_exceed)
      }
    }
    
    q_mean <- rowMeans(q_mat, na.rm = TRUE)
    q_sd <- apply(q_mat, 1, sd, na.rm = TRUE)
    n_valid <- rowSums(is.finite(q_mat))
    
    valid <- is.finite(q_mean)
    
    if (!any(valid)) {
      stop("Não foi possível calcular nenhum valor válido de q.")
    }
    
    k0 <- k_grid[valid][which.min(q_mean[valid])]
    
    tabela <- data.frame(
      k = k_grid,
      q_mean = q_mean,
      q_sd = q_sd,
      n_valid = n_valid
    )
    
    return(list(k0 = k0, tabela = tabela))
  }
  
  res_n1 <- estimate_k0(n1)
  res_n2 <- estimate_k0(n2)
  
  inconsistente <- res_n2$k0 > res_n1$k0
  
  k_final <- round((res_n1$k0^2) / res_n2$k0)
  k_final <- max(1, min(k_final, n - 1))
  
  return(list(
    n = n,
    n1 = n1,
    n2 = n2,
    p_exceed = p_exceed,
    k0_n1 = res_n1$k0,
    k0_n2 = res_n2$k0,
    k0_n = k_final,
    inconsistente = inconsistente,
    tabela_n1 = res_n1$tabela,
    tabela_n2 = res_n2$tabela
  ))
}

# Função para obter threshold a partir de k
threshold_from_k <- function(x, k) {
  x <- x[is.finite(x) & x > 0]
  sort(x, decreasing = TRUE)[k + 1]
}

# Aplicação às 3 LOBs

res_041 <- ferreira_bootstrap_k(
  lob_041_MTPL_Bodily_Injury$Incurred_Claims,
  n1 = floor(length(lob_041_MTPL_Bodily_Injury$Incurred_Claims)^0.9),
  r = 200,
  k_min = 20,
  usar_indicador = FALSE
)

res_011 <- ferreira_bootstrap_k(
  lob_011_Health$Incurred_Claims,
  n1 = floor(length(lob_011_Health$Incurred_Claims)^0.9),
  r = 200,
  k_min = 20,
  usar_indicador = FALSE
)

res_042 <- ferreira_bootstrap_k(
  lob_042_MTPL_Material_Damage$Incurred_Claims,
  n1 = floor(length(lob_042_MTPL_Material_Damage$Incurred_Claims)^0.9),
  r = 200,
  k_min = 20,
  usar_indicador = FALSE
)

# Tabela resumo dos resultados
resultados_ferreira <- data.frame(
  LOB = c("MTPL Bodily Injury", "Health", "MTPL Material Damage"),
  n = c(res_041$n, res_011$n, res_042$n),
  n1 = c(res_041$n1, res_011$n1, res_042$n1),
  n2 = c(res_041$n2, res_011$n2, res_042$n2),
  k0_n1 = c(res_041$k0_n1, res_011$k0_n1, res_042$k0_n1),
  k0_n2 = c(res_041$k0_n2, res_011$k0_n2, res_042$k0_n2),
  k0_n = c(res_041$k0_n, res_011$k0_n, res_042$k0_n),
  inconsistente = c(res_041$inconsistente, res_011$inconsistente, res_042$inconsistente)
)

resultados_ferreira$threshold <- c(
  threshold_from_k(lob_041_MTPL_Bodily_Injury$Incurred_Claims, res_041$k0_n),
  threshold_from_k(lob_011_Health$Incurred_Claims, res_011$k0_n),
  threshold_from_k(lob_042_MTPL_Material_Damage$Incurred_Claims, res_042$k0_n)
)

resultados_ferreira$prob_threshold <- 1 - resultados_ferreira$k0_n / resultados_ferreira$n

resultados_ferreira


# Gráfico do erro médio bootstrap
plot_ferreira <- function(res, nome_lob, x_original) {
  
  dados_n1 <- res$tabela_n1 %>%
    mutate(
      escala = paste0("n1 = ", scales::comma(res$n1)),
      k0 = res$k0_n1
    )
  
  dados_n2 <- res$tabela_n2 %>%
    mutate(
      escala = paste0("n2 = ", scales::comma(res$n2)),
      k0 = res$k0_n2
    )
  
  dados_plot <- bind_rows(dados_n1, dados_n2)
  
  threshold_final <- threshold_from_k(x_original, res$k0_n)
  
  ggplot(dados_plot, aes(x = k, y = q_mean)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 0.7, alpha = 0.5) +
    geom_vline(aes(xintercept = k0), linetype = "dashed") +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ escala, scales = "free_x") +
    labs(
      title = paste(nome_lob),
      subtitle = paste0(
        "k final = ", scales::comma(res$k0_n),
        " | threshold final = ", scales::comma(round(threshold_final, 2))
      ),
      x = "k",
      y = expression(bar(q)[k]^"*")
    ) +
    theme_minimal()
}


plot_ferreira(
  res = res_041,
  nome_lob = "MTPL Bodily Injury",
  x_original = lob_041_MTPL_Bodily_Injury$Incurred_Claims
)

# pdf(
#   file = "C:/Users/fatim/Downloads/ferreira_041.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot_ferreira(
#   res = res_041,
#   nome_lob = "MTPL Bodily Injury",
#   x_original = lob_041_MTPL_Bodily_Injury$Incurred_Claims
# )
# 
# dev.off()

plot_ferreira(
  res = res_011,
  nome_lob = "Health",
  x_original = lob_011_Health$Incurred_Claims
)

# pdf(
#   file = "C:/Users/fatim/Downloads/ferreira_011.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot_ferreira(
#   res = res_011,
#   nome_lob = "Health",
#   x_original = lob_011_Health$Incurred_Claims
# )
# 
# dev.off()


plot_ferreira(
  res = res_042,
  nome_lob = "MTPL Material Damage",
  x_original = lob_042_MTPL_Material_Damage$Incurred_Claims
)

# pdf(
#   file = "C:/Users/fatim/Downloads/ferreira_042.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot_ferreira(
#   res = res_042,
#   nome_lob = "MTPL Material Damage",
#   x_original = lob_042_MTPL_Material_Damage$Incurred_Claims
# )
# 
# dev.off()

#Caers

#simular excessos
rgpd_excess <- function(n, scale, shape) {
  
  U <- runif(n)
  
  if (abs(shape) < 1e-8) {
    y <- -scale * log(1 - U)
  } else {
    y <- (scale / shape) * ((1 - U)^(-shape) - 1)
  }
  
  return(y)
}

#Função para ajustar a GP
fit_gpd_threshold <- function(x, u, est = "mle", min_excessos = 10) {
  
  x <- x[!is.na(x) & x > 0]
  excessos <- x[x > u] - u
  k <- length(excessos)
  
  if (k < min_excessos) {
    return(NULL)
  }
  
  fit <- tryCatch(
    POT::fitgpd(
      data = x,
      threshold = u,
      est = est
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(NULL)
  }
  
  sigma_hat <- as.numeric(fit$param["scale"])
  xi_hat <- as.numeric(fit$param["shape"])
  
  if (is.na(sigma_hat) || is.na(xi_hat) || sigma_hat <= 0) {
    return(NULL)
  }
  
  return(
    list(
      scale = sigma_hat,
      shape = xi_hat,
      k = k,
      n = length(x),
      phi_u = k / length(x),
      fit = fit
    )
  )
}

#Estimador extremo de interesse
estimate_theta <- function(x, u,
                           theta = "xi",
                           epsilon = NULL,
                           est = "mle",
                           min_excessos = 10) {
  
  ajuste <- fit_gpd_threshold(
    x = x,
    u = u,
    est = est,
    min_excessos = min_excessos
  )
  
  if (is.null(ajuste)) {
    return(NULL)
  }
  
  sigma_hat <- ajuste$scale
  xi_hat <- ajuste$shape
  phi_u <- ajuste$phi_u
  
  if (theta == "xi") {
    
    theta_hat <- xi_hat
    
  } else if (theta == "quantile") {
    
    if (is.null(epsilon)) {
      stop("Para theta = 'quantile', tens de fornecer epsilon.")
    }
    
    if (epsilon >= phi_u) {
      return(NULL)
    }
    
    if (abs(xi_hat) < 1e-8) {
      theta_hat <- u + sigma_hat * log(phi_u / epsilon)
    } else {
      theta_hat <- u + (sigma_hat / xi_hat) *
        ((phi_u / epsilon)^xi_hat - 1)
    }
    
  } else {
    
    stop("theta deve ser 'xi' ou 'quantile'.")
  }
  
  return(
    list(
      theta_hat = as.numeric(theta_hat),
      scale = sigma_hat,
      shape = xi_hat,
      k = ajuste$k,
      n = ajuste$n,
      phi_u = phi_u
    )
  )
}

#Gerar uma amostra bootstrap semiparamétrica
semiparametric_boot_sample <- function(x, u, scale, shape) {
  
  x <- x[!is.na(x) & x > 0]
  n <- length(x)
  
  body <- x[x <= u]
  k <- sum(x > u)
  p_tail <- k / n
  
  is_tail <- runif(n) < p_tail
  
  x_boot <- numeric(n)
  
  n_tail <- sum(is_tail)
  n_body <- n - n_tail
  
  if (n_body > 0) {
    x_boot[!is_tail] <- sample(body, size = n_body, replace = TRUE)
  }
  
  if (n_tail > 0) {
    y_tail <- rgpd_excess(
      n = n_tail,
      scale = scale,
      shape = shape
    )
    
    x_boot[is_tail] <- u + y_tail
  }
  
  return(x_boot)
}

#Método completo 
caers_semiparametric_bootstrap <- function(dados_lob,
                                           nome_lob,
                                           claim_var = "Incurred_Claims",
                                           probs = seq(0.70, 0.98, by = 0.01),
                                           theta = "xi",
                                           epsilon = NULL,
                                           B = 500,
                                           est = "mle",
                                           min_excessos = 10,
                                           seed = 123) {
  
  set.seed(seed)
  
  x <- dados_lob %>%
    filter(!is.na(.data[[claim_var]]),
           .data[[claim_var]] > 0) %>%
    pull(.data[[claim_var]])
  
  n <- length(x)
  
  thresholds <- as.numeric(
    quantile(x, probs = probs, na.rm = TRUE)
  )
  
  resultados <- lapply(seq_along(thresholds), function(j) {
    
    u <- thresholds[j]
    p <- probs[j]
    
    # Estimativa original no threshold u
    theta_original <- estimate_theta(
      x = x,
      u = u,
      theta = theta,
      epsilon = epsilon,
      est = est,
      min_excessos = min_excessos
    )
    
    if (is.null(theta_original)) {
      return(NULL)
    }
    
    theta_hat <- theta_original$theta_hat
    sigma_hat <- theta_original$scale
    xi_hat <- theta_original$shape
    k <- theta_original$k
    phi_u <- theta_original$phi_u
    
    # Estimativas bootstrap
    theta_boot <- rep(NA, B)
    k_boot <- rep(NA, B)
    
    for (b in 1:B) {
      
      x_b <- semiparametric_boot_sample(
        x = x,
        u = u,
        scale = sigma_hat,
        shape = xi_hat
      )
      
      theta_b <- estimate_theta(
        x = x_b,
        u = u,
        theta = theta,
        epsilon = epsilon,
        est = est,
        min_excessos = min_excessos
      )
      
      if (!is.null(theta_b)) {
        theta_boot[b] <- theta_b$theta_hat
        k_boot[b] <- theta_b$k
      }
    }
    
    theta_boot_valid <- theta_boot[!is.na(theta_boot)]
    k_boot_valid <- k_boot[!is.na(theta_boot)]
    
    B_valid <- length(theta_boot_valid)
    
    if (B_valid < 30) {
      return(NULL)
    }
    
    theta_bar_boot <- mean(theta_boot_valid)
    
    bias_hat <- theta_bar_boot - theta_hat
    
    var_hat <- var(theta_boot_valid)
    
    mse_hat <- bias_hat^2 + var_hat
    
    ci_90 <- quantile(theta_boot_valid, probs = c(0.05, 0.95), na.rm = TRUE)
    ci_95 <- quantile(theta_boot_valid, probs = c(0.025, 0.975), na.rm = TRUE)
    
    data.frame(
      LOB = nome_lob,
      theta = theta,
      prob_threshold = p,
      threshold = u,
      n = n,
      n_excessos = k,
      phi_u = phi_u,
      scale = sigma_hat,
      shape = xi_hat,
      theta_hat = theta_hat,
      theta_boot_mean = theta_bar_boot,
      bias = bias_hat,
      variance = var_hat,
      MSE = mse_hat,
      B = B,
      B_valid = B_valid,
      mean_k_boot = mean(k_boot_valid),
      sd_k_boot = sd(k_boot_valid),
      CI90_lower = ci_90[1],
      CI90_upper = ci_90[2],
      CI95_lower = ci_95[1],
      CI95_upper = ci_95[2]
    )
  })
  
  tabela <- bind_rows(resultados)
  
  if (nrow(tabela) == 0) {
    stop("Nenhum threshold produziu resultados válidos.")
  }
  
  tabela <- tabela %>%
    arrange(MSE)
  
  threshold_otimo <- tabela %>%
    slice(1)
  
  grafico <- ggplot(tabela, aes(x = threshold, y = MSE)) +
    geom_line() +
    geom_point(size = 1.5) +
    geom_vline(
      xintercept = threshold_otimo$threshold,
      linetype = "dashed"
    ) +
    scale_x_continuous(
      labels = scales::comma
    ) +
    scale_y_continuous(
      labels = scales::comma
    ) +
    labs(
      title = paste("Semiparametric bootstrap MSE -", nome_lob),
      subtitle = paste(
        "Quantity of interest:", theta,
        "| optimal u =", scales::comma(round(threshold_otimo$threshold, 0))
      ),
      x = "u",
      y = expression(widehat(MSE))
    ) +
    theme_minimal()
  
  return(
    list(
      table = tabela,
      optimal = threshold_otimo,
      plot = grafico
    )
  )
}

res_caers_041_xi <- caers_semiparametric_bootstrap(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  probs = seq(0.80, 0.98, by = 0.005),
  theta = "xi",
  B = 500,
  est = "mle",
  min_excessos = 10,
  seed = 123
)

res_caers_041_xi$optimal
res_caers_041_xi$table
res_caers_041_xi$plot

# ggsave(
#   filename = "C:/Users/fatim/Downloads/caers_041_xi.pdf",
#   plot = res_caers_041_xi$plot,
#   width = 8,
#   height = 6
# )

res_caers_041_q <- caers_semiparametric_bootstrap(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  probs = seq(0.80, 0.98, by = 0.005),
  theta = "quantile",
  epsilon = 0.001,
  B = 500,
  est = "mle",
  min_excessos = 10,
  seed = 123
)

res_caers_041_q$optimal
res_caers_041_q$table
res_caers_041_q$plot

# ggsave(
#   filename = "C:/Users/fatim/Downloads/caers_041_q.pdf",
#   plot = res_caers_041_q$plot,
#   width = 8,
#   height = 6
# )

res_caers_011_xi <- caers_semiparametric_bootstrap(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  probs = seq(0.80, 0.98, by = 0.005),
  theta = "xi",
  B = 500,
  est = "moments",
  min_excessos = 10,
  seed = 123
)

res_caers_011_xi$optimal
res_caers_011_xi$table
res_caers_011_xi$plot

# ggsave(
#   filename = "C:/Users/fatim/Downloads/caers_011_xi.pdf",
#   plot = res_caers_011_xi$plot,
#   width = 8,
#   height = 6
# )

res_caers_011_q <- caers_semiparametric_bootstrap(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  probs = seq(0.80, 0.98, by = 0.005),
  theta = "quantile",
  epsilon = 0.001,
  B = 500,
  est = "moments",
  min_excessos = 10,
  seed = 123
)

res_caers_011_q$optimal
res_caers_011_q$table
res_caers_011_q$plot

# ggsave(
#   filename = "C:/Users/fatim/Downloads/caers_011_q.pdf",
#   plot = res_caers_011_q$plot,
#   width = 8,
#   height = 6
# )

res_caers_042_xi <- caers_semiparametric_bootstrap(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  probs = seq(0.8, 0.98, by = 0.005),
  theta = "xi",
  B = 500,
  est = "moments",
  min_excessos = 10,
  seed = 123
)

res_caers_042_xi$optimal
res_caers_042_xi$table
res_caers_042_xi$plot

# ggsave(
#   filename = "C:/Users/fatim/Downloads/caers_042_xi.pdf",
#   plot = res_caers_042_xi$plot,
#   width = 8,
#   height = 6
# )

res_caers_042_q <- caers_semiparametric_bootstrap(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  probs = seq(0.80, 0.98, by = 0.005),
  theta = "quantile",
  epsilon = 0.001,
  B = 500,
  est = "moments",
  min_excessos = 10,
  seed = 123
)

res_caers_042_q$optimal
res_caers_042_q$table
res_caers_042_q$plot

# ggsave(
#   filename = "C:/Users/fatim/Downloads/caers_042_q.pdf",
#   plot = res_caers_042_q$plot,
#   width = 8,
#   height = 6
# )

#L-moments LOMBA E ALVES, usando o pacote

#Curva teórica da GP no L-moment Ratio Diagram
gpd_lmoment_curve <- function(tau3) {
  tau3 * (1 + 5 * tau3) / (5 + tau3)
}

#Estimador alpha_r dos PWM
alpha_hat <- function(y, r) {
  
  y <- sort(y)
  k <- length(y)
  
  if (k <= r) {
    return(NA_real_)
  }
  
  j <- seq_len(k)
  
  weights <- exp(lchoose(k - j, r) - lchoose(k - 1, r))
  
  alpha <- mean(weights * y)
  
  return(alpha)
}

#L-moment ratios: t3 e t4
lmoment_ratios_excesses <- function(y) {
  
  y <- as.numeric(y)
  y <- y[is.finite(y)]
  y <- y[y > 0]
  
  k <- length(y)
  
  if (k < 5) {
    return(c(t3 = NA_real_, t4 = NA_real_))
  }
  
  a0 <- alpha_hat(y, 0)
  a1 <- alpha_hat(y, 1)
  a2 <- alpha_hat(y, 2)
  a3 <- alpha_hat(y, 3)
  
  l1 <- a0
  l2 <- a0 - 2 * a1
  l3 <- a0 - 6 * a1 + 6 * a2
  l4 <- a0 - 12 * a1 + 30 * a2 - 20 * a3
  
  if (!is.finite(l2) || abs(l2) < 1e-12) {
    return(c(t3 = NA_real_, t4 = NA_real_))
  }
  
  t3 <- l3 / l2
  t4 <- l4 / l2
  
  return(c(t3 = t3, t4 = t4))
}

#Distância mínima à curva teórica da GPD
distance_to_gpd_curve <- function(t3,
                                  t4,
                                  tau3_lower = -0.999,
                                  tau3_upper = 0.999) {
  
  if (!is.finite(t3) || !is.finite(t4)) {
    return(NA_real_)
  }
  
  obj <- function(s3) {
    (t3 - s3)^2 + (t4 - gpd_lmoment_curve(s3))^2
  }
  
  opt <- optimize(
    f = obj,
    interval = c(tau3_lower, tau3_upper)
  )
  
  sqrt(opt$objective)
}

#Grelha flexível de quantis
alrsm_probs_flexible <- function(q_min = 0.25,
                                 q_max = 0.98,
                                 I = 30) {
  
  if (q_min <= 0 || q_min >= 1) {
    stop("q_min tem de estar entre 0 e 1.")
  }
  
  if (q_max <= 0 || q_max >= 1) {
    stop("q_max tem de estar entre 0 e 1.")
  }
  
  if (q_min >= q_max) {
    stop("q_min tem de ser menor do que q_max.")
  }
  
  if (I < 2) {
    stop("I tem de ser pelo menos 2.")
  }
  
  seq(q_min, q_max, length.out = I)
}

alrsm_mev_custom <- function(x,
                             lob_name = NULL,
                             q_min = 0.25,
                             q_max = 0.98,
                             I = 30,
                             min_excessos = 10,
                             fit_method = "Grimshaw",
                             return_probs = c(0.01, 0.001),
                             make_plot = TRUE) {
  
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  x <- x[x > 0]
  
  n <- length(x)
  
  if (is.null(lob_name)) {
    lob_name <- "LOB"
  }
  
  fmt <- scales::comma
  
  if (n < 30) {
    stop("A amostra tem poucas observações positivas.")
  }
  
  probs <- alrsm_probs_flexible(
    q_min = q_min,
    q_max = q_max,
    I = I
  )
  
  thresholds <- as.numeric(
    quantile(x, probs = probs, na.rm = TRUE, type = 7)
  )
  
  cand <- data.frame(
    prob_threshold = probs,
    threshold = thresholds
  )
  
  cand <- cand[!duplicated(cand$threshold), ]
  
  resultados <- lapply(seq_len(nrow(cand)), function(i) {
    
    u <- cand$threshold[i]
    y <- x[x > u] - u
    k <- length(y)
    
    if (k < min_excessos) {
      return(data.frame(
        prob_threshold = cand$prob_threshold[i],
        threshold = u,
        n_excessos = k,
        t3 = NA_real_,
        t4 = NA_real_,
        distance = NA_real_
      ))
    }
    
    ratios <- lmoment_ratios_excesses(y)
    
    dist <- distance_to_gpd_curve(
      t3 = ratios["t3"],
      t4 = ratios["t4"]
    )
    
    data.frame(
      prob_threshold = cand$prob_threshold[i],
      threshold = u,
      n_excessos = k,
      t3 = ratios["t3"],
      t4 = ratios["t4"],
      distance = dist
    )
  })
  
  resultados <- do.call(rbind, resultados)
  
  resultados_validos <- resultados[
    is.finite(resultados$distance),
  ]
  
  if (nrow(resultados_validos) == 0) {
    stop("Nenhum threshold válido. Tenta reduzir min_excessos.")
  }
  
  escolhido <- resultados_validos[
    which.min(resultados_validos$distance),
  ]
  
  u_hat <- escolhido$threshold
  
  fit <- tryCatch(
    mev::fit.gpd(
      xdat = x,
      threshold = u_hat,
      method = fit_method,
      show = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    
    warning("O ALRSM escolheu threshold, mas mev::fit.gpd falhou.")
    
    return(list(
      summary = data.frame(
        n = n,
        q_min = q_min,
        q_max = q_max,
        I = I,
        prob_threshold = escolhido$prob_threshold,
        threshold = u_hat,
        n_excessos = escolhido$n_excessos,
        p_excessos = escolhido$n_excessos / n,
        t3 = escolhido$t3,
        t4 = escolhido$t4,
        distance = escolhido$distance,
        scale_mev = NA_real_,
        shape_mev = NA_real_,
        convergence = NA
      ),
      threshold_table = resultados,
      selected = escolhido,
      fit = NULL,
      return_levels = NULL,
      plot = NULL,
      distance_plot = NULL
    ))
  }
  
  sigma_hat <- unname(fit$estimate["scale"])
  xi_hat <- unname(fit$estimate["shape"])
  
  k_hat <- sum(x > u_hat)
  p_u <- k_hat / n
  
  return_levels <- data.frame(
    p_exceed = return_probs,
    return_level = sapply(return_probs, function(p) {
      
      if (p >= p_u) {
        return(NA_real_)
      }
      
      if (abs(xi_hat) < 1e-8) {
        u_hat + sigma_hat * log(p_u / p)
      } else {
        u_hat + (sigma_hat / xi_hat) * ((p_u / p)^xi_hat - 1)
      }
    })
  )
  
  resumo <- data.frame(
    n = n,
    q_min = q_min,
    q_max = q_max,
    I = I,
    prob_threshold = escolhido$prob_threshold,
    threshold = u_hat,
    n_excessos = k_hat,
    p_excessos = p_u,
    t3 = escolhido$t3,
    t4 = escolhido$t4,
    distance = escolhido$distance,
    scale_mev = sigma_hat,
    shape_mev = xi_hat,
    convergence = ifelse(is.null(fit$convergence), NA, fit$convergence)
  )
  
  grafico_lmrd <- NULL
  
  if (make_plot) {
    
    curve_df <- data.frame(
      tau3 = seq(-0.2, 1, length.out = 500)
    )
    
    curve_df$tau4 <- gpd_lmoment_curve(curve_df$tau3)
    
    grafico_lmrd <- ggplot() +
      geom_line(
        data = curve_df,
        aes(x = tau3, y = tau4),
        linewidth = 1
      ) +
      geom_point(
        data = resultados,
        aes(x = t3, y = t4),
        size = 2,
        alpha = 0.7
      ) +
      geom_point(
        data = escolhido,
        aes(x = t3, y = t4),
        size = 4
      ) +
      geom_text(
        data = resultados,
        aes(x = t3, y = t4, label = round(prob_threshold, 3)),
        vjust = -0.7,
        size = 3
      ) +
      labs(
        title = paste0("L-moment Ratio Diagram - ", lob_name),
        subtitle = paste0(
          "Selected threshold = ",
          fmt(round(u_hat, 2)),
          " | quantile = ",
          round(escolhido$prob_threshold, 3)
        ),
        x = "t3 - L-skewness",
        y = "t4 - L-kurtosis"
      ) +
      theme_minimal()
  }
  
  grafico_dist <- NULL
  
  if (make_plot) {
    
    grafico_dist <- ggplot(
      resultados,
      aes(x = prob_threshold, y = distance)
    ) +
      geom_line() +
      geom_point(size = 2) +
      geom_point(
        data = escolhido,
        aes(x = prob_threshold, y = distance),
        size = 4
      ) +
      geom_vline(
        xintercept = escolhido$prob_threshold,
        linetype = "dashed"
      ) +
      labs(
        title = paste0("Minimum distance to GP curve - ", lob_name),
        subtitle = paste0(
          "Selected threshold = ",
          fmt(round(u_hat, 2)),
          " | quantile = ",
          round(escolhido$prob_threshold, 3)
        ),
        x = "Threshold probability",
        y = "Euclidean distance to GP curve"
      ) +
      theme_minimal()
  }
  
  list(
    summary = resumo,
    threshold_table = resultados,
    selected = escolhido,
    fit = fit,
    return_levels = return_levels,
    plot = grafico_lmrd,
    distance_plot = grafico_dist
  )
}

# MTPL Bodily Injury
res_041_98 <- alrsm_mev_custom(
  x = lob_041_MTPL_Bodily_Injury$Incurred_Claims,
  lob_name = "MTPL Bodily Injury",
  q_min = 0.25,
  q_max = 0.98,
  I = 30,
  min_excessos = 10,
  fit_method = "Grimshaw",
  return_probs = c(0.01, 0.001),
  make_plot = TRUE
)

res_041_98$summary
res_041_98$threshold_table
res_041_98$return_levels
res_041_98$plot
res_041_98$distance_plot

# ggsave(
#   filename = "C:/Users/fatim/Downloads/res_041_distancia.pdf",
#   plot = res_041_98$distance_plot,
#   width = 8,
#   height = 6
# )

# Health
res_health_98 <- alrsm_mev_custom(
  x = lob_011_Health$Incurred_Claims,
  lob_name = "Health",
  q_min = 0.25,
  q_max = 0.98,
  I = 30,
  min_excessos = 10,
  fit_method = "Grimshaw",
  return_probs = c(0.01, 0.001),
  make_plot = TRUE
)

res_health_98$summary
res_health_98$threshold_table
res_health_98$return_levels
res_health_98$plot
res_health_98$distance_plot

# ggsave(
#   filename = "C:/Users/fatim/Downloads/res_011_distancia.pdf",
#   plot = res_health_98$distance_plot,
#   width = 8,
#   height = 6
# )

# MTPL Material Damage
res_material_98 <- alrsm_mev_custom(
  x = lob_042_MTPL_Material_Damage$Incurred_Claims,
  lob_name = "MTPL Material Damage",
  q_min = 0.25,
  q_max = 0.98,
  I = 30,
  min_excessos = 10,
  fit_method = "Grimshaw",
  return_probs = c(0.01, 0.001),
  make_plot = TRUE
)

res_material_98$summary
res_material_98$threshold_table
res_material_98$return_levels
res_material_98$plot
res_material_98$distance_plot

# ggsave(
#   filename = "C:/Users/fatim/Downloads/res_042_distancia.pdf",
#   plot = res_material_98$distance_plot,
#   width = 8,
#   height = 6
# )

resumo_alrsm_98 <- rbind(
  cbind(LOB = "MTPL Bodily Injury", res_041_98$summary),
  cbind(LOB = "Health", res_health_98$summary),
  cbind(LOB = "MTPL Material Damage", res_material_98$summary)
)

resumo_alrsm_98

rl_alrsm_98 <- rbind(
  cbind(LOB = "MTPL Bodily Injury", res_041_98$return_levels),
  cbind(LOB = "Health", res_health_98$return_levels),
  cbind(LOB = "MTPL Material Damage", res_material_98$return_levels)
)

rl_alrsm_98

#graficos melhores

nearest_point_on_gpd_curve <- function(t3, t4,
                                       tau3_lower = -0.999,
                                       tau3_upper = 0.999) {
  
  if (!is.finite(t3) || !is.finite(t4)) {
    return(c(
      proj_t3 = NA_real_,
      proj_t4 = NA_real_,
      distance_proj = NA_real_
    ))
  }
  
  obj <- function(s3) {
    (t3 - s3)^2 + (t4 - gpd_lmoment_curve(s3))^2
  }
  
  opt <- optimize(
    f = obj,
    interval = c(tau3_lower, tau3_upper)
  )
  
  s3_hat <- opt$minimum
  s4_hat <- gpd_lmoment_curve(s3_hat)
  
  c(
    proj_t3 = s3_hat,
    proj_t4 = s4_hat,
    distance_proj = sqrt(opt$objective)
  )
}

add_projection_to_threshold_table <- function(res_obj) {
  
  tab <- res_obj$threshold_table
  
  # remover colunas antigas de projeção, caso já existam
  cols_remover <- c("proj_t3", "proj_t4", "distance_proj")
  tab <- tab[, !(names(tab) %in% cols_remover), drop = FALSE]
  
  proj <- t(mapply(
    FUN = nearest_point_on_gpd_curve,
    t3 = tab$t3,
    t4 = tab$t4
  ))
  
  proj <- as.data.frame(proj)
  
  out <- cbind(tab, proj)
  
  # garantir que não há nomes duplicados
  names(out) <- make.unique(names(out))
  
  out
}

plot_alrsm_lmrd_pretty <- function(res_obj,
                                   lob_name,
                                   label_quantiles_above = 0.75,
                                   show_all_segments = TRUE,
                                   zoom = TRUE) {
  
  tab <- add_projection_to_threshold_table(res_obj)
  
  sel_prob <- res_obj$selected$prob_threshold
  
  sel_row <- tab[
    abs(tab$prob_threshold - sel_prob) < 1e-10,
  ]
  
  curve_df <- data.frame(
    tau3 = seq(0, 0.85, length.out = 500)
  )
  curve_df$tau4 <- gpd_lmoment_curve(curve_df$tau3)
  
  tab_labels <- tab[
    tab$prob_threshold >= label_quantiles_above &
      is.finite(tab$t3) &
      is.finite(tab$t4),
  ]
  
  p <- ggplot() +
    geom_line(
      data = curve_df,
      aes(x = tau3, y = tau4),
      linewidth = 0.8,
      colour = "grey60"
    )
  
  if (show_all_segments) {
    p <- p +
      geom_segment(
        data = tab[
          is.finite(tab$t3) &
            is.finite(tab$t4) &
            is.finite(tab$proj_t3) &
            is.finite(tab$proj_t4),
        ],
        aes(
          x = t3,
          y = t4,
          xend = proj_t3,
          yend = proj_t4
        ),
        linewidth = 0.4,
        alpha = 0.5,
        colour = "steelblue"
      )
  }
  
  p <- p +
    geom_point(
      data = tab[
        is.finite(tab$t3) &
          is.finite(tab$t4),
      ],
      aes(x = t3, y = t4),
      shape = 1,
      size = 2,
      stroke = 0.7,
      colour = "black"
    ) +
    geom_point(
      data = tab[
        is.finite(tab$proj_t3) &
          is.finite(tab$proj_t4),
      ],
      aes(x = proj_t3, y = proj_t4),
      size = 1.7,
      colour = "blue"
    ) +
    geom_point(
      data = sel_row,
      aes(x = t3, y = t4),
      size = 3.4,
      colour = "red"
    ) +
    geom_text(
      data = tab_labels,
      aes(x = t3, y = t4, label = round(prob_threshold, 3)),
      vjust = -0.7,
      size = 3
    ) +
    geom_text(
      data = sel_row,
      aes(x = t3, y = t4, label = round(prob_threshold, 3)),
      vjust = -1.1,
      fontface = "bold",
      size = 3.5,
      colour = "red"
    ) +
    labs(
      title = paste0("L-moment Ratio Diagram - ", lob_name),
      subtitle = paste0(
        "Selected threshold = ",
        scales::comma(round(res_obj$summary$threshold, 2)),
        " | quantile = ",
        round(res_obj$summary$prob_threshold, 3)
      ),
      x = expression(t[3]),
      y = expression(t[4])
    ) +
    theme_classic()
  
  if (zoom) {
    x_min <- min(c(tab$t3, tab$proj_t3), na.rm = TRUE) - 0.03
    x_max <- max(c(tab$t3, tab$proj_t3), na.rm = TRUE) + 0.03
    y_min <- min(c(tab$t4, tab$proj_t4), na.rm = TRUE) - 0.03
    y_max <- max(c(tab$t4, tab$proj_t4), na.rm = TRUE) + 0.03
    
    p <- p +
      coord_cartesian(
        xlim = c(x_min, x_max),
        ylim = c(max(0, y_min), y_max)
      )
  }
  
  p
}

res_041_alrsm <- plot_alrsm_lmrd_pretty(
  res_obj = res_041_98,
  lob_name = "MTPL Bodily Injury"
)

res_041_alrsm

# ggsave(
#   filename = "C:/Users/fatim/Downloads/res_041_alrsm.pdf",
#   plot = res_041_alrsm,
#   width = 8,
#   height = 6
# )

res_011_alrsm <- plot_alrsm_lmrd_pretty(
  res_obj = res_health_98,
  lob_name = "Health"
)

res_011_alrsm

# ggsave(
#   filename = "C:/Users/fatim/Downloads/res_011_alrsm.pdf",
#   plot = res_011_alrsm,
#   width = 8,
#   height = 6
# )

res_042_alrsm <- plot_alrsm_lmrd_pretty(
  res_obj = res_material_98,
  lob_name = "MTPL Material Damage"
)

res_042_alrsm

# ggsave(
#   filename = "C:/Users/fatim/Downloads/res_042_alrsm.pdf",
#   plot = res_042_alrsm,
#   width = 8,
#   height = 6
# )

##mixture models

#behrens 2004

#estudo da bulk
analise_bulk_completa <- function(dados_lob, nome_lob,
                                  claim_var = "Incurred_Claims",
                                  prob_u = 0.90,
                                  boot = 1000,
                                  make_plots = TRUE) {
  
  # -----------------------------
  # 1. Preparar dados
  # -----------------------------
  
  x <- dados_lob %>%
    filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    ) %>%
    pull(.data[[claim_var]])
  
  u <- as.numeric(quantile(x, prob_u, na.rm = TRUE))
  
  bulk <- x[x <= u]
  bulk <- bulk[is.finite(bulk) & bulk > 0]
  
  if (length(bulk) < 30) {
    stop("Poucas observações no bulk.")
  }
  
  # -----------------------------
  # 2. Ajustar distribuições
  # -----------------------------
  
  fits <- list()
  
  fits$Normal <- tryCatch(
    fitdist(bulk, "norm"),
    error = function(e) {
      message("Normal falhou: ", e$message)
      NULL
    }
  )
  
  fits$Weibull <- tryCatch(
    fitdist(bulk, "weibull"),
    error = function(e) {
      message("Weibull falhou: ", e$message)
      NULL
    }
  )
  
  fits$Lognormal <- tryCatch(
    fitdist(bulk, "lnorm"),
    error = function(e) {
      message("Lognormal falhou: ", e$message)
      NULL
    }
  )
  
  fits$Gamma_MLE <- tryCatch(
    fitdist(
      bulk,
      "gamma",
      start = list(
        shape = mean(bulk)^2 / var(bulk),
        rate  = mean(bulk) / var(bulk)
      )
    ),
    error = function(e) {
      message("Gamma MLE falhou: ", e$message)
      NULL
    }
  )
  
  fits$Gamma_MME <- tryCatch(
    fitdist(
      bulk,
      "gamma",
      method = "mme"
    ),
    error = function(e) {
      message("Gamma MME falhou: ", e$message)
      NULL
    }
  )
  
  fits$Loglogistic <- tryCatch(
    fitdist(
      bulk,
      "llogis",
      start = list(
        shape = 2,
        scale = median(bulk)
      )
    ),
    error = function(e) {
      message("Loglogistic falhou: ", e$message)
      NULL
    }
  )
  
  fits$Burr <- tryCatch(
    fitdist(
      bulk,
      "burr",
      start = list(
        shape1 = 1,
        shape2 = 2,
        rate = 1 / mean(bulk)
      )
    ),
    error = function(e) {
      message("Burr falhou: ", e$message)
      NULL
    }
  )
  
  fits$InvGaussian <- tryCatch(
    fitdist(
      bulk,
      "invgauss",
      start = list(
        mean = mean(bulk),
        shape = mean(bulk)^3 / var(bulk)
      )
    ),
    error = function(e) {
      message("Inverse Gaussian falhou: ", e$message)
      NULL
    }
  )
  
  fits <- fits[!sapply(fits, is.null)]
  
  if (length(fits) == 0) {
    stop("Nenhuma distribuição conseguiu ser ajustada.")
  }
  
  nomes <- names(fits)
  
  # -----------------------------
  # 3. Gráficos
  # -----------------------------
  
  if (make_plots) {
    
    oldpar <- par(
      mfrow = c(2, 3),
      mar = c(4, 4, 3, 1),
      oma = c(0, 0, 3, 0)
    )
    
    descdist(bulk, boot = boot)
    
    denscomp(
      fits,
      legendtext = nomes,
      main = "Density"
    )
    
    qqcomp(
      fits,
      legendtext = nomes,
      main = "Q-Q plot"
    )
    
    cdfcomp(
      fits,
      legendtext = nomes,
      main = "CDF"
    )
    
    ppcomp(
      fits,
      legendtext = nomes,
      main = "P-P plot"
    )
    
    mtext(
      paste("Bulk distribution diagnostics -", nome_lob),
      outer = TRUE,
      cex = 1.3,
      font = 2
    )
    
    par(oldpar)
  }
  
  # -----------------------------
  # 4. Goodness-of-fit
  # -----------------------------
  
  gof <- gofstat(fits)
  
  print(gof)
  
  # Tabela resumo
  resumo <- data.frame(
    distribuicao = names(fits),
    KS = as.numeric(gof$ks),
    CvM = as.numeric(gof$cvm),
    AD = as.numeric(gof$ad),
    AIC = as.numeric(gof$aic),
    BIC = as.numeric(gof$bic)
  )
  
  resumo <- resumo %>%
    arrange(KS)
  
  # -----------------------------
  # 5. Escolhas automáticas
  # -----------------------------
  
  melhor_KS <- resumo$distribuicao[which.min(resumo$KS)]
  melhor_CvM <- resumo$distribuicao[which.min(resumo$CvM)]
  melhor_AD <- resumo$distribuicao[which.min(resumo$AD)]
  melhor_AIC <- resumo$distribuicao[which.min(resumo$AIC)]
  melhor_BIC <- resumo$distribuicao[which.min(resumo$BIC)]
  
  escolhas <- data.frame(
    criterio = c("KS", "CvM", "AD", "AIC", "BIC"),
    melhor_distribuicao = c(
      melhor_KS,
      melhor_CvM,
      melhor_AD,
      melhor_AIC,
      melhor_BIC
    )
  )
  
  print(resumo)
  print(escolhas)
  
  return(list(
    LOB = nome_lob,
    prob_threshold = prob_u,
    threshold = u,
    n_total = length(x),
    n_bulk = length(bulk),
    bulk = bulk,
    fits = fits,
    gof = gof,
    resumo = resumo,
    escolhas = escolhas
  ))
}

bulk_041_full <- analise_bulk_completa(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  prob_u = 0.90
)

# pdf(
#   file = "C:/Users/fatim/Downloads/bulk041.pdf",
#   width = 8,
#   height = 6
# )
# 
# bulk_041_full <- analise_bulk_completa(
#   dados_lob = lob_041_MTPL_Bodily_Injury,
#   nome_lob = "MTPL Bodily Injury",
#   prob_u = 0.90
# )
# 
# dev.off()

#escolher weibull

bulk_011_full <- analise_bulk_completa(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  prob_u = 0.90
)

# pdf(
#   file = "C:/Users/fatim/Downloads/bulk011.pdf",
#   width = 8,
#   height = 6
# )
# 
# bulk_011_full <- analise_bulk_completa(
#   dados_lob = lob_011_Health,
#   nome_lob = "Health",
#   prob_u = 0.90
# )
# 
# dev.off()

#escolher weibull


bulk_042_full <- analise_bulk_completa(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  prob_u = 0.90
)

# pdf(
#   file = "C:/Users/fatim/Downloads/bulk042.pdf",
#   width = 8,
#   height = 6
# )
# 
# bulk_042_full <- analise_bulk_completa(
#   dados_lob = lob_042_MTPL_Material_Damage,
#   nome_lob = "MTPL Material Damage",
#   prob_u = 0.90
# )
# 
# dev.off()

#escolher gamma mme


#continuaçao modelo


# Densidade log da GPD para excessos y = x - u

log_gpd_density_beh <- function(y, sigma, xi) {
  
  if (sigma <= 0) {
    return(rep(-Inf, length(y)))
  }
  
  if (any(y < 0)) {
    return(rep(-Inf, length(y)))
  }
  
  if (abs(xi) < 1e-8) {
    return(-log(sigma) - y / sigma)
  }
  
  z <- 1 + xi * y / sigma
  
  if (any(z <= 0)) {
    return(rep(-Inf, length(y)))
  }
  
  -log(sigma) - (1 + 1 / xi) * log(z)
}

bulk_logdens_cdf <- function(x_bulk, u, bulk_dist, bulk_par) {
  
  bulk_dist <- match.arg(bulk_dist, choices = c("weibull", "gamma"))
  
  if (bulk_dist == "weibull") {
    
    shape <- bulk_par[1]
    scale <- bulk_par[2]
    
    if (shape <= 0 || scale <= 0) return(NULL)
    
    log_h <- dweibull(
      x_bulk,
      shape = shape,
      scale = scale,
      log = TRUE
    )
    
    H_u <- pweibull(
      u,
      shape = shape,
      scale = scale
    )
  }
  
  if (bulk_dist == "gamma") {
    
    shape <- bulk_par[1]
    rate <- bulk_par[2]
    
    if (shape <= 0 || rate <= 0) return(NULL)
    
    log_h <- dgamma(
      x_bulk,
      shape = shape,
      rate = rate,
      log = TRUE
    )
    
    H_u <- pgamma(
      u,
      shape = shape,
      rate = rate
    )
  }
  
  if (any(!is.finite(log_h))) return(NULL)
  if (!is.finite(H_u) || H_u <= 0 || H_u >= 1) return(NULL)
  
  list(
    log_h = log_h,
    H_u = H_u
  )
}


# 4. Log-likelihood Behrens

loglik_behrens <- function(x, u, sigma, xi,
                           bulk_dist,
                           bulk_par) {
  
  if (sigma <= 0) return(-Inf)
  
  A <- x < u
  B <- x >= u
  
  x_bulk <- x[A]
  y_tail <- x[B] - u
  
  if (length(x_bulk) < 5 || length(y_tail) < 5) {
    return(-Inf)
  }
  
  bulk_obj <- bulk_logdens_cdf(
    x_bulk = x_bulk,
    u = u,
    bulk_dist = bulk_dist,
    bulk_par = bulk_par
  )
  
  if (is.null(bulk_obj)) {
    return(-Inf)
  }
  
  log_gpd <- log_gpd_density_beh(
    y = y_tail,
    sigma = sigma,
    xi = xi
  )
  
  if (any(!is.finite(log_gpd))) {
    return(-Inf)
  }
  
  ll <- sum(bulk_obj$log_h) +
    length(y_tail) * log(1 - bulk_obj$H_u) +
    sum(log_gpd)
  
  if (!is.finite(ll)) return(-Inf)
  
  ll
}


# 5. Priors

log_prior_bulk <- function(bulk_dist, bulk_par, x) {
  
  bulk_dist <- match.arg(bulk_dist, choices = c("weibull", "gamma"))
  
  if (bulk_dist == "weibull") {
    
    shape <- bulk_par[1]
    scale <- bulk_par[2]
    
    if (shape <= 0 || scale <= 0) return(-Inf)
    
    return(
      dlnorm(shape, meanlog = 0, sdlog = 3, log = TRUE) +
        dlnorm(scale, meanlog = log(mean(x)), sdlog = 3, log = TRUE)
    )
  }
  
  if (bulk_dist == "gamma") {
    
    shape <- bulk_par[1]
    rate <- bulk_par[2]
    
    if (shape <= 0 || rate <= 0) return(-Inf)
    
    return(
      dlnorm(shape, meanlog = 0, sdlog = 3, log = TRUE) +
        dlnorm(rate, meanlog = log(1 / mean(x)), sdlog = 3, log = TRUE)
    )
  }
  
  -Inf
}


log_prior_behrens <- function(u, sigma, xi, bulk_par,
                              x, u_grid, bulk_dist) {
  
  # Prior discreta uniforme para u
  if (!(u %in% u_grid)) return(-Inf)
  lp_u <- -log(length(u_grid))
  
  # Prior vaga para sigma
  if (sigma <= 0) return(-Inf)
  
  lp_sigma <- dlnorm(
    sigma,
    meanlog = log(sd(x)),
    sdlog = 2,
    log = TRUE
  )
  
  # Prior vaga para xi
  lp_xi <- dnorm(
    xi,
    mean = 0,
    sd = 1,
    log = TRUE
  )
  
  # Prior do bulk
  lp_bulk <- log_prior_bulk(
    bulk_dist = bulk_dist,
    bulk_par = bulk_par,
    x = x
  )
  
  lp <- lp_u + lp_sigma + lp_xi + lp_bulk
  
  if (!is.finite(lp)) return(-Inf)
  
  lp
}


log_posterior_behrens <- function(x, u, sigma, xi,
                                  bulk_dist, bulk_par,
                                  u_grid) {
  
  lp <- log_prior_behrens(
    u = u,
    sigma = sigma,
    xi = xi,
    bulk_par = bulk_par,
    x = x,
    u_grid = u_grid,
    bulk_dist = bulk_dist
  )
  
  if (!is.finite(lp)) return(-Inf)
  
  ll <- loglik_behrens(
    x = x,
    u = u,
    sigma = sigma,
    xi = xi,
    bulk_dist = bulk_dist,
    bulk_par = bulk_par
  )
  
  if (!is.finite(ll)) return(-Inf)
  
  lp + ll
}

# 6. Inicialização dos parâmetros do bulk

init_bulk_par <- function(x, u, bulk_dist) {
  
  bulk_dist <- match.arg(bulk_dist, choices = c("weibull", "gamma"))
  
  bulk <- x[x < u]
  bulk <- bulk[is.finite(bulk) & bulk > 0]
  
  if (length(bulk) < 20) {
    stop("Poucas observações no bulk para inicializar.")
  }
  
  if (bulk_dist == "weibull") {
    
    fit0 <- tryCatch(
      fitdist(bulk, "weibull"),
      error = function(e) NULL
    )
    
    if (!is.null(fit0)) {
      return(c(
        shape = as.numeric(fit0$estimate["shape"]),
        scale = as.numeric(fit0$estimate["scale"])
      ))
    }
    
    return(c(
      shape = 1,
      scale = mean(bulk)
    ))
  }
  
  if (bulk_dist == "gamma") {
    
    shape0 <- mean(bulk)^2 / var(bulk)
    rate0 <- mean(bulk) / var(bulk)
    
    if (!is.finite(shape0) || shape0 <= 0) shape0 <- 1
    if (!is.finite(rate0) || rate0 <= 0) rate0 <- 1 / mean(bulk)
    
    return(c(
      shape = shape0,
      rate = rate0
    ))
  }
}

# 7. Inicialização da GPD

init_tail_par <- function(x, u) {
  
  y <- x[x >= u] - u
  y <- y[is.finite(y) & y >= 0]
  
  if (length(y) < 20) {
    return(c(sigma = sd(x), xi = 0.1))
  }
  
  nll <- function(par) {
    
    sigma <- exp(par[1])
    xi <- par[2]
    
    lg <- log_gpd_density_beh(
      y = y,
      sigma = sigma,
      xi = xi
    )
    
    if (any(!is.finite(lg))) return(1e100)
    
    -sum(lg)
  }
  
  fit <- tryCatch(
    optim(
      par = c(log(sd(y)), 0.1),
      fn = nll,
      method = "Nelder-Mead",
      control = list(maxit = 5000)
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(c(sigma = sd(y), xi = 0.1))
  }
  
  sigma_hat <- exp(fit$par[1])
  xi_hat <- fit$par[2]
  
  if (!is.finite(sigma_hat) || sigma_hat <= 0) {
    sigma_hat <- sd(y)
  }
  
  if (!is.finite(xi_hat)) {
    xi_hat <- 0.1
  }
  
  c(
    sigma = sigma_hat,
    xi = xi_hat
  )
}

# 8. Posterior na escala transformada

logpost_transformed <- function(x, u, log_sigma, xi,
                                bulk_dist, log_bulk_par,
                                u_grid) {
  
  sigma <- exp(log_sigma)
  bulk_par <- exp(log_bulk_par)
  
  lp <- log_posterior_behrens(
    x = x,
    u = u,
    sigma = sigma,
    xi = xi,
    bulk_dist = bulk_dist,
    bulk_par = bulk_par,
    u_grid = u_grid
  )
  
  if (!is.finite(lp)) return(-Inf)
  
  # Jacobiano:
  # sigma = exp(log_sigma)
  # bulk_par = exp(log_bulk_par)
  lp + log_sigma + sum(log_bulk_par)
}

# 9. Proposta discreta para u

propose_u_index <- function(current_idx, m, step = 3) {
  
  cand <- seq(
    from = max(1, current_idx - step),
    to = min(m, current_idx + step)
  )
  
  new_idx <- sample(cand, size = 1)
  
  log_q_forward <- -log(length(cand))
  
  cand_back <- seq(
    from = max(1, new_idx - step),
    to = min(m, new_idx + step)
  )
  
  log_q_backward <- -log(length(cand_back))
  
  list(
    idx = new_idx,
    log_q_forward = log_q_forward,
    log_q_backward = log_q_backward
  )
}

# 10. Função principal MCMC

mcmc_behrens <- function(dados_lob,
                         nome_lob,
                         bulk_dist,
                         claim_var = "Incurred_Claims",
                         probs_u = seq(0.70, 0.98, by = 0.005),
                         n_iter = 30000,
                         burn = 10000,
                         thin = 5,
                         proposal_sd = list(
                           xi = 0.04,
                           log_sigma = 0.10,
                           log_bulk = c(0.05, 0.05)
                         ),
                         u_step = 3,
                         seed = 123,
                         make_plots = TRUE) {
  
  set.seed(seed)
  
  bulk_dist <- match.arg(
    bulk_dist,
    choices = c("weibull", "gamma")
  )
  
  # -----------------------------
  # Dados
  # -----------------------------
  
  x <- dados_lob %>%
    filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    ) %>%
    pull(.data[[claim_var]])
  
  x <- sort(as.numeric(x))
  x <- x[is.finite(x) & x > 0]
  
  n <- length(x)
  
  if (n < 100) {
    stop("Amostra demasiado pequena para este MCMC.")
  }
  
  # -----------------------------
  # Grelha discreta para u
  # -----------------------------
  
  u_grid <- as.numeric(
    quantile(
      x,
      probs = probs_u,
      na.rm = TRUE,
      type = 7
    )
  )
  
  u_grid <- sort(unique(u_grid))
  
  probs_u_grid <- sapply(
    u_grid,
    function(u) mean(x <= u)
  )
  
  m_u <- length(u_grid)
  
  if (m_u < 5) {
    stop("A grelha de thresholds tem poucos valores únicos.")
  }
  
  # -----------------------------
  # Valores iniciais
  # -----------------------------
  
  idx_u <- which.min(abs(probs_u_grid - 0.90))
  u <- u_grid[idx_u]
  
  bulk_par <- init_bulk_par(
    x = x,
    u = u,
    bulk_dist = bulk_dist
  )
  
  tail_par <- init_tail_par(
    x = x,
    u = u
  )
  
  log_sigma <- log(as.numeric(tail_par["sigma"]))
  xi <- as.numeric(tail_par["xi"])
  log_bulk_par <- log(as.numeric(bulk_par))
  
  if (length(proposal_sd$log_bulk) == 1) {
    proposal_sd$log_bulk <- rep(
      proposal_sd$log_bulk,
      length(log_bulk_par)
    )
  }
  
  if (length(proposal_sd$log_bulk) != length(log_bulk_par)) {
    stop("proposal_sd$log_bulk tem comprimento errado.")
  }
  
  lp_current <- logpost_transformed(
    x = x,
    u = u,
    log_sigma = log_sigma,
    xi = xi,
    bulk_dist = bulk_dist,
    log_bulk_par = log_bulk_par,
    u_grid = u_grid
  )
  
  if (!is.finite(lp_current)) {
    stop("Valor inicial tem posterior -Inf. Muda probs_u ou bulk_dist.")
  }
  
  # -----------------------------
  # Guardar cadeia
  # -----------------------------
  
  chain <- matrix(
    NA_real_,
    nrow = n_iter,
    ncol = 5 + length(log_bulk_par)
  )
  
  colnames(chain) <- c(
    "u",
    "prob_threshold",
    "sigma",
    "xi",
    "logpost",
    paste0("bulk_par_", seq_along(log_bulk_par))
  )
  
  acc <- c(
    xi = 0,
    sigma = 0,
    u = 0,
    bulk = 0
  )
  
  # ============================================================
  # MCMC
  # ============================================================
  
  for (iter in seq_len(n_iter)) {
    
    # ----------------------------------------------------------
    # Bloco 1: xi
    # ----------------------------------------------------------
    
    xi_prop <- rnorm(
      1,
      mean = xi,
      sd = proposal_sd$xi
    )
    
    lp_prop <- logpost_transformed(
      x = x,
      u = u,
      log_sigma = log_sigma,
      xi = xi_prop,
      bulk_dist = bulk_dist,
      log_bulk_par = log_bulk_par,
      u_grid = u_grid
    )
    
    log_alpha <- lp_prop - lp_current
    
    if (is.finite(log_alpha) && log(runif(1)) < log_alpha) {
      xi <- xi_prop
      lp_current <- lp_prop
      acc["xi"] <- acc["xi"] + 1
    }
    
    
    # ----------------------------------------------------------
    # Bloco 2: log(sigma)
    # ----------------------------------------------------------
    
    log_sigma_prop <- rnorm(
      1,
      mean = log_sigma,
      sd = proposal_sd$log_sigma
    )
    
    lp_prop <- logpost_transformed(
      x = x,
      u = u,
      log_sigma = log_sigma_prop,
      xi = xi,
      bulk_dist = bulk_dist,
      log_bulk_par = log_bulk_par,
      u_grid = u_grid
    )
    
    log_alpha <- lp_prop - lp_current
    
    if (is.finite(log_alpha) && log(runif(1)) < log_alpha) {
      log_sigma <- log_sigma_prop
      lp_current <- lp_prop
      acc["sigma"] <- acc["sigma"] + 1
    }
    
    
    # ----------------------------------------------------------
    # Bloco 3: u discreto
    # ----------------------------------------------------------
    
    prop_u <- propose_u_index(
      current_idx = idx_u,
      m = m_u,
      step = u_step
    )
    
    idx_u_prop <- prop_u$idx
    u_prop <- u_grid[idx_u_prop]
    
    lp_prop <- logpost_transformed(
      x = x,
      u = u_prop,
      log_sigma = log_sigma,
      xi = xi,
      bulk_dist = bulk_dist,
      log_bulk_par = log_bulk_par,
      u_grid = u_grid
    )
    
    log_alpha <- lp_prop - lp_current +
      prop_u$log_q_backward -
      prop_u$log_q_forward
    
    if (is.finite(log_alpha) && log(runif(1)) < log_alpha) {
      idx_u <- idx_u_prop
      u <- u_prop
      lp_current <- lp_prop
      acc["u"] <- acc["u"] + 1
    }
    
    
    # ----------------------------------------------------------
    # Bloco 4: parâmetros do bulk
    # ----------------------------------------------------------
    
    log_bulk_prop <- rnorm(
      length(log_bulk_par),
      mean = log_bulk_par,
      sd = proposal_sd$log_bulk
    )
    
    lp_prop <- logpost_transformed(
      x = x,
      u = u,
      log_sigma = log_sigma,
      xi = xi,
      bulk_dist = bulk_dist,
      log_bulk_par = log_bulk_prop,
      u_grid = u_grid
    )
    
    log_alpha <- lp_prop - lp_current
    
    if (is.finite(log_alpha) && log(runif(1)) < log_alpha) {
      log_bulk_par <- log_bulk_prop
      lp_current <- lp_prop
      acc["bulk"] <- acc["bulk"] + 1
    }
    
    
    # ----------------------------------------------------------
    # Guardar iteração
    # ----------------------------------------------------------
    
    chain[iter, ] <- c(
      u,
      mean(x <= u),
      exp(log_sigma),
      xi,
      lp_current,
      exp(log_bulk_par)
    )
    
    if (iter %% 5000 == 0) {
      message("Iteração ", iter, " / ", n_iter)
    }
  }
  
  # -----------------------------
  # Taxas de aceitação
  # -----------------------------
  
  acc_rate <- acc / n_iter
  
  # -----------------------------
  # Burn-in e thinning
  # -----------------------------
  
  keep <- seq(
    from = burn + 1,
    to = n_iter,
    by = thin
  )
  
  post <- as.data.frame(chain[keep, , drop = FALSE])
  
  # -----------------------------
  # Resumo posterior
  # -----------------------------
  
  resumo <- post %>%
    summarise(
      LOB = nome_lob,
      n = n,
      bulk_dist = bulk_dist,
      
      u_mean = mean(u),
      u_median = median(u),
      u_q025 = quantile(u, 0.025),
      u_q975 = quantile(u, 0.975),
      
      prob_threshold_mean = mean(prob_threshold),
      prob_threshold_median = median(prob_threshold),
      prob_threshold_q025 = quantile(prob_threshold, 0.025),
      prob_threshold_q975 = quantile(prob_threshold, 0.975),
      
      sigma_mean = mean(sigma),
      sigma_median = median(sigma),
      sigma_q025 = quantile(sigma, 0.025),
      sigma_q975 = quantile(sigma, 0.975),
      
      xi_mean = mean(xi),
      xi_median = median(xi),
      xi_q025 = quantile(xi, 0.025),
      xi_q975 = quantile(xi, 0.975)
    )
  
  print(resumo)
  print(acc_rate)
  
  # -----------------------------
  # Gráficos
  # -----------------------------
  
  if (make_plots) {
    
    print(
      ggplot(post, aes(x = u)) +
        geom_histogram(bins = 40) +
        labs(
          title = paste("Posterior do threshold u -", nome_lob),
          x = "u",
          y = "Frequência"
        ) +
        theme_minimal()
    )
    
    print(
      ggplot(post, aes(x = prob_threshold)) +
        geom_histogram(bins = 40) +
        labs(
          title = paste("Posterior de P(X <= u) -", nome_lob),
          x = "Probabilidade do threshold",
          y = "Frequência"
        ) +
        theme_minimal()
    )
    
    print(
      ggplot(post, aes(x = xi)) +
        geom_histogram(bins = 40) +
        labs(
          title = paste("Posterior de xi -", nome_lob),
          x = expression(xi),
          y = "Frequência"
        ) +
        theme_minimal()
    )
    
    print(
      ggplot(post, aes(x = sigma)) +
        geom_histogram(bins = 40) +
        labs(
          title = paste("Posterior de sigma -", nome_lob),
          x = expression(sigma),
          y = "Frequência"
        ) +
        theme_minimal()
    )
    
    print(
      ggplot(post, aes(x = seq_along(u), y = u)) +
        geom_line() +
        labs(
          title = paste("Traceplot de u -", nome_lob),
          x = "Iteração pós burn-in",
          y = "u"
        ) +
        theme_minimal()
    )
    
    print(
      ggplot(post, aes(x = seq_along(xi), y = xi)) +
        geom_line() +
        labs(
          title = paste("Traceplot de xi -", nome_lob),
          x = "Iteração pós burn-in",
          y = expression(xi)
        ) +
        theme_minimal()
    )
    
    print(
      ggplot(post, aes(x = seq_along(sigma), y = sigma)) +
        geom_line() +
        labs(
          title = paste("Traceplot de sigma -", nome_lob),
          x = "Iteração pós burn-in",
          y = expression(sigma)
        ) +
        theme_minimal()
    )
  }
  
  return(list(
    LOB = nome_lob,
    n = n,
    bulk_dist = bulk_dist,
    u_grid = u_grid,
    probs_u_grid = probs_u_grid,
    chain = as.data.frame(chain),
    posterior = post,
    resumo = resumo,
    acc_rate = acc_rate,
    settings = list(
      probs_u = probs_u,
      n_iter = n_iter,
      burn = burn,
      thin = thin,
      proposal_sd = proposal_sd,
      u_step = u_step,
      seed = seed
    )
  ))
}

# 11. Diagnóstico simples da cadeia

diagnostico_mcmc_behrens <- function(obj) {
  
  post <- obj$posterior
  
  cat("\n==============================\n")
  cat("LOB:", obj$LOB, "\n")
  cat("Bulk:", obj$bulk_dist, "\n")
  cat("==============================\n\n")
  
  cat("Taxas de aceitação:\n")
  print(obj$acc_rate)
  
  cat("\nResumo posterior:\n")
  print(obj$resumo)
  
  par(mfrow = c(2, 3))
  
  plot(post$u, type = "l", main = "Trace u", ylab = "u", xlab = "Iteração")
  plot(post$xi, type = "l", main = "Trace xi", ylab = "xi", xlab = "Iteração")
  plot(post$sigma, type = "l", main = "Trace sigma", ylab = "sigma", xlab = "Iteração")
  
  hist(post$u, breaks = 40, main = "Posterior u", xlab = "u")
  hist(post$xi, breaks = 40, main = "Posterior xi", xlab = "xi")
  hist(post$sigma, breaks = 40, main = "Posterior sigma", xlab = "sigma")
  
  par(mfrow = c(1, 1))
}

# 12. Quantis extremos posteriores

quantis_posteriores_behrens <- function(obj,
                                        probs = c(0.99, 0.995, 0.999)) {
  
  post <- obj$posterior
  
  out <- lapply(probs, function(p) {
    
    q_post <- numeric(nrow(post))
    
    for (i in seq_len(nrow(post))) {
      
      u <- post$u[i]
      pu <- post$prob_threshold[i]
      sigma <- post$sigma[i]
      xi <- post$xi[i]
      
      # Queremos q_p tal que P(X <= q_p) = p
      # Para p > P(X <= u):
      # p = pu + (1 - pu) * GPD(q - u)
      # Logo:
      # GPD(q - u) = (p - pu) / (1 - pu)
      
      if (p <= pu) {
        q_post[i] <- NA_real_
      } else {
        
        g_prob <- (p - pu) / (1 - pu)
        
        if (g_prob <= 0 || g_prob >= 1) {
          q_post[i] <- NA_real_
        } else {
          
          if (abs(xi) < 1e-8) {
            q_post[i] <- u - sigma * log(1 - g_prob)
          } else {
            q_post[i] <- u + sigma / xi * ((1 - g_prob)^(-xi) - 1)
          }
        }
      }
    }
    
    q_post <- q_post[is.finite(q_post)]
    
    data.frame(
      prob = p,
      mean = mean(q_post),
      median = median(q_post),
      q025 = quantile(q_post, 0.025),
      q975 = quantile(q_post, 0.975)
    )
  })
  
  bind_rows(out)
}

# 13. Aplicação às LOBs

# MTPL Bodily Injury

mcmc_041 <- mcmc_behrens(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  bulk_dist = "weibull",
  probs_u = seq(0.8, 0.995, by = 0.005),
  n_iter = 50000,
  burn = 20000, #interaçoes descartadas
  thin = 5, #guarda 1 iteraçao a cada 5
  proposal_sd = list(
    xi = 0.06,
    log_sigma = 0.10,
    log_bulk = c(0.04, 0.04)
  ),
  u_step = 3,
  seed = 123,
  make_plots = TRUE
)


# Health

mcmc_011 <- mcmc_behrens(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  bulk_dist = "weibull",
  probs_u = seq(0.80, 0.98, by = 0.005),
  n_iter = 50000,
  burn = 20000,
  thin = 5,
  proposal_sd = list(
    xi = 0.04,
    log_sigma = 0.10,
    log_bulk = c(0.04, 0.04)
  ),
  u_step = 3,
  seed = 123,
  make_plots = TRUE
)

# MTPL Material Damage

mcmc_042 <- mcmc_behrens(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  bulk_dist = "gamma",
  probs_u = seq(0.8, 0.995, by = 0.005),
  n_iter = 50000,
  burn = 20000,
  thin = 5,
  proposal_sd = list(
    xi = 0.04,
    log_sigma = 0.10,
    log_bulk = c(0.02, 0.02)
  ),
  u_step = 5,
  seed = 123,
  make_plots = TRUE
)


diagnostico_mcmc_behrens(mcmc_041)
diagnostico_mcmc_behrens(mcmc_011)
diagnostico_mcmc_behrens(mcmc_042)

# 17. Quantis extremos posteriores

quantis_041 <- quantis_posteriores_behrens(
  mcmc_041,
  probs = c(0.99, 0.995, 0.999)
)

quantis_011 <- quantis_posteriores_behrens(
  mcmc_011,
  probs = c(0.99, 0.995, 0.999)
)

quantis_042 <- quantis_posteriores_behrens(
  mcmc_042,
  probs = c(0.99, 0.995, 0.999)
)

quantis_041
quantis_011
quantis_042

##carreau

# 1. Função erro Erf

erf_base <- function(x) {
  2 * pnorm(x * sqrt(2)) - 1
}

# 2. Função Lambert W 

lambertW0_num <- function(z, tol = 1e-12, max_iter = 100) {
  
  if (any(z < -1 / exp(1))) {
    stop("z fora do domínio da função Lambert W principal.")
  }
  
  out <- numeric(length(z))
  
  for (i in seq_along(z)) {
    
    zi <- z[i]
    
    if (abs(zi) < .Machine$double.eps) {
      out[i] <- 0
      next
    }
    
    # Valor inicial
    w <- ifelse(zi < 1, zi, log(zi))
    
    for (it in 1:max_iter) {
      
      ew <- exp(w)
      f <- w * ew - zi
      fp <- ew * (w + 1)
      
      if (abs(fp) < .Machine$double.eps) break
      
      w_new <- w - f / fp
      
      if (!is.finite(w_new)) break
      
      if (abs(w_new - w) < tol) {
        w <- w_new
        break
      }
      
      w <- w_new
    }
    
    out[i] <- w
  }
  
  out
}

# 3. Log-densidade da GPD para excessos y >= 0

log_gpd_density <- function(y, beta, xi) {
  
  y <- as.numeric(y)
  beta <- as.numeric(beta)
  xi <- as.numeric(xi)
  
  if (beta <= 0) {
    return(rep(-Inf, length(y)))
  }
  
  if (any(y < 0)) {
    return(rep(-Inf, length(y)))
  }
  
  if (abs(xi) < 1e-8) {
    return(-log(beta) - y / beta)
  }
  
  z <- 1 + xi * y / beta
  
  logdens <- rep(-Inf, length(y))
  ok <- z > 0
  
  logdens[ok] <- -log(beta) - (1 + 1 / xi) * log(z[ok])
  
  logdens
}

# 4. Parâmetros da Pareto híbrida

hybrid_params <- function(mu, sigma, xi) {
  
  mu <- as.numeric(mu)
  sigma <- as.numeric(sigma)
  xi <- as.numeric(xi)
  
  if (!is.finite(mu) || !is.finite(sigma) || !is.finite(xi)) {
    stop("Parâmetros não finitos.")
  }
  
  if (sigma <= 0) {
    stop("sigma deve ser positivo.")
  }
  
  if (xi <= -1) {
    stop("xi deve ser maior do que -1.")
  }
  
  z <- (1 + xi)^2 / (2 * pi)
  Wz <- as.numeric(lambertW0_num(z))
  
  if (!is.finite(Wz) || Wz <= 0) {
    stop("Valor inválido da função Lambert W.")
  }
  
  beta <- sigma * (1 + xi) / sqrt(Wz)
  alpha <- mu + sigma * sqrt(Wz)
  
  gamma <- 1 + 0.5 * (1 + erf_base(sqrt(Wz / 2)))
  
  list(
    mu = mu,
    sigma = sigma,
    xi = xi,
    z = z,
    Wz = Wz,
    alpha = alpha,
    beta = beta,
    gamma = gamma
  )
}

# 5. Log-densidade da Pareto híbrida

log_hybrid_pareto_density <- function(y, mu, sigma, xi) {
  
  y <- as.numeric(y)
  
  hp <- tryCatch(
    hybrid_params(mu = mu, sigma = sigma, xi = xi),
    error = function(e) NULL
  )
  
  if (is.null(hp)) {
    return(rep(-Inf, length(y)))
  }
  
  alpha <- hp$alpha
  beta <- hp$beta
  gamma <- hp$gamma
  
  logdens <- rep(NA_real_, length(y))
  
  idx_body <- y <= alpha
  idx_tail <- y > alpha
  
  # Parte Gaussiana
  if (any(idx_body)) {
    logdens[idx_body] <- dnorm(
      y[idx_body],
      mean = mu,
      sd = sigma,
      log = TRUE
    ) - log(gamma)
  }
  
  # Parte GPD
  if (any(idx_tail)) {
    
    excess <- y[idx_tail] - alpha
    
    logdens[idx_tail] <- log_gpd_density(
      y = excess,
      beta = beta,
      xi = xi
    ) - log(gamma)
  }
  
  logdens
}

# 6. Log-verosimilhança da Pareto híbrida simples

hybrid_loglik <- function(par, y) {
  
  mu <- as.numeric(par[1])
  sigma <- exp(as.numeric(par[2]))
  xi <- exp(as.numeric(par[3]))   # versão xi > 0
  
  logdens <- log_hybrid_pareto_density(
    y = y,
    mu = mu,
    sigma = sigma,
    xi = xi
  )
  
  if (any(!is.finite(logdens))) {
    return(-Inf)
  }
  
  sum(logdens)
}

# 7. Ajuste da Pareto híbrida simples

fit_hybrid_pareto <- function(x,
                              log_transform = TRUE,
                              n_start = 20,
                              seed = 123) {
  
  set.seed(seed)
  
  x <- as.numeric(x)
  x <- x[is.finite(x) & !is.na(x)]
  
  if (log_transform) {
    x <- x[x > 0]
    y <- log(x)
  } else {
    y <- x
  }
  
  n <- length(y)
  
  if (n < 20) {
    stop("Amostra demasiado pequena.")
  }
  
  y_mean <- mean(y)
  y_sd <- sd(y)
  
  if (!is.finite(y_sd) || y_sd <= 0) {
    stop("Desvio-padrão inválido.")
  }
  
  starts <- vector("list", n_start)
  
  starts[[1]] <- c(
    y_mean,
    log(y_sd),
    log(0.2)
  )
  
  for (s in 2:n_start) {
    starts[[s]] <- c(
      rnorm(1, y_mean, y_sd / 2),
      log(y_sd * runif(1, 0.5, 2)),
      log(runif(1, 0.05, 0.8))
    )
  }
  
  fits <- lapply(starts, function(start) {
    
    tryCatch(
      optim(
        par = start,
        fn = function(par) -hybrid_loglik(par, y),
        method = "Nelder-Mead",
        control = list(maxit = 20000)
      ),
      error = function(e) NULL
    )
  })
  
  fits <- Filter(Negate(is.null), fits)
  
  if (length(fits) == 0) {
    stop("Nenhum ajuste convergiu.")
  }
  
  values <- sapply(fits, function(f) f$value)
  best <- fits[[which.min(values)]]
  
  par_hat <- best$par
  
  mu_hat <- as.numeric(par_hat[1])
  sigma_hat <- as.numeric(exp(par_hat[2]))
  xi_hat <- as.numeric(exp(par_hat[3]))
  
  hp <- hybrid_params(
    mu = mu_hat,
    sigma = sigma_hat,
    xi = xi_hat
  )
  
  par_final <- c(
    mu = as.numeric(mu_hat),
    sigma = as.numeric(sigma_hat),
    xi = as.numeric(xi_hat),
    alpha = as.numeric(hp$alpha),
    beta = as.numeric(hp$beta),
    gamma = as.numeric(hp$gamma)
  )
  
  loglik <- as.numeric(-best$value)
  
  k_par <- 3
  
  AIC <- 2 * k_par - 2 * loglik
  BIC <- log(n) * k_par - 2 * loglik
  
  out <- list(
    par = par_final,
    loglik = loglik,
    AIC = AIC,
    BIC = BIC,
    convergence = best$convergence,
    n = n,
    y = y,
    x_original = x,
    log_transform = log_transform,
    fit = best
  )
  
  class(out) <- "hybrid_pareto_fit"
  
  return(out)
}

# 8. Aplicar Pareto híbrida simples a uma LOB

fit_hybrid_lob <- function(dados_lob,
                           nome_lob,
                           claim_var = "Incurred_Claims",
                           log_transform = TRUE,
                           n_start = 20,
                           seed = 123) {
  
  x <- dados_lob %>%
    filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    ) %>%
    pull(.data[[claim_var]])
  
  fit <- fit_hybrid_pareto(
    x = x,
    log_transform = log_transform,
    n_start = n_start,
    seed = seed
  )
  
  resumo <- data.frame(
    LOB = nome_lob,
    n = as.numeric(fit$n),
    log_transform = log_transform,
    mu = as.numeric(fit$par["mu"]),
    sigma = as.numeric(fit$par["sigma"]),
    xi = as.numeric(fit$par["xi"]),
    alpha = as.numeric(fit$par["alpha"]),
    beta = as.numeric(fit$par["beta"]),
    gamma = as.numeric(fit$par["gamma"]),
    loglik = as.numeric(fit$loglik),
    AIC = as.numeric(fit$AIC),
    BIC = as.numeric(fit$BIC),
    convergence = as.numeric(fit$convergence),
    row.names = NULL
  )
  
  if (log_transform) {
    resumo <- resumo %>%
      mutate(
        alpha_original_scale = exp(alpha)
      )
  }
  
  list(
    summary = resumo,
    fit = fit
  )
}

# 9. CDF da Pareto híbrida

phybrid_pareto <- function(y, mu, sigma, xi) {
  
  y <- as.numeric(y)
  
  hp <- hybrid_params(mu = mu, sigma = sigma, xi = xi)
  
  alpha <- hp$alpha
  beta <- hp$beta
  gamma <- hp$gamma
  
  F <- numeric(length(y))
  
  idx_body <- y <= alpha
  idx_tail <- y > alpha
  
  if (any(idx_body)) {
    F[idx_body] <- pnorm(
      y[idx_body],
      mean = mu,
      sd = sigma
    ) / gamma
  }
  
  if (any(idx_tail)) {
    
    excess <- y[idx_tail] - alpha
    
    G <- if (abs(xi) < 1e-8) {
      1 - exp(-excess / beta)
    } else {
      1 - (1 + xi * excess / beta)^(-1 / xi)
    }
    
    F_alpha <- pnorm(alpha, mean = mu, sd = sigma) / gamma
    
    F[idx_tail] <- F_alpha + G / gamma
  }
  
  pmin(pmax(F, 0), 1)
}

# 10. Quantis numéricos da Pareto híbrida

qhybrid_pareto <- function(p, mu, sigma, xi) {
  
  p <- as.numeric(p)
  out <- numeric(length(p))
  
  hp <- hybrid_params(mu = mu, sigma = sigma, xi = xi)
  
  alpha <- hp$alpha
  beta <- hp$beta
  
  for (i in seq_along(p)) {
    
    fun <- function(y) {
      phybrid_pareto(y, mu, sigma, xi) - p[i]
    }
    
    lower <- mu - 20 * sigma
    upper <- alpha + beta * 1000
    
    f_lower <- fun(lower)
    f_upper <- fun(upper)
    
    while (f_upper < 0) {
      upper <- upper * 2
      f_upper <- fun(upper)
    }
    
    out[i] <- uniroot(fun, lower = lower, upper = upper)$root
  }
  
  out
}

# 11. Plot da densidade ajustada

plot_hybrid_fit <- function(fit_obj, title = "Hybrid Pareto fit") {
  
  y <- fit_obj$y
  
  grid <- seq(min(y), max(y), length.out = 1000)
  
  dens <- exp(log_hybrid_pareto_density(
    y = grid,
    mu = as.numeric(fit_obj$par["mu"]),
    sigma = as.numeric(fit_obj$par["sigma"]),
    xi = as.numeric(fit_obj$par["xi"])
  ))
  
  alpha <- as.numeric(fit_obj$par["alpha"])
  
  df_dens <- data.frame(
    y = grid,
    density = dens
  )
  
  ggplot(data.frame(y = y), aes(x = y)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 80,
      fill = "grey80",
      color = "grey40"
    ) +
    geom_line(
      data = df_dens,
      aes(x = y, y = density),
      linewidth = 1
    ) +
    geom_vline(
      xintercept = alpha,
      linetype = "dashed"
    ) +
    labs(
      title = title,
      subtitle = paste0(
        "alpha = ", round(alpha, 4),
        ", xi = ", round(as.numeric(fit_obj$par["xi"]), 4)
      ),
      x = ifelse(fit_obj$log_transform,
                 "log(Incurred Claims)",
                 "Incurred Claims"),
      y = "Density"
    ) +
    theme_minimal()
}

# 12. QQ plot da Pareto híbrida simples

qqplot_hybrid_fit <- function(fit_obj, title = "QQ plot - Hybrid Pareto") {
  
  y <- sort(fit_obj$y)
  n <- length(y)
  
  p_emp <- ppoints(n)
  
  q_theo <- qhybrid_pareto(
    p = p_emp,
    mu = as.numeric(fit_obj$par["mu"]),
    sigma = as.numeric(fit_obj$par["sigma"]),
    xi = as.numeric(fit_obj$par["xi"])
  )
  
  df <- data.frame(
    theoretical = q_theo,
    empirical = y
  )
  
  ggplot(df, aes(x = theoretical, y = empirical)) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(
      title = title,
      x = "Theoretical quantiles",
      y = "Empirical quantiles"
    ) +
    theme_minimal()
}

# 13. Funções auxiliares para mistura

log_sum_exp <- function(a) {
  m <- max(a)
  m + log(sum(exp(a - m)))
}

softmax <- function(eta) {
  e <- exp(eta - max(eta))
  e / sum(e)
}

# 14. Log-verosimilhança da mistura de Paretos híbridas

hybrid_mixture_loglik <- function(par, y, m) {
  
  eta <- par[1:m]
  weights <- softmax(eta)
  
  theta <- matrix(
    par[(m + 1):length(par)],
    nrow = m,
    byrow = TRUE
  )
  
  logdens_mat <- matrix(NA_real_, nrow = length(y), ncol = m)
  
  for (j in 1:m) {
    
    mu_j <- as.numeric(theta[j, 1])
    sigma_j <- exp(as.numeric(theta[j, 2]))
    xi_j <- exp(as.numeric(theta[j, 3]))
    
    logdens_mat[, j] <- log(weights[j]) +
      log_hybrid_pareto_density(
        y = y,
        mu = mu_j,
        sigma = sigma_j,
        xi = xi_j
      )
  }
  
  logdens <- apply(logdens_mat, 1, log_sum_exp)
  
  if (any(!is.finite(logdens))) {
    return(-Inf)
  }
  
  sum(logdens)
}

# 15. Ajustar mistura com m Paretos híbridas

fit_hybrid_mixture <- function(x,
                               m = 2,
                               log_transform = TRUE,
                               n_start = 10,
                               seed = 123) {
  
  set.seed(seed)
  
  x <- as.numeric(x)
  x <- x[is.finite(x) & !is.na(x)]
  
  if (log_transform) {
    x <- x[x > 0]
    y <- log(x)
  } else {
    y <- x
  }
  
  n <- length(y)
  
  if (n < 30) {
    stop("Amostra demasiado pequena.")
  }
  
  fits <- vector("list", n_start)
  
  for (s in 1:n_start) {
    
    km <- tryCatch(
      kmeans(y, centers = m, nstart = 10),
      error = function(e) NULL
    )
    
    if (is.null(km)) {
      next
    }
    
    eta_start <- rep(0, m)
    theta_start <- c()
    
    for (j in 1:m) {
      
      yj <- y[km$cluster == j]
      
      if (length(yj) < 5) {
        yj <- sample(y, size = max(5, floor(n / m)), replace = TRUE)
      }
      
      sd_j <- sd(yj)
      if (!is.finite(sd_j) || sd_j <= 0) sd_j <- sd(y)
      
      theta_start <- c(
        theta_start,
        mean(yj),
        log(sd_j + 1e-6),
        log(runif(1, 0.05, 0.8))
      )
    }
    
    start <- c(eta_start, theta_start)
    
    fits[[s]] <- tryCatch(
      optim(
        par = start,
        fn = function(par) -hybrid_mixture_loglik(par, y, m),
        method = "Nelder-Mead",
        control = list(maxit = 30000)
      ),
      error = function(e) NULL
    )
  }
  
  fits <- Filter(Negate(is.null), fits)
  
  if (length(fits) == 0) {
    stop("Nenhum ajuste da mistura convergiu.")
  }
  
  values <- sapply(fits, function(f) f$value)
  best <- fits[[which.min(values)]]
  
  par_hat <- best$par
  
  eta_hat <- par_hat[1:m]
  weights_hat <- softmax(eta_hat)
  
  theta_hat <- matrix(
    par_hat[(m + 1):length(par_hat)],
    nrow = m,
    byrow = TRUE
  )
  
  comp <- lapply(1:m, function(j) {
    
    mu_j <- as.numeric(theta_hat[j, 1])
    sigma_j <- as.numeric(exp(theta_hat[j, 2]))
    xi_j <- as.numeric(exp(theta_hat[j, 3]))
    
    hp <- hybrid_params(mu_j, sigma_j, xi_j)
    
    data.frame(
      component = j,
      weight = as.numeric(weights_hat[j]),
      mu = mu_j,
      sigma = sigma_j,
      xi = xi_j,
      alpha = as.numeric(hp$alpha),
      beta = as.numeric(hp$beta),
      gamma = as.numeric(hp$gamma),
      row.names = NULL
    )
  }) %>%
    bind_rows()
  
  dominant <- comp %>%
    arrange(desc(xi), desc(beta)) %>%
    slice(1)
  
  loglik <- as.numeric(-best$value)
  
  # Correção importante:
  # m componentes * 3 parâmetros livres cada = 3m
  # pesos livres = m - 1
  # total = 4m - 1
  k_par <- 4 * m - 1
  
  AIC <- 2 * k_par - 2 * loglik
  BIC <- log(n) * k_par - 2 * loglik
  
  list(
    m = m,
    n = n,
    loglik = loglik,
    AIC = AIC,
    BIC = BIC,
    k_par = k_par,
    convergence = best$convergence,
    components = comp,
    dominant_component = dominant,
    par_raw = par_hat,
    y = y,
    x_original = x,
    log_transform = log_transform,
    fit = best
  )
}

# 16. Aplicar mistura a uma LOB

fit_hybrid_mixture_lob <- function(dados_lob,
                                   nome_lob,
                                   m = 2,
                                   claim_var = "Incurred_Claims",
                                   log_transform = TRUE,
                                   n_start = 10,
                                   seed = 123) {
  
  x <- dados_lob %>%
    filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    ) %>%
    pull(.data[[claim_var]])
  
  fit <- fit_hybrid_mixture(
    x = x,
    m = m,
    log_transform = log_transform,
    n_start = n_start,
    seed = seed
  )
  
  resumo <- data.frame(
    LOB = nome_lob,
    m = as.numeric(m),
    n = as.numeric(fit$n),
    k_par = as.numeric(fit$k_par),
    loglik = as.numeric(fit$loglik),
    AIC = as.numeric(fit$AIC),
    BIC = as.numeric(fit$BIC),
    convergence = as.numeric(fit$convergence),
    dominant_component = as.numeric(fit$dominant_component$component),
    xi_dominant = as.numeric(fit$dominant_component$xi),
    alpha_dominant = as.numeric(fit$dominant_component$alpha),
    beta_dominant = as.numeric(fit$dominant_component$beta),
    row.names = NULL
  )
  
  if (log_transform) {
    resumo <- resumo %>%
      mutate(
        alpha_dominant_original_scale = exp(alpha_dominant)
      )
  }
  
  list(
    summary = resumo,
    components = fit$components,
    fit = fit
  )
}

# 17. Grid para escolher m por AIC/BIC

fit_hybrid_m_grid_lob <- function(dados_lob,
                                  nome_lob,
                                  m_grid = 1:4,
                                  claim_var = "Incurred_Claims",
                                  log_transform = TRUE,
                                  n_start = 10,
                                  seed = 123) {
  
  resultados <- lapply(m_grid, function(m) {
    
    fit <- tryCatch(
      fit_hybrid_mixture_lob(
        dados_lob = dados_lob,
        nome_lob = nome_lob,
        m = m,
        claim_var = claim_var,
        log_transform = log_transform,
        n_start = n_start,
        seed = seed + m
      ),
      error = function(e) {
        message("Falhou para m = ", m, ": ", e$message)
        NULL
      }
    )
    
    if (is.null(fit)) {
      return(NULL)
    }
    
    fit$summary
  })
  
  bind_rows(resultados)
}

# 18. Densidade da mistura ajustada

log_hybrid_mixture_density_from_fit <- function(y, fit_mix) {
  
  comp <- fit_mix$components
  
  logdens_mat <- matrix(NA_real_, nrow = length(y), ncol = nrow(comp))
  
  for (j in 1:nrow(comp)) {
    
    logdens_mat[, j] <- log(comp$weight[j]) +
      log_hybrid_pareto_density(
        y = y,
        mu = comp$mu[j],
        sigma = comp$sigma[j],
        xi = comp$xi[j]
      )
  }
  
  apply(logdens_mat, 1, log_sum_exp)
}

plot_hybrid_mixture_fit <- function(fit_mix, title = "Hybrid Pareto mixture fit") {
  
  y <- fit_mix$y
  
  grid <- seq(min(y), max(y), length.out = 1000)
  
  dens <- exp(log_hybrid_mixture_density_from_fit(grid, fit_mix))
  
  df_dens <- data.frame(
    y = grid,
    density = dens
  )
  
  alphas <- fit_mix$components$alpha
  
  ggplot(data.frame(y = y), aes(x = y)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 80,
      fill = "grey80",
      color = "grey40"
    ) +
    geom_line(
      data = df_dens,
      aes(x = y, y = density),
      linewidth = 1
    ) +
    geom_vline(
      xintercept = alphas,
      linetype = "dashed"
    ) +
    labs(
      title = title,
      subtitle = paste0("m = ", fit_mix$m),
      x = ifelse(fit_mix$log_transform,
                 "log(Incurred Claims)",
                 "Incurred Claims"),
      y = "Density"
    ) +
    theme_minimal()
}

# APLICAR PARETO HÍBRIDA SIMPLES

hp_041 <- fit_hybrid_lob(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  claim_var = "Incurred_Claims",
  log_transform = TRUE,
  n_start = 20
)

hp_041$summary

hp_011 <- fit_hybrid_lob(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  claim_var = "Incurred_Claims",
  log_transform = TRUE,
  n_start = 20
)

hp_011$summary

hp_042 <- fit_hybrid_lob(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  claim_var = "Incurred_Claims",
  log_transform = TRUE,
  n_start = 20
)

hp_042$summary

plot_hybrid_fit(
  hp_041$fit,
  title = "Hybrid Pareto fit - MTPL Bodily Injury"
)

plot_hybrid_fit(
  hp_011$fit,
  title = "Hybrid Pareto fit - Health"
)

plot_hybrid_fit(
  hp_042$fit,
  title = "Hybrid Pareto fit - MTPL Material Damage"
)

qqplot_hybrid_fit(
  hp_041$fit,
  title = "QQ plot - Hybrid Pareto - MTPL Bodily Injury"
)

qqplot_hybrid_fit(
  hp_011$fit,
  title = "QQ plot - Hybrid Pareto - Health"
)

qqplot_hybrid_fit(
  hp_042$fit,
  title = "QQ plot - Hybrid Pareto - MTPL Material Damage"
)


# ESCOLHER m POR BIC

grid_hp_041 <- fit_hybrid_m_grid_lob(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  m_grid = 1:4,
  claim_var = "Incurred_Claims",
  log_transform = TRUE,
  n_start = 10
)

grid_hp_011 <- fit_hybrid_m_grid_lob(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  m_grid = 1:4,
  claim_var = "Incurred_Claims",
  log_transform = TRUE,
  n_start = 10
)

grid_hp_042 <- fit_hybrid_m_grid_lob(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  m_grid = 1:4,
  claim_var = "Incurred_Claims",
  log_transform = TRUE,
  n_start = 10
)

grid_hp_041
grid_hp_011
grid_hp_042

grid_hp_041 %>% arrange(BIC) %>% slice(1)
grid_hp_011 %>% arrange(BIC) %>% slice(1)
grid_hp_042 %>% arrange(BIC) %>% slice(1)


m_041 <- grid_hp_041 %>% arrange(BIC) %>% slice(1) %>% pull(m)
m_011 <- grid_hp_011 %>% arrange(BIC) %>% slice(1) %>% pull(m)
m_042 <- grid_hp_042 %>% arrange(BIC) %>% slice(1) %>% pull(m)

mix_041_m4 <- fit_hybrid_mixture_lob(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  m = 4,
  claim_var = "Incurred_Claims",
  log_transform = TRUE,
  n_start = 50
)

mix_011 <- fit_hybrid_mixture_lob(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  m = m_011,
  claim_var = "Incurred_Claims",
  log_transform = TRUE,
  n_start = 20
)

mix_042 <- fit_hybrid_mixture_lob(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  m = m_042,
  claim_var = "Incurred_Claims",
  log_transform = TRUE,
  n_start = 20
)

mix_041_m4$summary
mix_011$summary
mix_042$summary

mix_041_m4$components
mix_011$components
mix_042$components

plot_hybrid_mixture_fit(
  mix_041_m4$fit,
  title = "Hybrid Pareto mixture - MTPL Bodily Injury"
)

plot_hybrid_mixture_fit(
  mix_011$fit,
  title = "Hybrid Pareto mixture - Health"
)

plot_hybrid_mixture_fit(
  mix_042$fit,
  title = "Hybrid Pareto mixture - MTPL Material Damage"
)

##Cabras e Castellanos

#Log-densidade da GPD para excessos y = x - u
log_gpd_density_cab <- function(y, sigma, xi) {
  
  if (sigma <= 0) {
    return(rep(-Inf, length(y)))
  }
  
  if (any(y < 0)) {
    return(rep(-Inf, length(y)))
  }
  
  if (abs(xi) < 1e-8) {
    return(-log(sigma) - y / sigma)
  }
  
  z <- 1 + xi * y / sigma
  
  if (any(z <= 0)) {
    return(rep(-Inf, length(y)))
  }
  
  -log(sigma) - (1 + 1 / xi) * log(z)
}


# 2. Log-priori de Jeffreys para (xi, sigma)
# pi(xi, sigma) propto sigma^{-1}(1+xi)^{-1}(1+2xi)^{-1/2} com xi > -1/2 e sigma > 0

log_prior_gpd_jeffreys <- function(sigma, xi) {
  
  if (sigma <= 0) return(-Inf)
  if (xi <= -0.5) return(-Inf)
  if (1 + xi <= 0) return(-Inf)
  if (1 + 2 * xi <= 0) return(-Inf)
  
  -log(sigma) - log(1 + xi) - 0.5 * log(1 + 2 * xi)
}

#Estimador semiparamétrico de Lindsey para h_u

fit_lindsey_density <- function(x_body, u, d = 3, nbins = 30) {
  
  x_body <- x_body[is.finite(x_body)]
  x_body <- x_body[x_body <= u]
  
  if (length(x_body) < d + 2) {
    return(NULL)
  }
  
  # Histograma dos dados abaixo de u
  hist_obj <- hist(x_body, breaks = nbins, plot = FALSE)
  
  mids <- hist_obj$mids
  counts <- hist_obj$counts
  
  # Remover classes fora do intervalo relevante
  valid <- mids <= u & is.finite(mids) & is.finite(counts)
  mids <- mids[valid]
  counts <- counts[valid]
  
  if (length(mids) <= d + 1) {
    return(NULL)
  }
  
  # Polinómios ortogonais
  mp <- poly(mids, degree = d)
  
  # Regressão de Poisson nas contagens do histograma
  fit <- tryCatch(
    glm(counts ~ mp, family = poisson),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(NULL)
  }
  
  # Função k_u(x) = exp(polynomial)
  k_fun <- function(z) {
    
    basis <- predict(mp, newdata = z)
    eta <- cbind(1, basis) %*% coef(fit)
    
    as.numeric(exp(eta))
  }
  
  lower <- min(x_body)
  upper <- u
  
  # Constante de normalização
  norm_const <- tryCatch(
    integrate(k_fun, lower = lower, upper = upper,
              subdivisions = 1000, rel.tol = 1e-6)$value,
    error = function(e) NA_real_
  )
  
  if (!is.finite(norm_const) || norm_const <= 0) {
    return(NULL)
  }
  
  # Densidade truncada estimada
  h_hat <- function(z) {
    
    dens <- k_fun(z) / norm_const
    dens[z < lower | z > upper] <- 0
    
    dens
  }
  
  return(
    list(
      fit = fit,
      mp = mp,
      h_hat = h_hat,
      norm_const = norm_const,
      lower = lower,
      upper = upper,
      d = d
    )
  )
}

#estimar h_u para cada u

prepare_semiparametric_grid <- function(x,
                                        probs_u = seq(0.70, 0.95, by = 0.01),
                                        d = 3,
                                        nbins = 30,
                                        min_excessos = 20) {
  
  x <- x[is.finite(x) & x > 0]
  x <- sort(x)
  n <- length(x)
  
  u_grid <- as.numeric(quantile(x, probs = probs_u, na.rm = TRUE))
  u_grid <- sort(unique(u_grid))
  
  grid_list <- lapply(seq_along(u_grid), function(j) {
    
    u <- u_grid[j]
    
    x_body <- x[x <= u]
    x_tail <- x[x > u]
    
    n_body <- length(x_body)
    n_tail <- length(x_tail)
    
    if (n_tail < min_excessos) {
      return(NULL)
    }
    
    if (n_body < d + 2) {
      return(NULL)
    }
    
    H_hat <- n_body / n
    
    if (H_hat <= 0 || H_hat >= 1) {
      return(NULL)
    }
    
    lindsey <- fit_lindsey_density(
      x_body = x_body,
      u = u,
      d = d,
      nbins = nbins
    )
    
    if (is.null(lindsey)) {
      return(NULL)
    }
    
    h_vals <- lindsey$h_hat(x_body)
    
    if (any(h_vals <= 0) || any(!is.finite(h_vals))) {
      return(NULL)
    }
    
    # Contribuição da parte central:
    # sum log( H(u) * h_hat_u(x_i) )
    log_body <- sum(log(H_hat) + log(h_vals))
    
    list(
      u = u,
      prob_u = probs_u[j],
      H_hat = H_hat,
      n_body = n_body,
      n_tail = n_tail,
      x_body = x_body,
      y_tail = x_tail - u,
      log_body = log_body,
      lindsey = lindsey
    )
  })
  
  grid_list <- Filter(Negate(is.null), grid_list)
  
  if (length(grid_list) == 0) {
    stop("Nenhum threshold válido. Tenta reduzir min_excessos ou alterar probs_u.")
  }
  
  return(grid_list)
}

# Log-verosimilhança perfilada

loglik_profile_semiparam <- function(grid_obj, sigma, xi) {
  
  if (sigma <= 0) return(-Inf)
  
  y_tail <- grid_obj$y_tail
  H_hat <- grid_obj$H_hat
  n_tail <- grid_obj$n_tail
  
  log_tail_density <- log_gpd_density_cab(
    y = y_tail,
    sigma = sigma,
    xi = xi
  )
  
  if (any(!is.finite(log_tail_density))) {
    return(-Inf)
  }
  
  log_tail <- n_tail * log(1 - H_hat) + sum(log_tail_density)
  
  loglik <- grid_obj$log_body + log_tail
  
  if (!is.finite(loglik)) return(-Inf)
  
  loglik
}

# Log-posterior

logposterior_semiparam <- function(grid_list,
                                   idx_u,
                                   eta_sigma,
                                   xi) {
  
  sigma <- exp(eta_sigma)
  
  if (idx_u < 1 || idx_u > length(grid_list)) {
    return(-Inf)
  }
  
  grid_obj <- grid_list[[idx_u]]
  
  ll <- loglik_profile_semiparam(
    grid_obj = grid_obj,
    sigma = sigma,
    xi = xi
  )
  
  if (!is.finite(ll)) {
    return(-Inf)
  }
  
  lp <- log_prior_gpd_jeffreys(
    sigma = sigma,
    xi = xi
  )
  
  if (!is.finite(lp)) {
    return(-Inf)
  }
  
  # Como estamos a trabalhar em eta = log(sigma),
  # adiciona-se o Jacobiano: log(sigma) = eta_sigma
  jac <- eta_sigma
  
  # Priori uniforme discreta para u
  lp_u <- -log(length(grid_list))
  
  ll + lp + jac + lp_u
}

profile_u_semiparam <- function(grid_list) {
  
  res <- lapply(seq_along(grid_list), function(j) {
    
    grid_obj <- grid_list[[j]]
    
    y <- grid_obj$y_tail
    
    sigma0 <- sd(y)
    if (!is.finite(sigma0) || sigma0 <= 0) sigma0 <- mean(y)
    
    opt <- tryCatch(
      optim(
        par = c(log(sigma0), 0.1),
        fn = function(par) {
          
          sigma <- exp(par[1])
          xi <- par[2]
          
          ll <- loglik_profile_semiparam(
            grid_obj = grid_obj,
            sigma = sigma,
            xi = xi
          )
          
          lp <- log_prior_gpd_jeffreys(
            sigma = sigma,
            xi = xi
          )
          
          if (!is.finite(ll + lp)) return(1e10)
          
          -(ll + lp)
        },
        method = "Nelder-Mead",
        control = list(maxit = 5000)
      ),
      error = function(e) NULL
    )
    
    if (is.null(opt)) return(NULL)
    
    data.frame(
      idx = j,
      prob_u = grid_obj$prob_u,
      u = grid_obj$u,
      n_tail = grid_obj$n_tail,
      H_hat = grid_obj$H_hat,
      sigma_hat = exp(opt$par[1]),
      xi_hat = opt$par[2],
      logpost_max = -opt$value,
      convergence = opt$convergence
    )
  })
  
  dplyr::bind_rows(res)
}

b_041 <- prepare_semiparametric_grid(
  x = lob_041_MTPL_Bodily_Injury$Incurred_Claims,
  probs_u = seq(0.80, 0.98, by = 0.005),
  d = 3,
  nbins = 30,
  min_excessos = 20
)

prof_041 <- profile_u_semiparam(b_041)

prof_041 %>%
  arrange(desc(logpost_max)) %>%
  head(10)

ggplot(prof_041, aes(x = prob_u, y = logpost_max)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Profile log-posterior by threshold - MTPL Bodily Injury",
    x = "Threshold quantile",
    y = "Maximized log-posterior"
  ) +
  theme_minimal()

#MCMC

mcmc_cabras_castellanos <- function(dados_lob,
                                    nome_lob,
                                    claim_var = "Incurred_Claims",
                                    probs_u = seq(0.70, 0.95, by = 0.01),
                                    init_prob_u = 0.90,
                                    d = 3,
                                    nbins = 30,
                                    min_excessos = 20,
                                    n_iter = 30000,
                                    burn = 10000,
                                    thin = 10,
                                    proposal_sd = list(
                                      eta_sigma = 0.15,
                                      xi = 0.08
                                    ),
                                    seed = 123) {
  
  set.seed(seed)
  
  x <- dados_lob %>%
    filter(
      !is.na(.data[[claim_var]]),
      is.finite(.data[[claim_var]]),
      .data[[claim_var]] > 0
    ) %>%
    pull(.data[[claim_var]])
  
  x <- sort(x)
  n <- length(x)
  
  grid_list <- prepare_semiparametric_grid(
    x = x,
    probs_u = probs_u,
    d = d,
    nbins = nbins,
    min_excessos = min_excessos
  )
  
  #--------------------------------------------------------
  # Valor inicial para u
  #--------------------------------------------------------
  # Se init_prob_u for NULL, começa num threshold aleatório.
  # Caso contrário, começa no threshold mais próximo de init_prob_u.
  
  if (is.null(init_prob_u)) {
    
    idx_u <- sample(seq_along(grid_list), size = 1)
    
  } else {
    
    idx_u <- which.min(
      abs(sapply(grid_list, `[[`, "prob_u") - init_prob_u)
    )
  }
  
  y0 <- grid_list[[idx_u]]$y_tail
  
  sigma <- sd(y0)
  if (!is.finite(sigma) || sigma <= 0) {
    sigma <- mean(y0)
  }
  
  eta_sigma <- log(sigma)
  xi <- 0.1
  
  current_lp <- logposterior_semiparam(
    grid_list = grid_list,
    idx_u = idx_u,
    eta_sigma = eta_sigma,
    xi = xi
  )
  
  chain <- matrix(NA, nrow = n_iter, ncol = 6)
  colnames(chain) <- c(
    "u",
    "prob_u",
    "H_hat",
    "sigma",
    "xi",
    "logpost"
  )
  
  acc_sigma <- 0
  acc_xi <- 0
  acc_u <- 0
  
  for (t in 1:n_iter) {
    
    #--------------------------------------------------------
    # Atualizar eta_sigma = log(sigma)
    #--------------------------------------------------------
    
    eta_prop <- rnorm(1, eta_sigma, proposal_sd$eta_sigma)
    
    prop_lp <- logposterior_semiparam(
      grid_list = grid_list,
      idx_u = idx_u,
      eta_sigma = eta_prop,
      xi = xi
    )
    
    if (log(runif(1)) < prop_lp - current_lp) {
      eta_sigma <- eta_prop
      current_lp <- prop_lp
      acc_sigma <- acc_sigma + 1
    }
    
    #--------------------------------------------------------
    # Atualizar xi
    #--------------------------------------------------------
    
    xi_prop <- rnorm(1, xi, proposal_sd$xi)
    
    prop_lp <- logposterior_semiparam(
      grid_list = grid_list,
      idx_u = idx_u,
      eta_sigma = eta_sigma,
      xi = xi_prop
    )
    
    if (log(runif(1)) < prop_lp - current_lp) {
      xi <- xi_prop
      current_lp <- prop_lp
      acc_xi <- acc_xi + 1
    }
    
    #--------------------------------------------------------
    # Atualizar u com proposta local
    #--------------------------------------------------------
    # Em vez de propor qualquer threshold da grelha,
    # propõe apenas um vizinho: idx_u - 1 ou idx_u + 1.
    
    idx_candidates <- c(idx_u - 1, idx_u + 1)
    idx_candidates <- idx_candidates[
      idx_candidates >= 1 & idx_candidates <= length(grid_list)
    ]
    
    if (length(idx_candidates) > 0) {
      
      idx_prop <- sample(idx_candidates, size = 1)
      
      prop_lp <- logposterior_semiparam(
        grid_list = grid_list,
        idx_u = idx_prop,
        eta_sigma = eta_sigma,
        xi = xi
      )
      
      if (log(runif(1)) < prop_lp - current_lp) {
        idx_u <- idx_prop
        current_lp <- prop_lp
        acc_u <- acc_u + 1
      }
    }
    
    sigma <- exp(eta_sigma)
    
    chain[t, ] <- c(
      grid_list[[idx_u]]$u,
      grid_list[[idx_u]]$prob_u,
      grid_list[[idx_u]]$H_hat,
      sigma,
      xi,
      current_lp
    )
  }
  
  keep <- seq(burn + 1, n_iter, by = thin)
  posterior <- as.data.frame(chain[keep, ])
  
  acceptance <- c(
    sigma = acc_sigma / n_iter,
    xi = acc_xi / n_iter,
    u = acc_u / n_iter
  )
  
  summary <- data.frame(
    LOB = nome_lob,
    n = n,
    d = d,
    nbins = nbins,
    n_iter = n_iter,
    burn = burn,
    thin = thin,
    init_prob_u = init_prob_u,
    u_median = median(posterior$u),
    u_mean = mean(posterior$u),
    u_q025 = quantile(posterior$u, 0.025),
    u_q975 = quantile(posterior$u, 0.975),
    prob_u_median = median(posterior$prob_u),
    prob_u_mean = mean(posterior$prob_u),
    sigma_median = median(posterior$sigma),
    sigma_mean = mean(posterior$sigma),
    sigma_q025 = quantile(posterior$sigma, 0.025),
    sigma_q975 = quantile(posterior$sigma, 0.975),
    xi_median = median(posterior$xi),
    xi_mean = mean(posterior$xi),
    xi_q025 = quantile(posterior$xi, 0.025),
    xi_q975 = quantile(posterior$xi, 0.975),
    acc_sigma = acceptance["sigma"],
    acc_xi = acceptance["xi"],
    acc_u = acceptance["u"]
  )
  
  return(
    list(
      LOB = nome_lob,
      x = x,
      grid_list = grid_list,
      chain = as.data.frame(chain),
      posterior = posterior,
      acceptance = acceptance,
      summary = summary
    )
  )
}

# MTPL Bodily Injury
cc_041 <- mcmc_cabras_castellanos(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  claim_var = "Incurred_Claims",
  probs_u = seq(0.85, 0.98, by = 0.005),
  d = 2,
  nbins = 30,
  min_excessos = 20,
  n_iter = 30000,
  burn = 10000,
  thin = 10,
  seed = 123
)

cc_041$summary
cc_041$acceptance

# Health
cc_011 <- mcmc_cabras_castellanos(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  claim_var = "Incurred_Claims",
  probs_u = seq(0.80, 0.98, by = 0.005),
  d = 3,
  nbins = 30,
  min_excessos = 20,
  n_iter = 30000,
  burn = 10000,
  thin = 10,
  seed = 123
)

cc_011$summary
cc_011$acceptance


# MTPL Material Damage
cc_042 <- mcmc_cabras_castellanos(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  claim_var = "Incurred_Claims",
  probs_u = seq(0.8, 0.95, by = 0.005),
  d = 3,
  nbins = 30,
  min_excessos = 20,
  n_iter = 30000,
  burn = 10000,
  thin = 10,
  seed = 123
)

cc_042$summary
cc_042$acceptance

plot_posterior_u <- function(res) {
  
  ggplot(res$posterior, aes(x = u)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black") +
    labs(
      title = paste("Posterior distribution of threshold u -", res$LOB),
      x = "Threshold u",
      y = "Frequency"
    ) +
    theme_minimal()
}

plot_posterior_u(cc_041)
plot_posterior_u(cc_011)
plot_posterior_u(cc_042)

posterior_threshold_table <- function(res) {
  
  res$posterior %>%
    group_by(prob_u, u) %>%
    summarise(
      posterior_prob = n() / nrow(res$posterior),
      .groups = "drop"
    ) %>%
    arrange(desc(posterior_prob))
}

posterior_threshold_table(cc_041)
posterior_threshold_table(cc_011)
posterior_threshold_table(cc_042)

plot_trace <- function(res) {
  
  chain <- res$chain %>%
    mutate(iter = row_number())
  
  p1 <- ggplot(chain, aes(x = iter, y = u)) +
    geom_line() +
    labs(
      title = paste("Trace plot of u -", res$LOB),
      x = "Iteration",
      y = "u"
    ) +
    theme_minimal()
  
  p2 <- ggplot(chain, aes(x = iter, y = sigma)) +
    geom_line() +
    labs(
      title = paste("Trace plot of sigma -", res$LOB),
      x = "Iteration",
      y = expression(sigma)
    ) +
    theme_minimal()
  
  p3 <- ggplot(chain, aes(x = iter, y = xi)) +
    geom_line() +
    labs(
      title = paste("Trace plot of xi -", res$LOB),
      x = "Iteration",
      y = expression(xi)
    ) +
    theme_minimal()
  
  list(u = p1, sigma = p2, xi = p3)
}

tr_041 <- plot_trace(cc_041)
tr_041$u
tr_041$sigma
tr_041$xi


##calcular medidas de risco 

pot_risk_measures <- function(x,
                              u,
                              probs = c(0.99, 0.995, 0.999),
                              return_periods = c(2, 5, 10, 20, 50, 100),
                              years_observed = 15) {
  
  # Keep only valid positive observations
  x <- x[is.finite(x) & x > 0]
  
  n <- length(x)
  
  # Exceedances
  excesses <- x[x > u] - u
  k <- length(excesses)
  
  if (k < 10) {
    warning("Very few exceedances above the selected threshold.")
  }
  
  # ----------------------------------------------------------
  # 2. GPD fit by MLE
  # ----------------------------------------------------------
  
  fit <- fpot(
    x,
    threshold = u,
    model = "gpd",
    std.err = FALSE
  )
  
  sigma_hat <- unname(fit$estimate["scale"])
  xi_hat    <- unname(fit$estimate["shape"])
  
  p_u <- k / n
  
  # ----------------------------------------------------------
  # 3. VaR
  # ----------------------------------------------------------
  
  var_fun <- function(p) {
    
    if (p <= 1 - p_u) {
      return(NA_real_)
    }
    
    if (abs(xi_hat) < 1e-8) {
      
      u + sigma_hat *
        log(p_u / (1 - p))
      
    } else {
      
      u +
        (sigma_hat / xi_hat) *
        (
          (p_u / (1 - p))^xi_hat - 1
        )
    }
  }
  
  VaR <- sapply(probs, var_fun)
  
  # ----------------------------------------------------------
  # 4. Expected Shortfall
  # ----------------------------------------------------------
  
  es_fun <- function(p) {
    
    VaR_p <- var_fun(p)
    
    if (is.na(VaR_p)) {
      return(NA_real_)
    }
    
    if (xi_hat >= 1) {
      return(Inf)
    }
    
    (VaR_p + sigma_hat - xi_hat * u) /
      (1 - xi_hat)
  }
  
  ES <- sapply(probs, es_fun)
  
  # ----------------------------------------------------------
  # 5. Return Levels
  # ----------------------------------------------------------
  
  # Annual exceedance rate
  lambda_u <- k / years_observed
  
  return_level_fun <- function(T) {
    
    if (abs(xi_hat) < 1e-8) {
      
      u + sigma_hat * log(lambda_u * T)
      
    } else {
      
      u +
        (sigma_hat / xi_hat) *
        (
          (lambda_u * T)^xi_hat - 1
        )
    }
  }
  
  RL <- sapply(return_periods, return_level_fun)
  
  # ----------------------------------------------------------
  # 6. Output tables
  # ----------------------------------------------------------
  
  parameter_table <- data.frame(
    n = n,
    threshold = u,
    k = k,
    exceedance_probability = p_u,
    sigma_hat = sigma_hat,
    xi_hat = xi_hat,
    annual_exceedance_rate = lambda_u
  )
  
  risk_table <- data.frame(
    probability = probs,
    VaR = VaR,
    ES = ES
  )
  
  return_level_table <- data.frame(
    return_period_years = return_periods,
    return_level = RL
  )
  
  list(
    parameters = parameter_table,
    risk_measures = risk_table,
    return_levels = return_level_table,
    fit = fit
  )
}


# MTPL Bodily Injury

u_041 <- 132655.90

res_041 <- pot_risk_measures(
  x = x_041,
  u = u_041,
  probs = c(0.99, 0.995, 0.999),
  return_periods = c(2, 5, 10, 20),
  years_observed = 15
)

# MTPL Material Damage

u_042 <- 149944.53

res_042 <- pot_risk_measures(
  x = x_042,
  u = u_042,
  probs = c(0.99, 0.995, 0.999),
  return_periods = c(2, 5, 10, 20),
  years_observed = 15
)

# Health

u_011 <- 73147.79

res_011 <- pot_risk_measures(
  x = x_011,
  u = u_011,
  probs = c(0.99, 0.995, 0.999),
  return_periods = c(2, 5, 10, 20),
  years_observed = 15
)


res_041$parameters
res_041$risk_measures
res_041$return_levels

res_042$parameters
res_042$risk_measures
res_042$return_levels

res_011$parameters
res_011$risk_measures
res_011$return_levels
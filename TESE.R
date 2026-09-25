#Dissertação "Threshold selection to split attritional claims from large claims"
#Ana Bernardo, Setembro 2026

rm(list = ls())

#Pacotes necessários
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
library(heavytails)

#Importação dos dados
parte1 <- read.csv("C:/Users/fatim/Downloads/parte1.csv")
parte2 <- read.csv("C:/Users/fatim/Downloads/parte2.csv")
dados <- rbind(parte1, parte2)

summary(dados)
#View(dados)


#Análise inicial + limpeza e tratamento dos dados
dados <- dados %>%
  mutate(
    Paid_Claims = as.numeric(Paid_Claims),
    Incurred_Claims = as.numeric(Incurred_Claims),
    Case_Reserve = as.numeric(Case_Reserve)
  )

dados

#Retirar dados antes de 2010
dados <- dados %>%
  mutate(Accident_Date = as.Date(Accident_Date))

dados <- dados %>%
  mutate(Accident_Year = year(Accident_Date)) %>%
  filter(Accident_Year >= 2011)


#View(dados)

#Ver cada LOB individualmente
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

#Número de observaçoes por LOB (o mesmo sinistro dá aso a muitos pagamentos)
tabela_lob <- dados %>%
  group_by(LOB_SII_1) %>%
  summarise(
    n_observacoes = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n_observacoes))

tabela_lob

#Claim_id único por LOB (ver quanto se gastou com cada claim_id) 
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

dados_agrupados_2011 <- dados %>%
  group_by(Claim_ID, LOB_SII_1) %>%
  summarise(
    Accident_Date = first(Accident_Date),
    Paid_Claims = sum(Paid_Claims, na.rm = TRUE),
    Incurred_Claims = sum(Incurred_Claims, na.rm = TRUE),
    Case_Reserve = sum(Case_Reserve, na.rm = TRUE),
    .groups = "drop"
  )

dados_agrupados_2011
summary(dados_agrupados_2011)

#Número de sinistros por LOB
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

#Número de sinistros únicos (Dados por claim_id)
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

#Sinistros por anos
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


datas <- ggplot(
  claims_por_ano,
  aes(x = Accident_Year, y = n_claims)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  scale_x_continuous(
    breaks = sort(unique(claims_por_ano$Accident_Year))
  ) +
  
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

#Sinistros temporais para as LOBs com maior número de sinistros
claims_por_ano_lob <- dados_agrupados_2011 %>%
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
    breaks = c(2011, 2015, 2020, 2025)
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

#Estatísticas para as principais LOBs

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
  arrange(desc(n_claims))

print(estatisticas_incurred_lob_principais, n = Inf, width = Inf)

#Boxplot apenas com valores positivos
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

#Auto Danos corporais outros (1)
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

#Auto Danos a terceiros (2)
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

#Saúde (3)
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


#BLOCK MAXIMA

#Anual

#1
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

#2
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

#3
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

#Ajuste GEV

#1
fit_gev_052 <- fevd(
  x = max_anuais_052$max_incurred,
  type = "GEV",
  method = "MLE"
)

summary(fit_gev_052)
plot(fit_gev_052)

# pdf(
#   file = "C:/Users/fatim/Downloads/anual1.pdf",
#   width = 8,
#   height = 6
# )
# 
# plot(fit_gev_052)
# 
# dev.off()

#2
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
# dev.off()

#3
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


#Trimestral

#1
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

print(max_trimestrais_052, n=Inf)

#2
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

print(max_trimestrais_042, n=Inf)


#3
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

print(max_trimestrais_011, n=Inf)

#Ajuste GEV

#1
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

#2
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

#3
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

#Niveis de retorno

#Anual
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

#trimestral
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

#Quantis
q_041 <- round(quantile(x_041, probs = c(0.90, 0.94, 0.95, 0.96, 0.975, 0.99), na.rm = TRUE), 2); q_041
q_011 <- quantile(x_011, probs = c(0.90, 0.95, 0.96, 0.975, 0.99), na.rm = TRUE);q_011
q_042 <- round(quantile(x_042, probs = c(0.90, 0.95,0.96, 0.975, 0.99), na.rm = TRUE),2);q_042

#Gráficos

#MRL plot

#1
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

#Tabela análise mrl
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

#3
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

#2
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

#Stability threshold plot

#MLE (library(evd))
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

#MOM, PWM (library(POT))
#Também dá para calcular MLE mas as estimativas davam muito parecidas
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

#1
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

#Gráficos
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

#3
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

#2
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

#1

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

#Gráficos mais importantes
probs_041_3a <- seq(0.90, 0.94, by = 0.005)

par(
  mfrow = c(3, 3),
  mar = c(4, 4, 3, 1),
  oma = c(0, 0, 3, 0)
)

for (p in probs_041_3a) {
  
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

mtext(
  "QQ plots - MTPL Bodily Injury",
  outer = TRUE,
  cex = 1.3
)

par(mfrow = c(1, 1))

probs_041_3b <- seq(0.945, 0.98, by = 0.005)

par(
  mfrow = c(3, 3),
  mar = c(4, 4, 3, 1),
  oma = c(0, 0, 3, 0)
)

for (p in probs_041_3b) {
  
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

mtext(
  "QQ plots - MTPL Bodily Injury",
  outer = TRUE,
  cex = 1.3
)

par(mfrow = c(1, 1))

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots41_1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_041_3a) {
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
#   "QQ plots - MTPL Bodily Injury",
#   outer = TRUE,
#   cex = 1.3
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots41_2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_041_3b) {
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
#   "QQ plots - MTPL Bodily Injury",
#   outer = TRUE,
#   cex = 1.3
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()


#Tabela
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

#2

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

#Gráficos mais importantes
# probs_042_1 <- seq(0.90, 0.94, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots42_1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_042_1) {
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
#   "QQ plots - MTPL Material Damage",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

# probs_042_2 <- seq(0.945, 0.98, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots42_2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_042_2) {
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
#   "QQ plots - MTPL Material Damage",
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

#3

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

#Gráficos mais importantes

# probs_011_1 <- seq(0.90, 0.94, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots11_1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_011_1) {
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
#   "QQ plots - Health",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

# probs_011_2 <- seq(0.945, 0.98, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/qqplots11_2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_011_2) {
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
#   "QQ plots - Health",
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


#Probability plot

#1
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

#Gráficos mais importantes
# probs_041_prob_1 <- seq(0.90, 0.94, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/probplots41_1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_041_prob_1) {
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
#   "Probability plots - MTPL Bodily Injury",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

# probs_041_prob_2 <- seq(0.945, 0.98, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/probplots41_2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_041_prob_2) {
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
#   "Probability plots - MTPL Bodily Injury",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

#Tabela
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

#2

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

#Gráficos mais importantes
# probs_042_prob_1 <- seq(0.90, 0.94, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/probplots42_1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_042_prob_1) {
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
#     type = "probprob",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_042, 0))
#     )
#   )
# }
# 
# mtext(
#   "Probability plots - MTPL Material Damage",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

# probs_042_prob_2 <- seq(0.945, 0.98, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/probplots42_2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_042_prob_2) {
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
#     type = "probprob",
#     main = paste0(
#       "q = ", p,
#       "\nu = ", scales::comma(round(u_042, 0))
#     )
#   )
# }
# 
# mtext(
#   "Probability plots - MTPL Material Damage",
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

#3
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

#Gráficos mais importantes
# probs_011_prob_1 <- seq(0.90, 0.94, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/probplots11_1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_011_prob_1) {
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
#   "Probability plots - Health",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

# probs_011_prob_2 <- seq(0.945, 0.98, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/probplots11_2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_011_prob_2) {
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
#   "Probability plots - Health",
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

#Return level plot 

#1
probs_041_1 <- seq(0.80, 0.89, by = 0.005)

x_041 <- lob_041_MTPL_Bodily_Injury$Incurred_Claims

# Número de anos observados
n_years <- dplyr::n_distinct(
  lubridate::year(
    as.Date(lob_041_MTPL_Bodily_Injury$Accident_Date)
  )
)

par(
  mfrow = c(4, 5),
  mar = c(4, 4, 3, 1)
)

for (p in probs_041_1) {
  
  # Threshold
  u <- as.numeric(
    quantile(
      x_041,
      probs = p,
      na.rm = TRUE
    )
  )
  
  # Número de excedências acima do threshold
  k <- sum(
    x_041 > u,
    na.rm = TRUE
  )
  
  # Número médio de excedências por ano
  npy <- k / n_years
  
  # Ajuste GPD
  fit <- POT::fitgpd(
    data = x_041,
    threshold = u,
    est = "mle"
  )
  
  # Return level plot
  POT::retlev(
    fit,
    npy = npy,
    points = TRUE,
    ci = TRUE,
    xlimsup = 20,
    main = paste0(
      "q = ", sprintf("%.3f", p),
      "\nu = ", scales::comma(round(u))
    )
  )
}

par(mfrow = c(1, 1))

probs_041_2 <- seq(0.895, 0.98, by = 0.005)

x_041 <- lob_041_MTPL_Bodily_Injury$Incurred_Claims

# Número de anos observados
n_years <- dplyr::n_distinct(
  lubridate::year(
    as.Date(lob_041_MTPL_Bodily_Injury$Accident_Date)
  )
)

par(
  mfrow = c(4, 5),
  mar = c(4, 4, 3, 1)
)

for (p in probs_041_2) {
  
  # Threshold
  u <- as.numeric(
    quantile(
      x_041,
      probs = p,
      na.rm = TRUE
    )
  )
  
  # Número de excedências acima do threshold
  k <- sum(
    x_041 > u,
    na.rm = TRUE
  )
  
  # Número médio de excedências por ano
  npy <- k / n_years
  
  # Ajuste GPD
  fit <- POT::fitgpd(
    data = x_041,
    threshold = u,
    est = "mle"
  )
  
  # Return level plot
  POT::retlev(
    fit,
    npy = npy,
    points = TRUE,
    ci = TRUE,
    xlimsup = 20,
    main = paste0(
      "q = ", sprintf("%.3f", p),
      "\nu = ", scales::comma(round(u))
    )
  )
}

par(mfrow = c(1, 1))

#Guardar gráfico

# probs_041_ret_1 <- seq(0.90, 0.94, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/returnlevels41_1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_041_ret_1) {
#   
#   # Threshold
#   u <- as.numeric(
#     quantile(
#       x_041,
#       probs = p,
#       na.rm = TRUE
#     )
#   )
#   
#   # Número de excedências
#   k <- sum(x_041 > u, na.rm = TRUE)
#   
#   # Número médio de excedências por ano
#   npy <- k / n_years
#   
#   # Ajuste GPD
#   fit <- POT::fitgpd(
#     data = x_041,
#     threshold = u,
#     est = "mle"
#   )
#   
#   # Return level plot
#   POT::retlev(
#     fit,
#     npy = npy,
#     points = TRUE,
#     ci = TRUE,
#     xlimsup = 20,
#     main = paste0(
#       "q = ", sprintf("%.3f", p),
#       "\nu = ", scales::comma(round(u))
#     )
#   )
# }
# 
# mtext(
#   "Return level plots - MTPL Bodily Injury",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()
# 
# probs_041_ret_2 <- seq(0.945, 0.98, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/returnlevels41_2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_041_ret_2) {
#   
#   # Threshold
#   u <- as.numeric(
#     quantile(
#       x_041,
#       probs = p,
#       na.rm = TRUE
#     )
#   )
#   
#   # Número de excedências
#   k <- sum(x_041 > u, na.rm = TRUE)
#   
#   # Número médio de excedências por ano
#   npy <- k / n_years
#   
#   # Ajuste GPD
#   fit <- POT::fitgpd(
#     data = x_041,
#     threshold = u,
#     est = "mle"
#   )
#   
#   # Return level plot
#   POT::retlev(
#     fit,
#     npy = npy,
#     points = TRUE,
#     ci = TRUE,
#     xlimsup = 20,
#     main = paste0(
#       "q = ", sprintf("%.3f", p),
#       "\nu = ", scales::comma(round(u))
#     )
#   )
# }
# 
# mtext(
#   "Return level plots - MTPL Bodily Injury",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

#Tabela
return_level_table_POT <- function(
    dados_lob,
    probs = seq(0.80, 0.98, by = 0.005),
    return_periods = c(2, 5, 10, 15, 20)
) {
  
  # Dados válidos
  dados_validos <- dados_lob %>%
    dplyr::filter(
      !is.na(Incurred_Claims),
      is.finite(Incurred_Claims),
      Incurred_Claims > 0
    )
  
  x <- dados_validos$Incurred_Claims
  
  # Criar Accident_Year se necessário
  if (!"Accident_Year" %in% names(dados_validos)) {
    
    dados_validos <- dados_validos %>%
      dplyr::mutate(
        Accident_Year =
          lubridate::year(as.Date(Accident_Date))
      )
  }
  
  # Número de anos observados
  n_years <- dplyr::n_distinct(
    dados_validos$Accident_Year
  )
  
  # Número médio de sinistros por ano
  npy <- length(x) / n_years
  
  resultados <- lapply(probs, function(p) {
    
    # Threshold
    u <- as.numeric(
      quantile(
        x,
        probs = p,
        na.rm = TRUE
      )
    )
    
    # Número de excessos
    k <- sum(x > u)
    
    # Ajuste GPD por MLE
    fit <- POT::fitgpd(
      data = x,
      threshold = u,
      est = "mle"
    )
    
    # Parâmetros estimados
    sigma <- as.numeric(
      fit$param["scale"]
    )
    
    xi <- as.numeric(
      fit$param["shape"]
    )
    
    # retlev produz um gráfico.
    # Abrimos um dispositivo nulo para não gerar os gráficos
    grDevices::pdf(NULL)
    
    rl_fun <- POT::retlev(
      fit,
      npy = npy,
      points = FALSE,
      ci = FALSE
    )
    
    grDevices::dev.off()
    
    # Return levels
    rl <- as.numeric(
      rl_fun(return_periods)
    )
    
    # Linha da tabela
    data.frame(
      q = p,
      u = u,
      k = k,
      xi = xi,
      sigma = sigma,
      t(rl)
    )
  })
  
  tabela <- dplyr::bind_rows(resultados)
  
  # Nomes das colunas de return level
  names(tabela)[6:ncol(tabela)] <-
    paste0("RL_", return_periods, "y")
  
  return(tabela)
}

table_rl_041 <- return_level_table_POT(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  probs = seq(0.80, 0.98, by = 0.005),
  return_periods = c(2, 5, 10, 15, 20)
)

#View(table_rl_041)

# write.csv(
#   table_rl_041,
#   "C:/Users/fatim/Downloads/return_levels_POT_041.csv",
#   row.names = FALSE
# )

#2

x_042 <- lob_042_MTPL_Material_Damage$Incurred_Claims

# Número de anos observados
n_years_042 <- dplyr::n_distinct(
  lubridate::year(
    as.Date(lob_042_MTPL_Material_Damage$Accident_Date)
  )
)


# PARTE 1: q = 0.80 a 0.89

probs_042_1 <- seq(0.80, 0.89, by = 0.005)

par(
  mfrow = c(4, 5),
  mar = c(4, 4, 3, 1)
)

for (p in probs_042_1) {
  
  u <- as.numeric(
    quantile(
      x_042,
      probs = p,
      na.rm = TRUE
    )
  )
  
  k <- sum(
    x_042 > u,
    na.rm = TRUE
  )
  
  npy <- k / n_years_042
  
  fit <- POT::fitgpd(
    data = x_042,
    threshold = u,
    est = "mle"
  )
  
  POT::retlev(
    fit,
    npy = npy,
    points = TRUE,
    ci = TRUE,
    xlimsup = 20,
    main = paste0(
      "q = ", sprintf("%.3f", p),
      "\nu = ", scales::comma(round(u))
    )
  )
}

par(mfrow = c(1, 1))


# PARTE 2: q = 0.895 a 0.98

probs_042_2 <- seq(0.895, 0.98, by = 0.005)

par(
  mfrow = c(4, 5),
  mar = c(4, 4, 3, 1)
)

for (p in probs_042_2) {
  
  u <- as.numeric(
    quantile(
      x_042,
      probs = p,
      na.rm = TRUE
    )
  )
  
  k <- sum(
    x_042 > u,
    na.rm = TRUE
  )
  
  npy <- k / n_years_042
  
  fit <- POT::fitgpd(
    data = x_042,
    threshold = u,
    est = "mle"
  )
  
  POT::retlev(
    fit,
    npy = npy,
    points = TRUE,
    ci = TRUE,
    xlimsup = 20,
    main = paste0(
      "q = ", sprintf("%.3f", p),
      "\nu = ", scales::comma(round(u))
    )
  )
}

par(mfrow = c(1, 1))

# q = 0.90 a 0.94

# probs_042_ret_1 <- seq(0.90, 0.94, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/returnlevels42_1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_042_ret_1) {
#   
#   u <- as.numeric(
#     quantile(x_042, probs = p, na.rm = TRUE)
#   )
#   
#   k <- sum(x_042 > u, na.rm = TRUE)
#   npy <- k / n_years_042
#   
#   fit <- POT::fitgpd(
#     data = x_042,
#     threshold = u,
#     est = "mle"
#   )
#   
#   POT::retlev(
#     fit,
#     npy = npy,
#     points = TRUE,
#     ci = TRUE,
#     xlimsup = 20,
#     main = paste0(
#       "q = ", sprintf("%.3f", p),
#       "\nu = ", scales::comma(round(u))
#     )
#   )
# }
# 
# mtext(
#   "Return level plots - MTPL Material Damage",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# dev.off()
# 
# 
# # q = 0.945 a 0.98
# 
# probs_042_ret_2 <- seq(0.945, 0.98, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/returnlevels42_2.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_042_ret_2) {
#   
#   u <- as.numeric(
#     quantile(x_042, probs = p, na.rm = TRUE)
#   )
#   
#   k <- sum(x_042 > u, na.rm = TRUE)
#   npy <- k / n_years_042
#   
#   fit <- POT::fitgpd(
#     data = x_042,
#     threshold = u,
#     est = "mle"
#   )
#   
#   POT::retlev(
#     fit,
#     npy = npy,
#     points = TRUE,
#     ci = TRUE,
#     xlimsup = 20,
#     main = paste0(
#       "q = ", sprintf("%.3f", p),
#       "\nu = ", scales::comma(round(u))
#     )
#   )
# }
# 
# mtext(
#   "Return level plots - MTPL Material Damage",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# dev.off()

table_rl_042 <- return_level_table_POT(
  dados_lob = lob_042_MTPL_Material_Damage,
  probs = seq(0.80, 0.98, by = 0.005),
  return_periods = c(2, 5, 10, 15, 20)
)

#View(table_rl_042)


# write.csv(
#   table_rl_042,
#   "C:/Users/fatim/Downloads/return_levels_POT_042.csv",
#   row.names = FALSE
# )

#3

x_011 <- lob_011_Health$Incurred_Claims

# Número de anos observados
n_years_011 <- dplyr::n_distinct(
  lubridate::year(
    as.Date(lob_011_Health$Accident_Date)
  )
)


# PARTE 1: q = 0.80 a 0.89

probs_011_1 <- seq(0.80, 0.89, by = 0.005)

par(
  mfrow = c(4, 5),
  mar = c(4, 4, 3, 1)
)

for (p in probs_011_1) {
  
  u <- as.numeric(
    quantile(
      x_011,
      probs = p,
      na.rm = TRUE
    )
  )
  
  k <- sum(
    x_011 > u,
    na.rm = TRUE
  )
  
  npy <- k / n_years_011
  
  fit <- POT::fitgpd(
    data = x_011,
    threshold = u,
    est = "mle"
  )
  
  POT::retlev(
    fit,
    npy = npy,
    points = TRUE,
    ci = TRUE,
    xlimsup = 20,
    main = paste0(
      "q = ", sprintf("%.3f", p),
      "\nu = ", scales::comma(round(u))
    )
  )
}

par(mfrow = c(1, 1))


# PARTE 2: q = 0.895 a 0.98

probs_011_2 <- seq(0.895, 0.98, by = 0.005)

par(
  mfrow = c(4, 5),
  mar = c(4, 4, 3, 1)
)

for (p in probs_011_2) {
  
  u <- as.numeric(
    quantile(
      x_011,
      probs = p,
      na.rm = TRUE
    )
  )
  
  k <- sum(
    x_011 > u,
    na.rm = TRUE
  )
  
  npy <- k / n_years_011
  
  fit <- POT::fitgpd(
    data = x_011,
    threshold = u,
    est = "mle"
  )
  
  POT::retlev(
    fit,
    npy = npy,
    points = TRUE,
    ci = TRUE,
    xlimsup = 20,
    main = paste0(
      "q = ", sprintf("%.3f", p),
      "\nu = ", scales::comma(round(u))
    )
  )
}

par(mfrow = c(1, 1))

# probs_011_ret_1 <- seq(0.80, 0.84, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/returnlevels11_1.pdf",
#   width = 8,
#   height = 6
# )
# 
# par(
#   mfrow = c(3, 3),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_011_ret_1) {
#   
#   u <- as.numeric(
#     quantile(
#       x_011,
#       probs = p,
#       na.rm = TRUE
#     )
#   )
#   
#   k <- sum(x_011 > u, na.rm = TRUE)
#   
#   npy <- k / n_years_011
#   
#   fit <- POT::fitgpd(
#     data = x_011,
#     threshold = u,
#     est = "mle"
#   )
#   
#   POT::retlev(
#     fit,
#     npy = npy,
#     points = TRUE,
#     ci = TRUE,
#     xlimsup = 20,
#     main = paste0(
#       "q = ", sprintf("%.3f", p),
#       "\nu = ", scales::comma(round(u))
#     )
#   )
# }
# 
# mtext(
#   "Return level plots - Health",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()
# 
# probs_011_ret_2 <- seq(0.845, 0.89, by = 0.005)
# 
# pdf(
#   file = "C:/Users/fatim/Downloads/returnlevels11_2.pdf",
#   width = 8,
#   height = 5
# )
# 
# par(
#   mfrow = c(2, 5),
#   mar = c(4, 4, 3, 1),
#   oma = c(0, 0, 3, 0)
# )
# 
# for (p in probs_011_ret_2) {
#   
#   u <- as.numeric(
#     quantile(
#       x_011,
#       probs = p,
#       na.rm = TRUE
#     )
#   )
#   
#   k <- sum(x_011 > u, na.rm = TRUE)
#   
#   npy <- k / n_years_011
#   
#   fit <- POT::fitgpd(
#     data = x_011,
#     threshold = u,
#     est = "mle"
#   )
#   
#   POT::retlev(
#     fit,
#     npy = npy,
#     points = TRUE,
#     ci = TRUE,
#     xlimsup = 20,
#     main = paste0(
#       "q = ", sprintf("%.3f", p),
#       "\nu = ", scales::comma(round(u))
#     )
#   )
# }
# 
# mtext(
#   "Return level plots - Health",
#   outer = TRUE,
#   cex = 1.3,
#   font = 2
# )
# 
# par(mfrow = c(1, 1))
# 
# dev.off()

table_rl_011 <- return_level_table_POT(
  dados_lob = lob_011_Health,
  probs = seq(0.80, 0.98, by = 0.005),
  return_periods = c(2, 5, 10, 15, 20)
)

#View(table_rl_011)

# write.csv(
#   table_rl_011,
#   "C:/Users/fatim/Downloads/return_levels_POT_011.csv",
#   row.names = FALSE
# )

#Hill plot

#1

x_041 <- lob_041_MTPL_Bodily_Injury$Incurred_Claims
x_041 <- x_041[
  !is.na(x_041) &
    is.finite(x_041) &
    x_041 > 0
]


hill_041_evir <- evir::hill(
  x_041,
  option = "xi",
  start = 10,
  end = 1000,
)

# pdf(
#   "C:/Users/fatim/Downloads/hill_evir_041.pdf",
#   width = 9,
#   height = 5.5
# )
# 
# par(
#   mar = c(5, 5, 4, 2) + 0.1,
#   cex.axis = 0.9,
#   cex.lab = 1
# )
# 
# hill_041_evir <- evir::hill(
#   x_041,
#   option = "xi",
#   start = 10,
#   end = 1000
# )
# 
# dev.off()

# Zoom do Hill plot
hill_041_zoom <- evir::hill(
  x_041,
  option = "xi",
  start = 20,
  end = 120
)

# Limites da região selecionada: k = 40 e k = 75
abline(
  v = c(40, 75),
  lty = 2,
  lwd = 1.2
)

# pdf(
#   "C:/Users/fatim/Downloads/hill_evir_041_zoom.pdf",
#   width = 8,
#   height = 5.5
# )
# 
# par(
#   mar = c(5, 5, 4, 2) + 0.1,
#   cex.axis = 0.9,
#   cex.lab = 1
# )
# 
# hill_041_zoom <- evir::hill(
#   x_041,
#   option = "xi",
#   start = 20,
#   end = 120
# )
# 
# abline(
#   v = c(40, 75),
#   lty = 2,
#   lwd = 1.2
# )
# 
# dev.off()

x_sort_041 <- sort(x_041, decreasing = TRUE)
n_041 <- length(x_sort_041)

table_041_evir <- data.frame(
  k = hill_041_evir$x,
  q = 1 - hill_041_evir$x / n_041,
  threshold = x_sort_041[hill_041_evir$x + 1],
  xi_Hill = hill_041_evir$y
) %>%
  arrange(k)

# write.csv(
#   table_041_evir,
#   "C:/Users/fatim/Downloads/hill_evir_table_041.csv",
#   row.names = FALSE
# )


#2

x_042 <- lob_042_MTPL_Material_Damage$Incurred_Claims
x_042 <- x_042[
  !is.na(x_042) &
    is.finite(x_042) &
    x_042 > 0
]

hill_042_evir <- evir::hill(
  x_042,
  option = "xi",
  start = 10,
  end = 1000
)

# pdf(
#   "C:/Users/fatim/Downloads/hill_evir_042.pdf",
#   width = 9,
#   height = 5.5
# )
# 
# par(
#   mar = c(5, 5, 4, 2) + 0.1,
#   cex.axis = 0.9,
#   cex.lab = 1
# )
# 
# hill_042_evir <- evir::hill(
#   x_042,
#   option = "xi",
#   start = 10,
#   end = 1000
# )
# 
# dev.off()

# pdf(
#   "C:/Users/fatim/Downloads/hill_evir_042_zoom.pdf",
#   width = 8,
#   height = 5.5
# )
# 
# par(
#   mar = c(5, 5, 4, 2) + 0.1,
#   cex.axis = 0.9,
#   cex.lab = 1
# )
# 
# evir::hill(
#   x_042,
#   option = "xi",
#   start = 300,
#   end = 750
# )
# 
# abline(
#   v = c(430, 650),
#   lty = 2,
#   lwd = 1.2
# )
# 
# dev.off()

table_042_evir <- data.frame(
  k = hill_042_evir$x,
  xi_Hill = hill_042_evir$y
) %>%
  arrange(k)

x_sort_042 <- sort(x_042, decreasing = TRUE)
n_042 <- length(x_sort_042)

table_042_evir <- data.frame(
  k = hill_042_evir$x,
  q = 1 - hill_042_evir$x / n_042,
  threshold = x_sort_042[hill_042_evir$x + 1],
  xi_Hill = hill_042_evir$y
) %>%
  arrange(k)


# write.csv(
#   table_042_evir,
#   "C:/Users/fatim/Downloads/hill_evir_table_042.csv",
#   row.names = FALSE
# )


#3

x_011 <- lob_011_Health$Incurred_Claims
x_011 <- x_011[
  !is.na(x_011) &
    is.finite(x_011) &
    x_011 > 0
]

hill_011_evir <- evir::hill(
  x_011,
  option = "xi",
  start = 10,
  end = 1000
)

# pdf(
#   "C:/Users/fatim/Downloads/hill_evir_011.pdf",
#   width = 9,
#   height = 5.5
# )
# 
# par(
#   mar = c(5, 5, 4, 2) + 0.1,
#   cex.axis = 0.9,
#   cex.lab = 1
# )
# 
# hill_011_evir <- evir::hill(
#   x_011,
#   option = "xi",
#   start = 10,
#   end = 1000
# )
# 
# dev.off()

# pdf(
#   "C:/Users/fatim/Downloads/hill_evir_011_zoom.pdf",
#   width = 8,
#   height = 5.5
# )
# 
# par(
#   mar = c(5, 5, 4, 2) + 0.1,
#   cex.axis = 0.9,
#   cex.lab = 1
# )
# 
# evir::hill(
#   x_011,
#   option = "xi",
#   start = 600,
#   end = 1000
# )
# 
# abline(
#   v = c(750, 1000),
#   lty = 2,
#   lwd = 1.2
# )
# 
# dev.off()

x_sort_011 <- sort(x_011, decreasing = TRUE)
n_011 <- length(x_sort_011)

table_011_evir <- data.frame(
  k = hill_011_evir$x,
  q = 1 - hill_011_evir$x / n_011,
  threshold = x_sort_011[hill_011_evir$x + 1],
  xi_Hill = hill_011_evir$y
) %>%
  arrange(k)

# write.csv(
#   table_011_evir,
#   "C:/Users/fatim/Downloads/hill_evir_table_011.csv",
#   row.names = FALSE
# )

#Hill,Dekkers e Pickands
tail_index_estimators_pkg <- function(dados_lob,
                                      nome_lob = NULL,
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
  
  n <- length(x)
  
  if (n < 5) {
    stop("Insufficient number of positive observations.")
  }
  
  # Pickands requires 4k <= n
  if (is.null(k_max)) {
    k_max <- floor(n / 4)
  }
  
  k_max <- min(k_max, floor(n / 4))
  
  if (k_max < k_min) {
    stop("k_max is smaller than k_min.")
  }
  
  k_values <- seq(k_min, k_max, by = by)
  

  x_sort <- sort(x, decreasing = TRUE)
  
  
  
  results <- lapply(k_values, function(k) {
    
    # Threshold corresponding to k exceedances
    threshold <- x_sort[k + 1]
    
    # Hill
    hill <- heavytails::hill_estimator(
      x,
      k = k
    )
    
    # Dekkers-Einmahl-de Haan moment estimator
    dekkers <- heavytails::moments_estimator(
      x,
      k = k
    )
    
    # Pickands
    pickands <- heavytails::pickands_estimator(
      x,
      k = k
    )
    
    data.frame(
      LOB = nome_lob,
      k = k,
      q = 1 - k / n,
      threshold = threshold,
      Hill = hill,
      Dekkers = dekkers,
      Pickands = pickands
    )
  })
  
  dplyr::bind_rows(results)
}



#1
tail_041_pkg <- tail_index_estimators_pkg(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  k_min = 10,
  k_max = 1000,
  by = 10
)


#2
tail_042_pkg <- tail_index_estimators_pkg(
  dados_lob = lob_042_MTPL_Material_Damage,
  nome_lob = "MTPL Material Damage",
  k_min = 10,
  k_max = 1000,
  by = 10
)


#3
tail_011_pkg <- tail_index_estimators_pkg(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  k_min = 10,
  k_max = 1000,
  by = 10
)



plot_tail_estimators_pkg <- function(tail_data,
                                     nome_lob,
                                     k_region = NULL) {
  
  dados_long <- tail_data %>%
    tidyr::pivot_longer(
      cols = c(Hill, Dekkers, Pickands),
      names_to = "Estimator",
      values_to = "xi_hat"
    ) %>%
    dplyr::filter(
      !is.na(xi_hat),
      is.finite(xi_hat)
    )
  
  
  p <- ggplot(
    dados_long,
    aes(
      x = k,
      y = xi_hat,
      color = Estimator,
      linetype = Estimator
    )
  ) +
    geom_line(linewidth = 0.8) +
    scale_x_continuous(
      labels = scales::comma
    ) +
    labs(
      title = paste("Tail index estimators -", nome_lob),
      x = "Number of upper order statistics (k)",
      y = expression(hat(xi)),
      color = "Estimator",
      linetype = "Estimator"
    ) +
    theme_minimal()
  
  
  # Add selected Hill stability region
  if (!is.null(k_region)) {
    
    p <- p +
      geom_vline(
        xintercept = k_region,
        linetype = "dashed",
        color = "black",
        linewidth = 0.6
      )
  }
  
  
  return(p)
}


#1
p_041 <- plot_tail_estimators_pkg(
  tail_data = tail_041_pkg,
  nome_lob = "MTPL Bodily Injury",
  k_region = c(40, 75)
)

p_041


#2

p_042 <- plot_tail_estimators_pkg(
  tail_data = tail_042_pkg,
  nome_lob = "MTPL Material Damage",
  k_region = c(430, 650)
)

p_042



#3
p_011 <- plot_tail_estimators_pkg(
  tail_data = tail_011_pkg,
  nome_lob = "Health",
  k_region = c(750, 1000)
)

p_011



# ggsave(
#   "C:/Users/fatim/Downloads/tail_estimators_041.pdf",
#   plot = p_041,
#   width = 8,
#   height = 6
# )
# 
# ggsave(
#   "C:/Users/fatim/Downloads/tail_estimators_042.pdf",
#   plot = p_042,
#   width = 8,
#   height = 6
# )
# 
# ggsave(
#   "C:/Users/fatim/Downloads/tail_estimators_011.pdf",
#   plot = p_011,
#   width = 8,
#   height = 6
# )


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


#Estatísticas AD, CvM e KS, AIC e BIC

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

#1
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

#3
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

#2
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

#MÉTODOS AUTOMATICOS

#Algoritmo 1 - Cabras e Morales

#Testar distribuições

#Simulação multivariada Normal robusta
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


#Lognormal
#Posterior predictive exata na escala log
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


#Gama
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


#Weibull
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
  
  # Fisher Information aproximada
  
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


# Posterior predictive para Gama e Weibull
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
  
  
  # Reescalamento apenas por estabilidade numérica
  
  scale_factor <- median(x_train)
  
  if (
    !is.finite(scale_factor) ||
    scale_factor <= 0
  ) {
    
    return(NA_real_)
  }
  
  y_train <- x_train / scale_factor
  y_obs <- x_obs / scale_factor
  
  
  # Ajuste paramétrico
  
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
  
  
  # Transformar para log-parâmetros
  #
  # eta = log(theta)
  #
  # Delta method:
  #
  # Var(log theta)
  # ≈ J Var(theta) J'
  
  eta_hat <- log(theta_hat)
  
  J <- diag(
    1 / theta_hat,
    nrow = length(theta_hat)
  )
  
  vcov_eta <-
    J %*%
    vcov_theta %*%
    J
  
  
  # Draws da posterior aproximada
  
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
  
  
  # Probabilidade preditiva
  
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
  
  
  if (body_dist == "lognormal") {
    
    return(
      ppp_normal_leave_one_out(
        z_obs = log(x_obs),
        z_train = log(x_train),
        tail = tail
      )
    )
  }
  
  
  posterior_predictive_positive(
    x_obs = x_obs,
    x_train = x_train,
    body_dist = body_dist,
    tail = tail,
    n_draws = n_draws
  )
}


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


#Aplicação LOBS
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



set.seed(20260808) # Para tornar a aproximação Monte Carlo reproduzível

#3
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

#Gráfico melhor
qqplot_corpo_comum_todas <- function(
    diagnostico_corpo,
    nome_lob = "") {
  
  x_body <- sort(
    diagnostico_corpo$x_body
  )
  
  p_emp <- ppoints(
    length(x_body)
  )
  
  body_dists <- c(
    "lognormal",
    "gamma",
    "weibull"
  )
  
  
  dados_qq <- dplyr::bind_rows(
    
    lapply(
      body_dists,
      function(body_dist) {
        
        fit_i <-
          diagnostico_corpo$fits[[body_dist]]
        
        if (is.null(fit_i)) {
          return(NULL)
        }
        
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
                  estimates["meanlog"]
                ),
              sdlog =
                unname(
                  estimates["sdlog"]
                )
            ),
          
          gamma =
            qgamma(
              p_emp,
              shape =
                unname(
                  estimates["shape"]
                ),
              rate =
                unname(
                  estimates["rate"]
                )
            ),
          
          weibull =
            qweibull(
              p_emp,
              shape =
                unname(
                  estimates["shape"]
                ),
              scale =
                unname(
                  estimates["scale"]
                )
            )
        )
        
        
        data.frame(
          theoretical =
            q_theo_scaled * sf,
          
          empirical =
            x_body,
          
          distribution =
            body_dist
        )
      }
    )
  )
  
  
  dados_qq$distribution <-
    factor(
      dados_qq$distribution,
      levels = c(
        "lognormal",
        "gamma",
        "weibull"
      ),
      labels = c(
        "Lognormal",
        "Gamma",
        "Weibull"
      )
    )
  
  
  ggplot(
    dados_qq,
    aes(
      x = theoretical,
      y = empirical,
      colour = distribution
    )
  ) +
    
    geom_point(
      alpha = 0.55,
      size = 1.5
    ) +
    
    geom_line(
      alpha = 0.8
    ) +
    
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      colour = "black"
    ) +
    
    scale_x_log10(
      labels = scales::comma
    ) +
    
    scale_y_log10(
      labels = scales::comma
    ) +
    
    labs(
      title =
        paste(
          "Body QQ plot -",
          nome_lob
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
      
      x = "Theoretical quantiles",
      
      y = "Empirical quantiles",
      
      colour = "Distribution"
    ) +
    
    theme_minimal() +
    
    theme(
      legend.position = "bottom"
    )
}

qq_health <- qqplot_corpo_comum_todas(
  health_body_diagnostics,
  nome_lob = "Health"
)

qq_health

# ggsave(
#   filename = "QQplot_Body_Health.pdf",
#   plot = qq_health,
#   width = 8,
#   height = 6,
#   units = "in"
# )



#1
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

qq_bodily <- qqplot_corpo_comum_todas(
  bodily_body_diagnostics,
  nome_lob = "MTPL Bodily Injury"
)

qq_bodily

# ggsave(
#   filename = "QQplot_Body_MTPL_Bodily_Injury.pdf",
#   plot = qq_bodily,
#   width = 8,
#   height = 6,
#   units = "in"
# )


#2
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

qq_material <- qqplot_corpo_comum_todas(
  material_body_diagnostics,
  nome_lob = "MTPL Material Damage"
)

qq_material


# ggsave(
#   filename = "QQplot_Body_MTPL_Material_Damage.pdf",
#   plot = qq_material,
#   width = 8,
#   height = 6,
#   units = "in"
# )



# P-value preditivo leave-one-out usando T = X_(n)

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

#1
cm_xmax_bodily <- cabras_morales_xmax_lob(
  dados_lob = lob_041_MTPL_Bodily_Injury,
  nome_lob = "MTPL Bodily Injury",
  claim_var = "Incurred_Claims",
  alpha = 0.05,
  body_dist = "lognormal"
)

#3
cm_xmax_health <- cabras_morales_xmax_lob(
  dados_lob = lob_011_Health,
  nome_lob = "Health",
  claim_var = "Incurred_Claims",
  alpha = 0.05,
  body_dist = "gamma"
)

#2
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

#Algoritmo 2 - Dupuis (OBRE) 

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

#Análise de c
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

#1
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

#3
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

#2
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

#Gráficos
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

#Descrição do método para um threshold
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


#SIMULAÇÃO DE CALIBRAÇÃO DOS PESOS

#Gerador de observações de uma distribuição GP
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

#Ajuste OBRE diretamente a uma amostra de excessos
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
  
  
  # Valores iniciais por MLE
  
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
    
    # Atualização dos parâmetros com redução do passo caso o suporte da GP seja violado
    
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


#Calibração dos pesos para um threshold
# Procedimento:
#   1. ajusta OBRE aos excessos observados;
#   2. obtém xi_hat;
#   3. simula B amostras GP com:
#          n = número de excessos
#          sigma = 1
#          xi = xi_hat
#   4. ajusta OBRE em cada amostra simulada;
#   5. compara os pesos observados com os pesos simulados associados à mesma posição (rank)

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
  
  # Ordenar os excessos observados
  
  ord_obs <- order(y)
  
  y_ord <- y[ord_obs]
  w_obs <- fit_obs$weights[ord_obs]
  
  # Matriz onde serão guardados os pesos simulados
  # linha = simulação ; coluna = rank da observação
  
  W_sim <- matrix(
    NA_real_,
    nrow = B,
    ncol = k
  )
  
  #Simulações
  
  b_success <- 0
  attempts <- 0
  
  max_attempts <- 5 * B
  
  
  while (
    b_success < B &&
    attempts < max_attempts
  ) {
    
    attempts <- attempts + 1
    
    # Dupuis fixa sigma = 1 devido à invariância de escala
    
    y_sim <- rgpd_std(
      n = k,
      sigma = 1,
      xi = xi_hat
    )
    
    # Ajustar novamente OBRE à amostra simulada
    
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
    
    # Ordenar a amostra simulada e os respetivos pesos
    
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
  
  # Verificar quantas simulações foram concluídas
  
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
  
  
  # Distribuição dos pesos simulados por rank
  
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
  
  
  # p-value empírico
  # Probabilidade, sob a GP, de obter um peso igual ou inferior ao peso observado naquele rank
  # p pequeno: downweighting observado é mais severo do que seria normalmente esperado sob a GP
  
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
  
  downweighted_table <-
    calibration_table[
      calibration_table$observed_weight <
        (1 - 1e-10),
      ,
      drop = FALSE
    ]

  
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


#Aplicação da calibração a vários thresholds
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

#1
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

#3
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

#2
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

#Algoritmo 3 - Northrop and Coleman
source("C:/Users/fatim/Downloads/NorthropColeman2014.fns")

#1
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


#3
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

#2
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

# Algoritmo 4 - Wadsworth
source("C:/Users/fatim/Downloads/JointMLEFunctions.r")

#Problema código autores
#os sinistros têm valores muito grandes;a integração numérica da matriz de informação podia falhar;
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

#1
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


#3
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

#2
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

#Algoritmo 5 - Beirlant

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

#Ajuste regressão para um dado k
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

#1
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

#3
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

#2
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

#Algoritmo 6 

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

#1
res_041 <- ferreira_bootstrap_k(
  lob_041_MTPL_Bodily_Injury$Incurred_Claims,
  n1 = floor(length(lob_041_MTPL_Bodily_Injury$Incurred_Claims)^0.9),
  r = 200,
  k_min = 20,
  usar_indicador = FALSE
)

#3
res_011 <- ferreira_bootstrap_k(
  lob_011_Health$Incurred_Claims,
  n1 = floor(length(lob_011_Health$Incurred_Claims)^0.9),
  r = 200,
  k_min = 20,
  usar_indicador = FALSE
)

#2
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

#Algoritmo 7

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

#Algoritmo 8 - LOMBA E ALVES

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

#1
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

#3
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

#2
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

#!/usr/bin/env Rscript
# Calcula o Rt baseado no registro de casos novos
#
# Fernando Ferreira 05/07/2020

if (!require('EpiEstim')) {install.packages('EpiEstim')}
if (!require ('tidyverse')) {install.packages('tidyverse')}
if (!require ('data.table')) {install.packages('data.table')}

library('EpiEstim')
library('tidyverse')
library('data.table')

# Le banco
dados <- fread(
  file = '../BancoDeDadosAtualizado/base_srag_boletim22Jul.csv',
  sep = ';',
  quote = "\"",
  encoding = 'Latin-1',
  header = TRUE
)

banco = '200722'
###########################################################
######  SELECIONE O NUMERO MINIMO DE DIAS COM CASOS  ######
minimo_dias <- 10
###########################################################
dados <- dados %>%
  rename(DRS = `17DRS`)

agregado <- c('SG_UF', 'DRS', 'ID_RG_RESI', 'ID_MN_RESI')

for (i in seq(1:length(agregado))) {
  
  f <- paste('DT_SIN_PRI + ', agregado[i],' ~ classi')
  dados_an <- do.call("dcast", list(as.formula(f), data=as.name("dados")))
  dados_an$COVID_PREV <- round(dados_an$`COVID-19` + 
    dados_an$`Em investigacao`*(dados_an$`COVID-19`/(dados_an$`COVID-19`+ 
                                                       dados_an$Influenza +
                                                       dados_an$`Outros agentes` +
                                                       dados_an$`Outros virus` +
                                                       dados_an$`SRAG nao especificado`)))
  dados_an$COVID_PREV[is.na(dados_an$COVID_PREV)] <- 0
  dados_covid <- dados_an %>%
    select(.data[[agregado[i]]], DT_SIN_PRI, COVID_PREV, `COVID-19`, `Em investigacao`) %>%
    rename(dates = DT_SIN_PRI, I = COVID_PREV, COVID_NOT = `COVID-19`, INVESTIG = `Em investigacao`) %>%
    mutate(dates = as.Date(dates))
  
  ids_agregados <- distinct(dados_covid, .data[[agregado[i]]])
  for (j in seq(1:lengths(ids_agregados))) {
    nivel_agregado <- dados_covid %>%
      filter(.data[[agregado[i]]] == ids_agregados[[1]][j]) %>%
      complete(dates = seq.Date(min(dates), max(dates), by = "day")) %>%
      replace_na(list(I = 0, COVID_NOT = 0, INVESTIG = 0))

    ggplot(nivel_agregado) +
      geom_point(mapping = aes(x = dates, y = I, color = 'red'), alpha = 0.6) +
      geom_line(mapping = aes(x = dates, y = I, color = 'red'), linetype = "dashed", size=0.1, alpha = 0.6) +
      geom_point(mapping = aes(x = dates, y = COVID_NOT, color = 'navy'), alpha = 0.6) +
      geom_line(mapping = aes(x = dates, y = COVID_NOT, color = 'navy'), alpha = 0.6) +
      geom_point(mapping = aes(x = dates, y = INVESTIG, color = 'darkgreen'), alpha = 0.6) +
      geom_line(mapping = aes(x = dates, y = INVESTIG, color = 'darkgreen'), alpha = 0.6) +
      scale_color_identity(name = "Legenda",
                            breaks = c("red", "navy", "darkgreen"),
                            labels = c("Estimado", "Observado", "Em investigação"),
                            guide = "legend") +
      labs(x = "Data", y = "Casos") +
      ggtitle(paste(agregado[i],"-",ids_agregados[[1]][j],sep = "")) 
    ggsave(filename = paste(banco, '_Inc_Corrig-', agregado[i], "-", ids_agregados[[1]][j], ".png", sep = ""),
            width = 12, height = 8, dpi = 300)
    nivel_agregado1 <- nivel_agregado %>%
      select(dates,I)

    if (count(nivel_agregado1 %>% filter(nivel_agregado1$I>0))>minimo_dias) {
      res <- estimate_R(incid = nivel_agregado1,
                        method = "parametric_si",
                        config = make_config(list(mean_si = 4.7, std_si = 2.9)))
      png(
        file = paste(banco, '_Rt_Corr-', agregado[i], "-", ids_agregados[[1]][j], ".png", sep = ""),
        width = 1800,
        height = 2200,
        res = 300
      )
      plot(res)
      dev.off()
    }
  }
}


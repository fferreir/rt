# Calcula o Rt baseado no registro de casos novos
#
# Fernando Ferreira 05/07/2020

if(!require('EpiEstim')) {install.packages('EpiEstim')}
if(!require ('tidyverse')) {install.packages('tidyverse')}
if(!require ('data.table')) {install.packages('data.table')}

library('EpiEstim')
library('tidyverse')
library('data.table')

# L? banco
dados <- fread(file = '../BancoDeDadosAtualizado/base_srag_boletim22Jul.csv',
               sep = ';',
               quote = "\"",
               encoding = 'Latin-1',
               header = TRUE)

banco = '200722'
###########################################################
######  SELECIONE O N?MERO M?NIMO DE DIAS COM CASOS  ######
minimo_dias <- 10
###########################################################

agregado <- c('SG_UF', '17DRS', 'ID_RG_RESI', 'ID_MN_RESI')

for (i in seq(1:length(agregado))) {
  # An?lise dos munic?pios com s?rie de dados maior que 27 dias
  dados_covid <- dados %>%
    filter(classi == 'COVID-19')  %>%
    select(.data[[agregado[i]]], DT_SIN_PRI) %>%
    rename(dates = DT_SIN_PRI) %>%
    mutate(dates = as.Date(dates)) %>%
    group_by(.data[[agregado[i]]], dates) %>%
    summarize( I = n())

  ids_agregados <- distinct(dados_covid,.data[[agregado[i]]])
  for (j in seq(1:lengths(ids_agregados))) {
    nivel_agregado <- dados_covid %>%
      filter(.data[[agregado[i]]] == ids_agregados[[1]][j]) %>%
      complete(dates = seq.Date(min(dates), max(dates), by = "day")) %>%
      replace_na(list(I = 0))

    if (nrow(nivel_agregado %>% filter(nivel_agregado$I>0)) > minimo_dias) {
      res <- estimate_R(incid = nivel_agregado,
                        method = "parametric_si",
                        config = make_config(list(mean_si = 4.7, std_si = 2.9)))
      png(file = paste(banco, agregado[i],"-",ids_agregados[[1]][j],".png",sep = ""),
          width = 1800, height = 2200, res = 300)
      plot(res)
      dev.off()
    }
  }
}

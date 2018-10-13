# Deputados SP

Esse app pode ser usado para visualizar a votação dos deputados estaduais e federais de SP por zona eleitoral.

Fiz só pra SP porque achei um mapa para as zonas eleitorais, mas seria relativamente fácil ampliar para outros estados se houver mapas disponíveis.

## Fonte dos dados  

1. Mapa das zonas eleitorais: https://github.com/mapaslivres/zonas-eleitorais
2. Mapa dos municípios: http://www.usp.br/nereus/?dados=unidades-federativas
3. Votação: http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais

## Pre-processamento

Para agilizar o app, os dados brutos da justiça eleitoral foram pré-processados com o seguinte código:

```{R}
library(tidyverse)

elec_res = read_delim('bweb_1t_SP_101020182030.csv',
                      delim=';',
                      locale = locale(encoding = 'latin1')) %>% 
  dplyr::select(CD_ELEICAO,
                NR_ZONA,
                NR_SECAO,
                CD_CARGO_PERGUNTA,
                DS_CARGO_PERGUNTA,
                SG_PARTIDO,
                DS_TIPO_URNA,
                NM_VOTAVEL,
                QT_VOTOS)

elec_filt = elec_res %>%
  dplyr::filter(DS_TIPO_URNA == 'Apurada',
                DS_CARGO_PERGUNTA %in% c("Deputado Federal", "Deputado Estadual"),
                !NM_VOTAVEL %in% c('Branco','Nulo'),
                !NM_VOTAVEL %in% unique(SG_PARTIDO)) %>%
  group_by(NR_ZONA, DS_CARGO_PERGUNTA, SG_PARTIDO, NM_VOTAVEL) %>%
  summarise(VOTOS=sum(QT_VOTOS)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(nome_pt = str_c(NM_VOTAVEL,' (',SG_PARTIDO,')')) %>%
  ungroup() %>%
  mutate(NM_VOTAVEL = as.factor(NM_VOTAVEL),
         nome_pt = as.factor(nome_pt),
         SG_PARTIDO = as.factor(SG_PARTIDO),
         DS_CARGO_PERGUNTA = as.factor(DS_CARGO_PERGUNTA))

save(elec_filt,file = 'summarized_results.Rdata')
```


Feito por Bruno de Medeiros em Outubro/2018


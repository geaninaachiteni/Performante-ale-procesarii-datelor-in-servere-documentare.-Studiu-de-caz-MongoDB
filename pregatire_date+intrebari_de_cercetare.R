####################################################################################
###                   Disertatie - Geanina Achiteni
####################################################################################

library(tidyverse)
library(janitor)
setwd('/Users/Geanina/TPCH_clean/rezultate_ok')



fisiere <- list.files(pattern = '.csv')
df_fisiere <- tibble(nume_fisier = fisiere) |>
    mutate(scale_factor = str_extract(nume_fisier, '^sf_[0-9]{1,3}(|_[0-9]{1,2})__')) |>
    mutate(scale_factor = str_remove_all(scale_factor, '^sf_|__')) |>
    mutate(scale_factor = str_replace_all(scale_factor, '_', '.')) |>
    mutate(scale_factor = as.numeric(scale_factor)) |>
    mutate(nr_sharduri = as.numeric(str_extract(str_extract(nume_fisier, '_[0-9]{1,2}shard'), '[0-9]{1,2}'))) |>
    mutate(executie_nr = str_extract(nume_fisier, 'run[0-9]{1,2}'))


df_interogari <- tibble()  
df_rezultate <- tibble()  


idx_fisier <- 1
for (idx_fisier in 1:nrow(df_fisiere)) {
    fisier_crt <- df_fisiere$nume_fisier[idx_fisier]
    temp <- read.csv(fisier_crt, header = TRUE) 
    #glimpse(temp)
    if (idx_fisier == 1) {
        df_interogari <- temp |>
            select(query_number, query_text)
    }
    
    df_rezultate <- bind_rows(df_rezultate, df_fisiere[idx_fisier,] |> cross_join(temp |> select (-query_text)))
    
}

glimpse(df_rezultate)

rezultate <- df_rezultate |>
    arrange(query_number, scale_factor, nr_sharduri, executie_nr) |>
    select(scale_factor:query_number, execution_time_ms) |>
    group_by(scale_factor, nr_sharduri, query_number) |>
    summarise(durata_ms = median(execution_time_ms, na.rm = TRUE)) |>
    ungroup()


parametri_interogari <- df_interogari |>
    mutate(nr_sort = str_count(query_text, '(\\.|\\$)sort(\\(|\\:)')) |>
    mutate(ordonare = if_else(nr_sort > 0, 'cu ordonare', 'fara ordonare')) |>
    mutate(nr_predicate = str_count(query_text, '(\\.find\\(|\\$match\\:)')) |>
    mutate(facet = if_else(str_detect(query_text, '\\$facet\\:'), 'cu facet', 'fara facet')) |>
    mutate(nr_grupari = str_count(query_text, 'groupBy\\:|\\$group\\:')) |>
    mutate(nr_limitari = str_count(query_text, '.limit\\(|\\$limit\\:')) |>
    mutate(nr_lookup = str_count(query_text, '\\$lookup\\:')) |>
    mutate(nr_unwind = str_count(query_text, '\\$unwind\\:')) |>
    transmute(query_number, ordonare, folosire_facet = facet, nr_grupari, nr_predicate, nr_limitari, nr_lookup, nr_unwind, nr_sort)
  



getwd()


df <- rezultate |>
    inner_join(parametri_interogari)


library(ggstatsplot)


# Problema nr. 1: Este durata de executie a interogarilor asociata cu marimea BD?
# Variabila durata_ms este numerica
shapiro.test(df$durata_ms)
# ipoteza nula a testului Shapiro-Wilk este ca distributia variabilei este relativ normala
# W = 0.073614, p-value < 2.2e-16
# p-value este mult sub pragul de semnificatie de 0.05, deci ipoteza nula a testlui S-W este respinsa, 
#   altfel spus, durata executiei nu are o distributie normala.

# Rezulta ce testele in care va fi implicata durata vor fi neparametrice

ggstatsplot::ggbetweenstats(
    data = df |> mutate(scale_factor = as.factor(scale_factor)), 
    x = scale_factor, 
    y = durata_ms, 
    type = 'np',  # testul va fi unul neparametric
    title = "Durata de execuție în funcție de dimensiunea bazei de date (scale factor)",
    xlab = "Scale Factor",
    ylab = "Durata execuției (milisecunde)"
)

# Interpretare
# Testul Kruslal-Wallis are ca ipoteza nula ca NU exista diferente in distributia duratei intre 
#   diferitele marimi ale bazei de date
# p-value = 2.26 * 10 ^-16, deci mult sub pragul de 0.05; ipoteza nula este respinsa, 
#    adica durata interogarii depinde de marimea bazei de date
# marimea efectului este 0.03
library(effectsize)
effectsize::interpret_eta_squared(.03)
# Pachetul effect size califca intensitatea relatiei dintre durata si marimea BD ca fiind mica





ggstatsplot::ggscatterstats(
  data = df, 
  x = scale_factor, 
  y = durata_ms, 
  type = 'np'  # testul va fi unul neparametric
  
)

interpret_phi(0.15)




# Problema nr. 2: Este durata de executie a interogarilor asociata cu nr de sharduri

ggstatsplot::ggbetweenstats(
  data = df |> mutate(nr_sharduri = as.factor(nr_sharduri)), 
  x = nr_sharduri, 
  y = durata_ms, 
  type = 'np',
  title = "Durata de execuție a interogărilor în funcție de numărul de shard-uri",
  xlab = "Număr shard-uri",
  ylab = "Durată execuție (milisecunde)"# testul va fi unul neparametric
  
)




# Problema nr. 2a: Este durata de executie a interogarilor asociata cu nr de sharduri pentru sf 10GB

ggstatsplot::ggbetweenstats(
  data = df |> mutate(nr_sharduri = as.factor(nr_sharduri)) |> filter(scale_factor == 10), 
  x = nr_sharduri, 
  y = durata_ms, 
  type = 'np',  # testul va fi unul neparametric
  title = "Durata de execuție a interogărilor în funcție de numărul de shard-uri (SF = 10GB)",
  xlab = "Număr shard-uri",
  ylab = "Durată execuție (milisecunde)"
)



# Problema nr. 4: Este durata de executie a interogarilor asociata cu ordonarea inregistrarilor

ggstatsplot::ggbetweenstats(
  data = df , 
  x = ordonare, 
  y = durata_ms, 
  type = 'np',  # testul va fi unul neparametric
  title = "Influența ordonării rezultatelor asupra duratei de execuție a interogărilor",
  xlab = "Tip interogare",
  ylab = "Durată execuție (milisecunde)"
)
interpret_rank_biserial(0.10)






ggstatsplot::ggscatterstats(
  data = df, 
  x = nr_predicate, 
  y = durata_ms, 
  type = 'np',  # testul va fi unul neparametric
  title = "Relația dintre numărul de predicate și durata de execuție a interogărilor",
  xlab = "Număr de predicate în interogare",
  ylab = "Durată execuție (milisecunde)"
)

interpret_phi(0.12)

#################################################################################################################

# Problema nr. 5: Influențează numărul de predicate dintr-o interogare durata de execuție a acesteia? 
ggstatsplot::ggbetweenstats(
  data = df |> mutate(nr_predicate = as.factor(nr_predicate)), 
  x = nr_predicate, 
  y = durata_ms, 
  type = 'np',  # testul va fi unul neparametric
  title = "Relația dintre numărul de predicate și durata de execuție a interogărilor",
  xlab = "Număr de predicate în interogare",
  ylab = "Durată execuție (milisecunde)"
)


ggstatsplot::ggscatterstats(
  data = df, 
  x = nr_predicate, 
  y = durata_ms, 
  type = 'np',  # testul va fi unul neparametric
  title = "Relația dintre numărul de predicate și durata de execuție a interogărilor",
  xlab = "Număr de predicate în interogare",
  ylab = "Durată execuție (milisecunde)"
)

interpret_phi(0.12)


# Problema nr. 6: Influenteaza folosirea operatorului lookup, durata de executie a unei interogari?

ggstatsplot::ggbetweenstats(
  data = df |> mutate(nr_lookup = as.factor(nr_lookup)), 
  x = nr_lookup, 
  y = durata_ms, 
  type = 'np',  # testul va fi unul neparametric
  title = "Relația dintre numărul de operațiuni de tip lookup și durata de execuție a interogărilor",
  xlab = "Număr operațiuni lookup în interogare",
  ylab = "Durată execuție (milisecunde)"
)  
  
###
ggstatsplot::ggscatterstats(
  data = df, 
  x = nr_lookup, 
  y = durata_ms, 
  type = 'np',  # testul va fi unul neparametric
  title = "Relația dintre numărul de operații $lookup și durata de execuție a interogărilor",
  xlab = "Număr de operații $lookup",
  ylab = "Durată execuție (milisecunde)"
)


# Problema nr. 7: Cum variază performanța (măsurată prin durata de execuție) interogărilor 
#               în funcție de numărul de grupări și sortări aplicate?

df_grup_sort <- df |>
  select(nr_grupari, nr_sort, durata_ms) |>
  mutate(grup_sort = paste0("G", nr_grupari, "_S", nr_sort))  

ggbetweenstats(
  data = df_grup_sort,
  x = grup_sort,     # grupuri combinate: ex. G2_S1, G3_S0 etc.
  y = durata_ms,
  type = "np", # # testul va fi unul neparametric (Kruskal-Wallis dacă >2 grupuri)
  title = "Efectul combinat al numărului de grupări și sortări asupra duratei de execuție",
  xlab = "Combinații Grupare-Sortare",
  ylab = "Durată execuție (milisecunde)"
)

interpret_eta_squared(0.06)

# Problema nr. 8: Există diferențe semnificative în durata de execuție a interogărilor care utilizează operatorul $unwind,
#               comparativ cu cele care nu îl folosesc?

ggstatsplot::ggbetweenstats(
  data = df |> mutate(nr_unwind = as.factor(nr_unwind)), 
  x = nr_unwind, 
  y = durata_ms, 
  type = 'np',  # testul va fi unul neparametric
  pairwise.comparisons = FALSE,
  mean.ci = TRUE,
  results.subtitle = TRUE,
  title = "Compararea duratei de execuție între interogările care utilizează\n operatorul $unwind și cele care nu îl folosesc",
  xlab = "Unwind",
  ylab = "Durată execuție (ms)"
)  


ggstatsplot::ggscatterstats(
  data = df, 
  x = nr_unwind, 
  y = durata_ms, 
  type = 'np'  # testul va fi unul neparametric
)  
  




#Problema nr.9: Utilizarea operatorului $facet influențează semnificativ durata de execuție?

ggstatsplot::ggbetweenstats(
  data = df |> mutate(folosire_facet = as.factor(folosire_facet)), 
  x = folosire_facet, 
  y = durata_ms, 
  type = 'np',  # testul va fi unul neparametric
  title = "Influența utilizării operatorului $facet asupra duratei de execuție a interogărilor",
  xlab = "Folosirea operatorului $facet",
  ylab = "Durată execuție (milisecunde)"
)  


#Problema nr.9A: Utilizarea operatorului $facet influențează semnificativ durata de execuție pentru un scale factor de 100 MB?

ggstatsplot::ggbetweenstats(
  data = df |> mutate(folosire_facet = as.factor(folosire_facet)) |> filter(scale_factor == 0.1), 
  x = folosire_facet, 
  y = durata_ms, 
  type = 'np',  # testul va fi unul neparametric
  title = "Influența operatorului $facet asupra duratei de execuție (scale factor = 0.1)",
  xlab = "Folosire operator $facet",
  ylab = "Durată execuție (milisecunde)"
)  

#Problema nr.10: Interogările care includ limitări (nr_limitari > 0) sunt mai rapide decât cele care nu includ acest parametru?

ggbetweenstats(
  data = df |> mutate(grup_limitari = ifelse(nr_limitari > 0, "cu_limitari", "fara_limitari")),
  x = grup_limitari,
  y = durata_ms,
  type = "np", # testul va fi unul neparametric
  title = " Compararea duratei de execuție în funcție de prezența sau absența limitărilor",
  xlab = "Prezența limitărilor",
  ylab = "Durată execuție (ms)",
  results.subtitle = TRUE
)


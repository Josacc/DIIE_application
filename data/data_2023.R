
# Default databases -------------------------------------------------------

source("data/reviewer_team.R")
source("data/federal_entities.R")

# Database DIIE dates (update every year!).
DIIE_dates <- tibble(
  name = c(
    "CNGE",
    "CNSPE",
    "CNSIPEE",
    "CNPJE",
    "CNIJE",
    "CNPLE",
    "CNDHE",
    "CNTAIPPDPE"
  ) %>% factor() %>% fct_inorder(),

  `start CE` = ymd(
    c(
      "2024-05-13",
      "2024-03-04",
      "2024-02-19",
      "2024-03-11",
      "2024-05-06",
      "2024-07-22",
      "2024-07-15",
      "2024-07-22"
    )
  ),

  `end CE` = ymd(
    c(
      "2024-08-02",
      "2024-05-03",
      "2024-04-19",
      "2024-06-07",
      "2024-08-09",
      "2024-09-27",
      "2024-09-20",
      "2024-09-27"
    )
  ),

  `start DIIE` = ymd(
    c(
      "2024-07-01",
      "2024-04-15",
      "2024-04-01",
      "2024-05-06",
      "2024-06-28",
      "2024-09-02",
      "2024-09-02",
      "2024-09-02"
    )
  ),

  `end DIIE` = ymd(
    c(
      "2024-08-30",
      "2024-06-14",
      "2024-05-31",
      "2024-07-19",
      "2024-09-06",
      "2024-10-11",
      "2024-10-04",
      "2024-10-11"
    )
  ),

  prosecution = ymd(
    c(
      "2024-08-05",
      "2024-05-06",
      "2024-04-22",
      "2024-06-03",
      "2024-08-26",
      "2024-10-14",
      "2024-10-07",
      "2024-10-14"
    )
  ),

  diffusion = ymd(
    c(
      "2024-12-02",
      "2024-08-07",
      "2024-07-18",
      "2024-10-03",
      "2024-11-04",
      "2024-12-13",
      "2024-12-13",
      "2024-12-13"
    )
  )
)

# Database on questionnaires (update every year!).
questionnaires <- tibble(
  Cuestionarios = c(
    "1101", "1102", "1103", "1104", "1105", "1106", "1107", "1108", "1109", "1110", "1111", "1201", "1301", "1401",
    "2101", "2201",
    "3101", "3201",
    "4101", "4201", "4301", "4401", "4501",
    "5101", "5201", "5301", "5401", "5501", "5601", "5701",
    "6101", "6201",
    "7101", "7201"
  )
)

# Database of relation between "Actividades" and "Estatus" (update every year!).
relacion_actividad_fase <- tibble(
  `Actividades (fases)` = c(
    "Integración de información preliminar (informantes)",
    "Revisión primaria y ajustes información preliminar (ROCE)",
    "Revisión OC y liberación de información definitiva",
    "Recuperación de firmas y formalización de cuestionarios"
  ) %>% factor() %>% fct_inorder(),

  `Estatus considerado` = c(
    "Revisión ROCE (1)",
    "Revisión OC (1)",
    "En proceso de firma y sello (1)",
    "Recuperado con firma y sello (1)"
  )
)

# Database DOE dates (update every year!).
DOE_dates <- tibble(
  Censo =  c(
    rep("CNGE",  4), rep("CNSPE", 4), rep("CNSIPEE", 4), rep("CNPJE",      4),
    rep("CNIJE", 4), rep("CNPLE", 4), rep("CNDHE",   4), rep("CNTAIPPDPE", 4)
  ) %>% factor() %>% fct_inorder(),
  `Actividades (fases)` = relacion_actividad_fase %>% pull(1) %>% rep(8),
  INICIO = ymd(c(
    "2023-03-13", "2023-03-21", "2023-04-24", "2023-06-09",
    "2023-05-08", "2023-05-16", "2023-06-12", "2023-07-28",
    "2023-03-06", "2023-03-13", "2023-04-17", "2023-05-29",
    "2023-04-24", "2023-05-02", "2023-06-26", "2023-07-24",
    "2023-05-15", "2023-05-22", "2023-07-10", "2023-08-07",
    "2023-08-07", "2023-08-14", "2023-08-21", "2023-10-09",
    "2023-08-07", "2023-08-14", "2023-08-21", "2023-10-09",
    "2023-08-07", "2023-08-14", "2023-08-21", "2023-10-09"
  )),
  FIN = ymd(c(
    "2023-05-12", "2023-05-26", "2023-06-16", "2023-07-21",
    "2023-06-30", "2023-07-14", "2023-07-21", "2023-08-31",
    "2023-04-28", "2023-05-12", "2023-05-26", "2023-06-30",
    "2023-06-23", "2023-06-30", "2023-08-25", "2023-09-08",
    "2023-07-14", "2023-07-21", "2023-09-08", "2023-09-15",
    "2023-09-22", "2023-09-29", "2023-10-20", "2023-11-03",
    "2023-09-22", "2023-09-29", "2023-10-20", "2023-11-03",
    "2023-09-22", "2023-09-29", "2023-10-20", "2023-11-03"
  ))
)

# Database on everybody "folios" (update every year!).
id_folio <- federal_entities %>%
  transmute(id_estado = as.character(id_estado)) %>%
  pull() %>%
  map(~str_c(., pull(questionnaires))) %>%
  unlist() %>%
  tibble(Folio = .) # %>%
  # add_row(Folio = "091501", .after = 319) questionnaire for 2023

# Databases on everybody "Folios" extended version (update every year!).
id_folio_extended <- id_folio %>%
  separate(Folio, into = c("id_estado", "Censo_n", "Módulo"), sep = c(2, 3), remove = FALSE) %>%
  mutate(Censo = str_replace_all(Censo_n,
                                 c("1" = "CNGE",
                                   "2" = "CNSPE",
                                   "3" = "CNSIPEE",
                                   "4" = "CNPJE",
                                   "5" = "CNIJE",
                                   "6" = "CNPLE",
                                   "7" = "CNDHE",
                                   "8" = "CNTAIPPDPE" ))) %>%
  mutate(id_estado = factor(id_estado, levels = levels(federal_entities[["id_estado"]]))) %>%
  mutate(Censo_n   = factor(Censo_n  , levels = 1:8)) %>%
  mutate(Censo     = factor(Censo    , levels = levels(pull(DOE_dates, Censo)))) %>%
  left_join(federal_entities, by = "id_estado") %>%
  select(-Abreviatura)

# Database días anhábiles 2023 (update every year!).
holidays <- tibble(
  `Días Festivos 2023` = ymd(c(
    "2023-01-01", "2023-02-06", "2023-03-20", "2023-04-06", "2023-04-07", "2023-05-01",
    "2023-05-05", "2023-07-08", "2023-09-16", "2023-11-02", "2023-11-20", "2023-12-25"
  ))
)

# Database days 2023. Attention in the year! (update every year!).
dates_2023 <- tibble(Registro = (ymd("2023-01-01") + c(0:364)))

# Database not-working days 2023 (update every year!).
nonworking_days <- dates_2023 %>%
  mutate(n = wday(Registro, week_start = 1)) %>%
  filter(n > 5 | Registro %in% pull(holidays)) %>%
  pull(Registro)

# Database function to get workday.
get_workday <- function(fecha) {
  while (fecha %in% nonworking_days) {
    fecha <- fecha + 1
  }
  return(fecha)
}

# Database class Tibble con fechas del año y ajustadas a días efectivos
# (update every year!).
working_dates <- dates_2023 %>%
  pull() %>%
  map_vec(get_workday) %>%
  tibble(aux_var = .) %>%
  bind_cols(dates_2023) %>%
  relocate(Registro)

# Database fechas finales concertación de citas y entrega de cuestionarios
# (update every year!).
dates_citas_cuestionarios <- tibble(
  Censo = levels(pull(DOE_dates, Censo)),
  Fin_citas = dmy(c(
    "03/03/2023",	"28/04/2023",	"24/02/2023",	"14/04/2023",
    "12/05/2023",	"14/07/2023", "14/07/2023", "14/07/2023"
  )),
  Fin_cuestionarios = dmy(c(
    "10/03/2023",	"08/05/2023",	"03/03/2023",	"21/04/2023",
    "19/05/2023",	"21/07/2023", "21/07/2023",	"21/07/2023"
  ))
)

# Database function to get folios with status "No aplica".
DT_folio_no_aplica <- function(principal_dataframe) {

  principal_dataframe %>%
    select(Folio, Estatus) %>%
    group_by(Folio) %>%
    mutate(n = seq_along(Folio)) %>%
    slice_max(n) %>%
    ungroup() %>%
    filter(str_detect(Estatus, "No aplica")) %>%
    select(Folio)

}


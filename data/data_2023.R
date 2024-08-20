
# Default databases -------------------------------------------------------

source("data/review_team.R")

# Database DIIE dates.
census_2023 <- tibble(
  name = c("CNGE",
           "CNSPE",
           "CNSIPEE",
           "CNPJE",
           "CNIJE",
           "CNPLE",
           "CNDHE",
           "CNTAIPPDPE") %>% factor() %>% fct_inorder(),

  `start CE` = ymd(c("2023-03-13",
                     "2023-05-08",
                     "2023-03-06",
                     "2023-04-24",
                     "2023-05-15",
                     "2023-08-07",
                     "2023-08-07",
                     "2023-08-07")),

  `end CE` = ymd(c("2023-05-26",
                   "2023-07-14",
                   "2023-05-12",
                   "2023-06-30",
                   "2023-07-21",
                   "2023-09-29",
                   "2023-09-29",
                   "2023-09-29")),

  `start DIIE` = ymd(c("2023-04-24",
                       "2023-06-12",
                       "2023-04-17",
                       "2023-06-26",
                       "2023-07-10",
                       "2023-08-21",
                       "2023-08-21",
                       "2023-08-21")),

  `end DIIE` = ymd(c("2023-06-16",
                     "2023-07-21",
                     "2023-05-26",
                     "2023-08-25",
                     "2023-09-08",
                     "2023-10-20",
                     "2023-10-20",
                     "2023-10-20")),

  prosecution = ymd(c("2023-05-03",
                      "2023-06-22",
                      "2023-05-02",
                      "2023-06-19",
                      "2023-07-07",
                      "2023-10-09",
                      "2023-10-09",
                      "2023-10-09")),

  diffusion = ymd(c("2023-08-31",
                    "2023-09-28",
                    "2023-07-18",
                    "2023-10-12",
                    "2023-10-30",
                    "2023-12-15",
                    "2023-12-15",
                    "2023-12-15"))
)


entities <- tibble(
  name = c("AGUASCALIENTES",
           "BAJA CALIFORNIA",
           "BAJA CALIFORNIA SUR",
           "CAMPECHE",
           "COAHUILA DE ZARAGOZA",
           "COLIMA",
           "CHIAPAS",
           "CHIHUAHUA",
           "CIUDAD DE MÉXICO",
           "DURANGO",
           "GUANAJUATO",
           "GUERRERO",
           "HIDALGO",
           "JALISCO",
           "MÉXICO",
           "MICHOACÁN DE OCAMPO",
           "MORELOS",
           "NAYARIT",
           "NUEVO LEÓN",
           "OAXACA",
           "PUEBLA",
           "QUERÉTARO",
           "QUINTANA ROO",
           "SAN LUIS POTOSÍ",
           "SINALOA",
           "SONORA",
           "TABASCO",
           "TAMAULIPAS",
           "TLAXCALA",
           "VERACRUZ DE IGNACIO DE LA LLAVE",
           "YUCATÁN",
           "ZACATECAS") %>%
    factor() %>% fct_inorder()
)

# Database on federal entities, names, Regional and ID.
federal_entities <- tibble(
  id_estado = c(
    "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16",
    "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32"
  ) %>% factor() %>% fct_inorder(),
  Abreviatura = c(
    "Ags", "BC", "BCS", "Camp", "Coah", "Col", "Chis", "Chih",
    "CDMX",  "Dgo", "Gto", "Gro", "Hgo", "Jal", "Mex", "Mich",
    "Mor", "Nay", "NL", "Oax", "Pue", "Qro", "Q_Roo", "SLP",
    "Sin", "Son", "Tab", "Tamps", "Tlax", "Ver", "Yuc", "Zac"
  ) %>% factor() %>% fct_inorder(),
  Entidad = c(
    "AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR",
    "CAMPECHE", "COAHUILA DE ZARAGOZA", "COLIMA", "CHIAPAS",
    "CHIHUAHUA", "CIUDAD DE MÉXICO", "DURANGO", "GUANAJUATO",
    "GUERRERO", "HIDALGO", "JALISCO", "MÉXICO", "MICHOACÁN DE OCAMPO",
    "MORELOS", "NAYARIT", "NUEVO LEÓN", "OAXACA",
    "PUEBLA", "QUERÉTARO", "QUINTANA ROO", "SAN LUIS POTOSÍ",
    "SINALOA", "SONORA", "TABASCO", "TAMAULIPAS", "TLAXCALA",
    "VERACRUZ DE IGNACIO DE LA LLAVE", "YUCATÁN", "ZACATECAS"
  ) %>% factor() %>% fct_inorder(),
  Regional = c(
    "Centro Norte", "Noroeste", "Noroeste", "Sureste", "Noreste", "Occidente",
    "Sur", "Norte", "Centro", "Norte", "Centro Norte", "Centro Sur",
    "Oriente", "Occidente", "Centro Sur", "Occidente", "Centro Sur", "Occidente",
    "Noreste", "Sur", "Oriente", "Centro Norte", "Sureste", "Centro Norte",
    "Noroeste", "Noroeste", "Sur", "Noreste", "Oriente", "Oriente", "Sureste", "Norte"
  ) %>% factor(levels = c("Centro", "Centro Norte", "Centro Sur", "Noreste",
                          "Noroeste", "Norte", "Occidente", "Oriente", "Sur", "Sureste"))
)

# Database on questionnaires in 2023 (except M5-CNGE from CDMX).
questionnaires <- tibble(
  Cuestionarios = c(
    "1101", "1102", "1103", "1104", "1105", "1106", "1107", "1108", "1109", "1110", "1111", "1112", "1201", "1301", "1401",
    "2101", "2201",
    "3101", "3201",
    "4101", "4201", "4301", "4401", "4501",
    "5101", "5201", "5301", "5401", "5501", "5601", "5701",
    "6101", "6201",
    "7101", "7201",
    "8101", "8201",  "8301"
  )
)

# Database of relation between "Actividades" and "Estatus".
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

# Database DOE dates
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

# Database on everybody "folios"
id_folio <- federal_entities %>%
  transmute(id_estado = as.character(id_estado)) %>%
  pull() %>%
  map(~str_c(., pull(questionnaires))) %>%
  unlist() %>%
  tibble(Folio = .) %>%
  add_row(Folio = "091501", .after = 319)

# Databases on everybody "Folios" extended version.
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

# Database días anhábiles 2023.
holidays <- tibble(
  `Días Festivos 2023` = ymd(c(
    "2023-01-01", "2023-02-06", "2023-03-20", "2023-04-06", "2023-04-07", "2023-05-01",
    "2023-05-05", "2023-07-08", "2023-09-16", "2023-11-02", "2023-11-20", "2023-12-25"
  ))
)

# Database days 2023. Attention in the year!
dates_2023 <- tibble(Registro = (ymd("2023-01-01") + c(0:364)))

# Database not-working days 2023.
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

# Database class Tibble con fechas del año y ajustadas a días efectivos.
working_dates <- dates_2023 %>%
  pull() %>%
  map_vec(get_workday) %>%
  tibble(aux_var = .) %>%
  bind_cols(dates_2023) %>%
  relocate(Registro)

# Database fechas finales concertación de citas y entrega de cuestionarios.
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


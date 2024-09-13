# "Aclaración de información OC" questionnaires database
db_q_aclaracion_oc <- function(database, delete_q) {

  database_1 <- database %>%
    filter(!(Folio %in% pull(DT_folio_no_aplica(database)))) %>%
    filter(Usuario %in% pull(reviewer_team, 1)) %>%
    filter(Perfil != "ADMINISTRADOR") %>%
    filter(str_detect(Estatus, "(Aclaración de información OC \\(\\d+\\))|(En proceso de firma y sello)")) %>%
    filter(!str_detect(str_extract(Observación, "[^ \\n]+"), "C\\.") | is.na(Observación)) %>%
    count(Folio, name = "Revisiones") %>%
    mutate(Revisiones = as.character(Revisiones)) %>%
    bind_rows(DT_folio_no_aplica(database) %>% mutate(Revisiones = "NA")) %>%
    arrange(Folio)

  database_2 <- id_folio_extended %>%
    select(Folio, id_estado) %>%
    rename(Estado = "id_estado") %>%
    mutate(Cuestionario = str_sub(Folio, 3)) %>%
    left_join(database_1, by = "Folio") %>%
    mutate(Revisiones = replace_na(Revisiones, "NR")) %>%
    select(-Folio) %>%
    pivot_wider(names_from = Cuestionario, values_from = Revisiones) %>%
    select(-all_of(delete_q)) %>%
    datatable(
      rownames = FALSE,
      options = list(
        ordering = F,
        pageLength = 32,
        dom = "t",
        autoWidth = TRUE,
        columnDefs = list(list(width = "10%", targets = "_all")),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '70%'});",
          "}")
      )
    ) %>%
    formatStyle(columns = c(1:38), fontSize = '70%')

  return(database_2)

}

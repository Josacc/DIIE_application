# "Aclaración de información OC" questionnaires database
db_q_aclaracion_oc <- function(database, delete_q) {

  database <- database %>%
    filter(!(Folio %in% pull(DT_folio_no_aplica(database)))) %>%
    filter(Usuario %in% pull(reviewer_team, 1)) %>%
    filter(Perfil != "ADMINISTRADOR") %>%
    filter(str_detect(Estatus, "(Aclaración de información OC \\(\\d+\\))|(En proceso de firma y sello)")) %>%
    filter(!str_detect(str_extract(Observación, "[^ \\n]+"), "C\\.") | is.na(Observación)) %>%
    count(Folio, name = "Revisiones") %>%
    mutate(Revisiones = as.character(Revisiones)) %>%
    bind_rows(DT_folio_no_aplica(database) %>% mutate(Revisiones = "NA")) %>%
    arrange(Folio)

  database <- id_folio_extended %>%
    select(Folio, id_estado) %>%
    rename(Estado = "id_estado") %>%
    mutate(Cuestionario = str_sub(Folio, 3)) %>%
    left_join(database, by = "Folio") %>%
    mutate(Revisiones = replace_na(Revisiones, "NR")) %>%
    select(-Folio) %>%
    pivot_wider(names_from = Cuestionario, values_from = Revisiones) %>%
    select(-all_of(delete_q)) %>%
    datatable(
      extensions = "SearchBuilder",
      rownames   = FALSE,
      options    = list(
        ordering   = F,
        pageLength = 16,
        dom        = "Qftip",
        searchbuilder = TRUE,
        scrollX    = 520,
        scrollY    = 505,
        language   = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',
          paginate = list(previous = "", `next` = "")
        ),
        columnDefs = list(
          list(width = "10px", targets = c(0:34)),
          list(className = 'dt-center', targets = c(1:34))
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'font-size': '75%'});",
          "}")
      )
    ) %>%
    formatStyle(
      columns         = c(1:38),
      fontWeight      = "bold",
      fontSize        = '70%'
    ) %>%
    formatStyle(
      columns         = c(2:38),
      backgroundColor = styleInterval(c(2, 3, 4), c("", "bisque", "yellow", "red"))
    ) %>%
    formatStyle(
      columns         = c(15, 17, 19, 24, 31, 33),
      `border-right`  = "3px solid #ddd"
    )

  return(database)

}

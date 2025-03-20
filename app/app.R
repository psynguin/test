# Aplicación Shiny para gestión de ítems psicológicos
# Permite seleccionar/deseleccionar ítems y crear nuevas escalas

library(shiny)
library(DT)
library(dplyr)
library(shinyjs)
library(shinydashboard)

# UI
ui <- dashboardPage(
    dashboardHeader(title = "Gestor de Ítems Psicológicos"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Gestionar Ítems Existentes", tabName = "items", icon = icon("list")),
            menuItem("Crear Nueva Escala", tabName = "nueva_escala", icon = icon("plus-circle")),
            menuItem("Descargar Datos", tabName = "descargar", icon = icon("download"))
        )
    ),
    dashboardBody(
        useShinyjs(),
        tabItems(
            # Tab para gestionar ítems existentes
            tabItem(
                tabName = "items",
                fluidRow(
                    box(
                        width = 12,
                        title = "Filtrar Ítems",
                        status = "primary",
                        solidHeader = TRUE,
                        column(
                            width = 3,
                            selectInput("filtro_escala", "Filtrar por Escala:", choices = NULL, multiple = TRUE)
                        ),
                        column(
                            width = 3,
                            textInput("filtro_texto", "Buscar en enunciados:", "")
                        ),
                        column(
                            width = 3,
                            actionButton("reset_filtros", "Resetear Filtros", icon = icon("refresh"))
                        ),
                        column(
                            width = 3,
                            actionButton("guardar_seleccion", "Guardar Selección", icon = icon("save"),
                                         class = "btn-success")
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Ítems Disponibles",
                        status = "primary",
                        solidHeader = TRUE,
                        DTOutput("tabla_items"),
                        tags$div(
                            style = "margin-top: 15px;",
                            actionButton("seleccionar_todos", "Seleccionar Todos", icon = icon("check-square")),
                            actionButton("deseleccionar_todos", "Deseleccionar Todos", icon = icon("square"))
                        )
                    )
                )
            ),

            # Tab para crear nueva escala
            tabItem(
                tabName = "nueva_escala",
                fluidRow(
                    box(
                        width = 12,
                        title = "Definir Nueva Escala",
                        status = "primary",
                        solidHeader = TRUE,
                        column(
                            width = 4,
                            textInput("nueva_escala_nombre", "Nombre de la Escala:", ""),
                            textInput("nueva_escala_id", "ID de la Escala (3 caracteres):", ""),
                            actionButton("crear_escala", "Crear Escala", class = "btn-success", icon = icon("plus"))
                        ),
                        column(
                            width = 8,
                            div(
                                style = "border-left: 1px solid #ddd; padding-left: 20px; height: 150px;",
                                h4("Instrucciones:"),
                                p("1. Asigne un nombre descriptivo a la nueva escala."),
                                p("2. Defina un ID único de 3 caracteres (preferiblemente letras mayúsculas)."),
                                p("3. Seleccione los ítems que desea incluir en la nueva escala."),
                                p("4. Haga clic en 'Crear Escala' para añadirla a la base de datos.")
                            )
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Seleccionar Ítems para la Nueva Escala",
                        status = "primary",
                        solidHeader = TRUE,
                        column(
                            width = 3,
                            selectInput("nuevo_filtro_escala", "Filtrar por Escala:", choices = NULL, multiple = TRUE)
                        ),
                        column(
                            width = 3,
                            textInput("nuevo_filtro_texto", "Buscar en enunciados:", "")
                        ),
                        column(
                            width = 3,
                            actionButton("nuevo_reset_filtros", "Resetear Filtros", icon = icon("refresh"))
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Ítems Disponibles para la Nueva Escala",
                        status = "primary",
                        solidHeader = TRUE,
                        DTOutput("tabla_items_nuevos"),
                        tags$div(
                            style = "margin-top: 15px;",
                            actionButton("nuevo_seleccionar_todos", "Seleccionar Todos", icon = icon("check-square")),
                            actionButton("nuevo_deseleccionar_todos", "Deseleccionar Todos", icon = icon("square"))
                        )
                    )
                )
            ),

            # Tab para descargar datos
            tabItem(
                tabName = "descargar",
                fluidRow(
                    box(
                        width = 12,
                        title = "Descargar Datos Actualizados",
                        status = "primary",
                        solidHeader = TRUE,
                        p("Descargue la base de datos con los ítems actualizados y las nuevas escalas creadas."),
                        downloadButton("descargar_csv", "Descargar CSV", class = "btn-lg btn-success"),
                        hr(),
                        h4("Resumen de Datos:"),
                        verbatimTextOutput("resumen_datos")
                    )
                )
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    # Cargar datos
    datos_reactivos <- reactiveVal()

    observeEvent(1, {
        # Verificar si el archivo existe, si no, crear datos de ejemplo
        if (file.exists("items_psicologicos.csv")) {
            datos <- read.csv("items_psicologicos.csv", stringsAsFactors = FALSE)
            # Asegurarse de que la columna seleccionado existe
            if (!"seleccionado" %in% names(datos)) {
                datos$seleccionado <- 1  # Por defecto, todos seleccionados
            }
        } else {
            # Crear datos de ejemplo si no existe el archivo
            # Esta parte solo se ejecutaría si no hay archivo existente
            datos <- data.frame(
                id_item = paste0("EJE", sprintf("%04d", 1:5)),
                enunciado = c(
                    "Este es un ítem de ejemplo",
                    "Otro ítem de ejemplo",
                    "Un tercer ítem ejemplar",
                    "Cuarto ítem de prueba",
                    "Quinto ítem de muestra"
                ),
                escala = rep("Ejemplo", 5),
                id_escala = rep("EJE", 5),
                seleccionado = rep(1, 5),
                stringsAsFactors = FALSE
            )
        }
        datos_reactivos(datos)

        # Actualizar las opciones del filtro de escalas
        escalas_unicas <- unique(datos$escala)
        updateSelectInput(session, "filtro_escala", choices = escalas_unicas)
        updateSelectInput(session, "nuevo_filtro_escala", choices = escalas_unicas)
    })

    # Filtrar datos para la tabla principal
    datos_filtrados <- reactive({
        datos <- datos_reactivos()

        # Aplicar filtro por escala
        if (!is.null(input$filtro_escala) && length(input$filtro_escala) > 0) {
            datos <- datos %>% filter(escala %in% input$filtro_escala)
        }

        # Aplicar filtro por texto
        if (!is.null(input$filtro_texto) && input$filtro_texto != "") {
            datos <- datos %>% filter(grepl(input$filtro_texto, enunciado, ignore.case = TRUE))
        }

        return(datos)
    })

    # Filtrar datos para la nueva escala
    datos_filtrados_nuevos <- reactive({
        datos <- datos_reactivos()

        # Aplicar filtro por escala
        if (!is.null(input$nuevo_filtro_escala) && length(input$nuevo_filtro_escala) > 0) {
            datos <- datos %>% filter(escala %in% input$nuevo_filtro_escala)
        }

        # Aplicar filtro por texto
        if (!is.null(input$nuevo_filtro_texto) && input$nuevo_filtro_texto != "") {
            datos <- datos %>% filter(grepl(input$nuevo_filtro_texto, enunciado, ignore.case = TRUE))
        }

        return(datos)
    })

    # Resetear filtros
    observeEvent(input$reset_filtros, {
        updateSelectInput(session, "filtro_escala", selected = character(0))
        updateTextInput(session, "filtro_texto", value = "")
    })

    observeEvent(input$nuevo_reset_filtros, {
        updateSelectInput(session, "nuevo_filtro_escala", selected = character(0))
        updateTextInput(session, "nuevo_filtro_texto", value = "")
    })

    # Tabla principal de ítems
    output$tabla_items <- renderDT({
        datos <- datos_filtrados()

        datatable(
            datos,
            selection = 'none',
            options = list(
                pageLength = 15,
                dom = 'ftip'
            ),
            rownames = FALSE
        ) %>%
            formatStyle(
                'seleccionado',
                target = 'row',
                backgroundColor = styleEqual(c(0, 1), c('#FFCCCC', 'white'))
            )
    })

    # Manejo de clics en la tabla principal
    observeEvent(input$tabla_items_cell_clicked, {
        info <- input$tabla_items_cell_clicked
        if (length(info) > 0 && !is.na(info$row)) {
            # Obtener los datos actuales
            datos <- datos_reactivos()
            datos_filt <- datos_filtrados()

            if (info$row <= nrow(datos_filt)) {
                # Obtener el id_item del ítem clicado
                id_item_clicado <- datos_filt$id_item[info$row]

                # Encontrar este ítem en los datos completos
                idx <- which(datos$id_item == id_item_clicado)

                if (length(idx) > 0) {
                    # Cambiar el valor de seleccionado
                    datos$seleccionado[idx] <- ifelse(datos$seleccionado[idx] == 1, 0, 1)
                    datos_reactivos(datos)
                }
            }
        }
    })

    # Configuración para la tabla de ítems para nueva escala
    # Creamos un vector reactivo para almacenar los ítems seleccionados
    items_seleccionados_para_nueva_escala <- reactiveVal(character(0))

    # Tabla de ítems para nueva escala
    output$tabla_items_nuevos <- renderDT({
        datos <- datos_filtrados_nuevos()

        # Añadir columna temporal para mostrar si está seleccionado
        datos$esta_seleccionado <- datos$id_item %in% items_seleccionados_para_nueva_escala()

        dt <- datatable(
            datos,
            selection = 'none',
            options = list(
                pageLength = 15,
                dom = 'ftip'
            ),
            rownames = FALSE
        ) %>%
            formatStyle(
                'esta_seleccionado',
                target = 'row',
                backgroundColor = styleEqual(c(FALSE, TRUE), c('white', '#CCE5FF'))
            )

        return(dt)
    })

    # Manejo de clics en la tabla de nuevos ítems
    observeEvent(input$tabla_items_nuevos_cell_clicked, {
        info <- input$tabla_items_nuevos_cell_clicked

        if (length(info) > 0 && !is.na(info$row)) {
            datos_filt <- datos_filtrados_nuevos()

            if (info$row <= nrow(datos_filt)) {
                # Obtener el id_item del ítem clicado
                id_item_clicado <- datos_filt$id_item[info$row]

                # Actualizar la lista de ítems seleccionados
                seleccionados <- items_seleccionados_para_nueva_escala()

                if (id_item_clicado %in% seleccionados) {
                    # Si ya está en la lista, lo quitamos
                    seleccionados <- seleccionados[seleccionados != id_item_clicado]
                } else {
                    # Si no está en la lista, lo añadimos
                    seleccionados <- c(seleccionados, id_item_clicado)
                }

                items_seleccionados_para_nueva_escala(seleccionados)
            }
        }
    })

    # Guardar selección de ítems existentes
    observeEvent(input$guardar_seleccion, {
        showModal(modalDialog(
            title = "Selección Guardada",
            "Los cambios en la selección de ítems han sido guardados en memoria. Para exportar los cambios, use la opción 'Descargar CSV' en la pestaña correspondiente.",
            easyClose = TRUE,
            footer = modalButton("Cerrar")
        ))
    })

    # Seleccionar/Deseleccionar todos los ítems
    observeEvent(input$seleccionar_todos, {
        datos <- datos_reactivos()
        datos_filt <- datos_filtrados()

        # Obtener los id_item de los ítems filtrados
        ids_filtrados <- datos_filt$id_item

        # Actualizar seleccionado a 1 para todos los ítems filtrados
        datos$seleccionado[datos$id_item %in% ids_filtrados] <- 1
        datos_reactivos(datos)
    })

    observeEvent(input$deseleccionar_todos, {
        datos <- datos_reactivos()
        datos_filt <- datos_filtrados()

        # Obtener los id_item de los ítems filtrados
        ids_filtrados <- datos_filt$id_item

        # Actualizar seleccionado a 0 para todos los ítems filtrados
        datos$seleccionado[datos$id_item %in% ids_filtrados] <- 0
        datos_reactivos(datos)
    })

    # Seleccionar/Deseleccionar todos para nueva escala
    observeEvent(input$nuevo_seleccionar_todos, {
        # Obtener todos los id_item de los ítems filtrados
        datos_filt <- datos_filtrados_nuevos()
        ids_filtrados <- datos_filt$id_item

        # Unir con los que ya están seleccionados para evitar duplicados
        items_seleccionados_para_nueva_escala(ids_filtrados)
    })

    observeEvent(input$nuevo_deseleccionar_todos, {
        # Vaciar la lista de ítems seleccionados
        items_seleccionados_para_nueva_escala(character(0))
    })

    # Crear nueva escala
    observeEvent(input$crear_escala, {
        # Validar datos de entrada
        if (input$nueva_escala_nombre == "" || input$nueva_escala_id == "") {
            showModal(modalDialog(
                title = "Error",
                "Debe ingresar un nombre y un ID para la nueva escala.",
                easyClose = TRUE,
                footer = modalButton("Cerrar")
            ))
            return()
        }

        if (nchar(input$nueva_escala_id) != 3) {
            showModal(modalDialog(
                title = "Error",
                "El ID de la escala debe tener exactamente 3 caracteres.",
                easyClose = TRUE,
                footer = modalButton("Cerrar")
            ))
            return()
        }

        # Obtener ítems seleccionados
        selected_ids <- items_seleccionados_para_nueva_escala()

        if (length(selected_ids) == 0) {
            showModal(modalDialog(
                title = "Error",
                "Debe seleccionar al menos un ítem para la nueva escala.",
                easyClose = TRUE,
                footer = modalButton("Cerrar")
            ))
            return()
        }

        # Obtener datos actuales
        datos <- datos_reactivos()

        # Verificar si el ID ya existe
        if (input$nueva_escala_id %in% unique(datos$id_escala)) {
            showModal(modalDialog(
                title = "Error",
                "El ID de escala ya existe. Por favor, elija otro ID.",
                easyClose = TRUE,
                footer = modalButton("Cerrar")
            ))
            return()
        }

        # Obtener los ítems seleccionados de la base de datos original
        items_seleccionados <- datos[datos$id_item %in% selected_ids, ]

        # Crear nuevas entradas para estos ítems pero con nueva escala e ID de escala
        nuevos_items <- items_seleccionados
        nuevos_items$escala <- input$nueva_escala_nombre
        nuevos_items$id_escala <- toupper(input$nueva_escala_id)

        # Generar nuevos IDs para estos ítems
        for (i in 1:nrow(nuevos_items)) {
            nuevos_items$id_item[i] <- paste0(
                nuevos_items$id_escala[i],
                sprintf("%04d", i)
            )
        }

        # Mantener el valor de seleccionado en 1 para los nuevos ítems
        nuevos_items$seleccionado <- 1

        # Agregar a la base de datos existente (la base crece)
        datos_actualizados <- rbind(datos, nuevos_items)
        datos_reactivos(datos_actualizados)

        # Actualizar las opciones del filtro de escalas
        escalas_unicas <- unique(datos_actualizados$escala)
        updateSelectInput(session, "filtro_escala", choices = escalas_unicas)
        updateSelectInput(session, "nuevo_filtro_escala", choices = escalas_unicas)

        # Resetear selección
        items_seleccionados_para_nueva_escala(character(0))

        # Resetear campos
        updateTextInput(session, "nueva_escala_nombre", value = "")
        updateTextInput(session, "nueva_escala_id", value = "")

        # Mostrar mensaje de éxito
        showModal(modalDialog(
            title = "Éxito",
            paste0("Escala '", input$nueva_escala_nombre, "' creada exitosamente con ", length(selected_ids), " ítems duplicados."),
            easyClose = TRUE,
            footer = modalButton("Cerrar")
        ))
    })

    # Descargar datos
    output$descargar_csv <- downloadHandler(
        filename = function() {
            paste("items_psicologicos_actualizados_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datos_reactivos(), file, row.names = FALSE)
        }
    )

    # Resumen de datos
    output$resumen_datos <- renderPrint({
        datos <- datos_reactivos()

        # Número total de ítems
        n_total <- nrow(datos)

        # Número de ítems seleccionados
        n_seleccionados <- sum(datos$seleccionado == 1)

        # Número de escalas
        escalas <- unique(datos$escala)
        n_escalas <- length(escalas)

        # Distribución de ítems por escala
        distribucion <- table(datos$escala)

        cat("Número total de ítems:", n_total, "\n")
        cat("Ítems seleccionados:", n_seleccionados, "\n")
        cat("Número de escalas:", n_escalas, "\n\n")
        cat("Distribución de ítems por escala:\n")
        print(distribucion)
    })
}

# Ejecutar la aplicación
shinyApp(ui, server)
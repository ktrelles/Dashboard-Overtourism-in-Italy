# =============================================================
#  ui.R — Overtourism Italia Dashboard (Modelli Spaziali)
# =============================================================

ui <- dashboardPage(
  dashboardHeader(title = "Overtourism Italia"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analisi Descrittiva", tabName = "Descrittiva", icon = icon("chart-line")),
      menuItem("Analisi Spaziale",    tabName = "Spatial",     icon = icon("map-marked-alt")),
      menuItem("Modelli Spaziali",    tabName = "Modelli",     icon = icon("project-diagram")),
      menuItem("Spillover & Effetti", tabName = "Spillover",   icon = icon("arrows-alt")),
      menuItem("Help",                tabName = "Help",        icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # ---- TAB 1 ----
      tabItem(tabName = "Descrittiva",
              fluidRow(
                box(width = 12, status = "info", solidHeader = FALSE,
                    p("Il grafico di sinistra mostra il trend storico della variabile selezionata per la provincia scelta. Il grafico di destra evidenzia la variazione percentuale anno su anno (YoY): barre sopra lo zero indicano una crescita del fenomeno, mentre barre sotto lo zero indicano una contrazione rispetto all'anno precedente.")
                )
              ), # <-- Aggiunta virgola di separazione strutturale
              fluidRow(
                box(title = "Filtri", width = 6, solidHeader = TRUE, status = "primary",
                    column(5, selectizeInput("reg_desc", "Provincia", choices = NULL)),
                    column(5, selectizeInput("var_desc", "Variabile", choices = NULL))
                ),
                box(title = "Info", background = "light-blue", width = 6,
                    "Serie storica e variazioni %")
              ),
              fluidRow(
                valueBoxOutput("box_media", width = 4),
                valueBoxOutput("box_max",   width = 4),
                valueBoxOutput("box_min",   width = 4)
              ),
              fluidRow(
                box(title = "Serie temporale",  width = 8, plotlyOutput("plot_timeseries") %>% withSpinner()),
                box(title = "Variazione % YoY", width = 4, plotlyOutput("plot_yoy")        %>% withSpinner())
              ),
              fluidRow(
                box(title = "Confronto province", width = 12,
                    column(3, selectInput("anno_bar", "Anno", choices = NULL)),
                    plotlyOutput("plot_barregioni") %>% withSpinner()
                )
              )
      ),
      
      # ---- TAB 2 ----
      tabItem(tabName = "Spatial",
              fluidRow(
                box(title = "Configurazione", width = 10, solidHeader = TRUE, status = "primary",
                    column(3, selectInput("anno_map",    "Anno",      choices = NULL)),
                    column(3, selectizeInput("var_map",  "Variabile", choices = NULL)),
                    column(3, selectInput("W_type_map",  "Matrice W", choices = c("Queen" = "queen", "k-NN" = "knn"))),
                    column(3, br(), actionButton("run_spatial", "Aggiorna mappe", icon = icon("refresh"), class = "btn-primary"))
                )
              ),
              fluidRow(
                valueBoxOutput("box_moran",       width = 4),
                valueBoxOutput("box_moran_p",     width = 4),
                valueBoxOutput("box_moran_interp",width = 4)
              ),
              fluidRow(
                box(width = 12, status = "info",
                    p(strong("Interpretazione spaziale:")),
                    p("L'Indice di Moran misura l'autocorrelazione spaziale (da -1 a +1). Un valore positivo e significativo indica che province con valori simili tendono a essere vicine nello spazio."),
                    p(strong("Mappa LISA (Local Indicators of Spatial Association):")),
                    tags$ul(
                      tags$li(tags$b("High-high (rosso):"), " cluster caldo. Province con alto overtourism circondate da province parimenti colpite."),
                      tags$li(tags$b("Low-low (blu):"), " cluster freddo. Province con bassi livelli di overtourism circondate da province stabili/poco turistiche."),
                      tags$li(tags$b("High-low / low-high (arancione/azzurro):"), " outliers spaziali. Province in controtendenza rispetto ai propri vicini.")
                    )
                )
              ),
              fluidRow(
                box(title = "Mappa coropletica",    width = 6, plotlyOutput("mappa_coropleta") %>% withSpinner()),
                box(title = "Moran scatter plot", width = 6, plotlyOutput("moran_scatter")   %>% withSpinner())
              ),
              fluidRow(
                box(title = "LISA cluster map", width = 12, plotlyOutput("mappa_lisa") %>% withSpinner())
              )
      ),
      
      # ---- TAB 3 ----
      tabItem(tabName = "Modelli",
              fluidRow(
                box(width = 12, status = "warning",
                    p(strong("Nota sui modelli:")),
                    tags$ul(
                      tags$li("L'Akaike Information Criterion (AIC) penalizza la complessità del modello: il modello con l'AIC più basso (in verde) garantisce il miglior compromesso tra bontà di adattamento e parsimonia nei parametri."),
                      tags$li("Nel grafico sottostante (gerarchia), i collegamenti verdi indicano che il test del rapporto di verosimiglianza (LR test) ha rifiutato il modello ristretto a favore di quello più generale. Se il collegamento è grigio, le restrizioni imposte sono statisticamente valide e il modello più semplice è preferibile.")
                    )
                )
              ), 
              fluidRow(
                box(title = "Specificazione", width = 12, solidHeader = TRUE, status = "primary",
                    column(3, selectizeInput("dep_mod",   "Dipendente (Y)",    choices = NULL)),
                    column(3, selectizeInput("indep_mod", "Indipendenti (X)",  choices = NULL, multiple = TRUE)),
                    column(2, selectInput("W_type_mod",   "Matrice W",         choices = c("Queen" = "queen", "k-NN" = "knn"))),
                    column(2, selectInput("anno_mod",     "Anno analisi",      choices = anni_disponibili, selected = max(anni_disponibili))),
                    column(2, br(), actionButton("run_models", "Stima modelli", icon = icon("play"), class = "btn-success"))
                )
              ),
              
              fluidRow(
                box(width = 12, status = "info", solidHeader = FALSE,
                    checkboxInput("std_x", "Standardizzare le variabili indipendenti (z-score) prima della stima",
                                  value = FALSE),
                    p(em("Utile nell'interpretazione dei coefficienti o degli effetti di ricaduta quando le variabili hanno scale molto diverse."))
                )
              ),
              
              fluidRow(
                box(title = "Seleziona modelli", width = 12, status = "warning",
                    checkboxGroupInput("modelli_sel", "",
                                       choices  = c("OLS","SAR","SEM","SAC","SDM","SLX","SDEM","GNS"),
                                       selected = c("OLS","SAR","SEM"),
                                       inline   = TRUE)
                )
              ),
              fluidRow(
                box(title = "Metriche di confronto", width = 7, plotlyOutput("plot_metriche") %>% withSpinner()),
                box(title = "Tabella modelli",        width = 5, tableOutput("tabella_metriche"))
              ),
              fluidRow(
                box(title = "Gerarchia modelli annidati (LR test)", width = 12,
                    plotlyOutput("plot_gerarchia") %>% withSpinner()
                )
              ),
              fluidRow(
                box(title = "Coefficienti di un modello", width = 6,
                    selectInput("mod_coeff_sel", "Modello:",
                                choices = c("OLS","SAR","SEM","SAC","SDM","SLX","SDEM","GNS")),
                    plotlyOutput("plot_coeff") %>% withSpinner()
                ),
                box(title = "Test LM – Scelta del modello spaziale", width = 6,
                    verbatimTextOutput("lm_tests"),
                    br(),
                    uiOutput("lm_raccomandazione")
                )
              )
      ),
      
      # ---- TAB 4 ----
      tabItem(tabName = "Spillover",
              fluidRow(
                box(title = "Seleziona modello con lag", width = 12, solidHeader = TRUE, status = "primary",
                    column(4, selectInput("mod_spillover", "Modello",   choices = c("SAR","SDM","SAC","GNS"))),
                    column(4, selectInput("W_type_spill",  "Matrice W", choices = c("Queen" = "queen", "k-NN" = "knn"))),
                    column(4, br(), actionButton("run_spill", "Calcola impatti", icon = icon("refresh"), class = "btn-primary"))
                )
              ),
              fluidRow(
                box(title = "Grafico degli impatti", width = 7, plotlyOutput("plot_spillover")   %>% withSpinner()),
                box(title = "Tabella impatti",        width = 5, tableOutput("tabella_spillover"))
              ),
              fluidRow(
                box(width = 12, status = "info", solidHeader = FALSE,
                    p(strong("Capire gli Impatti Marginali Spaziali:")),
                    tags$ul(
                      tags$li(tags$b("Effetto diretto:"), " misura l'impatto medio della variazione di una variabile esplicativa (X) di una specifica provincia sulla propria variabile dipendente (Y). Include gli effetti di feedback (l'impatto che ritorna indietro passando per i vicini)."),
                      tags$li(tags$b("Effetto indiretto (spillover):"), " misura l'effetto esternalità. Rappresenta l'impatto medio derivante dalla variazione di una X in tutte le altre province sulla Y della provincia di riferimento (o simmetricamente, come la variazione locale influenzi tutti i vicini)."),
                      tags$li(tags$b("Effetto totale:"), " la somma dell'effetto diretto e indiretto.")
                    )
                )
              )
      ),
      
      # ---- TAB 5 ----
      tabItem(tabName = "Help",
              fluidRow(
                box(title = "Guida all'uso", width = 12, background = "light-blue",
                    "Questa dashboard permette di analizzare l'overtourism in Italia a livello provinciale.",
                    br(), br(),
                    "• Nel tab Modelli Spaziali puoi stimare fino a 8 modelli (OLS, SAR, SEM, SAC, SDM, SLX, SDEM, GNS).",
                    br(),
                    "• Scegli anno, matrice W e variabili, poi clicca su 'Stima modelli'.",
                    br(),
                    "• Il grafico della gerarchia mostra i LR test tra modelli annidati: verde = modello più ricco significativamente migliore.",
                    br(),
                    "• Il test LM sui residui OLS indica se serve un modello spaziale (strategia classica di Anselin).",
                    br(),
                    "• Nel tab Spillover puoi visualizzare gli effetti diretti, indiretti e totali per i modelli con lag spaziale."
                )
              )
      )
      
    )
  )
)
# =============================================================
#  server.R — Overtourism Italia Dashboard (province level)
# =============================================================

cat("=== Avvio server.R ===\n")

server <- function(input, output, session) {
  
  # ---------- Inizializzazione ----------
  observe({
    province <- sort(unique(panel$nome_provincia))
    vars <- var_cols
    anni <- anni_disponibili
    
    updateSelectizeInput(session, "reg_desc",   choices = province, selected = province[1])
    updateSelectizeInput(session, "var_desc",   choices = vars,     selected = vars[1])
    updateSelectInput(session,    "anno_bar",   choices = anni,     selected = max(anni))
    updateSelectInput(session,    "anno_map",   choices = anni,     selected = max(anni))
    updateSelectInput(session,    "anno_mod",   choices = anni,     selected = max(anni))
    updateSelectizeInput(session, "var_map",    choices = vars,     selected = vars[1])
    updateSelectizeInput(session, "dep_mod",    choices = vars,     selected = vars[1])
    updateSelectizeInput(session, "indep_mod",  choices = vars,     selected = vars[2:min(4, length(vars))])
  })
  
  get_W <- function(tipo) if (tipo == "knn") W_knn else W_queen
  
  # ---------- TAB 1 – DESCRITTIVA ----------
  dati_prov <- reactive({
    req(input$reg_desc)
    req(input$reg_desc %in% panel$nome_provincia)
    panel %>% filter(nome_provincia == input$reg_desc)
  })
  
  output$box_media <- renderValueBox({
    val <- mean(dati_prov()[[input$var_desc]], na.rm = TRUE)
    valueBox(round(val, 2), paste("Media:", input$var_desc), icon = icon("calculator"), color = "blue")
  })
  output$box_max <- renderValueBox({
    val <- max(dati_prov()[[input$var_desc]], na.rm = TRUE)
    valueBox(round(val, 2), paste("Massimo:", input$var_desc), icon = icon("arrow-up"), color = "green")
  })
  output$box_min <- renderValueBox({
    val <- min(dati_prov()[[input$var_desc]], na.rm = TRUE)
    valueBox(round(val, 2), paste("Minimo:", input$var_desc), icon = icon("arrow-down"), color = "red")
  })
  
  # plot time series
  output$plot_timeseries <- renderPlotly({
    df <- dati_prov()
    
    plot_ly(
      df, 
      x = ~anno, 
      y = ~.data[[input$var_desc]], 
      type = "scatter", 
      mode = "lines+markers", 
      line = list(color = '#4575b4'), 
      marker = list(color = '#4575b4')
    ) %>%
      layout(
        title = list(text = paste("Historical Trend –", input$reg_desc), font = list(size = 14)),
        xaxis = list(title = "Anno", tickformat = "d", dtick = 1),
        yaxis = list(title = input$var_desc),
        paper_bgcolor = 'white', 
        plot_bgcolor = 'white'
      )
  })
  
  # plot YoY
  output$plot_yoy <- renderPlotly({
    df <- dati_prov() %>% arrange(anno)
    yoy <- (df[[input$var_desc]] / lag(df[[input$var_desc]]) - 1) * 100
    plot_ly(df, x = ~anno, y = ~yoy, type = "bar", marker = list(color = '#4575b4')) %>%
      layout(xaxis = list(title = "Anno"))
  })
  
  # plot comparison
  output$plot_barregioni <- renderPlotly({
    req(input$anno_bar, input$var_desc)
    df <- panel %>% filter(anno == input$anno_bar) %>%
      arrange(desc(.data[[input$var_desc]]))
    plot_ly(df,
            x = ~reorder(nome_provincia, -.data[[input$var_desc]]),
            y = ~.data[[input$var_desc]], type = "bar", marker = list(color = '#4575b4')) %>%
      layout(xaxis = list(title = "Province", tickangle = -45), margin = list(t = 80), yaxis = list(title = input$var_desc))
  })
  
  # ---------- TAB 2 – SPATIAL ----------
  map_anno <- eventReactive(input$run_spatial, {
    req(input$anno_map, input$var_map)
    df_anno <- panel %>% filter(anno == input$anno_map) %>%
      select(ID_provincia, nome_provincia, all_of(input$var_map))
    map_joined <- map_italia %>% left_join(df_anno, by = c("ID_provincia", "nome_provincia"))
    map_joined  <- map_joined %>% filter(ID_provincia %in% map_data$ID_provincia)
    map_joined
  })
  
  W_spatial <- reactive({ get_W(input$W_type_map) })
  
  moran_res <- reactive({
    req(map_anno(), input$var_map)
    df     <- map_anno()
    ordine <- match(map_data$ID_provincia, df$ID_provincia)
    x      <- df[[input$var_map]][ordine]
    valid  <- !is.na(x)
    if (sum(valid) < 3) return(NULL)
    W <- nb2listw(subset(nb_queen, subset = valid), style = "W", zero.policy = TRUE)
    moran.test(x[valid], W, zero.policy = TRUE)
  })
  
  output$box_moran <- renderValueBox({
    m <- moran_res()
    if (is.null(m)) valueBox("N/D", "Moran's I", color = "red")
    else valueBox(round(m$estimate[1], 3), "Moran's I",
                  color = ifelse(m$p.value < 0.05, "green", "yellow"))
  })
  output$box_moran_p <- renderValueBox({
    m <- moran_res()
    if (is.null(m)) valueBox("N/D", "P-value", color = "red")
    else valueBox(format(m$p.value, scientific = TRUE, digits = 3), "P-value",
                  color = ifelse(m$p.value < 0.05, "green", "yellow"))
  })
  output$box_moran_interp <- renderValueBox({
    m <- moran_res()
    if (is.null(m)) testo <- "Dati insufficienti"
    else testo <- ifelse(m$p.value < 0.05,
                         ifelse(m$estimate[1] > 0, "Autocorrelazione spaziale positiva", "Dispersione"),
                         "Distribuzione casuale")
    testo_formattato <- HTML(paste0("<span style='font-size: 20px;'>", testo, "</span>"))
    valueBox(testo_formattato, "Interpretazione", color = "blue")
  })
  
  output$mappa_coropleta <- renderPlotly({
    req(map_anno())
    df <- map_anno()
    if (all(is.na(df[[input$var_map]])))
      return(plot_ly() %>% layout(title = "Nessun dato per questo anno/variabile"))
    p <- ggplot(df) + geom_sf(aes(fill = .data[[input$var_map]])) +
      scale_fill_viridis_c(na.value = "grey90") + theme_void()
    ggplotly(p)
  })
  
  output$moran_scatter <- renderPlotly({
    req(map_anno(), input$var_map)
    df     <- map_anno()
    ordine <- match(map_data$ID_provincia, df$ID_provincia)
    x_raw  <- df[[input$var_map]][ordine]
    valid  <- !is.na(x_raw)
    if (sum(valid) < 3)
      return(plot_ly() %>% layout(title = "Dati insufficienti per Moran scatter plot"))
    x     <- scale(x_raw[valid])[, 1]
    W     <- nb2listw(subset(nb_queen, subset = valid), style = "W", zero.policy = TRUE)
    lag_x <- lag.listw(W, x, zero.policy = TRUE)
    fit   <- lm(lag_x ~ x)
    x_line <- range(x, na.rm = TRUE)
    y_line <- coef(fit)[1] + coef(fit)[2] * x_line
    plot_ly(x = x, y = lag_x, type = "scatter", mode = "markers",
            text = map_data$nome_provincia[valid]) %>%
      add_lines(x = x_line, y = y_line, inherit = FALSE,
                line = list(color = "red", dash = "dash")) %>%
      layout(xaxis = list(title = "Valore standardizzato"),
             yaxis = list(title = "Lag spaziale"))
  })
  
  output$mappa_lisa <- renderPlotly({
    req(map_anno())
    df     <- map_anno()
    ordine <- match(map_data$ID_provincia, df$ID_provincia)
    x      <- df[[input$var_map]][ordine]
    if (all(is.na(x))) return(plot_ly() %>% layout(title = "Dati non validi"))
    x     <- scale(x)[, 1]
    W     <- W_spatial()
    lisa  <- localmoran(x, W, zero.policy = TRUE, na.action = na.exclude)
    lag_x <- lag.listw(W, x, zero.policy = TRUE)
    p_vals   <- lisa[, 5]
    quadrant <- case_when(
      p_vals > 0.05             ~ "Non signif.",
      x > 0 & lag_x > 0        ~ "High-High",
      x < 0 & lag_x < 0        ~ "Low-Low",
      x > 0 & lag_x < 0        ~ "High-Low",
      x < 0 & lag_x > 0        ~ "Low-High",
      TRUE                      ~ "Non signif."
    )
    df$LISA         <- NA
    df$LISA[ordine] <- quadrant
    cols <- c("High-High" = "#d73027", "Low-Low" = "#4575b4",
              "High-Low"  = "#fc8d59", "Low-High" = "#91bfdb",
              "Non signif." = "#f0f0f0")
    p <- ggplot(df) + geom_sf(aes(fill = LISA)) +
      scale_fill_manual(values = cols) + theme_void()
    ggplotly(p)
  })
  
  # ---------- TAB 3 – MODELLI SPAZIALI ----------
  
  dati_cross_aligned <- reactive({
    req(input$dep_mod, input$indep_mod, input$anno_mod)
    df <- panel %>%
      filter(anno == input$anno_mod) %>%
      select(ID_provincia, all_of(c(input$dep_mod, input$indep_mod)))
    ordine <- match(map_data$ID_provincia, df$ID_provincia)
    df_ord <- df[ordine, ] %>% select(-ID_provincia)
    
    # Estandariza las X (no la Y) si el usuario activa el checkbox
    if (isTRUE(input$std_x)) {
      for (v in input$indep_mod) {
        df_ord[[v]] <- scale(df_ord[[v]])[, 1]
      }
    }
    
    df_ord
  })
  
  formula_mod <- reactive({
    dep   <- paste0("`", input$dep_mod,   "`")
    indep <- paste0("`", input$indep_mod, "`")
    as.formula(paste(dep, "~", paste(indep, collapse = " + ")))
  })
  
  modelli_stimati <- eventReactive(input$run_models, {
    req(input$dep_mod, length(input$indep_mod) > 0)
    df  <- dati_cross_aligned()
    frm <- formula_mod()
    W   <- get_W(input$W_type_mod)
    sel <- input$modelli_sel
    if (is.null(sel)) sel <- character(0)
    out <- list()
    
    withProgress(message = "Stima modelli in corso...", value = 0, {
      step <- 1 / max(length(sel), 1)
      if ("OLS"  %in% sel) { out$OLS  <- lm(frm, data = df); incProgress(step) }
      if ("SAR"  %in% sel) { out$SAR  <- tryCatch(lagsarlm(frm, data = df, listw = W, zero.policy = TRUE),                          error = function(e) NULL); incProgress(step) }
      if ("SEM"  %in% sel) { out$SEM  <- tryCatch(errorsarlm(frm, data = df, listw = W, zero.policy = TRUE),                        error = function(e) NULL); incProgress(step) }
      if ("SAC"  %in% sel) { out$SAC  <- tryCatch(sacsarlm(frm, data = df, listw = W, zero.policy = TRUE),                          error = function(e) NULL); incProgress(step) }
      if ("SDM"  %in% sel) { out$SDM  <- tryCatch(lagsarlm(frm, data = df, listw = W, type = "mixed", zero.policy = TRUE),          error = function(e) NULL); incProgress(step) }
      if ("SLX"  %in% sel) { out$SLX  <- tryCatch(lmSLX(frm, data = df, listw = W, zero.policy = TRUE),                            error = function(e) NULL); incProgress(step) }
      if ("SDEM" %in% sel) { out$SDEM <- tryCatch(errorsarlm(frm, data = df, listw = W, etype = "emixed", zero.policy = TRUE),      error = function(e) NULL); incProgress(step) }
      if ("GNS"  %in% sel) { out$GNS  <- tryCatch(sacsarlm(frm, data = df, listw = W, type = "sacmixed", zero.policy = TRUE),       error = function(e) NULL); incProgress(step) }
      
      # OLS interno para LM tests si el usuario no lo seleccionó
      if (!"OLS" %in% names(out)) {
        out$.OLS_interno <- tryCatch(lm(frm, data = df), error = function(e) NULL)
      }
    })
    
    out[!sapply(out, is.null)]
  })
  
  estrai_metriche <- function(nome, mod) {
    if (is.null(mod)) return(NULL)
    resid <- residuals(mod)
    rmse  <- sqrt(mean(resid^2))
    data.frame(
      Modello = nome,
      LogLik  = round(as.numeric(logLik(mod)), 2),
      AIC     = round(AIC(mod), 2),
      BIC     = round(BIC(mod), 2),
      RMSE    = round(rmse, 4)
    )
  }
  
  moran_residui <- reactive({
    mods <- modelli_stimati()
    W    <- get_W(input$W_type_mod)
    res  <- lapply(mods, function(m) {
      r   <- residuals(m)
      mor <- moran.test(r, W, zero.policy = TRUE)
      data.frame(Moran_I = mor$estimate[1], p_value = mor$p.value)
    })
    bind_rows(res, .id = "Modello")
  })
  
  metriche_df <- reactive({
    mods     <- modelli_stimati()
    mods_vis <- mods[!names(mods) %in% ".OLS_interno"]
    req(length(mods_vis) > 0)
    bind_rows(mapply(estrai_metriche, names(mods_vis), mods_vis, SIMPLIFY = FALSE))
  })
  
  output$plot_metriche <- renderPlotly({
    req(metriche_df())
    df   <- metriche_df()
    best <- df$Modello[which.min(df$AIC)]
    plot_ly(df, x = ~Modello, y = ~AIC, type = "bar",
            marker = list(color = ifelse(df$Modello == best, "#1D9E75", "steelblue"))) %>%
      layout(title = "AIC per modello (minore è meglio)",
             yaxis = list(title = "AIC"))
  })
  
  output$tabella_metriche <- renderTable({
    df <- metriche_df()
    df$Migliore_AIC <- ifelse(df$AIC == min(df$AIC, na.rm = TRUE), "✓", "")
    df
  }, striped = TRUE, bordered = TRUE)
  
  # ---------- GRAFICO COEFFICIENTI ----------
  output$plot_coeff <- renderPlotly({
    mods <- modelli_stimati()
    req(length(mods) > 0)
    
    nome_mod <- input$mod_coeff_sel
    
    if (!nome_mod %in% names(mods)) {
      return(
        plot_ly() %>%
          layout(
            title = paste("Il modello", nome_mod, "non è stato stimato"),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }
    
    mod <- mods[[nome_mod]]
    
    coefs <- tryCatch({
      as.data.frame(coef(summary(mod)))
    }, error = function(e) {
      tryCatch({
        as.data.frame(summary(mod)$Coef)
      }, error = function(e) NULL)
    })
    
    if (is.null(coefs) || nrow(coefs) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = "Coefficienti non disponibili per questo modello",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }
    
    coefs$Variabile <- rownames(coefs)
    
    # elimina intercetta se presente
    coefs <- coefs[coefs$Variabile != "(Intercept)", ]
    
    # prende la colonna dei coefficienti
    coefs$Stima <- coefs[[1]]
    
    plot_ly(
      data = coefs,
      x = ~Stima,
      y = ~reorder(Variabile, Stima),
      type = "bar",
      orientation = "h",
      text = ~round(Stima, 4),
      textposition = "auto"
    ) %>%
      layout(
        title = paste0("Coefficienti del modello ", nome_mod,
                       if (isTRUE(input$std_x)) " (X standardizzate)" else ""),
        xaxis = list(title = "Coefficiente stimato"),
        yaxis = list(title = "")
      )
  })
  
  # ---------- GERARCHIA MODELLI ANNIDATI (LR test visuale) ----------
  # Struttura annidata:
  #   OLS -> SAR -> SAC
  #   OLS -> SEM -> SAC
  #   OLS -> SLX -> SDM -> GNS
  #   OLS -> SLX -> SDEM -> GNS
  #
  # LR test: se modello B e' annidato in A, LR = 2*(logLik(A) - logLik(B))
  # con df = differenza parametri liberi
  
  output$plot_gerarchia <- renderPlotly({
    mods <- modelli_stimati()
    mods_vis <- mods[!names(mods) %in% ".OLS_interno"]
    req(length(mods_vis) >= 2)
    
    # Coppie annidate da testare (modello_ristretto -> modello_generale)
    coppie <- list(
      # SAC -> GNS (theta=0)
      c("SAC", "GNS"),
      # SDM -> GNS (lambda=0)
      c("SDM", "GNS"),
      # SDEM -> GNS (rho=0)
      c("SDEM", "GNS"),
      # SAR -> SAC (lambda=0)
      c("SAR", "SAC"),
      # SAR -> SDM (theta=0)
      c("SAR", "SDM"),
      # SEM -> SAC (rho=0)
      c("SEM", "SAC"),
      # SLX -> SDM (rho=0)
      c("SLX", "SDM"),
      # SLX -> SDEM (lambda=0)
      c("SLX", "SDEM"),
      # SEM -> SDEM (theta=0)
      c("SEM", "SDEM"),
      # OLS -> SAR (rho=0)
      c("OLS", "SAR"),
      # OLS -> SLX (theta=0)
      c("OLS", "SLX"),
      # OLS -> SEM (lambda=0)
      c("OLS", "SEM")
    )
    
    risultati <- lapply(coppie, function(cp) {
      ristretto <- cp[1]
      generale  <- cp[2]
      if (!ristretto %in% names(mods_vis) || !generale %in% names(mods_vis))
        return(NULL)
      m_r <- mods_vis[[ristretto]]
      m_g <- mods_vis[[generale]]
      ll_r <- tryCatch(as.numeric(logLik(m_r)), error = function(e) NA)
      ll_g <- tryCatch(as.numeric(logLik(m_g)), error = function(e) NA)
      if (is.na(ll_r) || is.na(ll_g)) return(NULL)
      lr_stat <- max(0, 2 * (ll_g - ll_r))
      # gradi di libertà approssimati come differenza parametri
      np_r <- tryCatch(attr(logLik(m_r), "df"), error = function(e) NA)
      np_g <- tryCatch(attr(logLik(m_g), "df"), error = function(e) NA)
      df_test <- if (!is.na(np_r) && !is.na(np_g)) abs(np_g - np_r) else 1
      if (df_test == 0) df_test <- 1
      p_val <- pchisq(lr_stat, df = df_test, lower.tail = FALSE)
      data.frame(
        da        = ristretto,
        a         = generale,
        LR        = round(lr_stat, 3),
        df        = df_test,
        p_value   = round(p_val, 4),
        sig       = p_val < 0.05,
        etichetta = paste0(ristretto, " → ", generale,
                           "\nLR=", round(lr_stat, 2),
                           " p=", round(p_val, 3))
      )
    })
    
    df_lr <- bind_rows(risultati)
    req(nrow(df_lr) > 0)
    
    # Posizioni fisse dei nodi per visualizzazione ad albero
    pos <- data.frame(
      nodo = c("GNS", "SAC", "SDM", "SDEM", "SAR", "SLX", "SEM", "OLS"),
      x    = c(1,      3,     3,      3,      5,     5,     5,     7),
      y    = c(3,      5,     3,      1,      5,     3,     1,     3),
      stringsAsFactors = FALSE
    )
    
    # Livello di generalità (1 = più generale, 4 = più semplice/restrittivo)
    livello <- c(GNS = 1, SAC = 2, SDM = 2, SDEM = 2, SAR = 3, SLX = 3, SEM = 3, OLS = 4)
    
    descrizione <- c(
      GNS  = "modello generale<br>(lag + errore + lag di X)",
      SAC  = "lag + errore spaziale",
      SDM  = "lag + lag di X (Durbin)",
      SDEM = "errore + lag di X (Durbin)",
      SAR  = "solo lag spaziale",
      SLX  = "solo lag di X",
      SEM  = "solo errore spaziale",
      OLS  = "nessuna componente<br>spaziale (più semplice)"
    )
    
    size_livello  <- c("1" = 46, "2" = 38, "3" = 32, "4" = 26)
    color_livello <- c("1" = "#1a3e7a", "2" = "#2c5aa0", "3" = "#5b9bd5", "4" = "#aac9e8")
    
    pos$livello        <- livello[pos$nodo]
    pos$size           <- size_livello[as.character(pos$livello)]
    pos$colore         <- color_livello[as.character(pos$livello)]
    pos$etichetta_nodo <- paste0("<b>", pos$nodo, "</b><br>", descrizione[pos$nodo])
    
    nodi_presenti <- pos[pos$nodo %in% c(names(mods_vis), "OLS"), ]
    
    # SLX<-SDM e SEM<-SAC condividono esattamente lo stesso punto medio
    # geometrico (4,3): per questi due casi fissiamo a mano la posizione
    # dell'etichetta, spostandola verso l'estremo opposto della propria
    # linea. Tutti gli altri archi usano semplicemente il punto medio.
    posizione_etichetta <- function(da, a, p_da, p_a) {
      if (da == "SLX" && a == "SDM") return(c(x = 3.5, y = 2.7))
      if (da == "SEM" && a == "SAC") return(c(x = 3.3, y = 4.4))
      c(x = (p_da$x + p_a$x) / 2, y = (p_da$y + p_a$y) / 2)
    }
    
    fig <- plot_ly()
    
    for (i in seq_len(nrow(df_lr))) {
      row  <- df_lr[i, ]
      p_da <- pos[pos$nodo == row$da, ]
      p_a  <- pos[pos$nodo == row$a,  ]
      if (nrow(p_da) == 0 || nrow(p_a) == 0) next
      
      lab_pos <- posizione_etichetta(row$da, row$a, p_da, p_a)
      
      colore <- if (row$sig) "#1D9E75" else "#cccccc"
      fig <- fig %>% add_trace(
        x          = c(p_da$x, p_a$x, NA),
        y          = c(p_da$y, p_a$y, NA),
        type       = "scatter", mode = "lines",
        line       = list(color = colore, width = 3),
        hoverinfo  = "skip",
        showlegend = FALSE
      )
      fig <- fig %>% add_trace(
        x          = lab_pos["x"],
        y          = lab_pos["y"],
        type       = "scatter", mode = "text",
        text       = paste0("LR=", row$LR, "<br>p=", row$p_value),
        textfont   = list(size = 10, color = colore),
        textposition = "middle center",
        hoverinfo  = "skip",
        showlegend = FALSE
      )
    }
    
    # Nodi: dimensione e colore decrescenti dal modello più generale (GNS) al più semplice (OLS)
    fig <- fig %>%
      add_trace(
        data       = nodi_presenti,
        x          = ~x,
        y          = ~y,
        type       = "scatter",
        mode       = "markers+text",
        text       = ~etichetta_nodo,
        textposition = "top center",
        textfont   = list(size = 11),
        marker     = list(size = ~size, color = ~colore, line = list(color = "white", width = 2)),
        hoverinfo  = "text",
        showlegend = FALSE
      ) %>%
      layout(
        title = "Gerarchia modelli annidati — LR test (verde = miglioramento sign. p<0.05)",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "",
                     range = c(0, 8)),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "",
                     range = c(-0.5, 6.5)),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        annotations = list(
          list(x = 1, y = 6.2, text = "<b>PIÙ GENERALE</b><br>(più parametri)",
               showarrow = FALSE, font = list(size = 11, color = "grey30"), xref = "x", yref = "y"),
          list(x = 7, y = 6.2, text = "<b>PIÙ SEMPLICE</b><br>(OLS, nessun termine spaziale)",
               showarrow = FALSE, font = list(size = 11, color = "grey30"), xref = "x", yref = "y"),
          list(x = 4, y = -0.4,
               text = "← maggiore complessità spaziale   |   maggiore parsimonia →",
               showarrow = FALSE, font = list(size = 10, color = "grey50"), xref = "x", yref = "y")
        )
      )
    
    fig
  })
  
  # ---------- LM TESTS ----------
  output$lm_tests <- renderPrint({
    mods <- modelli_stimati()
    ols  <- if ("OLS" %in% names(mods)) mods$OLS else mods$.OLS_interno
    req(!is.null(ols))
    W <- get_W(input$W_type_mod)
    lm.RStests(ols, W, test = c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
  })
  
  output$lm_raccomandazione <- renderUI({
    mods <- modelli_stimati()
    ols  <- if ("OLS" %in% names(mods)) mods$OLS else mods$.OLS_interno
    req(!is.null(ols))
    W <- get_W(input$W_type_mod)
    lmt <- tryCatch(
      lm.RStests(ols, W, test = "all", zero.policy = TRUE),
      error = function(e) NULL
    )
    req(!is.null(lmt))

    # Estrae p-value in modo robusto (nomi variano per versione di spdep)
    estrai_p <- function(obj, nomi_possibili) {
      for (nm in nomi_possibili) {
        if (!is.null(obj[[nm]]$p.value)) return(obj[[nm]]$p.value)
      }
      return(NA)
    }

    p_lag  <- estrai_p(lmt, c("LMlag",  "RSlag",  "RSlm"))
    p_err  <- estrai_p(lmt, c("LMerr",  "RSerr",  "RSerr"))
    p_rlag <- estrai_p(lmt, c("RLMlag", "adjRSlag"))
    p_rerr <- estrai_p(lmt, c("RLMerr", "adjRSerr"))

    # Fallback: se ancora NA usa i primi due elementi della lista
    nomi_lmt <- names(lmt)
    if (is.na(p_lag)  && length(nomi_lmt) >= 1) p_lag  <- lmt[[nomi_lmt[1]]]$p.value
    if (is.na(p_err)  && length(nomi_lmt) >= 2) p_err  <- lmt[[nomi_lmt[2]]]$p.value
    if (is.na(p_rlag) && length(nomi_lmt) >= 3) p_rlag <- lmt[[nomi_lmt[3]]]$p.value
    if (is.na(p_rerr) && length(nomi_lmt) >= 4) p_rerr <- lmt[[nomi_lmt[4]]]$p.value

    # Se ancora NA non possiamo concludere nulla
    if (any(is.na(c(p_lag, p_err)))) {
      return(div(
        style = "padding:10px; background:#fff3cd; border-left:4px solid #ffc107; border-radius:4px;",
        "⚠️ Test LM non disponibili per i modelli selezionati."
      ))
    }

    raccomandazione <- if (p_lag > 0.05 && p_err > 0.05) {
      "✅ OLS: nessuna dipendenza spaziale significativa. OLS è sufficiente."
    } else if (p_lag < 0.05 && p_err > 0.05) {
      "➡️ SAR (Spatial Lag): solo LMlag significativo."
    } else if (p_lag > 0.05 && p_err < 0.05) {
      "➡️ SEM (Spatial Error): solo LMerr significativo."
    } else {
      if (!is.na(p_rlag) && !is.na(p_rerr)) {
        if (p_rlag < 0.05 && p_rerr >= 0.05) {
          "➡️ SAR: RLMlag significativo, RLMerr no. Preferire lag spaziale."
        } else if (p_rlag >= 0.05 && p_rerr < 0.05) {
          "➡️ SEM: RLMerr significativo, RLMlag no. Preferire errore spaziale."
        } else {
          "⚠️ Entrambi i RLM significativi: considera SAC o SDM (modelli più generali)."
        }
      } else {
        "⚠️ Entrambi LMlag e LMerr significativi: considera SAC o SDM."
      }
    }

    div(
      style = "padding:10px; background:#f0f4ff; border-left:4px solid #3366cc; border-radius:4px; margin-top:10px;",
      strong("Raccomandazione (strategia classica Anselin):"),
      br(), raccomandazione
    )
  })
  
  # ---------- TAB 4 – SPILLOVER ----------
  spill_modello <- eventReactive(input$run_spill, {
    req(input$mod_spillover)
    df  <- dati_cross_aligned()
    frm <- formula_mod()
    W   <- get_W(input$W_type_spill)
    mod <- tryCatch(
      switch(input$mod_spillover,
             "SAR" = lagsarlm(frm, data = df, listw = W, zero.policy = TRUE),
             "SDM" = lagsarlm(frm, data = df, listw = W, type = "mixed", zero.policy = TRUE),
             "SAC" = sacsarlm(frm, data = df, listw = W, zero.policy = TRUE),
             "GNS" = sacsarlm(frm, data = df, listw = W, type = "sacmixed", zero.policy = TRUE)
      ),
      error = function(e) { message("Errore stima modello spillover: ", e$message); NULL }
    )
    mod
  })
  
  effetti_spill <- eventReactive(input$run_spill, {
    res <- modelli_stimati()
    req(!is.null(res))
    
    mod_name <- input$mod_spillover
    m <- res[[mod_name]]
    if (is.null(m)) return(NULL)
    
    W_tipo <- input$W_type_spill
    W_listw <- get_W(W_tipo)
    
    anno_sel <- as.integer(input$anno_mod)
    panel_anno <- panel[panel$anno == anno_sel, ]
    
    df_spillover <- if (isTRUE(input$std_x)) {
      # Se la spunta è attiva, standardizziamo le X esattamente come nella tab modelli
      X_vars <- input$indep_mod
      panel_std <- panel_anno
      if (length(X_vars) > 0) {
        panel_std[, X_vars] <- scale(panel_anno[, X_vars, drop = FALSE])
      }
      panel_std  
    } else {
      panel_anno 
    }
    
    
    imp <- tryCatch({
      if (mod_name == "SAR" || mod_name == "SAC") {
        impacts(m, listw = W_listw)
      } else if (mod_name == "SDM" || mod_name == "GNS") {
        impacts(m, listw = W_listw, data = df_spillover)
      } else {
        NULL
      }
    }, error = function(e) {
      cat("Errore nel calcolo degli impatti:", e$message, "\n")
      NULL
    })
    
    return(imp)
  })
  
  output$plot_spillover <- renderPlotly({
    imp <- effetti_spill()
    req(!is.null(imp))
    tab <- tryCatch({
      s <- summary(imp, zstats = FALSE)
      as.data.frame(s$impacts)
    }, error = function(e) NULL)
    if (is.null(tab) || nrow(tab) == 0) {
      direct_v   <- as.numeric(imp$direct)
      indirect_v <- as.numeric(imp$indirect)
      total_v    <- as.numeric(imp$total)
      vars <- if (!is.null(rownames(imp$direct))) rownames(imp$direct) else names(imp$direct)
      if (is.null(vars) || length(vars) == 0) vars <- paste0("var", seq_along(direct_v))
    } else {
      vars       <- rownames(tab)
      direct_v   <- tab$direct
      indirect_v <- tab$indirect
      total_v    <- tab$total
    }
    df_eff <- data.frame(
      variabile = rep(vars, 3),
      Effetto   = rep(c("Diretto", "Indiretto", "Totale"), each = length(vars)),
      Valore    = c(direct_v, indirect_v, total_v)
    )
    plot_ly(df_eff, x = ~Valore, y = ~variabile, color = ~Effetto,
            type = "bar", orientation = "h") %>%
      layout(barmode = "group",
             title  = paste0("Impatti – ", input$mod_spillover,
                             if (isTRUE(input$std_x)) " (X standardizzate)" else ""),
             xaxis  = list(title = "Effetto marginale"),
             yaxis  = list(title = ""))
  })
  
  output$tabella_spillover <- renderTable({
    imp <- effetti_spill()
    req(!is.null(imp))
    tab <- tryCatch({
      s <- summary(imp, zstats = FALSE)
      as.data.frame(s$impacts)
    }, error = function(e) NULL)
    if (is.null(tab) || nrow(tab) == 0) {
      direct_v   <- as.numeric(imp$direct)
      indirect_v <- as.numeric(imp$indirect)
      total_v    <- as.numeric(imp$total)
      vars <- if (!is.null(rownames(imp$direct))) rownames(imp$direct) else names(imp$direct)
      if (is.null(vars) || length(vars) == 0) vars <- paste0("var", seq_along(direct_v))
    } else {
      vars       <- rownames(tab)
      direct_v   <- tab$direct
      indirect_v <- tab$indirect
      total_v    <- tab$total
    }
    data.frame(
      Variabile = vars,
      Diretto   = round(direct_v,   4),
      Indiretto = round(indirect_v, 4),
      Totale    = round(total_v,    4)
    )
  }, striped = TRUE, bordered = TRUE)
  
} # cierra server <- function(input, output, session)
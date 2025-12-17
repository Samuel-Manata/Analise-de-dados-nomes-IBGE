# Carregamento de pacotes
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(bslib)
library(bsicons)
library(forecast)
library(lubridate)
library(scales)

# Lê o RDS do banco de dados já tratado
df_final <- readRDS("bdTratado.rds")

# O objeto df_app termina alguns ajustes finais do banco de dados, fazendo algumas transformações
df_app <- df_final %>%
  mutate(
    frequencia = as.numeric(frequencia),
    var_pct_num = readr::parse_number(as.character(var_pct)),
    rank_decada = as.integer(rank_decada),
    rotulo_decada = case_when(
      ano_fim == 1930 ~ "Antes de 1930",
      TRUE ~ paste0(ano_fim - 9, "-", ano_fim)
    ),
    
    # Criação de um fator ordenado pelo ano_fim para o eixo X
    decada_label_ordenada = factor(rotulo_decada, levels = unique(rotulo_decada)[order(unique(ano_fim))])
  )

# Lista de nomes para o selectizeInput
lista_nomes <- sort(unique(df_app$nome))

# Construção da parte da UI
ui <- page_sidebar(
  
  # Implementa o arquivo css contido dentro do arquivo com algumas estilizações
  tags$head(includeCSS("style.css")),
  
  # Trabalha com o tema do dashboard
  title = h2("Explorador de Nomes Brasileiros", class = "app-title-clean"),
  theme = bs_theme(
    bootswatch = "darkly",
    primary = "#2c3e50",    
    secondary = "#18bc9c",  
    success = "#2ecc71",    
    info = "#3498db",       
    warning = "#f39c12",    
    danger = "#e74c3c",     
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),
  
  # Código da barra lateral
  sidebar = sidebar(
    helpText("Compare a evolução de nomes ao longo das décadas."),
    # Seletores de nomes
    selectizeInput(
      inputId = "nomes_selecionados",
      label = "Escolha os Nomes:",
      choices = NULL, 
      selected = c("ANA", "MARIA"), 
      multiple = TRUE,
      options = list()
    ),
    hr(),
    sliderInput(
      # Filtro de período
      inputId = "periodo_range",
      label = "Filtrar Período (Ano Fim):",
      min = 1930,
      max = 2010,
      value = c(1930, 2010),
      step = 10,
      sep = ""
    ),
    selectInput(
      # Permite a troca do tema do dashboard com bootstrap
      "tema_escolhido",
      "Tema do App:",
      choices = c("darkly", "cyborg", "minty", "pulse", "cosmo", "solar"),
      selected = "darkly"
    )
  ),
  
  # Value Boxes (KPIs) com os principais indicadores
  layout_columns(
    value_box(
      # Total de nascimentos do nome filtrado
      title = "Total de Registros",
      value = textOutput("kpi_total"),
      showcase = bsicons::bs_icon("people-fill"),
      theme = "primary",
      class = "value-box"
    ),
    value_box(
      # Nome vencedor do período
      title = "Nome Vencedor (Volume)",
      value = textOutput("kpi_vencedor"),
      showcase = bsicons::bs_icon("trophy-fill"),
      theme = "success",
      class = "value-box"
    ),
    value_box(
      # Década com mais registros do nome filtrado
      title = "Década com Mais Registros",
      value = textOutput("kpi_pico"),
      showcase = bsicons::bs_icon("calendar-event"),
      theme = "info",
      class = "value-box"
    )
  ),
  
  br(),
  
  # Painel principal (Outputs), com as abas
  navset_card_underline(
    
    # Aba 1: Visão Geral
    nav_panel("Visão Geral", 
              layout_columns(
                col_widths = c(8, 4), 
                # Evolução dos registros dos nomes filtrados
                card(
                  card_header("Evolução"),
                  plotlyOutput("plot_linha", height = "500px")
                ),
                card(
                  # Total de registros acumulados no período selecionado
                  card_header("Total Acumulado"),
                  plotlyOutput("plot_barras", height = "500px")
                )
              )
    ),
    
    #Aba 2: Frequência Geral
    nav_panel("Frequência Geral", 
              # Mostra o total de registros de todo o banco de dados em cada década
              card(
                card_header("Total de Registros de Nomes por Período"),
                helpText("Mostra o total de nascimentos registrados em todo o dataset para cada período."),
                plotlyOutput("plot_total_frequencia", height = "500px")
              )
    ),
    
    # Aba 3: Ranking
    nav_panel("Ranking",
              # Ranking dos nomes filtrados no momento, indicando qual é mais popular em cada década
              card(
                card_header("Disputa de Popularidade (Posição no Ranking)"),
                helpText("Quanto mais alto, mais popular. Linhas cruzando mostram ultrapassagens."),
                plotlyOutput("plot_ranking", height = "500px")
              )
    ),
    
    # Aba 4: Dinâmica & Variação
    nav_panel("Dinâmica & Variação", 
              layout_columns(
                col_widths = c(6, 6),
                
                # Gráfico de velocidade de crescimento, mostrando quantos registrou o nome filtrado ganhou ou perdeu de uma década para outra
                card(
                  card_header("Velocidade de Crescimento (Ganho/Perda Real)"),
                  helpText("Barras verdes indicam ganhos de registro de uma década para outra, já barras vermelhas indicam queda."),
                  plotlyOutput("plot_velocidade", height = "400px")
                ),
                
                # Gráfico pizza com a proporção dos nomes selecionados no período de décadas selecionado
                card(
                  card_header("Gráfico de pizza"),
                  helpText("Dentre os selecionados, quem dominava a 'fatia da pizza' em relação ao total?"),
                  plotlyOutput("plot_proporcao", height = "600px")
                )
              )
    ),
    
    # Aba 5: Volatilidade
    nav_panel("Volatilidade",
              layout_columns(
                col_widths = c(6, 6),
                # Mostra a estabilidade dos 20 nomes mais instáveis do banco de dados de 1930 até 2010     
                card(
                  card_header("Índice de Volatilidade dos Nomes (1930–2010)"),
                  helpText("Quanto maior a volatilidade, mais instável é o nome ao longo das décadas."),
                  plotlyOutput("plot_volatilidade", height = "400px")
                ),
                
                # Tabela que mostra a estabilidade de todos os nomes do banco de dados
                card(
                  DTOutput("tabela_volatilidade")
                )
              )
    ),
    
    # Aba 6: Análises Estatísticas
    nav_panel("Análises Estatísticas",
              
              layout_columns(
                col_widths = c(6, 6),
                
                # Crescimento médio de registros por década
                card(
                  card_header("Crescimento Médio por Década"),
                  helpText("Mostra a tendência geral de longo prazo para cada nome selecionado."),
                  plotlyOutput("plot_crescimento_medio", height = "450px")
                ),
                
                # Centralidade temporal, mostrando quando foi que o nome teve mais registros, utiliizando média ponderada
                card(
                  card_header("Centralidade Temporal do Nome"),
                  helpText("Mostra o ano de dominância dos nomes selecionados por média ponderada."),
                  plotlyOutput("plot_centralidade", height = "500px")
                )
              )
    ),
    
    # Aba 7: Tendência Linear
    # Coeficiente B1: Direção da tendência
    # Intercepto: Ponto Inicial
    # R^2: Qualidade da Regressão
    nav_panel("Tendência Linear",
              layout_columns(
                col_widths = c(6, 6),
                # Calcula a direção da popularidade do nome ao longo do tempo, se a tendência foi subir ou descer
                card(
                  card_header("Tendência Linear — Regressão por Nome"),
                  helpText("Mostra a direção geral da popularidade do nome ao longo do tempo."),
      
                  plotlyOutput("plot_tendencia", height = "400px")
                ),
                
                # Mostra a tabela da tendência de cada nome
                card(
                  DTOutput("tabela_tendencia")
                )
              )
    ),
    
    # Aba 8: Previsão
    nav_panel("Previsão (2020–2030)",
              
              layout_columns(
                col_widths = c(12, 12),
                # Gráfico preditivo com a frequência "esperada" para 2020-2030   
                card(
                  card_header("Previsão da Frequência para 2020–2030"),
                  uiOutput("texto_previsao"),
                  plotlyOutput("plot_previsao", height = "500px")
                )
              )
    ),
    
    # ABA 9: Tabela
    # Tabela dos nomes
    nav_panel("Tabela",
              DTOutput("tabela_dados")
    )
    
  ) 
)

# Construindo a parte do server, com as funções em si
server <- function(input, output, session) {
  
  # Atualiza dinâmicamente o selectizeInput com o vetor das opções disponíveis que são os nomes do dataset  
  updateSelectizeInput(session, "nomes_selecionados", choices = lista_nomes, selected = c("ANA", "JOAO"), server = TRUE)
  
  # Para quando o tema do dashboard for trocado, e atualizar todo o aplicativo com o tema escolhido
  observeEvent(input$tema_escolhido, {
    session$setCurrentTheme(
      bs_theme(bootswatch = input$tema_escolhido)
    )
  })
  
  # Reactive para filtrar os nomes e período
  dados_filtrados <- reactive({
    req(input$nomes_selecionados)
    df_app %>%
      filter(nome %in% input$nomes_selecionados) %>%
      filter(ano_fim >= input$periodo_range[1] & ano_fim <= input$periodo_range[2])
  })
  
  # Total geral de todos os nomes por década no período selecionado
  dados_total_por_decada <- reactive({
    df_app %>%
      filter(ano_fim >= input$periodo_range[1] & ano_fim <= input$periodo_range[2]) %>%
      group_by(decada_label_ordenada) %>%
      summarise(Frequencia_Total = sum(frequencia, na.rm = TRUE)) %>%
      ungroup()
  })
  
  # Atualiza o gráfico de pizza com os nomes selecionados e com os nomes restantes
  dados_pizza_real <- reactive({
    req(input$nomes_selecionados)
    
    total_geral_periodo <- df_app %>%
      ungroup() %>%
      filter(ano_fim >= input$periodo_range[1] & ano_fim <= input$periodo_range[2]) %>%
      summarise(total = sum(frequencia, na.rm = TRUE)) %>%
      pull(total)

    df_selecionados <- dados_filtrados() %>%
      group_by(nome) %>%
      summarise(valor = sum(frequencia, na.rm = TRUE)) %>%
      arrange(desc(valor))

    total_selecionados <- sum(df_selecionados$valor)
    valor_outros <- total_geral_periodo - total_selecionados
    
    if (valor_outros > 0) {
      df_final <- bind_rows(
        df_selecionados,
        tibble(nome = "Outros (Restante)", valor = valor_outros)
      )
    } else {
      df_final <- df_selecionados
    }

    df_final
  })
  
  # Nome com maior soma no período filtrado
  resumo_vencedor <- reactive({
    dados_filtrados() %>%
      group_by(nome) %>%
      summarise(total = sum(frequencia, na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      slice(1)
  })
  
  # KPIs simples, com total, vencedor e pico
  output$kpi_total <- renderText({ scales::number(sum(dados_filtrados()$frequencia, na.rm=TRUE), big.mark = ".") })
  output$kpi_vencedor <- renderText({ resumo_vencedor()$nome })
  output$kpi_pico <- renderText({ 
    pico <- dados_filtrados() %>% group_by(rotulo_decada) %>% summarise(t = sum(frequencia)) %>% arrange(desc(t)) %>% slice(1)
    pico$rotulo_decada
  })
  
  # Gráfico de linhas da evolução dos registros ao decorrer das décadas 
  output$plot_linha <- renderPlotly({
    req(nrow(dados_filtrados()) > 0)
    g <- ggplot(dados_filtrados(), aes(x = ano_fim, y = frequencia, color = nome)) +
      geom_line(linewidth = 1) +
      geom_point(aes(text = paste0("Nome: ", nome, "<br>Década: ", rotulo_decada, "<br>Frequência: ", frequencia))) +
      labs(x = "Década", y = "Total de Registros", color = "") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(1930, 2010, 10))
    ggplotly(g, tooltip = "text")
  })
  
  # Gráfico de barras com o total acumulado de registros das décadas dentro do período selecionado
  output$plot_barras <- renderPlotly({
    req(nrow(dados_filtrados()) > 0)
    df_barras <- dados_filtrados() %>%
      group_by(nome) %>%
      summarise(total = sum(frequencia, na.rm = TRUE)) %>%
      mutate(nome = fct_reorder(nome, total))
    
    g <- ggplot(df_barras, aes(x = total, y = nome, fill = nome)) +
      geom_col() +
      scale_x_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Total Acumulado", y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(g, tooltip = c("x", "y"))
  })
  
  # Gráfico de linhas mostrando a disputa de popularidade dos nomes selecionados no filtro, mostrando períodos de altas ou baixas e quando um ultrapassa ou não o outro
  output$plot_ranking <- renderPlotly({
    req(nrow(dados_filtrados()) > 0)
    g <- ggplot(dados_filtrados(), aes(x = ano_fim, y = rank_decada, color = nome)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2, aes(text = paste0("Posição: #", rank_decada))) +
      scale_y_reverse() +
      labs(x = "Década", y = "Posição no Ranking", color = "") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(1930, 2010, 10))
    ggplotly(g, tooltip = "text")
  })
  
  # Velocidade de crescimento dos registros dos nomes selecionados com base na diferença absoluta de uma década pra outra
  output$plot_velocidade <- renderPlotly({
    req(nrow(dados_filtrados()) > 0)
    
    df_vel <- dados_filtrados() %>%
      mutate(
        tipo_crescimento = ifelse(dif_abs >= 0, "Crescimento", "Queda"),
        texto_tooltip = paste0(
          "Nome: ", nome,
          "<br>Década: ", rotulo_decada,
          "<br>Mudança: ", ifelse(dif_abs > 0, "+", ""),
          scales::number(dif_abs, big.mark=".", decimal.mark=",")
        )
      )
    
    g <- ggplot(df_vel, aes(
      x = ano_fim,
      y = dif_abs,
      fill = tipo_crescimento,
      text = texto_tooltip              
    )) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      scale_fill_manual(values = c("Crescimento" = "#18bc9c", "Queda" = "#e74c3c")) +
      labs(x = "Década", y = "Diferença Absoluta", fill = "") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(1930, 2010, 10)) +
      scale_y_continuous(                    
        labels = scales::label_number(big.mark=".", decimal.mark=",")
      )
    
    ggplotly(g, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })
  
  # Mostra entre os nomes filtrados quem dominava o gráfico de pizza no intervalo de tempo selecionado
  output$plot_proporcao <- renderPlotly({
    dados <- dados_pizza_real()
    
    validate(
      need(nrow(dados) > 0, "Nenhum dado para exibir.")
    )
    
    cores_personalizadas <- c(
      scales::hue_pal()(nrow(dados) - 1), 
      "#d3d3d3"                           
    )
    
    plot_ly(dados, labels = ~nome, values = ~valor, type = 'pie',
            textinfo = 'label+percent', 
            hoverinfo = 'text',
            text = ~paste(nome, '<br>Total:', scales::comma(valor, big.mark = ".")),
            sort = FALSE, 
            marker = list(colors = cores_personalizadas, line = list(color = '#FFFFFF', width = 1))) %>% 
      layout(
        showlegend = TRUE,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  
  # Gráfico de barras com o total de registro de nomes por período
  output$plot_total_frequencia <- renderPlotly({
    dados <- dados_total_por_decada()
    
    g <- ggplot(dados, aes(
      x = decada_label_ordenada, 
      y = Frequencia_Total,
      text = paste0(
        "Período: ", decada_label_ordenada,
        "<br>Total: ", scales::comma(Frequencia_Total, big.mark = ".", decimal.mark = ",")
      )
    )) +
      geom_bar(stat = "identity", fill = "#5F9EA0", alpha = 0.8) +
      labs(x = "Período", y = "Total de Registros (Todos os Nomes)") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
    
    ggplotly(g, tooltip = "text") %>%
      layout(yaxis = list(title = "Total de Registros"))
  })
  
  # Calcula a volatilidade por nome (utilizando a raiz da média dos quadrados das variações percentuais)
  volatilidade_nomes <- reactive({
    
    df <- df_app %>%
      group_by(nome) %>%
      arrange(ano_fim) %>%
      mutate(
        var_pct = (frequencia - lag(frequencia)) / lag(frequencia)
      ) %>%
      filter(!is.na(var_pct)) %>% 
      summarise(
        volatilidade = sqrt(mean((var_pct)^2)),
        .groups = "drop"
      ) %>%
      arrange(desc(volatilidade))
    
    df
  })
  
  # Gráfico com os 20 nomes mais voláteis
  output$plot_volatilidade <- renderPlotly({
    
    df_vol <- volatilidade_nomes() %>% slice(1:20)
    
    g <- ggplot(df_vol, aes(x = reorder(nome, volatilidade), y = volatilidade)) +
      geom_col(fill = "#458B74") +
      coord_flip() +
      labs(
        x = "Nome",
        y = "Volatilidade",
        title = "Top 20 Nomes Mais Voláteis (Crescimento Instável)"
      ) +
      theme_minimal()
    
    ggplotly(g, tooltip = c("x", "y"))
  })
  
  # Tabela com as volatilidades de TODOS os nomes do banco de dados
  output$tabela_volatilidade <- renderDT({
    
    df_vol <- volatilidade_nomes() %>%
      arrange(desc(volatilidade)) %>% 
      mutate(volatilidade = round(volatilidade, 4))
    
    datatable(
      df_vol,
      extensions = "Responsive",
      options = list(
        pageLength = 10,
        order = list(list(1, "desc"))
      )
    )
  })
  
  # Crescimento médio dos registros dos nomes filtrados por década
  output$plot_crescimento_medio <- renderPlotly({
    df_cresc <- dados_filtrados() %>%
      group_by(nome) %>%
      summarise(
        f0 = first(frequencia),
        ft = last(frequencia),
        n_decadas = n(),
        crescimento_medio = (ft - f0) / n_decadas
      )
    
    g <- ggplot(df_cresc, aes(x = nome, y = crescimento_medio, fill = nome,
                              text = paste0(
                                "Nome: ", nome, "<br>",
                                "Crescimento médio: ", scales::comma(crescimento_medio, big.mark=".")
                              ))) +
      geom_col() +
      labs(x = NULL, y = "Crescimento Médio", title = "Crescimento Médio por Década") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(g, tooltip = "text")
  })
  
  # Ano médio ponderado pela frequência, mostrando quando o nome teve mais força
  output$plot_centralidade <- renderPlotly({
    df_cent <- dados_filtrados() %>%
      group_by(nome) %>%
      summarise(
        centralidade = weighted.mean(ano_fim, frequencia, na.rm = TRUE)
      )
    
    g <- ggplot(df_cent, aes(x = centralidade, y = reorder(nome, centralidade), color = nome,
                             text = paste0(
                               "Nome: ", nome, "<br>",
                               "Centro temporal: ", centralidade
                             ))) +
      geom_point(size = 5) +
      labs(y = NULL, x = "Ano Médio Ponderado",
           title = "Centralidade Temporal da Popularidade") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(1930, 2010, 10))
    
    ggplotly(g, tooltip = "text")
  })
  
  # Regressão Linear por nome
  tendencia_linear <- reactive({
    
    df_app %>%
      group_by(nome) %>%
      do({
        # Ajusta o lm() e extrai o coeficiente da inclinação, intercepto e R^2
        modelo <- lm(frequencia ~ ano_fim, data = .)
        
        tibble(
          coeficiente = coef(modelo)[2],        
          intercepto  = coef(modelo)[1],
          r2          = summary(modelo)$r.squared
        )
      }) %>%
      ungroup()
  })
  
  # Gráfico que ilustra a regressão de cada nome com linhas indivíduais
  output$plot_tendencia <- renderPlotly({
    
    req(input$nomes_selecionados)
    
    df <- df_app %>% 
      filter(nome %in% input$nomes_selecionados)
    
    g <- ggplot(df, aes(x = ano_fim, y = frequencia, color = nome)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "Regressão Linear por Nome Selecionado",
        x = "Ano",
        y = "Frequência"
      ) +
      theme_minimal()
    
    ggplotly(g)
  })
  
  # Tabela com as informações da regressão de cada nome do banco de dados
  output$tabela_tendencia <- renderDT({
    
    df_tend <- tendencia_linear() %>%
      mutate(
        coeficiente = round(coeficiente, 4),
        r2 = round(r2, 4)
      ) %>%
      arrange(desc(coeficiente))   
    
    datatable(
      df_tend,
      extensions = "Responsive",
      options = list(
        pageLength = 10,
        order = list(list(1, "desc"))
      ),
      colnames = c("Nome", "Coeficiente", "Intercepto", "R2")  
    )
  })
  
  # Série agregada por nome e ano para as previsões
  serie_nome <- reactive({
    req(input$nomes_selecionados)
    
    dados_filtrados() %>%
      group_by(nome, ano_fim) %>%
      summarise(freq = sum(frequencia), .groups = "drop")
  })
  
  # Texto explicativo da previsão
  output$texto_previsao <- renderUI({
    HTML("O modelo ARIMA automático é utilizado para prever a frequência dos nomes na década de 2020-2030.")
  })
  
  # Gera previsões por nome usando o forecast::auto.arima
  output$plot_previsao <- renderPlotly({
    
    df <- serie_nome()
    
    lista_prev <- list()  
    
    for (nom in unique(df$nome)) {
      
      df_nome <- df %>% filter(nome == nom) %>% arrange(ano_fim)
      
      ts_nome <- ts(df_nome$freq, start = df_nome$ano_fim[1], frequency = 1)
      
      modelo <- forecast::auto.arima(ts_nome)
      
      ultimo_ano <- max(df_nome$ano_fim)
      anos_futuros <- seq(from = ultimo_ano + 10, to = 2030, by = 10)
      
      h_prev <- length(anos_futuros)
      
      previsao <- forecast::forecast(modelo, h = h_prev)
      
      df_prev_nome <- tibble(
        ano_fim = anos_futuros,
        freq = as.numeric(previsao$mean),
        nome = nom
      )
      
      lista_prev[[nom]] <- df_prev_nome
    }
    
    df_prev <- bind_rows(lista_prev)
    
    # Gera o gráfico com as previsões 2020-2030
    g <- ggplot() +
      geom_line(data = df, aes(x = ano_fim, y = freq, color = nome), linewidth = 1) +
      geom_line(data = df_prev, aes(x = ano_fim, y = freq, color = nome),
                linewidth = 1, linetype = "dashed") +
      labs(x = "Ano", y = "Frequência Estimada", color = "") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(1930, 2030, 10)) +
      scale_y_continuous(
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      )   
    
    ggplotly(g, tooltip = c("x", "y", "color"))
  })
  
  # Mostra a tabela de dados filtrados com algumas colunas selecionadas do banco de dados
  output$tabela_dados <- renderDT({
    req(nrow(dados_filtrados()) > 0)
    
    dados_filtrados() %>%
      select(
        `Início da Década` = ano_inicio,  
        Nome = nome,
        Década = rotulo_decada,
        Frequência = frequencia,
        `Variação %` = var_pct,
        Ranking = rank_decada
      ) %>%
      datatable(options = list(pageLength = 10))
  })
}

shinyApp(ui, server)

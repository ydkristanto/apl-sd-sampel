# Paket yang dibutuhkan ----
library(shiny)
library(tidyverse)

# Antarmuka ----
ui <- navbarPage(
  title = "Simpangan Baku Sampel",
  tabPanel("Eksplorasi",
           sidebarLayout(
             sidebarPanel(
               wellPanel(
                 p("Sampel-sampel dihasilkan dari populasi yang berdistribusi normal dengan rerata 50 dan simpangan baku 10.")
               ),
               wellPanel(
                 sliderInput("n_sampel", "Jangkauan ukuran sampel:",
                             min = 2, max = 100, value = c(2, 20), step = 2),
                 sliderInput("k_sampel", "Banyak sampel:",
                             min = 5, max = 1000, value = 50)
                 )
               ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Pembagi",
                          br(),
                          plotOutput("plot_pembagi")
                          ),
                 tabPanel("Pengurang",
                          br(),
                          plotOutput("plot_pengurang")
                          )
                 )
               )
             )
           ),
  tabPanel("Informasi",
    br()
  )
)

# Fungsi peladen ----
server <- function(input, output) {
  
  ## Menghitung rerata sd setiap ukuran sampelnya ----
  hitung_rerata_sd <- function(n, k_sampel) {
    seed <- as.numeric(Sys.Date())
    set.seed(seed)
    
    data_sd_x_n <- numeric(k_sampel)
    data_sd_x_n1 <- numeric(k_sampel)
    data_sd_mu_n <- numeric(k_sampel)
    data_sd_mu_n1 <- numeric(k_sampel)
    
    for (i in 1:k_sampel) {
      set_sampel <- rnorm(n, mean = 50, sd = 10)
      
      # Menghitung sd dengan pengurang x_bar dan pembagi n
      sd_x_n <- sqrt(sum((set_sampel - mean(set_sampel))^2) / n)
      
      # Menghitung sd dengan pengurang x_bar dan pembagi n - 1
      sd_x_n1 <- sqrt(sum((set_sampel - mean(set_sampel))^2) / (n - 1))
      
      # Simpangan baku dengan pegurang mu dan pembagi n
      sd_mu_n <- sqrt(sum((set_sampel - 50)^2) / n)
      
      # Simpangan baku denga pengurang mu dan pembagi n - 1
      sd_mu_n1 <- sqrt(sum((set_sampel - 50)^2) / (n - 1))
      
      data_sd_x_n[i] <- sd_x_n
      data_sd_x_n1[i] <- sd_x_n1
      data_sd_mu_n[i] <- sd_mu_n
      data_sd_mu_n1[i] <- sd_mu_n1
    }
    
    return(c(mean(data_sd_x_n), mean(data_sd_x_n1),
             mean(data_sd_mu_n), mean(data_sd_mu_n1)))
  }
  
  rep_hitung_rerata_sd <- repeatable(hitung_rerata_sd)
  
  data_plot <- reactive({
    # Ukuran sampel
    n_min <- input$n_sampel[1]
    n_maks <- input$n_sampel[2]
    ukuran_sampel <- seq(n_min, n_maks, by = 1)
    
    # Menghitung sd untuk tiap-tiap sampelnya dengan setiap versi sd
    k <- input$k_sampel
    rerata_sd <- sapply(ukuran_sampel, function(n)
      rep_hitung_rerata_sd(n, k))
    
    # Membuat data untuk diplot
    data_sd <- tibble(
      n = rep(n_min:n_maks, 4),
      m_sd = rep(c(rerata_sd[1,], rerata_sd[2,],
                      rerata_sd[3,], rerata_sd[4,])),
      pembagi = rep(c("x_n", "x_n1", "mu_n", "mu_n1"),
                    each = length(ukuran_sampel))
    )
    
    return(data_sd)
  })
  
  ## Render plot_pembagi ----
  output$plot_pembagi <- renderPlot({
    data <- data_plot() %>% 
      filter(pembagi == c("x_n", "x_n1"))
    ggplot(data, aes(x = n, y = m_sd, color = pembagi)) +
      geom_hline(yintercept = 10, linewidth = 1, linetype = "dashed") +
      geom_line(linewidth = 1) +
      geom_line(stat = "smooth", method = "lm",
                formula = y ~ I(1 / x), se = FALSE,
                linewidth = 1, alpha = .6) +
      labs(title = "Dua Versi Simpangan Baku Sampel",
           subtitle = "Tren rerata dua versi simpangan baku sampel ketika ukuran sampelnya semakin besar",
           x = "Ukuran Sampel", y = "Rerata Simpangan Baku") +
      scale_color_brewer(palette = "Dark2",
                         name = "Pembagi",
                         labels = c("x_n" = "n",
                                    "x_n1" = "n - 1")) +
      theme_bw(base_size = 14) +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold"))
  })
  
  ## Render plot_pengurang ----
  output$plot_pengurang <- renderPlot({
    data <- data_plot() %>% 
      filter(pembagi == c("x_n1", "mu_n"))
    ggplot(data, aes(x = n, y = m_sd, color = pembagi)) +
      geom_hline(yintercept = 10, linewidth = 1, linetype = "dashed") +
      geom_line(linewidth = 1) +
      geom_line(stat = "smooth", method = "lm",
                formula = y ~ I(1 / x), se = FALSE,
                linewidth = 1, alpha = .6) +
      labs(title = "Dua Versi Simpangan Baku Sampel",
           subtitle = "Tren rerata dua versi simpangan baku sampel ketika ukuran sampelnya semakin besar",
           x = "Ukuran Sampel", y = "Rerata Simpangan Baku") +
      scale_color_brewer(palette = "Dark2",
                         name = "Pengurang",
                         labels = c("x_n1" = "rerata sampel (x_bar)",
                                    "mu_n" = "rerata populasi (mu)")) +
      theme_bw(base_size = 14) +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold"))
  })
  
}

# Run the application
shinyApp(ui, server)
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
                 p("Semua sampel dihasilkan dari populasi yang berdistribusi normal dengan rerata 50 dan simpangan baku 10.")
               ),
               wellPanel(
                 sliderInput("n_sampel", "Jangkauan ukuran sampel:",
                             min = 2, max = 100, value = c(2, 20), step = 1),
                 sliderInput("k_sampel", "Banyak sampel:",
                             min = 5, max = 1000, value = 50)
                 )
               ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Pembagi",
                          br(),
                          plotOutput("plot_pembagi"),
                          tableOutput("tabel_pembagi"),
                          textOutput("teks_pembagi"),
                          hr(),
                          helpText(textOutput("teks_pembagi_info")),
                          br()
                          ),
                 tabPanel("Pengurang",
                          br(),
                          plotOutput("plot_pengurang"),
                          textOutput("teks_pengurang"),
                          hr(),
                          helpText(textOutput("teks_pengurang_info")),
                          br()
                          )
                 )
               )
             )
           ),
  tabPanel("Informasi",
           sidebarLayout(
             sidebarPanel(
               wellPanel(
                 div(h4("Deskripsi",
                        style = "font-size: inherit;
                             font-weight: bold")),
                 div(p("Aplikasi Shiny ini mendemonstrasikan mengapa dalam rumus simpangan baku sampel tidak menggunakan ukuran sampel (n) sebagai pembaginya melainkan n - 1."))
               ),
               wellPanel(
                 div(h4("Kode sumber",
                        style = "font-size: inherit;
                             font-weight: bold")),
                 div(p("Kode sumber aplikasi ini tersedia di", a("repositori Github.", href = "https://github.com/ydkristanto/apl-sd-sampel", target = "_blank"), "Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan", a("buat sebuah isu", href = "https://github.com/ydkristanto/apl-sd-sampel/issues", target = "_blank"), "atau lebih baik lagi", a("minta penarikan", href = "https://github.com/ydkristanto/apl-sd-sampel/pulls", target = "_blank"), "di repositori tersebut."))
               ),
               wellPanel(
                 div(h4("Lisensi",
                        style = "font-size: inherit;
                             font-weight: bold")),
                 div(p("Lisensi MIT"),
                     p("Copyright (c) 2024 Yosep Dwi Kristanto"))
               )
             ),
             mainPanel(
               div(h3("Aplikasi Shiny Simpangan Baku Sampel")),
               div(p("Aplikasi interaktif ini bertujuan untuk menunjukkan mengapa rumus simpangan baku sampel menggunakan n - 1 sebagai pembaginya. Untuk mencapai tujuan tersebut, aplikasi ini mendemonstrasikan dua hal penting berikut."), align = "justify"),
               div(tags$ul(tags$li("Meskipun rumus simpangan baku dengan pembagi n dan n - 1 sama-sama mendekati simpangan baku populasi ketika ukuran sampelnya besar, tetapi simpangan baku sampel dengan pembagi n - 1 jauh lebih baik dengan pembagi n ketika sampelnya kecil."),
                           tags$li("Simpangan baku sampel dengan pembagi n - 1 tidak berbeda terlalu jauh dengan simpangan baku sampel ketika rerata populasinya diketahui.")), align = "justify"),
               hr(),
               div(p("Aplikasi interaktif ini dikembangkan dengan menggunakan bahasa pemrogram", a("R", href = "https://www.R-project.org/", target = "_blank"), "dan paket", a("Shiny.", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), "Paket", a("shinylive", href = "https://posit-dev.github.io/r-shinylive/", target = "_blank"), "digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah."), align = "justify"),
               div(p("Pengembang dan pemelihara aplikasi ini adalah", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"), "seorang dosen dan peneliti di program studi", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta."), align = "justify"),
               width = 6)
           )
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
    rerata_sd <- sapply(ukuran_sampel, function(n)
      rep_hitung_rerata_sd(n, input$k_sampel))
    
    # Membuat data untuk diplot
    data_sd <- tibble(
      n = rep(ukuran_sampel, 4),
      m_sd = c(rerata_sd[1,], rerata_sd[2,],
                      rerata_sd[3,], rerata_sd[4,]),
      pembagi = rep(c("x_n", "x_n1", "mu_n", "mu_n1"),
                    each = length(ukuran_sampel))
    )
    
    return(data_sd)
  })
  
  ## Render plot_pembagi ----
  output$plot_pembagi <- renderPlot({
    data <- data_plot() %>% 
      filter(pembagi == "x_n" | pembagi == "x_n1")
    n_min <- input$n_sampel[1]
    n_maks <- input$n_sampel[2]
    n_range <- n_maks - n_min
    besar_titik <- function(n) {
      -1 / 7744 * n^2 +3
    }
    ggplot(data, aes(x = n, y = m_sd, color = pembagi)) +
      geom_hline(yintercept = 10, linewidth = 1, linetype = "dashed") +
      geom_line(linewidth = 1.5) +
      geom_point(size = besar_titik(n_range)) +
      geom_line(stat = "smooth", method = "lm",
                formula = y ~ I(1 / x),
                se = FALSE, linewidth = 1, alpha = .6) +
      labs(title = "Dua Versi Simpangan Baku Sampel*",
           subtitle = "Tren rerata dua versi simpangan baku sampel ketika ukuran sampelnya semakin besar",
           x = "Ukuran Sampel", y = "Rerata Simpangan Baku") +
      scale_color_brewer(palette = "Dark2",
                         name = "Pembagi",
                         labels = c("x_n" = "n",
                                    "x_n1" = "n - 1")) +
      theme_bw(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold"))
  })
  
  ## Render teks pembagi ----
  output$teks_pembagi <- renderText({
    "Gambar 1: Tren rerata dua versi simpangan baku ketika ukuran sampel semakin besar. Tampak bahwa kedua versi tersebut sama-sama mendekati simpangan baku populasi ketika ukuran sampelnya besar. Akan tetapi, ketika ukuran sampelnya kecil simpangan baku sampel dengan pembagi n - 1 secara signifikan lebih mendekati simpangan baku populasi dibandingkan dengan versi yang pembaginya n."
  })
  
  ## Render teks pembagi info ----
  output$teks_pembagi_info <- renderText({
    "*Versi pertama menggunakan ukuran sampel (n) sebagai pembagi sedangkan versi kedua menggunakan n - 1."
  })
  
  ## Render plot_pengurang ----
  output$plot_pengurang <- renderPlot({
    data <- data_plot() %>% 
      filter(pembagi == "x_n1" | pembagi == "mu_n")
    
    n_min <- input$n_sampel[1]
    n_maks <- input$n_sampel[2]
    n_range <- n_maks - n_min
    besar_titik <- function(n) {
      -1 / 7744 * n^2 +3
    }
    
    ggplot(data, aes(x = n, y = m_sd, color = pembagi)) +
      geom_hline(yintercept = 10, linewidth = 1, linetype = "dashed") +
      geom_line(linewidth = 1.5) +
      geom_line(stat = "smooth", method = "lm",
                formula = y ~ I(1 / x), se = FALSE,
                linewidth = 1, alpha = .6) +
      geom_point(size = besar_titik(n_range)) +
      labs(title = "Dua Versi Simpangan Baku Sampel*",
           subtitle = "Tren rerata dua versi simpangan baku sampel ketika ukuran sampelnya semakin besar",
           x = "Ukuran Sampel", y = "Rerata Simpangan Baku") +
      scale_color_brewer(palette = "Dark2",
                         name = "Pengurang",
                         labels = c("x_n1" = "rerata sampel (x_bar)",
                                    "mu_n" = "rerata populasi (mu)")) +
      theme_bw(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold"))
  })
  
  ## Render teks pengurang ----
  output$teks_pengurang <- renderText({
    "Gambar 2: Tren rerata dua versi simpangan baku ketika ukuran sampel semakin besar. Tampak bahwa kedua versi tersebut sama-sama mendekati simpangan baku populasi ketika ukuran sampelnya besar. Ketika ukuran sampelnya kecil, perbedaan kedua versi tersebut tidak terlalu besar."
  })
  
  ## Render teks pengurang info ----
  output$teks_pengurang_info <- renderText({
    "*Versi pertama simpangan baku menganggap rerata populasi diketahui sehingga rerata tersebut digunakan sebagai pengurang. Pembagi simpangan baku versi pertama ini adalah ukuran sampel (n). Versi simpangan baku kedua menggunakan rerata sampel sebagai pengurang dan n - 1 sebagai pembaginya."
  })
  
}

# Run the application
shinyApp(ui, server)
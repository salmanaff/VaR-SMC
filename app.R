
# install.packages(c("shiny", "shinythemes", "bslib", "openxlsx", "DT", "dplyr", "quantmod", "ggpubr")

library(shiny)
library(shinythemes)
library(dplyr)
library(bslib)
library(openxlsx)
library(quantmod)
library(DT)
library(ggpubr)

#Links
link_github <- tags$a(shiny::icon("github"), "Github", href = "https://github.com/salmanaff", target = "_blank")

# List saham IDX30
saham <- c("PT Adaro Energy Tbk" = "ADRO.JK",
           "PT AKR Corporindo Tbk" = "AKRA.JK",
           "PT Sumber Alfaria Trijaya Tbk" = "AMRT.JK",
           "PT Aneka Tambang (Persero) Tbk" = "ANTM.JK",
           "PT Bank Artos Indonesia Tbk" = "ARTO.JK",
           "PT Astra International Tbk" = "ASII.JK",
           "PT Bank Central Asia Tbk" = "BBCA.JK",
           "PT Bank Negara Indonesia (Persero) Tbk" = "BBNI.JK",
           "PT Bank Rakyat Indonesia (Persero) Tbk" = "BBRI.JK",
           "PT Bank Mandiri (Persero) Tbk" = "BMRI.JK",
           "PT Barito Pacific Tbk" = "BRPT.JK",
           "PT Bukalapak.com Tbk" = "BUKA.JK",
           "PT Charoen Pokphand Indonesia Tbk" = "CPIN.JK",
           "PT Elang Mahkota Teknologi Tbk" = "EMTK.JK",
           "PT Surya Esa Perkasa Tbk" = "ESSA.JK",
           "PT GoTo Gojek Tokopedia Tbk" = "GOTO.JK",
           "PT Harum Energy Tbk" = "HRUM.JK",
           "PT Vale Indonesia Tbk" = "INCO.JK",
           "PT Indofood Sukses Makmur Tbk" = "INDF.JK",
           "PT Indo Tambangraya Megah Tbk" = "ITMG.JK",
           "PT Kalbe Farma Tbk" = "KLBF.JK",
           "PT Merdeka Copper Gold Tbk" = "MDKA.JK",
           "PT Medco Energi Internasional Tbk" = "MEDC.JK",
           "PT Perusahaan Gas Negara (Persero) Tbk" = "PGAS.JK",
           "PT Tambang Batubara Bukit Asam Tbk" = "PTBA.JK",
           "PT Semen Indonesia (Persero) Tbk" = "SMGR.JK",
           "PT Telkom Indonesia (Persero) Tbk" = "TLKM.JK",
           "PT Sarana Menara Nusantara Tbk" = "TOWR.JK",
           "PT United Tractors Tbk" = "UNTR.JK",
           "PT Unilever Indonesia Tbk" = "UNVR.JK"
)

ui <- page_navbar(
  theme = bs_theme(preset = "superhero"),
  tags$head(tags$style(HTML(".bslib-card {height:100%;}"))),
  title = "VaR-SMC",
  id = "nav",
  nav_panel("Title", uiOutput("info")),
  nav_panel("Data", uiOutput('pg2')),
  nav_panel("Return", uiOutput('pg3')),
  nav_panel("Monte Carlo", uiOutput('pg4')),
  
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_github)
  ),
  
  sidebar = sidebar(
    accordion(
      accordion_panel("Input", icon = bsicons::bs_icon("sliders"),
        selectInput("intype", "Pilih metode input file:", c("Upload File" = "up", "Pilih IDX30" = "idx")),
        uiOutput('outype')
      ),
      uiOutput("pilih"),
    )
  )
)


server <- function(input, output) {
  
  # =======================================================================
  # info
  
  output$info <- renderUI({
    navset_card_tab(title = "",
                    nav_panel("Inventor",
                              align = "center",
                              h4("Aplikasi GUI R"),
                              h4("Value at Risk Saham Tunggal dengan Simulasi Monte Carlo", style = "margin-top: -20px;"),
                              br(),
                              img(src="https://upload.wikimedia.org/wikipedia/id/2/2d/Undip.png", width="200px", style="display: block; margin-left: auto; margin-right: auto;"),
                              h5("Oleh:"),
                              h5("Salman Ahmad Faishal (24050121140120)", style = "margin-top: -20px;"),
                              
                              br(),
                              h5("DEPARTEMEN STATISTIKA", style = "margin-top: -20px;"),
                              h5("FAKULTAS SAINS DAN MATEMATIKA", style = "margin-top: -20px;"),
                              h5("UNIVERSITAS DIPONEGORO", style = "margin-top: -20px;"),
                              h5("TAHUN 2023", style = "margin-top: -20px;")
                    ),
                    nav_panel("Tinjauan Pustaka", 
                              h4("Metode Simulasi Monte-Carlo untuk Prediksi VaR"),
                              p("Simulasi Monte-Carlo adalah metode prediksi VaR yang didasarkan pada penciptaan berbagai macam kemungkinan risiko di masa depan menggunakan algoritma simulasi. Metode simulasi Monte-Carlo merupakan salah satu metode prediksi VaR dengan pendekatan parametrik."),
                              p("Pada simulasi Monte-Carlo, hasil simulasi yang diperoleh secara acak dijalankan dengan menggunakan estimasi parameter yang dipilih berdasarkan nilai parameter return historis. Setiap simulasi pada metode ini akan menghasilkan nilai yang berbeda, tetapi parameter dari hasil simulasi akan sama dengan parameter return historis yang dimiliki."),
                              # br(),
                              h4("Algoritma simulasi monte carlo pada saham tunggal:", style = "margin-bottom: -5px; margin-top: 5px"),
                              HTML("<ol type = '1'>
                              <li>Menentukan lama pengamatan atau <span style='color:orange'>selang waktu (T)</span> yang akan digunakan untuk prediksi nilai VaR.</li>
                              <li>Menghitung nilai return saham pada selang waktu yang telah ditentukan pada langkah (1).</li>
                              <li>Melakukan uji normalitas data return saham. Jika hasil uji normalitas memberikan kesimpulan bahwa data return saham berdistribusi Normal, maka prediksi VaR-SMC dapat dilanjutkan menuju langkah (4).</li>
                              <li>Menentukan nilai parameter return saham. Jika return saham berdistribusi normal, maka parameternya adalah μ dan σ.</li>
                              <li>Menentukan nilai <span style='color:orange'>tingkat kepercayaan (α)</span> .</li>
                              <li>Menentukan panjang holding period.</li>
                              <li>Menentukan besarnya nilai <span style='color:orange'>investasi awal (V0)</span> (langkah bersifat opsional).</li>
                              <li>Mensimulasikan nilai return dengan membangkitkan secara random return saham dengan parameter yang diperoleh dari langkah (4) sebanyak m kali sehingga terbentuk distribusi empiris dari return hasil simulasi.</li>
                              <li>Mencari <span style='color:orange'>nilai kuantil (X*)</span> yaitu estimasi kerugian maksimum pada tingkat kepercayaan α atau kuantil ke-(1-α) dari distribusi empiris return yang diperoleh pada langkah (8).</li>
                              <li>Menghitung prediksi nilai VaR pada tingkat kepercayaan α, dalam holding period sepanjang T hari dan investasi awal sebesar V0. <span style='color:orange'>VaR(a,X) = V0 × X* × √T</span></li>
                              <li>Mengulangi langkah (8) sampai langkah (10) sebanyak m kali sehingga mencerminkan berbagai kemungkinan nilai VaR, yaitu R1, R2, ... , Rm.</li>
                              <li>Menghitung rata-rata hasil dari langkah (11) untuk menstabilkan nilai karena nilai VaR yang dihasilkan pada setiap simulasi berbeda.</li>
                              </ol>")
                    )
    )
  })
  
  # =======================================================================
  # Input
  
  output$outype <- renderUI({
    if (input$intype == "idx") {
      tagList(
        selectInput("saham", "Pilih saham dari index IDX30:", saham),
        numericInput("lama", "Lama Pengamatan (hari kerja):", 
                     min = 1, max = 242, value = 50),
        helpText("Maksimum pengamatan adalah 1 tahun (242 hari kerja)"),
        hr(),
        actionButton(inputId = "save", "Save Data", class = "btn btn-dark")
      )
    }
    else {
      tagList(
        fileInput("saham","Input File Here:",multiple=FALSE,
                accept = c(".csv",".xlsx",".xls",".txt")),
        radioButtons(inputId = 'sep', label = 'Separator',
                   choices = c(Comma=',',Semicolon=';',Tab='\t',Space=''),
                   selected = ','),
        hr(),
        numericInput("lama", "Lama Pengamatan (hari kerja):", 
                   min = 1, max = 242, value = 100),
        helpText("Maksimum pengamatan adalah 1 tahun (242 hari kerja)")
      )
    }
  })
  
  observeEvent(input$saham,{
    if (is.null(input$saham)==F){nav_select("nav", selected = "Data")}
  })
  
  data = reactive({
    if (input$intype == "idx") {
      start = Sys.Date() - 366
      end = Sys.Date()
      Y = getSymbols(input$saham,src = "yahoo", from=start,to=end, auto.assign = F)
      names(Y) = gsub(paste0(input$saham, "."), "", names(Y))
      Date = as.Date(index(Y))
      Y = data.frame(Y)
      Y = cbind(Date, Y)
      rownames(Y) = index(Y)
    }
    else {
      if (endsWith(input$saham$name, ".csv")) {
        Y = read.csv(input$saham$datapath, sep = input$sep)
      }
      else if (endsWith(input$saham$name, ".txt")) { 
        Y = read.table(input$saham$datapath, sep = input$sep, header = TRUE)
      }
      else if (endsWith(input$saham$name, ".xls") || endsWith(input$saham$name, ".xlsx")) { 
        Y = read.xlsx(input$saham$datapath)
        Y$Date = convertToDate(Y$Date)
      }
    }
    Y = arrange(Y, desc(row_number()))
    Y = Y[1:input$lama,]
    Y = arrange(Y, desc(row_number()))
    Y
  })
  
  observeEvent(input$save,{
    start = Sys.Date() - 366
    end = Sys.Date()
    Y = getSymbols(input$saham,src = "yahoo", from=start,to=end, auto.assign = F)
    names(Y) = gsub(paste0(input$saham, "."), "", names(Y))
    Date = as.Date(index(Y))
    Y = data.frame(Y)
    Y = cbind(Date, Y)
    Y = arrange(Y, desc(row_number()))
    Y = Y[1:input$lama,]
    Y = arrange(Y, desc(row_number()))
    write.xlsx(Y, file = paste0(names(saham[saham=="ADRO.JK"]), ".xlsx"), overwrite = TRUE)
  })
  
  output$plot1 <- renderPlot(
    plot(data()$Date, data()$Close, type = "l", main = 'Grafik Saham', 
         ylab = "Close", xlab = "Date")
  )
  
  output$table1 <- renderDataTable(
    data()
  )
  
  output$sum1 <- renderPrint(
    summary(data())
  )
  
  output$pg2 <- renderUI(
    if(is.null(input$saham)) {return()}
    else {
      navset_card_tab(title = "Input Data",
                      nav_panel("Plot", plotOutput('plot1')),
                      nav_panel("Table", dataTableOutput('table1')),
                      nav_panel("Summary", verbatimTextOutput('sum1'))
      )
    }
  )
  
  # =======================================================================
  # Return
  
  opts = reactive(unique(colnames(data())))
  retur = eventReactive(input$nr, {
   data = data()
   y = data[input$R]
   r = log(y/lag(y))
   x = data.frame(y, r)
   colnames(x) = c(input$R, "Return")
   x
  })
  
  output$table2 <- renderDataTable(
    retur()
  )
  
  output$text1 <- renderPrint({
    r = na.omit(retur()$Return)
    ks = suppressWarnings(ks.test(r, "pnorm", mean(r), sd(r)))
    print(ks)
    alpha = 1-input$cl/100
    cat("==========================================================\n")
    cat("\nHipotesis\n")
    cat("H0: return berdistribusi normal\n")
    cat("H1: return tidak berdistribusi normal\n")
    cat("daerah kritis: tolak H0 jika p-value <", alpha,"\n")
    cat("statistik uji: nilai p-value =",ks$p.value,"\n")
    if(ks$p.value<alpha){
      cat("kesimpulan: return tidak berdistribusi normal, gunakan data saham lain\n")
    }else{
      cat("kesimpulan: return berdistribusi normal\n\n")
    }
  })
  
  output$plot2 <- renderPlot({
    r = na.omit(retur()$Return)
    ggqqplot(r)
  })
  
  observeEvent(input$nr,{
    if (input$nr>0){nav_select("nav", selected = "Return")}
  })
  
  output$pg3 <- renderUI(
    if(is.null(input$saham)) {return()}
    else {
      navset_card_tab(title = "Return",
                      nav_panel("Table", dataTableOutput('table2')),
                      nav_panel("Normality", verbatimTextOutput('text1')),
                      nav_panel("Plot", plotOutput('plot2'))
      )
    }
  )
  
  # =======================================================================
  # Simulasi Monte Carlo
  
  sim = eventReactive(input$smc, {
    r = na.omit(retur()$Return)
    n = length(r)
    mu = mean(r)
    stdev = sd(r)
    b = input$ngen
    hasil = matrix(nrow=n, ncol=b)
    for (i in 1:b) {
      hasil[,i] = rnorm(n, mu, stdev) %>% sort(.)
    }
    colnames(hasil) = paste0('s', 1:b) #simulaso
    rownames(hasil) = paste0('h', 1:n) #hari
    hasil
  })
  
  output$text2 <- renderPrint({
    r = na.omit(retur()$Return)
    n = length(r)
    mu = mean(r)
    stdev = sd(r)
    cat("membangkitkan data berdistribusi normal dengan mean =", mu, "dan standar deviasi =", stdev)
  })
  output$table3 <- renderDataTable(
    sim()
  )
  
  output$text3 <- renderPrint({
    loc = ceiling((1-input$cl/100)*nrow(sim()))
    cat("nilai percentil untuk confidence level", input$cl, 
        "% berada pada data ke -", loc)
  })
  output$table4 <- renderDataTable({
    sim = sim()
    name = colnames(sim)
    loc = ceiling((1-input$cl/100)*nrow(sim))
    Percentil = as.numeric(sim[loc,])
    VaR = vector()
    VaR.P = vector()
    for (i in (1:length(Percentil))) {
      x = -(Percentil[i]*sqrt(input$hp))
      y = x*input$iv
      VaR.P[i] = x
      VaR[i] = y
    }
    VaR = data.frame(cbind(Percentil, VaR, Var.P = round(VaR.P,3)))
    colnames(VaR) = c("Nilai Percentil", "Value At Risk", "Persentase VaR")
    rownames(VaR) = name
    VaR
  })
  
  
  
  output$text4 <- renderPrint({
    sim = sim()
    loc = ceiling((1-input$cl/100)*nrow(sim))
    Percentil = as.numeric(sim[loc,])
    VaR = vector()
    for (i in (1:length(Percentil))) {
      x = -(Percentil[i]*sqrt(input$hp))
      VaR[i] = x
    }
    
    cat("Dari hasil simulasi, didapatkan rata-rata nilai VaR untuk confidence level", input$cl,
        "%\ndan holding period", input$hp, "hari adalah", mean(VaR)*100, "%\n\n")
    cat("Artinya, kerugian maksimum yang diharapkan dari investasi sebesar Rp", input$iv,
        "\ntidak akan melebihi Rp", mean(VaR)*input$iv, "\n\n")
  })
  
  observeEvent(input$smc,{
    if (input$smc>0){nav_select("nav", selected = "Monte Carlo")}
  })
  
  output$pg4 <- renderUI(
    if(is.null(input$saham)) {return()}
    else {
      navset_card_tab(title = "Simulasi Monte Carlo",
                      nav_panel("Simulasi", verbatimTextOutput("text2"), dataTableOutput('table3')),
                      nav_panel("Percentile", verbatimTextOutput("text3"), dataTableOutput('table4')),
                      nav_panel("Value At Risk", verbatimTextOutput("text4"))
      )
    }
  )
  
  # ============================================================================
  # Additional UI
  
  output$pilih <- renderUI(
    if(is.null(input$saham)) {}
    else {
      tagList(
        accordion_panel(
          "Normalitas Return", icon = bsicons::bs_icon("sliders"),
          selectInput(inputId = "R", label = "Select variabel for return", choices = opts(), selected = opts()[5]),
          sliderInput(inputId = "cl", label = "Confidence Level:", 0, 100, value = 95),
          actionButton(inputId = "nr", "Uji Normalitas")
        ),
        accordion_panel(
          "Simulasi", icon = bsicons::bs_icon("sliders"),
          numericInput("ngen", "Banyaknya simulasi:", value = 100),
          hr(),
          numericInput("hp", "Holding period (hari):", 1),
          numericInput("iv", "Investasi awal (rupiah):", 1000000),
          actionButton(inputId = "smc", "Jalankan Simulasi")
        )
      )
      
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

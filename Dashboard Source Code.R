packages <- c('readxl','shiny','shinydashboard','tidyverse','reshape2','plotly','sf','cowplot')
install.packages(setdiff(packages, rownames(installed.packages())))
library(readxl)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(reshape2)
library(plotly)
library(sf)
library(cowplot)

#Load
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
fpath <- 'data/Dashboard Shiny R.xlsx'
xlsheet <- excel_sheets(fpath)
list.nama.kolom <- c('Unit Pelaksana','Daya Terpasang (KW)', 'Produksi Listrik (MWh)','Listrik Terjual (MWh)', 'Pemakaian Sendiri (MWh)','Listrik Susut (MWh)', 'Persentase Susut terhadap Produksi', 'Jumlah Pelanggan','Tahun')
listrik.2019 <- read_excel(fpath, sheet = xlsheet[1])
listrik.2019 <- listrik.2019[,1:8]
listrik.2019 <- cbind(listrik.2019, rep(2019, 16))

listrik.2020 <- read_excel(fpath, sheet = xlsheet[2])
listrik.2020 <- cbind(listrik.2020, rep(2020, 16))

listrik.2021 <- read_excel(fpath, sheet = xlsheet[3])
listrik.2021 <- cbind(listrik.2021, rep(2021, 16))

colnames(listrik.2019) <- list.nama.kolom
colnames(listrik.2020) <- list.nama.kolom
colnames(listrik.2021) <- list.nama.kolom

listrik.2019 <- listrik.2019[c(9,1:8)]
listrik.2020 <- listrik.2020[c(9,1:8)]
listrik.2021 <- listrik.2021[c(9,1:8)]

gabungan.listrik <- bind_rows(listrik.2019, listrik.2020, listrik.2021)

#Map
indonesia <- st_read("shp/gadm36_IDN_2.shp")
jatim <- subset(indonesia, indonesia$NAME_1 == 'Jawa Timur')
colCode <- c('#00d4ff','#01bdf0','#029edb',rep('#0475c0',5),rep('#0461b4',5),rep('#044da6',5),rep('#043b9b',5),rep('#06288e',5),rep('#071b85',5),rep('#090979',5))

air <- read_excel(fpath, sheet = xlsheet[5])
air <- air[,-3]
air[c(nrow(air) - 1,nrow(air)),1] <- c('Surabaya','Batu')
colnames(air) <- c('Kabupaten/Kota','Jumlah Pelanggan','Air Disalurkan (m3)','Nilai (Rp)')
air <- air[order(air$`Kabupaten/Kota`),]
pelanggan <- read_excel(fpath, sheet = xlsheet[4])
pelanggan.asli <- pelanggan
names(pelanggan.asli)[1] <- 'Unit Pelaksana Pelayanan Pelanggan'

#Reshape Data biar rapi :)
pelanggan.melt <- as_tibble(melt(pelanggan))
pelanggan <- dcast(pelanggan.melt, variable ~ `Unit Pelaksana Pelayanan Pelanggan Customer Service Implementation Unit`)
pelanggan.melt <- melt(pelanggan, id.vars = 'variable')

#Hitung Pertumbuhan
calcGrowth <- function(df, numYear){
  for(i in 1:(nrow(df)-1))
  {
    df[i+1,4] <- (df[i+1,3] - df[i,3])/df[i,3] * 100
    df[i+1,5] <- df[i+1,3] - df[i,3]
  }
  df[seq(from = 1, to = nrow(df), by = numYear),4] <- NA
  df[seq(from = 1, to = nrow(df), by = numYear),5] <- NA
  return(df)
}

pelanggan.melt <- calcGrowth(pelanggan.melt,5)
colnames(pelanggan.melt) <- c('Tahun','Unit Pelaksana','value','Pertumbuhan (%)','Pertumbuhan')

#Shiny
headPage <- dashboardHeader(title = "Dashboard Pertambangan dan Energi Provinsi Jawa Timur 2022")

sidePage <- dashboardSidebar(
  sidebarMenu(
    menuItem("Beranda", tabName = 'beranda', icon = icon("home")),
    menuItem("Tim Penyusun", tabName = 'penyusun', icon = icon("users")),
    menuItem("Penjelasan Teknis", tabName = 'penjelasan', icon = icon("cogs")),
    menuItem("Listrik", tabName = 'listrik', icon = icon("bolt"),
             menuSubItem('Ulasan', tabName = 'ulasanlis'),
             menuSubItem('Data', tabName = 'datalistrik'),
             menuSubItem('Grafik', tabName = 'grafiklistrik')),
    menuItem('Air', tabName = 'air', icon = icon("water"),
             menuSubItem('Ulasan', tabName = 'ulasanair'),
             menuSubItem('Data', tabName = 'dataair'),
             menuSubItem('Grafik', tabName = 'grafikair'))
  )
)
bodyPage <- dashboardBody(
  fluidPage(
    tabItems(
      tabItem(tabName = 'penyusun',
              column(width=6,
                     box(
                       title = strong("Anggota 1"), width = NULL, solidHeader = TRUE,style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                       div(imageOutput("foto1"),style="text-align: center;",style = "height:205px;"),
                       p("Nama     = Fadhil Aryo Nugroho"),
                       p("Nrp      = 5003201068"),
                       p("Email    = fadhilaryo11@gmail.com"),
                       p("LinkedIn = https://www.linkedin.com/in/fadhil-aryo-nugroho-984554212")
                     )),
              column(width=6,
                     box(
                       title = strong("Anggota 2"), width = NULL, solidHeader = TRUE,style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                       div(imageOutput("foto2"),style="text-align: center;",style = "height:205px;"),
                       p("Nama     = Akbar Fikriawan"),
                       p("Nrp      = 5003201064"),
                       p("Email    = fikriawanakbar@gmail.com"),
                       p("LinkedIn = https://www.linkedin.com/in/akbar-fikriawan-129830241")
                     ))),
      tabItem(tabName = 'datalistrik',
              box(width = 12,
                  solidHeader = T,
                  background = NULL,
                  selectInput('listrikPelanggan', label = 'Pilih data :', choices = c('Data Listrik','Data Jumlah Pelanggan')),
                  uiOutput('pilihtahunListrik'),
                  dataTableOutput('tabel.listrik')
                  )
              ),
      tabItem(tabName = 'dataair',
              box(
                width = 12,
                solidHeader = T,
                background = NULL,
                dataTableOutput('tabel.air')
                )
              ),
      tabItem(tabName = 'penjelasan',
              column(width = 12,
                     box(
                       title = strong("PENJELASAN TEKNIS"), width = NULL, solidHeader = TRUE,style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                       p("1. Produksi Listrik adalah produksi kotor, yaitu termasuk konsumsi yang dipakai stasiun pembantu dan hilang dalam perjalanan/ transformer dianggap sebagian dari stasiun."),
                       p("2. Listrik umum adalah listrik yang dihasilkan untuk tujuan dijual dengan memproduksi, mentransmisikan dan mendistribusikan energi listrik. Ini dilaksanakan oleh perusahaan swasta, koperasi, pemerintah daerah/desa, dan pemerintah pusat."),
                       p("3. Listrik yang diproduksi dan digunakan sendiri adalah listrik yang diproduksi untuk memenuhi kebutuhan sendiri. Misalnya, rumah tangga atau perusahaan industri yang memproduksi listrik yang digunakan untuk keperluan rumah tangga atau perusahaan tersebut. Penggunaan pada stasiun pembangkit dan yang hilang termasuk konsumsi oleh stasiun pembantu dan hilang dalam perjalanan dianggap sebagai bagian dari pembangkit energi listrik."),
                       p("4. Pelanggan adalah individu atau kelompok, baik rumah tangga, perusahaan atau institusi non profit yang membeli barang/jasa."),
                       p("5. Air disalurkan adalah  volume air bersih dari perusahaan air bersih yang disalurkan kepada pelanggan")
                     ),
                     box(
                       title = strong("TECHNICAL NOTES"), width = NULL, solidHeader = TRUE,style="text-align: italic;",style = "font-family: 'times'; font-si16pt",
                       p("1. Electricity Production is gross  production, which includes consumption that is used by auxiliary stations and is lost in the trip / transformer is considered to be part of the station."),
                       p("2. General electricity is electricity produced for the purpose of being sold by producing, transmitting and distributing electrical energy. This is carried out by private companies, cooperatives, regional / village governments, and the central government."),
                       p("3. Electricity produced and used alone is electricity produced to meet their own needs. For example, a household or industrial company that produces electricity used for household or company needs. Usage at the generating station and lost including consumption by the auxiliary station and lost on the way are considered as part of the electricity generation."),
                       p("4. Customers are individuals or groups, both households, companies or non-profit institutions that buy goods / services."),
                       p("5. Piped water is the volume of clean water from a clean water company that is distributed to customers")
                     )
              )
      ),
      tabItem(tabName = 'beranda',
              h2("Pendahuluan"),
              column(width = 12,
                     tabsetPanel(
                       tabPanel("Latar Belakang",
                                box(
                                  title = strong("Latar Belakang"), width = NULL, solidHeader = TRUE,style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                                  p("Data   kependudukan   merupakan   salah   satu informasi yang dibutuhkan untuk perencanaan    pembangunan    berkelanjutan.  Pelaksanaan  Sistem  Informasi  Administrasi Kependudukan    di    seluruh    tanah    air,    tidak    saja mempermudah  pembuatan  data  kependudukan  secara cepat  dan  akurat.  Data-data  yang  terangkum  dalam Sistem Informasi Administrasi Kependudukan ini akan dimutakhirkan dan ditertibkan Nomor Induk Kependudukannya  oleh  Pemerintah  Pusat  dan  segera dicetak  Kartu  Tanda  Penduduk  Berbasis  Nomor  Induk Kependudukan."),
                                  p("Database Kependudukan yang mutakhir dan akurat akan  sangat  mendukung  dalam  perencanaan  kegiatan pemerintahan,    pembangunan    dan    kemasyarakatan dimaksud.    Pemutakhiran    Database    kependudukan tersebut dilakukan agar hasil pemutakhiran benar-benar dapat dimanfaatkan secara optimal."),
                                  p("Secara    umum,    data    kependudukan    tersebut digunakan untuk berbagai keperluan, yang diantaranya : (1) Pelayanan publik antara lain  untuk penerbitan surat izin  mengemudi,  izin  usaha,  pelayanan  wajib  pajak, pelayanan  perbankan,  pelayanan  penerbitan  sertifikat tanah,  asuransi,  jaminan  kesehatan  masyarakat,  dan jaminan     sosial     tenaga     kerja.     (2)     Perencanaan pembangunan  yakni  untuk  perencanaan  pembangunan nasional, perencanaan pendidikan, perencanaan kesehatan,  perencanaan  tenaga  kerja,  dan  pengentasan masyarakat   dari   kemiskinan.   (3)   Alokasi   anggaran meliputi  penentuan  Dana  Alokasi  Umum  (DAU)  dan perhitungan   potensi   perpajakan.   (4)   Pembangunan demokrasi yaitu penyiapan Data Agregat Kependudukan per  kecamatan  (DAK2)  dan  penyiapan  data  Penduduk Potensial Pemilih Pemilu (DP4). (5) Penegakan hukum dan pencegahan kriminal antara lain untuk memudahkan pelacakan   pelaku   kriminal,   mencegah   perdagangan orang dan mencegah pengiriman tenaga kerja illegal."),
                                  p("Pemanfaatan    data  ini  untuk  mendorong    semua SKPD    (Satuan   Kerja   Perangkat   Daerah)   untuk menggunakan pendekatan kebijakan satu data (one data policy)  di  mana  data    itu  berasal  dari  Dinas  Dukcapil. Tujuan    dari    pemanfaatan    data    ini    diantaranya pemanfaatan  data  untuk  sekolah,  mengurus  perijinan data,  mengurusbantuan  sosial  semuanya  harus  sama dengan   sumber   data   yang   ada   di   Dinas   Dukcapil sehingga  tidak  ada  lagi  orang  yang  memiliki  identitas yang berbeda-beda."),
                                  p("Informasi  dan  publikasi  terkait  dengan  e-KTP, NIK, database kependudukan dan aspek terkait lainnyasudah    banyak    ditemukan,    namun    terkait    dengan pemanfaatan   database   kependudukan   di   pemerintah kabupaten/kota  (Pemkab/Pemkot)  masih  minim. Jurnal ini  mengungkap  bagaimana  proses  untuk  memperoleh akses dan pemanfaan data kependudukan di lingkungan kabupaten/kota.")
                                )),
                       tabPanel("Tujuan",
                                box(
                                  title = strong("Tujuan"), width = NULL, solidHeader = TRUE,style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                                  "Adapun tujuan dari penelitian ini adalah untuk mengetahui kondisi sektor Pertambangan dan Energi tiap Kabupaten/Kota Provinsi Jawa Timur Tahun 2022. Dari mulai hal detail dalam listrik seperti daya terpasang, produksi listrik, listrik terjual, listrik susut, dan persentase susut terhadap jumlah produksi, sampai dengan perbandingan jumlah pelanggan tiap tahunnya dari tiap-tiap upt /kota/kabupaten dalam energi air dan listrik."
                                )),
                       tabPanel("Manfaat",
                                box(
                                  title = strong("Manfaat"), width = NULL, solidHeader = TRUE,style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                                  p("Adapun manfaat dari penelitian ini adalah sebagai berikut."),
                                  p("1.	Sebagai penyelia informasi kondisi sektor Pertambangan dan Energi di Provinsi Jawa Timur 2022."),
                                  p("2.	Sebagai media untuk melakukan monitoring terhadap perkembangan sumber daya dalam sektor Pertambangan dan Energi di Provinsi Jawa Timur 2022."),
                                  p("3.	Sebagai dasar pengambilan keputusan bagi pemerintah atau pihak-pihak terkait dalam hal Pertambangan dan Energi di Provinsi Jawa Timur 2022.")
                                ))
                     )
              )
      ),
      tabItem(tabName = 'ulasanlis',
              h2("Overview"),
              infoBoxOutput('customer.listrik'),
              infoBoxOutput('hasil.listrik'),
              infoBoxOutput('jual.listrik'),
              column(width = 12,
                     box(
                       title = strong("ULASAN"), width = NULL, solidHeader = TRUE,style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                       "Pada tahun 2021 produksi listrik yang dihasilkan oleh PT. PLN di Provinsi Jawa Timur sejumlah 39.610.232 MWh. Dari total produksi tersebut berhasil dijual   sebanyak   37.613.562   MWh. Penjualan terbanyak berasal dari UP3 Mojokerto   yang   berhasil   menjual sebanyak 4.197.359 MWh, disusul UP3 Surabaya  Selatan  dengan  penjualan sebesar  4.018.256  MWh  kemudian UP3   Pasuruan   dengan   penjualan sebesar  4.001.546  MWh.  Sedangkan jumlah pelanggan PT. PLN pada tahun 2021  di  Provinsi  Jawa Timur  adalah sebanyak 12.431.589 pelanggan."
                     ),
                     box(
                       title = strong("DESCRIPTION"), width = NULL, solidHeader = TRUE,style="text-align: italic;",style = "font-family: 'times'; font-si16pt",
                       "In   2021   electricity   production produced  by  PT.  PLN  in  Jawa  Timur Province  is  39,610,232  MWh.  Of  the total   production,   37,613,562   MWh were sold. The most sales came from UP3  Mojokerto,  which  managed  to sell  4,197,359  MWh,  followed  by  UP3 South Surabaya with sales of 4,018,256 MWh  then  UP3  Pasuruan  with  sales of 4,001,546 MWh. While the number of  customers  of  PT.  PLN  in  2021  in Jawa Timur Province were 12,431,589 customers."
                     )
              )
      ),
      tabItem(tabName = 'ulasanair',
              h2("Overview"),
              infoBoxOutput('customer.air'),
              infoBoxOutput('hasil.air'),
              infoBoxOutput('nilai.air'),
              column(width = 12,
                     box(
                       title = strong("ULASAN"), width = NULL, solidHeader = TRUE,style="text-align: justify;",style = "font-family: 'times'; font-si16pt",
                       "Pada   tahun   2020   di   Provinsi Jawa     Timur     tercatat     sebanyak 2.147.830   pelanggan   PDAM   yang tersebar  di  seluruh  Kabupaten/Kota. Seluruh PDAM di Provinsi Jawa Timur menghasilkan 732.523.664 M2 dengan total  nilai  sebesar  3.002.203.242.922 rupiah."
                     ),
                     box(
                       title = strong("DESCRIPTION"), width = NULL, solidHeader = TRUE,style="text-align: italic;",style = "font-family: 'times'; font-si16pt",
                       "In  2020  in  Jawa  Timur  Province there were 2,147,830 PDAM customers spread across all Regencies / Cities. All PDAMs in Jawa Timur Province produce 732,523,664  M2   with  a  total  value  of 3,002,203,242,922 rupiah."
                     )
              )
      ),
      tabItem(tabName = 'grafiklistrik',
              fluidRow(
                tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),
                column(width = 12,
                       box(width = 12,
                           solidHeader = T,
                           background = NULL,
                           box(width = 12,
                               collapsible = TRUE,
                               collapsed = TRUE,
                               solidHeader = T,
                               background = NULL,
                               title = "Bar Chart",
                               print("Bar Chart menggambarkan persebaran data variabel listrik numerik yang dapat diatur berdasarkan select input variabel yang dipilih. Adapun terdapat select input tahun yang mana data variabel yang telah dipilih akan diambil dari tahun yang telah dimasukkan. Selain itu, terdapat dua buah radio button tertinggi dan terendah yang mana berfungsi untuk mengurutkan bar chart sesuai dengan urutan yantg telah diinputkan.")
                               ),
                           box(
                             width = 10,
                             solidHeader = T,
                             background = NULL,
                             plotOutput('vertBar1')
                             ),
                           box(
                             width = 2,
                             solidHeader = T,
                             background = NULL,
                             selectInput('tahunListrik2', label = 'Tahun :', choices = c(2019,2020,2021), selected = 2021),
                             selectInput('upListrik', label = 'Data : ', choices = colnames(listrik.2019[,c(3:9)])),
                             radioButtons('sortAscDesc', label = 'Urutkan berdasarkan :', choices = c('Tertinggi', 'Terendah'))
                             )
                       )
                ),
                column(width = 12,
                       box(width = 12,
                           solidHeader = T,
                           background = NULL,
                           box(width = 12,
                               collapsible = TRUE,
                               collapsed = TRUE,
                               solidHeader = T,
                               background = NULL,
                               title = "Scatter Plot",
                               print("Scatter Plot menggambarkan pola hubungan dua buah variabel listrik numerik yang dapat diatur berdasarkan select input variabel yang dipilih. Adapun terdapat check box untuk dapat dipilih apabila diinginkan terdapat garis regresi. Selain itu dibawah header plot terdapat info box deskripsi mengenai nilai koefisien korelasi yang terbentuk pada scatter plot.")
                           ),
                           box(
                             width = 10,
                             solidHeader = T,
                             background = NULL,
                             plotOutput('scatter1')
                           ),
                           box(
                             width = 2,
                             solidHeader = T,
                             background = NULL,
                             selectInput('xaxisListrik', label = 'Sumbu X :', choices = colnames(listrik.2019[,c(3:9)]), selected = colnames(listrik.2019)[3]),
                             selectInput('yaxisListrik', label = 'Sumbu Y :', choices = colnames(listrik.2019[,c(3:9)]), selected = colnames(listrik.2019)[4]),
                             selectInput('tahunListrik3', label = 'Tahun :', choices = c(2019,2020,2021), selected = 2021),
                             checkboxInput('regLine1', label = 'Garis regresi', value = F)
                           )
                        )
                       ),
                column(width = 12,
                       solidHeader = T,
                       background = NULL,
                       box(width = 12,
                           solidHeader = T,
                           background = NULL,
                           box(width = 12,
                               collapsible = TRUE,
                               collapsed = TRUE,
                               solidHeader = T,
                               background = NULL,
                               title = "Line Chart",
                               print("Line Chart menggambarkan pola kenaikan atau penurunan data seiring waktu mulai 2018 sampai dengan 2021 dari variabel jumlah pelanggan oleh daftar check box Unit Pelaksana Pelayanan Pelanggan (UP3) yang dipilih. Selain itu terdapat check box lain yakni pertumbuhan yang apabila di pilih maka akan terdapat sebuah barchart jumlah pertumbuhan/penurunan pelanggan dibandingkan tahun sebelumnya dalam UP3 yang telah dipilih awal. Selain itu apabila check box baru mengenai persentase dipilih, akan terdapat sebuah bar chart persentase pertumbuhan/penurunan pelanggan dibandingkan tahun sebelumnya dalam UP3 yang telah dipilih awal.")
                           ),
                           box(
                             width = 10,
                             solidHeader = T,
                             background = NULL,
                             plotOutput('line1')
                           ),
                           box(
                             width = 2,
                             solidHeader = T,
                             background = NULL,
                             selectInput('dataListrik2', label = 'Pilih data :', choices = colnames(gabungan.listrik[,3:9])),
                             checkboxInput('growthRate', label = 'Pertumbuhan', value = F),
                             uiOutput('numGrowthCheck'),
                             uiOutput('labelCheckBox'),
                             checkboxGroupInput('cboxg1', label = 'Pilih unit pelaksana :', choiceNames = as.matrix(listrik.2019[,2]), choiceValues = as.matrix(listrik.2019[,2]))
                           )
                          )
                      )
                )
              ),
      tabItem(tabName = 'grafikair',
              fluidRow(
                tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),
                column(
                  width = 12,
                  box(
                    width = 12,
                    solidHeader = T,
                    background = NULL,
                    box(width = 12,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      solidHeader = T,
                      background = NULL,
                      title = "Bar Chart",
                      print("Bar Chart menggambarkan persebaran data variabel air numerik yang dapat diatur berdasarkan select input variabel yang dipilih. Selain itu, terdapat dua buah radio button tertinggi dan terendah yang mana berfungsi untuk mengurutkan bar chart sesuai dengan urutan yantg telah diinputkan.")
                    ),
                    box(
                      width = 12,
                      solidHeader = T,
                      background = NULL,
                      selectInput('varAir', label = 'Data :', choices = colnames(air[,2:4])),
                      radioButtons('sortAscDesc2', label = 'Urutkan berdasarkan :', choices = c('Tertinggi', 'Terendah')),
                      plotlyOutput('barchartair')
                    )
                  )
                ),
                column(width = 12,
                       box(
                         width = 12,
                         solidHeader = T,
                         background = NULL,
                         box(width = 12,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             solidHeader = T,
                             background = NULL,
                             title = "Scatter Plot",
                             print("Scatter Plot menggambarkan pola hubungan dua buah variabel listrik numerik yang dapat diatur berdasarkan select input variabel yang dipilih. Adapun terdapat check box untuk dapat dipilih apabila diinginkan terdapat garis regresi. Selain itu dibawah header plot terdapat info box deskripsi mengenai nilai koefisien korelasi yang terbentuk pada scatter plot.")
                         ),
                         box(
                           width = 10,
                           solidHeader = T,
                           background = NULL,
                           plotOutput('scatterair')
                         ),
                         box(
                           width = 2,
                           solidHeader = T,
                           background = NULL,
                           selectInput('xaxisAir', label = 'Variabel X :', choices = colnames(air[,2:4])),
                           selectInput('yaxisAir', label = 'Variabel Y :', choices = colnames(air[,2:4]), selected = colnames(air[,3])),
                           checkboxInput('regLine2', label = 'Garis regresi', value = F)
                         )
                       )
                ),
                column(width = 12,
                       box(
                         width = 12,
                         solidHeader = T,
                         background = NULL,
                         box(width = 12,
                             collapsible = TRUE,
                             collapsed = TRUE,
                             solidHeader = T,
                             background = NULL,
                             title = "Map Chart",
                             print("Maps Chart menggambarkan persebaran variabel yang dipilih berdasarkan input variabel numerik dari Kota/Provinsi di Jawa Timur dari data air. Perbedaaan persebaran di tiap wilayah digambarkan dengan perbedaan warna yakni dengan semakin terang warna maka menandakan semakin tinggi juga nilai variabel numerik dari variabel yang telah dipilih tadi pada wilayah tersebut. Begitu juga sebaliknya, apabila warna pada sebuah wilayah semakin gelap, maka nilai variabel numerik yang telah dipilih tadi bernilai lebih kecil.")
                         ),
                         box(
                           width = 10,
                           solidHeader = T,
                           background = NULL,
                           plotOutput('mapAir')
                         ),
                         box(
                           width = 2,
                           solidHeader = T,
                           background = NULL,
                           selectInput('dataAir', label = 'Data :', choices = colnames(air[,2:4]))
                         )
                        )
                       )
                )
      )
    )
  )
)

ui <- dashboardPage(headPage, sidePage, bodyPage)

server <- function(input, output, session) {

  observeEvent(input$linkAkbar, {
    a("test", href="https://www.linkedin.com/in/akbar-fikriawan-129830241/", target="_blank")
  })

  map.data.air <- reactive({
    jatim <- cbind(jatim, air[,input$dataAir])
    colnames(jatim)[14] <- 'Value'
    jatim <- jatim[order(desc(jatim$Value)),]
    jatim <- cbind(jatim, colCode)
    return(jatim)
  })

  pelanggan.melt.data <- reactive({
    if(input$dataListrik2 == 'Jumlah Pelanggan')
    {
      pelanggan.melt <- pelanggan.melt[pelanggan.melt$`Unit Pelaksana` %in% input$cboxg1, ]
      return(pelanggan.melt)
    }
    else
    {
      m.gab.listrik <- gabungan.listrik[,c('Tahun','Unit Pelaksana',input$dataListrik2)] %>%
        dcast(Tahun ~ `Unit Pelaksana`)
      m.gab.listrik[,1] <- as.factor(m.gab.listrik[,1])
      m.gab.listrik <- m.gab.listrik %>% melt(id.vars = 'Tahun') %>% calcGrowth(3)
      colnames(m.gab.listrik) <- c('Tahun','Unit Pelaksana','value','Pertumbuhan (%)','Pertumbuhan')
      m.gab.listrik <- m.gab.listrik[m.gab.listrik$`Unit Pelaksana` %in% input$cboxg1, ]
      return(m.gab.listrik)
    }
  })

  output$numGrowthCheck <- renderUI(expr = if(input$growthRate == T) {
    checkboxInput('growthRatePercentage', label = 'Persentase', value = T)
  } else {NULL})

  output$labelCheckBox <- renderUI(expr = if(input$growthRate == T) {
    checkboxInput('labelCheck2', label = 'Label', value = T)
  } else {NULL})

  output$pilihtahunListrik <- renderUI(expr = if(input$listrikPelanggan == 'Data Listrik') {
    selectInput('tahunListrik', label = 'Tahun : ', choices = c('2019','2020','2021'))
  } else {NULL})

  tahun.listrik.plot1 <- reactive({
    if(input$tahunListrik2 == 2019)
    {
      return(listrik.2019)
    }
    else if(input$tahunListrik2 == 2020)
    {
      return(listrik.2020)
    }
    else
    {
      return(listrik.2021)
    }
  })

  tahun.listrik.plot2 <- reactive({
    if(input$tahunListrik3 == 2019)
    {
      return(listrik.2019)
    }
    else if(input$tahunListrik3 == 2020)
    {
      return(listrik.2020)
    }
    else if(input$tahunListrik3 == 2021)
    {
      return(listrik.2021)
    }
  })

  data.column.listrik <- reactive({
    return(data.frame(tahun.listrik.plot1() %>% select(2), tahun.listrik.plot1() %>% select(input$upListrik)))
  })

  data.scatter.listrik <- reactive(return(data.frame(tahun.listrik.plot2() %>% select(input$xaxisListrik), tahun.listrik.plot2() %>% select(input$yaxisListrik))))

  output$tabel.listrik <- renderDataTable({
    if(input$listrikPelanggan == "Data Jumlah Pelanggan") pelanggan.asli
    else if(input$tahunListrik == '2019') listrik.2019[,-c(1,9)]
    else if(input$tahunListrik == '2020') listrik.2020[,-c(1,9)]
    else listrik.2021[,-c(1,9)]
  })

  output$tabel.air <- renderDataTable({
    air
  })

  output$customer.listrik <- renderInfoBox({
    infoBox(
      "Jumlah Pelanggan Listrik", sum(pelanggan.melt %>% filter(Tahun == 2020) %>% select(3)), icon = icon("user"),
      color = "yellow", fill = TRUE,
      p("orang")
    )
  })

  output$customer.air <- renderInfoBox({
    infoBox(
      "Jumlah Pelanggan Air", sum(air[,2]), icon = icon("user"),
      color = "purple", fill = TRUE,
      p("orang")
    )
  })

  output$hasil.listrik <- renderInfoBox({
    infoBox(
      "Produksi Listrik", sum(listrik.2020[,3]), icon = icon("bolt"),
      color = "yellow", fill = TRUE,
      p("MWh")
    )
  })

  output$jual.listrik <- renderInfoBox({
    infoBox(
      "Listrik Terjual", sum(listrik.2020[,4]), icon = icon("car-battery"),
      color = "yellow", fill = TRUE,
      p("MWh")
    )
  })

  output$hasil.air <- renderInfoBox({
    infoBox(
      "Air Disalurkan", sum(air[,4]), icon = icon("water"),
      color = "purple", fill = TRUE,
      p("M kubik")
    )
  })

  output$nilai.air <- renderInfoBox({
    infoBox(
      "Nilai", sum(air[,4]), icon = icon("dollar"),
      color = "purple", fill = TRUE,
      p("Rupiah")
    )
  })

  output$vertBar1 <- renderPlot({
    {
      if(input$sortAscDesc == 'Tertinggi')
      {
        plot1 <- ggplot(data.column.listrik()) +
          aes(
            x = reorder(data.column.listrik()[,1], -data.column.listrik()[,2]),
            y = data.column.listrik()[,2],
            fill = data.column.listrik()[,1]) +
          geom_col()
      }
      else
      {
        plot1 <- ggplot(data.column.listrik()) +
          aes(
            x = reorder(data.column.listrik()[,1], data.column.listrik()[,2]),
            y = data.column.listrik()[,2],
            fill = data.column.listrik()[,1]) +
          geom_col()
      }

      plot1 + geom_text(label = data.column.listrik()[,2],
                        position = position_dodge(width = 0.9),
                        vjust = -0.5,
                        check_overlap = TRUE) +
        labs(title = paste0('Grafik ', input$upListrik, ' di tahun ', input$tahunListrik2,'.'),
             x = NULL,
             y = input$upListrik) +
        theme_cowplot() +
        ylim(0, (max(data.column.listrik()[,2]) * 1.02)) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = 'none')
    }
  })

  output$scatter1 <- renderPlot({
    plot2 <- ggplot(data.scatter.listrik()) +
      aes(
        x = data.scatter.listrik()[,1],
        y = data.scatter.listrik()[,2]) +
      geom_point(size = 5) +
      labs(
        title = paste0(input$xaxisListrik, ' vs ', input$yaxisListrik, ' di tahun ', input$tahunListrik3,'.'),
        x = input$xaxisListrik,
        y = input$yaxisListrik
      ) +
      theme_cowplot() +
      annotate(geom = 'text', label = paste0('Korelasi : ', round(cor(x = data.scatter.listrik()[,1], y = data.scatter.listrik()[,2]), digits = 3)), x = -Inf, y = Inf, hjust = 0, vjust = 1)

    if(input$regLine1 == TRUE) plot2 + geom_smooth(method = MASS::rlm, se = F, size = 1.25)
    else plot2
  })

  output$line1 <- renderPlot(height = 550,{
    if(input$growthRate == FALSE)
    {
      plot3 <- ggplot(pelanggan.melt.data()) +
        aes(
          x = Tahun,
          y = value,
          group = `Unit Pelaksana`,
          colour = `Unit Pelaksana`
        ) +
        labs(title = paste0('Jumlah ', input$dataListrik2, ' tiap tahun.'),
             x = 'Tahun',
             y = input$dataListrik2) +
        geom_line(size = 1.2) +
        theme_cowplot()
      plot3
    }
    else
    {
      if(input$growthRatePercentage == T)
      {
        plot3 <- pelanggan.melt.data() %>%
          filter(if(input$dataListrik2 == 'Jumlah Pelanggan'){.[,1] != 2017}
                 else {.[,1] != 2019}) %>%
          ggplot(aes(x = Tahun,
                     y = `Pertumbuhan (%)`,
                     group = `Unit Pelaksana`,
                     fill = `Unit Pelaksana`
          )) +
          labs(title = paste0('Jumlah ', input$dataListrik2, ' tiap tahun.'),
               x = 'Tahun',
               y = 'Pertumbuhan (%)') +
          geom_col(position = position_dodge(width = 0.95)) +
          theme_cowplot()

        if(input$labelCheck2 == T)
        {
          plot3 +
            geom_text(label = paste0(round(na.omit(pelanggan.melt.data()[,4]),2),'%'),
                      position = position_dodge(width = 0.95),
                      vjust = -0.5,
                      check_overlap = TRUE)
        }
        else
        {
          plot3
        }
      }
      else
      {
        plot3 <- pelanggan.melt.data() %>%
          filter(if(input$dataListrik2 == 'Jumlah Pelanggan'){.[,1] != 2017}
                 else {.[,1] != 2019}) %>%
          ggplot(aes(
            x = Tahun,
            y = Pertumbuhan,
            group = `Unit Pelaksana`,
            fill = `Unit Pelaksana`
          )) +
          labs(title = paste0('Pertumbuhan ', input$dataListrik2,' tiap tahun.'),
               x = 'Tahun',
               y = 'Pertumbuhan') +
          geom_col(position = position_dodge(width = 0.95)) +
          theme_cowplot()

        if(input$labelCheck2 == T)
        {
          plot3 +
            geom_text(label = na.omit(pelanggan.melt.data()[,5]),
                      position = position_dodge(width = 0.95),
                      vjust = -0.5,
                      check_overlap = TRUE)
        }
        else
        {
          plot3
        }
      }
    }
  })

  output$scatterair <- renderPlot({
    data.scatter.air <- data.frame(air %>% select(input$xaxisAir), air %>% select(input$yaxisAir))
    plot5 <- data.scatter.air %>% ggplot(aes(x = .[,1], y = .[,2])) +
      geom_point(size = 2) +
      labs(title = paste0("Scatter Plot dari ", input$xaxisAir, ' dan ', input$yaxisAir),
           x = input$xaxisAir,
           y = input$yaxisAir) +
      theme_cowplot() +
      annotate(geom = 'text', label = paste0('Korelasi : ', round(cor(x = data.scatter.air[,1], y = data.scatter.air[,2]), digits = 3)), x = -Inf, y = Inf, hjust = 0, vjust = 1)

    if(input$regLine2 == T) plot5 + geom_smooth(method = MASS::rlm, se = F, size = 1.25)
    else plot5
  })

  output$barchartair <- renderPlotly({
    if(input$sortAscDesc2 == 'Tertinggi')
    {
      data.column.air() %>% plot_ly(x = reorder(.[,1], -.[,2]), y = .[,2], type = 'bar', mode = 'markers')
    }
    else
    {
      data.column.air() %>% plot_ly(x = reorder(.[,1], .[,2]), y = .[,2], type = 'bar', mode = 'markers')
    }
  })

  data.column.air <- reactive({
    return(data.frame(air %>% select(1), air %>% select(input$varAir)))
  })

  output$mapAir <- renderPlot({ map.data.air() %>%
      ggplot() +
      geom_sf(fill = colCode) +
      theme_void() +
      labs(title = 'Persebaran Data di Jawa Timur') +
      annotate(geom = "text", label = paste0("Semakin terang warna menandakan tingginya ", tolower(input$dataAir), " di daerah tersebut."), x = -Inf, y = Inf, hjust = 0, vjust = 1) #+
      #theme(aspect.ratio = 9/16)
  }, width = 1000)

  output[["foto1"]] <- renderImage({
    list(src = "foto/fadhil.jpg", height = 200, width = 150)
  },deleteFile = F)

  output[["foto2"]] <- renderImage({
    list(src = "foto/akbar.png", height = 200, width = 210)
  },deleteFile = F)

}

shinyApp(ui, server)
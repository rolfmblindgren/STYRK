library(shiny)
library(readr)
library(readxl)
library(DT)
library(grendelshiny)
library(shinyseo)
library(yaml)

app_meta <- yaml::read_yaml("meta.yaml")

ui <- fluidPage(
  grendelshiny::grendelshiny_css(),
  grendelshiny::grendelshiny_js(),
  shinyseo::social_meta(app_meta),

  tags$section(
    class = "hero",
    div(class = "hero-mark", grendelshiny::grendel_mark()),
    tags$h1("Yrkestilpasningskalkulator")
  ),

  sidebarLayout(
    sidebarPanel(
      sliderInput("I",
                  "Extraversion:",
                  min = 20,
                  max = 80,
                  value = 50),
      sliderInput("II",
                  "Agreeableness:",
                  min = 20,
                  max = 80,
                  value = 50),
      sliderInput("III",
                  "Conscientiousness:",
                  min = 20,
                  max = 80,
                  value = 50),
      sliderInput("IV",
                  "Emotional stability:",
                  min = 20,
                  max = 80,
                  value = 50),
      sliderInput("V",
                  "Openness to Experience:",
                  min = 20,
                  max = 80,
                  value = 50)

    ),

    mainPanel(
      DT::dataTableOutput("result"),
      p("Hvis du mener at lista ikke stemmer for deg, kan det ha en rekke årsaker."),
      p("Formlene er basert på amerikansk forskning, noe som gir en viss usikkerhet."),
      p("Videre er det slik at det som bestemmer hva som passer for deg, ikke bare er personlighet, men også hva du har av erfaring, ambisjoner, interesser, og hvor intelligent du er. Av disse faktorene, er personlighet det som er vanskeligst å finne ut av selv."),
      p("Selv om noen av forslagene er langt utenfor hva du selv har erfaring med, og ikke ligner stort på hverandre, så kan samme personlighet godt passe til svært forskjellige yrker. Bare tenk selv på hva folk kan finne på å ha av overraskende hobbier eller fritidsinteresser. "),
      p("Lista er altså forslag som ikke er tatt rett ut av løse lufta, men som du må se i lys av hva du ellers vet om deg selv. Den vil forbedres over tid.")

    )
  )
)

server <- function(input, output) {

  if ( length(getwd()) == 0 ) {
    homedir <- "/srv/shiny-server/STYRK/"
  } else {
    homedir <- paste0(getwd(),"/")
  }

  ISCONEO <- read_delim(paste0(homedir,"share_isco_big5_ratings.csv"),
                        "\t", escape_double = FALSE,
                        col_types = cols(profession = col_character()),
                        trim_ws = TRUE)

  STYRK <- read_delim(paste0(homedir,"STYRK-08.csv"),
                      ";", escape_double = FALSE,
                      col_types = cols(
                        code = col_integer(),
                        parentCode = col_character(),
                        level = col_integer(),
                        name = col_character(),
                        shortName = col_character(),
                        notes = col_character(),
                        validFrom = col_character(),
                        validTo = col_character()),
                      trim_ws = TRUE)

  ISCO <- read_excel(paste0(homedir,"index08-draft.xlsx"),
                     col_types = c("text", "text", "text"))

  mI <- 4.86;  sdI <- 1.13
  mII <- 5.33; sdII <- 0.98
  mIII <- 6.01; sdIII <- 0.84
  mIV <- 4.33; sdIV <- 1.18
  mV <- 4.53;  sdV <- 1.16

  ISCONEO_t <- ISCONEO
  ISCONEO_t[["job_ext"]] <- (ISCONEO[["job_ext"]] - mI)/sdI * 10 + 50
  ISCONEO_t[["job_agr"]] <- (ISCONEO[["job_agr"]] - mII)/sdII * 10 + 50
  ISCONEO_t[["job_con"]] <- (ISCONEO[["job_con"]] - mIII)/sdIII * 10 + 50
  ISCONEO_t[["job_sta"]] <- (ISCONEO[["job_sta"]] - mIV)/sdIV * 10 + 50
  ISCONEO_t[["job_ope"]] <- (ISCONEO[["job_ope"]] - mV)/sdV * 10 + 50

  output$result <- DT::renderDataTable({

    score <- c(input$I, input$II, input$III, input$IV, input$V)

    b5sim <- function (needle) {
      simscores <-
        cbind(
          ISCONEO_t[[1]],
          apply(
            ISCONEO_t[c("job_ext","job_agr","job_con","job_sta","job_ope")],
            1,
            function(X){
              proxy::dist(rbind(needle,X),method="Euclidean")
            }
          )
        )

      yrker <- apply(matrix(simscores[,1]),1,function(X){

        isco88 <- ISCO[which(ISCO[[2]]==X),1][[1]][1]

        idx <- which(STYRK[[1]]==isco88)

        ret <- NA

        if ( length(idx) > 0 ) {
          ret <- STYRK[[idx,4]]
        } else {
          idx <- which(ISCO[[2]]==isco88)
          if ( length(idx) > 0) {
            ret <- ISCO[idx,][[1,3]]
          }
        }

        ret
      })

      yrker <- yrker[order(simscores[,2])]
      yrker <- cbind(sort(simscores[,2]),yrker)

      max_dist <- as.numeric(yrker[nrow(yrker),1])
      yrker[,1] <- format(
        100 - as.numeric(yrker[,1])/max_dist*100,
        digits=2,
        width=3
      )

      colnames(yrker) <- c("Match i prosent","Yrker")

      yrker[complete.cases(yrker),]
    }

    b5sim(score)

  })
}

shinyApp(ui = ui, server = server)

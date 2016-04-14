library(shiny)
library(ggplot2)

donnees = mtcars[,c("mpg", "cyl", "hp", "am")]
names(donnees) = c("A", "B", "C", "D")
donnees$B = factor(donnees$B)
donnees$D = factor(donnees$D)

## Fonction permettant de récupérer le code
code <- function(package, type, choix) {
    if (is.null(type) | is.null(choix)) return(NULL)
    if (type == 1) {
        # Univarié - Quanti
        if (choix == 1) {
            # Histogramme
            if (package == "base")
                return(expression(hist(donnees$A)))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes(A)) +
                                      geom_histogram()))
        } else if (choix == 2) {
            # Boîte à moustache
            if (package == "base")
                return(expression(boxplot(donnees$A)))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes("", A)) +
                                      geom_boxplot()))
        } else if (choix == 3) {
            # QQ-plot            
            if (package == "base")
                return(expression({qqnorm(donnees$A);qqline(donnees$A)}))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes(sample = A)) +
                                      geom_qq()))
        }
    } else if (type == 2) {
        # Univarié - qualitatif
        if (choix == 1) {
            # Diagramme en barres
            if (package == "base")
                return(expression(barplot(table(donnees$B))))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes(B)) +
                                      geom_bar()))
        } else if (choix == 2) {
            # Diagramme en barres empilées
            if (package == "base")
                return(expression(barplot(as.matrix(table(donnees$B)))))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes("", fill = B)) +
                                      geom_bar()))
        } else if (choix == 3) {
            # Diagramme circulaire
            if (package == "base")
                return(expression(pie(table(donnees$B))))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes("", fill = B)) +
                                      geom_bar(width = 1) +
                                      coord_polar(theta = "y")))
                    }
    } else if (type == 3) {
        # Bivarié - Quanti-Quanti
        if (choix == 1) {
            # Nuage de points
            if (package == "base")
                return(expression(plot(A ~ C, data = donnees)))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes(C, A)) +
                                      geom_point()))
        } else if (choix == 2) {
            # Heatmap
            if (package == "base")
                return(expression({
                    cutX = cut(donnees$C, breaks = 30)
                    cutY = cut(donnees$A, breaks = 30)
                    image(table(cutX, cutY))
                }))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes(C, A)) +
                                      geom_bin2d()))
        }
    } else if (type == 4) {
        # Bivarié - Quali-Quali
        if (choix == 1) {
            # Diagramme en barres séparées
            if (package == "base")
                return(expression(barplot(table(donnees$D, donnees$B), beside = T)))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes(B, fill = D)) +
                                      geom_bar(position = "dodge")))
        } else if (choix == 2) {
            # Diagramme en barres empilées
            if (package == "base")
                return(expression(barplot(table(donnees$D, donnees$B))))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes(B, fill = D)) +
                                      geom_bar()))
        } else if (choix == 3) {
            # Diagramme en barres empilées à 100%
            if (package == "base")
                return(expression({
                    t = table(donnees$D, donnees$B)
                    p = prop.table(t, margin = 2)
                    barplot(p)
                }))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes(B, fill = D)) +
                                      geom_bar(position = "fill")))
        } else if (choix == 4) {
            # Heatmap
            if (package == "base")
                return(expression({
                    image(table(donnees$B, donnees$D))
                }))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes(B, D)) +
                                      geom_bin2d()))
        }
    } else if (type == 5) {
        # Bivarié - Quali-Quanti
        if (choix == 1) {
            # Histogrammes
            if (package == "base")
                return(expression({
                    mod = levels(donnees$B)
                    par(mfrow = c(length(mod), 1), mar = c(2, 2, 2, 0) + .1)
                    for(m in mod) {
                        hist(donnees$A[donnees$B == m], main = "",
                             xlim = range(donnees$A))
                    }
                }))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes(A)) +
                                      geom_histogram() +
                                      facet_grid(B~.)))
        } else if (choix == 2) {
            # Boîtes à moustaches
            if (package == "base")
                return(expression(boxplot(A ~ B, data = donnees)))
            else if (package == "ggplot2")
                return(expression(ggplot(donnees, aes(B, A)) +
                                      geom_boxplot()))
        }
    }
}

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("Comparaison des graphiques de base et de ggplot2"),
    p("Application web permettant de comparer le code à produire pour réaliser les mêmes graphiques avec le package graphics (de base dans R) et le package ggplot2."),
    
    fluidRow(
        column(
            width = 6,
            selectInput(
                "type",
                "Type de graphiques",
                c("..." = 0,
                  "Univarié - Quantitatif" = 1,
                  "Univarié - Qualitatif" = 2,
                  "Bivarié - quanti-quanti" = 3,
                  "Bivarié - quali-quali" = 4,
                  "Bivarié - quali-quanti" = 5)
            )
        ),
        column(
            width = 6,
            selectInput(
                "choix",
                "Graphique à comparer",
                choices = NULL
            )
        )
   ),
   fluidRow(
       column(
           width = 6,
           h2("graphics"),
           pre(textOutput("codeBase")),
           plotOutput("plotBase")
       ),
       column(
           width = 6,
           h2("ggplot2"),
           pre(textOutput("codeGG")),
           plotOutput("plotGG")
       )
   )
))

server <- shinyServer(function(input, output, session) {
   
    # Définition des graphiques à comparer
    observe({
        if (input$type == 1) {
            # Univarié - Quanti
            updateSelectInput(session, "choix", 
                              choices = c("Histogramme" = 1, 
                                          "Boîte à moustaches" = 2, 
                                          "QQ-plot" = 3))
        } else if (input$type == 2) {
            # Univarié - Quali
            updateSelectInput(session, "choix", 
                              choices = c("Diagramme en barres" = 1,
                                          "Diagramme en barres empilées" = 2,
                                          "Diagramme circulaire" = 3))
        } else if (input$type == 3) {
            # Bivarié - Quanti-Quanti
            updateSelectInput(session, "choix", 
                              choices = c("Nuage de points" = 1,
                                          "Heatmap" = 2))
        } else if (input$type == 4) {
            # Bivarié - Quali-Quali
            updateSelectInput(session, "choix",
                              choices = c("Diagramme en barres séparées" = 1,
                                          "Diagramme en barres empilées" = 2,
                                          "Diagramme en barres empilées à 100%" = 3,
                                          "Heatmap" = 4))
        } else if (input$type == 5) {
            # Bivarié - Quanli-Quanti
            updateSelectInput(session, "choix",
                              choices = c("Histogrammes" = 1,
                                          "Boîtes à moustaches" = 2))
        }
    })

    # Création des graphiques
    output$codeBase <- renderText({
        as.character(code("base", type = input$type, choix = input$choix))
    })
    output$plotBase <- renderPlot({
        eval(code("base", type = input$type, choix = input$choix))
    })
    output$codeGG <- renderText({
        gsub("\\+", "+\n\t", as.character(code("ggplot2", type = input$type, choix = input$choix)))
    })
    output$plotGG <- renderPlot({
        eval(code("ggplot2", type = input$type, choix = input$choix))
    })
})

# Run the application 
shinyApp(ui = ui, server = server)


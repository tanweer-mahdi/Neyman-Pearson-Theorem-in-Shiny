library(shiny)
library(ggplot2)
#library(methods)
ui <- fluidPage(
  titlePanel('Neyman Pearson Theorem'),
  sidebarLayout(
    sidebarPanel(
      #Input: Slider for mean of first distribution
      sliderInput(inputId = 'mean_1',
                  label = 'Mean of Gaussian observation',
                  min = -5,
                  max = 10,
                  value  =3),
      sliderInput(inputId = 'pfa',
                  label = 'Probability of False Alarm',
                  min = 0,
                  max = 1,
                  value = 0.23)
    ),
    mainPanel(
      plotOutput(outputId = 'pdf'),
      plotOutput(outputId = 'roc')
    )
  )
)
server <- function(input, output) {
  sd = 2
  output$pdf <-renderPlot({
    #set.seed(3000)
    #xseq<-seq(-8,8,.01)
    #dens1<-dnorm(xseq,input$mean_1,1)
    #dens2<-dnorm(xseq,input$mean_2,1)
    #plot(xseq,dens1,ylab='Likelihood',xlab = 'Decision stastic value',type='l',col='blue')
    #lines(xseq,dens2,col='green')
    
    funcShaded_1 <- function(x, m, lower_bound,sd) {
      y = dnorm(x, mean = m, sd = sd)
      y[x <= lower_bound] <- NA
      return(y)
    }
    funcShaded_2 <- function(x, m, higher_bound,sd) {
      y = dnorm(x, mean = m, sd = sd)
      y[x > higher_bound] <- NA
      return(y)
    }
    ggplot(data.frame(x=c(-9,15)),aes(x=x)) +
    stat_function(fun = dnorm, args = list(0, sd), geom = "area", fill = "lightgreen", alpha = .8) +
    #stat_function(fun = funcShaded_1, args = list(0,qnorm(1-input$pfa,0,sd),sd), geom = 'area', fill = 'green', alpha = 0.8) +
    stat_function(fun = dnorm, args = list(input$mean_1, sd), geom = "area", fill = "lightpink", alpha = .5) +
    #stat_function(fun = funcShaded_2, args = list(input$mean_1,qnorm(1-input$pfa,0,sd),sd), geom = 'area', fill = 'red', alpha = 0.8) +
    #scale_x_continuous(name = "Decision statistic value") +
    scale_y_continuous(name = "Likelihood") +
    geom_vline(xintercept = qnorm(1-input$pfa,0,sd)) 
  })
  output$roc <-renderPlot({
    pfa<-seq(0,1,.01)
    xx <-qnorm(1-pfa,0,sd)
    pd <-1-pnorm(xx,mean=input$mean_1,sd)
    plot(pfa,pd,type='l',col='blue',xlab='Probability of False Alarm',ylab='Probability of Correct Detection')
  })
}
shinyApp(ui = ui, server = server)
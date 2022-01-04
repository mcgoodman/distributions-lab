# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(tidyverse)
library(shiny)

theme_set(theme_minimal() + theme(text = element_text(size = 16)))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lab: Distributions and their means"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "dist",
                label = "Statistical Distribution",
                choices = c(
                    "Normal", "Poisson", "Binomial", "Gamma", "Beta", "Pareto"
                )),
            uiOutput("params"),
            uiOutput("xlim"),
            fluidPage(
                sliderInput("sample_size", "Sample size", 1, 500, 10, 1),
                sliderInput("n_samples", "Number of samples", 10, 1000, 500, 10)
            )
        ),


        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
             tabPanel("Density Function", fluidPage(
                 plotOutput("density"),
                 uiOutput("description")
             )),
             tabPanel("Samples", fluidPage(
                 plotOutput("samples"),
                 fluidPage(
                     h2("Sample distributions"),
                     p(paste(
                         "Here, we're only visualizing the first 10 samples. Pay attention to the",
                         "shape of the distributions as you increase the sample size, and to the variation",
                         "in sample means (shown as a black line) among samples. What do you notice?"
                     ))
                 )
             )),
             tabPanel("Sampling distribution", fluidPage(
                 plotOutput("sampling_dist"),
                 fluidPage(
                     h2("The Sampling Distribution"),
                     p(paste(
                         "The sampling distribution is the distribution of sample means. In practice",
                         "we usually only have 1 sample - we might have many observations in that sample",
                         "but we only have 1 sample - so we can't know the sampling distribution. However,",
                         "we can use the properties of our sample, and central limit theorem, to infer",
                         "the variance and shape of the sampling distribution."
                     )),
                     p(paste(
                         "Why do we want to infer the sampling distribution if we already have the mean from",
                         "our sample? Well, the sample mean is just that - a mean from a sample, and we",
                         "usually want to make inferences about the true mean value of the population from",
                         "which we're sampling. How much uncertainty is there in our estimate of the mean?",
                         "Which range of other mean values could also be considered consistent with the",
                         "data we collected?"
                     )),
                    p(paste(
                        "Mess around with the parameters to the left and see what happens to the sampling",
                        "distribution. As you increase the sample size, what happens to variance in the",
                        "sampling distribution (note the change in x axis scale)? What does this mean",
                        "about our certainty / uncertainty in our estimate of the mean from a sample",
                        "of that size? Play with some distributions that produce weird or skewed shapes -",
                        "what do you notice? In all this, it might help to keep the number of samples",
                        "very high, since you'll be able to see that sampling distribution most clearly that",
                        "way"
                    ))
                 )
             ))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    source("distributions.R")

    dist <- reactive({
         switch(
            input$dist,
            "Normal" = normal,
            "Poisson" = poisson,
            "Binomial" = binomial,
            "Beta" = beta,
            "Pareto" = pareto,
            "Gamma" = gamma
        )
    })

    output$params <- renderUI({
        if (length(dist()$pars) == 1){
            fluidPage(
                sliderInput("par1", dist()$par[1], min = dist()$scale[[1]][1],
                            max = dist()$scale[[1]][2], value = dist()$scale[[1]][3],
                            step = dist()$scale[[1]][4])
            )
        } else if (length(dist()$pars) > 1){
            fluidPage(
                sliderInput("par1", dist()$par[1], min = dist()$scale[[1]][1],
                            max = dist()$scale[[1]][2], value = dist()$scale[[1]][3],
                            step = dist()$scale[[1]][4]),
                sliderInput("par2", dist()$par[2], min = dist()$scale[[2]][1],
                            max = dist()$scale[[2]][2], value = dist()$scale[[2]][3],
                            step = dist()$scale[[2]][4])
            )
        }
    })

    output$xlim <- renderUI({
        if(input$dist == "Normal") {
            fluidPage(
                numericInput("xmin", "x axis minimum", dist()$xlim[1], min = dist()$xlim[3], max = dist()$xlim[4]),
                numericInput("xmax", "x axis maximum", dist()$xlim[2], min = dist()$xlim[3], max = dist()$xlim[4])
            )
        } else if (input$dist == "Pareto" | input$dist == "Gamma") {
            fluidPage(
                numericInput("xmax", "x axis maximum", dist()$xmax, min = 1)
            )
        } else {
            fluidPage()
        }
    })

    output$density <- renderPlot({
        if (length(dist()$pars) == 1){
            dist()$plot(input$par1)
        } else if (input$dist == "Pareto" | input$dist == "Gamma") {
            dist()$plot(input$par1, input$par2, xmax = input$xmax)
        } else {
            dist()$plot(input$par1, input$par2, xlim = c(input$xmin, input$xmax))
        }
    })

    sample_df <- reactive({
        if (length(dist()$pars) == 2) {
            pars <- list(input$par1, input$par2); names(pars) <- names(dist()$pars)
        } else {
            pars <- list(input$par1); names(pars) <- names(dist()$pars)
        }
        x <- draw_samples(input$sample_size, input$n_samples, dist(), pars)
        x <- reshape2::melt(x)
        names(x) <- c("obs", "sample", "value")
        x
    })

    sample_means <- reactive({
        sample_df() %>% group_by(sample) %>% summarize(mean = mean(value))
    })

    output$samples <- renderPlot({
        if(input$dist == "Normal") {
            sample_df() %>%
                filter(sample <= 10) %>%
                ggplot(aes(value)) +
                geom_vline(aes(xintercept = mean), data = sample_means() %>% filter(sample <= 10),
                           color = "black", size = 1.5) +
                geom_histogram(fill = "dodgerblue3", color = "dodgerblue4", size = 1) +
                facet_wrap(~sample, nrow = 2) +
                theme_bw() +
                scale_x_continuous(limits = c(input$xmin, input$xmax))
        } else if (input$dist == "Binomial") {
            sample_df() %>%
                filter(sample <= 10) %>%
                group_by(sample, value) %>%
                tally() %>%
                ggplot(aes(value, n)) +
                geom_vline(aes(xintercept = mean), data = sample_means() %>% filter(sample <= 10),
                           color = "black", size = 1.5) +
                geom_bar(stat = "identity", fill = "dodgerblue3", color = "dodgerblue4", binwidth = 1, size = 1) +
                facet_wrap(~sample, nrow = 2) +
                theme_bw() +
                scale_x_continuous(limits = c(-1, input$par1 + 1))
        } else if (input$dist == "Poisson") {
            sample_df() %>%
                filter(sample <= 10) %>%
                group_by(sample, value) %>%
                tally() %>%
                ggplot(aes(value, n)) +
                geom_vline(aes(xintercept = mean), data = sample_means() %>% filter(sample <= 10),
                           color = "black", size = 1.5) +
                geom_bar(stat = "identity", fill = "dodgerblue3", color = "dodgerblue4", binwidth = 1, size = 1) +
                facet_wrap(~sample, nrow = 2) +
                theme_bw() +
                scale_x_continuous(limits = c(-1, ifelse(input$par1 <= 1, 4, ceiling(input$par1*3) + 1)))
        } else if (input$dist == "Beta") {
            sample_df() %>%
                filter(sample <= 10) %>%
                ggplot(aes(value)) +
                geom_vline(aes(xintercept = mean), data = sample_means() %>% filter(sample <= 10),
                           color = "black", size = 1.5) +
                geom_histogram(fill = "dodgerblue3", color = "dodgerblue4", binwidth = 0.05, size = 1) +
                facet_wrap(~sample, nrow = 2) +
                theme_bw() +
                scale_x_continuous(limits = c(-0.05, 1.05))
        } else if (input$dist == "Pareto" | input$dist == "Gamma") {
            sample_df() %>%
                filter(sample <= 10) %>%
                ggplot(aes(value)) +
                geom_vline(aes(xintercept = mean), data = sample_means() %>% filter(sample <= 10),
                           color = "black", size = 1.5) +
                geom_histogram(fill = "dodgerblue3", color = "dodgerblue4", binwidth = 1, size = 1) +
                facet_wrap(~sample, nrow = 2) +
                theme_bw() +
                scale_x_continuous(limits = c(0, input$xmax + 1))
        }
    })

    output$sampling_dist <- renderPlot({
        sample_means() %>%
            ggplot(aes(mean)) +
            geom_histogram(fill = "dodgerblue3", color = "dodgerblue4", size = 1) +
            xlab("sample mean")
    })

    output$description <- renderUI({
        if(input$dist == "Normal") {
            fluidPage(
                h2("The Normal Distribution"),
                p("The Normal Distribution is a continuous probability distribution which spans all real numbers."),
                p(paste(
                    "Because the mean / location of the normal distribution and the variance / scale",
                    "are independent, we can use it to build simple models linking the mean value of a response",
                    "variable to the values of some predictors, so normal distributions are often used",
                    "in statistical models for their simplicity and flexibility."
                )),
                p(paste(
                    "However, as we'll find out in this lab, that's not the only reason the normal distribution",
                    "is so prevalent in statistics: Central limit theorem states that the mean of any random variable",
                    "with a finite variance will converge to a normal distribution as the sample size increases"
                )),
                p(paste(
                    "In this lab, we'll explore a few statistical distributions, some of which are commonly used",
                    "in ecological models. We'll sample from those distributions, visualize the samples",
                    "and then visualize the distribution of the sample means."
                )),
                p(paste(
                    "The distinction between the 'Sample size' and 'Number of samples' sliders is important.",
                    "Imagine we have a bag of marbles, some of which are blue and some are red, and what we're",
                    "doing is taking marbles from the bag, noting down the proportion that are blue, and then",
                    "mixing all the marbles up into the bag and repeating. The sample size is the number of",
                    "marbles we take from the bag each time before calculating the proportion that are blue",
                    "and the number of samples is the number of times we draw a set of marbles from the bag."
                ))
            )
        } else if (input$dist == "Binomial") {
            fluidPage(
                h2("The Binomial Distribution"),
                p(paste(
                    "The binomial distribution is a discrete probability distribution that represents",
                    "the sum of n independent Bernoulli (binary outcome) trials,",
                    "where the probability of 'success' on each trial is p - for example, if we survey 10",
                    "transects in a kelp forest, the number of transects we find seastars on would have a",
                    "binomial distribution with n = 10 and p equal to the probability that we observe a",
                    "seastar on any given transect."
                )),
                p(paste(
                    "A special case of the binomial distribution, when n = 1, is the Bernoulli distribution."
                ))
            )
        } else if (input$dist == "Poisson") {
            fluidPage(
                h2("The Poisson Distribution"),
                p("The Poisson distribution is a discrete probability distribution which is commonly used for count data."),
                p(paste(
                    "The Poisson distribution expresses the number of events that happen in a fixed amount of time",
                    "if they happen at a fixed mean rate. For example,let's say I;m sitting at my desk by the window",
                    "for 8 hours a day. If dogs pass every 30 minutes, the number of dogs I observe in a given day would",
                    "be Poisson distributed with mean 16, because dogs pass once every half hour for eight hours."
                )),
                p(paste(
                    "We can also consider space, instead of time, as the dimension over which some species of interest is",
                    "distributed with some mean rate. So, for example, we might use the Poisson distribution to model",
                    "the number of seastars that we find along transects in a kelp forest."
                )),
                p(paste(
                    "The Poisson distribution has only one parameter, which is both the mean and the variance, so that",
                    "as the expected mean increases, the variance does too. This is a common feature of count data.",
                    "A distribution closely related to the Poisson is the negative binomial - this is also a discrete",
                    "distribution commonly used for count data, but it includes an overdispersion parameter which",
                    "loosens the restrictive assumption that the mean and variance are equal."
                ))
            )
        } else if (input$dist == "Beta") {
            fluidPage(
                h2("The Beta Distribution"),
                p("The beta distribution is a continous probability distribution bounded between 0 and 1."),
                p(paste(
                    "The beta distribution is frequently used for proportion data - for example, the proportion of",
                    "a species diet composed of a single prey item, or the proportion of a surface covered in some",
                    "species of algae or another. The shape of the beta distribution is extremely flexible - ",
                    "play around with the shape parameters. Does the shape of the beta distribution that we're",
                    "sampling from impact the shape of the distribution of sample means?"
                )),
                p(paste(
                    "As a side note, the beta distribution is closely related to the Dirichlet distribution",
                    "which can be used for modeling the proportions of multiple categories simultaneously",
                    "- e.g. multiple prey items in predator diets."
                ))
            )
        } else if (input$dist == "Pareto") {
            fluidPage(
                h2("The Pareto Distribution"),
                p(paste(
                    "The Pareto distribution is a continuous probability distribution for values greater than 1.",
                    "It's really not used in ecology ever, but it's interesting because for a range of parameters,",
                    "the variance of the Pareto distribution is infinite. Notice what this does to the sampling",
                    "distribution."
                ))
            )
        } else if (input$dist == "Gamma") {
            fluidPage(
                h2("The Gamma Distribution"),
                p(paste(
                    "The Gamma distribution is a continuous probability distribution for values greater than 0.",
                    "It models the time required for the occurrence of a certain number of events to occur under",
                    "a Poisson process. This mechanistic description is usually not what it's applied for however -",
                    "in practice, the Gamma distribution is useful for many problems where the data are right-skewed,",
                    "positive, and continuous."
                )),
                p(paste(
                    "The shape parameter is neither a location nor scale parameter - i.e., it does not impact solely",
                    "the mean or variance. This can make it a little confusing to use in models, because we are used to",
                    "describing variables in terms of their means. Fortunately, the Gamma distribution can instead be",
                    "parameterized using a mean (/location) and variance (see Bolker 2007 Chapter 4)."
                ))
            )
        }
    })

}

# Run the application
shinyApp(ui = ui, server = server)

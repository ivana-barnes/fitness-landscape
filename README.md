# Interactive Evolutionary Fitness Landscapes

An interactive R Shiny web application designed to visualize single-locus evolutionary fitness landscapes based on customizable genotype fitnesses. 

## Live Demo
You can interact with the live application here: 
[https://ivana-barnes.shinyapps.io/fitness-landscape/](https://ivana-barnes.shinyapps.io/fitness-landscape/)

## How to Run Locally

1. Clone or download this GitHub repository to your computer.
2. Open the `fitness-landscape.Rproj` file in RStudio (this automatically sets your working directory to the correct folder).
3. Open an R console and execute:

```R
install.packages(c("shiny", "rsconnect"))
shiny::runApp()
```

## Description of the Visualization

This visualization shows the fitness landscape as a function of genotype fitnesses that are input through sliders on the app. 

w11 = fitness of the A1A1 genotype

w12 = fitness of the A1A2 genotype

w22 = fitness of the A2A2 genotype

The left plot shows the fitness landscape: the population mean fitness as a function of the allele frequency of A1 in the population.
The right plot is a $\Delta p$ plot - it shows how the allele frequency is changing due to selection as a function of the allele frequency.
The visualization also points out the equilibrium allele frequencies and plots them on both plots. Stable equilibria are shown in green and unstable are shown in brown.

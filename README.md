# dhaferShinyApps

# Prelimanry

You need first to install

```{r}
require(devtools)
install_github('rstudio/shiny')
install_github('rstudio/shiny-incubator')
require(shiny)
```

# Running the PCA app

```{r}
require(shiny)
shiny::runGitHub(repo = 'dhaferShinyApps',username =  'malouche', subdir='PCA_app_v3')
```



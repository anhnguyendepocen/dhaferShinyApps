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

Here's a tutorial video on how you can use this app: https://www.youtube.com/watch?v=Z_0pXXPOKV0


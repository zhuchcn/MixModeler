## Mix Modeler

This is a shiny app to fit linear model with your data. 

To run the app locally:

1. Install from github

```
devtools::install_github("zhuchcn/MixModeler")
```

2. Make sure your have 

```
pkgs = c("shiny", "rhandsontable", "bs4Dash", "shinyjs")
for(pkg in pkgs){
    if(!require(pkg, character.only = TRUE)){
        install.packages(pkg)
    }
}
```

3. Launch it
```
MixModeler::launch()
```

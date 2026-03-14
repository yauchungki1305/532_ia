# Vancouver Park Dashboard

An interactive R Shiny dashboard for exploring parks in Vancouver, BC. This application allows users to filter parks by size, neighbourhood, and facilities, while visualizing data through interactive maps and charts.

## Link of the deployed application on Posit Connect Cloud
https://019cee73-0bfd-74d9-6fe2-24a365d5308e.share.connect.posit.cloud/

## Features
* **Interactive Map:** Built with `leaflet` to show park locations with custom status overlays.
* **Data Exploration:** A searchable, scrollable table powered by `DT`.
* **Washroom Analytics:** A dynamic `plotly` bar chart highlighting washroom availability across neighbourhoods.
* **Modern UI:** Responsive card-based layout using `bslib`.

## Installation

To run this application locally, you must have [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/) installed.

1. **Clone the repository (terminal):**
   ```bash
   git clone https://github.com/yauchungki1305/532_ia.git
   ```
2. **Move to the project directory (terminal):**
   ```bash
   cd 532_ia
   ```
3. **In r package:**
  Open R Studio console or open r in terminal, then past the code below to install packages
   ```r
   install.packages(c("shiny", "leaflet", "plotly", "dplyr", "readr", "DT", "bslib", "tidyr"))
   ```
4. **run the app (R console)**
   Remember to ensure you are in the project directory
   ```r
   runApp("App-1")
   ```
   Or, open the file `app.R` in `App-1` on Rstudio and press the `Run App` button in the top right of the interface.
   

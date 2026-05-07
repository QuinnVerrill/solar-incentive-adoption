This subrepository contains all code used to generate the sand mine complaint dashboard and related visualizations. 

`mine_locs_2024` is the dataframe containing mine locations and purposes.

`plymouth_weather` is a deprecated dataframe of weather for the Carver area

`TestCLWCMap.rmd` processes the complaints data and generates the initial Leaflet map plot. It is essential to run this entire RMD prior to attempting to run the app.

`ws_plot_testing.rmd` runs an API call from OpenMeteo to fetch up-to-date weather data from Plymouth and generate relevant visualizations. It is also where geocoding of sandmine data and general cleaning occurs. As above, it is essential to run this file prior to running the app.

`testapp/single_app.R` is the code for the Shiny dashboard.

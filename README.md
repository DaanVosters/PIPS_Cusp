# Interactive Cusp Catastrophe Tool: Visualisation & Data-Analysis
Welcome to our cusp app!

## About the Project
This Shiny app was developed as part of the PIPS course at the UvA. Our app enables users to visualise (psychological) phenomena which are theorised to follow the dynamics of a cusp. For an example about attitudes, see van der Maas et al. (2003). Additionally, the app allows users to fit the cusp to uploaded data.

### Visualisation Feature
The base model contains only the visualisation feature. Users can add several people to (and removed from) the 3D graph and move them along the cusp using two sliders, which correspond to the normal and splitting variables. People can also be restored to their starting position with the "Reset everyone's controls" button. The user can adjust how quickly people move over the cusp. 

For an easier overview, users can change the names of variables near the bottom of the left panel to the corresponding constructs from their psychological theory. The coordinates of the selected person are displayed on the left panel or can be checked by hovering over the graph. 

### Data Analysis Feature
The data analysis module, located under the main graph when loaded, enables users to fit the cusp to data. For this, a CSV file is required with response, splitting and normal variables. All variables must be numeric and continuous. The user can choose which variable is which using drop-down menus. A demo dataset is available based on the attitude cusp model (van der Maas et al., 2003). 

After the model is fit, parameter estimates and the fit information is displayed. Optionally, the fit of the cusp model can be compared to linear regression using AIC and BIC. Fitted coordinates and analysis results can easily be downloaded using the buttons at the bottom. A 3D cusp graph is also created, to which all datapoints are added. Black dots show the true locations of the datapoints (possibly standardised), and a purple dot corresponding to each black dot shows the closest projection onto the cusp, since datapoints need not lie perfectly on the cusp. Some aid in interpretation is also provided in the notes displayed under graphs and buttons.

### Aesthetics Feature
At the top of the app, above the main 3D graph, users also have the option to change to dark mode or adjust the colours of the different features of the graph. This may prove useful for colourblind users, but we encourage everyone to have fun with some pretty colours. Who said statistics isn't fun?

## Collaborators
This project was completed by Daan Vosters, Madeleine Limone and Virág Nyisztor

## Reference
van der Maas, H. L. J., Kolstein, R., & van der Pligt, J. (2003). Sudden transitions in attitudes. *Sociological Methods & Research, 32*(2), 125–152. https://doi.org/10.1177/0049124103253773



This is a series of RMarkdown files that generates maps of crashes between motor vehicles and bikers and pedestrians in Wisconsin.

## Data sources
- The crash data is from the UW TOPS lab: https://topslab.wisc.edu/
- School location data is from the Wisconsin DPI: https://data-wi-dpi.opendata.arcgis.com/
- County data is from the US census
- Basemaps are from StadiaMaps: https://stadiamaps.com/

## Example figures
### A map of crashes near East High School in Madison, WI:
![example routes figure](examples/example-school.png)

## Here is where the web maps are hosted
[Wisconsin Bike Pedestrian Crash Map](https://wisconsinbikefed.org/what-we-do/advocacy/wisconsin-bike-pedestrian-crash-map/)

## To process data and generate figures

- Pre Process TOPS data `make TOPS_data_process`: This combines TOPS data exports from different years and reformats the data.
- OpenStreetMap Routing Machine `make osrm`: this downloads the most recent OpenStreetMap data for Wisconsin, and starts docker containers to run the OpenStreetMap Routing Machine to calculate IsoDistances.
- School Crash Maps `make schoolpdfs`: generates maps of all the schools in Wisconsin and where cars are hitting kids This script pulls in the TOPS data from a folder of csv’s that you’ve downloaded from the data retrieval tool. It also draws the walk boundary around each school, this is done with a OpenStreetMap routing engine that is running in a docker container. It also needs access to an API key for StadiaMaps to download all the basemap tiles. I recommend downloading the whole state and filtering the data with the script. You can edit file `parameters/run_parameters` to choose what county, school type, and district to generate maps for
- Dynamic Map `make crashmaps_dynamic`: This takes the TOPS data and generates dynamic leaflet maps to host on our website. It generates 4 maps: without a title (for in a frame), and with a title, in both English and Spanish.
- Milwaukee Specific map `crashmaps_dynamic_milwaukee`: generate a web map for Milwaukee that includes more fine-grained visualizations of crash densities.
- Crash summaries `make crash_data_summaries`: generates summary csv files that summarize the crash data for a given municipality and county by age, sex, race.


## R Scripts
I'm working to move these to RMarkdown files
- City maps - generates maps for cities in Wisconsin where pedestrian fatalities occur.

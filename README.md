## Scripts
- Crash Summaries - generates summary csv files that summarize the crash data for a given municipality and county by age, sex, race.
This script pulls in the TOPS data from a folder of csv’s that you’ve downloaded from the data retrieval tool. I recommend downloading the whole state and filtering the data with the script.
You can edit the variables: municipality_focus, county_focus to adjust what city you're looking at. Line 50
- City maps - generates maps for cities in Wisconsin where pedestrian fatalities occur
This script pulls in the TOPS data from a folder of csv’s that you’ve downloaded from the data retrieval tool. 
It also needs access to an API key for StadiaMaps to download all the basemap tiles. 
You can specify the cities by changing the focus parameters at line ~80
- School Crash maps - generates maps of all the schools in Wisconsin and where cars are hitting kids
This script pulls in the TOPS data from a folder of csv’s that you’ve downloaded from the data retrieval tool. 
It also draws the walk boundary around each school, this is done with a OpenStreetMap routing engine that is running in a docker container.
It also needs access to an API key for StadiaMaps to download all the basemap tiles. 
It took my computer 12 hours to generate all the maps for the state. The script that ends “parallel” runs it in parallel to speed this up. Use that script for bulk map generation, use the other one for generating the maps of a couple school districts.
You can change what districts or counties or school types you are generating maps for by editing the variables at line 116 or so.
- Dynamic Map: This takes the TOPS data and generates a dynamic map to host

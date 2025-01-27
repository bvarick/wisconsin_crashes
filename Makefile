osrm: osrm-data osrm-containers

crashmaps_pdf: route_analysis.Rmd
	R -e 'library("rmarkdown"); old_path <- Sys.getenv("PATH"); Sys.setenv(PATH = paste(old_path, "/usr/local/bin", sep = ":")); rmarkdown::render(knit_root_dir = "./", output_dir = "./html", input = "./R/route_analysis.Rmd", output_file = "./html/route_analysis.html")'

crashmaps_dynamic: cycle_brouter

osrm-data:
	cd ./docker/osrm/; wget https://download.geofabrik.de/north-america/us/wisconsin-latest.osm.pbf -O ./data-raw/wisconsin-latest.osm.pbf
	cd ./docker/osrm/; docker run --rm -t -v "./data-foot:/data" -v "./data-raw/wisconsin-latest.osm.pbf:/data/wisconsin-latest.osm.pbf" osrm/osrm-backend osrm-extract -p /opt/foot.lua /data/wisconsin-latest.osm.pbf
	cd ./docker/osrm/; docker run --rm -t -v "./data-foot:/data" -v "./data-raw/wisconsin-latest.osm.pbf:/data/wisconsin-latest.osm.pbf" osrm/osrm-backend osrm-partition /data/wisconsin-latest.osrm
	cd ./docker/osrm/; docker run --rm -t -v "./data-foot:/data" -v "./data-raw/wisconsin-latest.osm.pbf:/data/wisconsin-latest.osm.pbf" osrm/osrm-backend osrm-customize /data/wisconsin-latest.osrm

osrm-containers: ./docker/osrm/docker-compose.yml
	cd ./docker/osrm/; docker compose up -d

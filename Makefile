all: osrm TOPS_data_process crashmaps

osrm: osrm-data osrm-containers

crashmaps: schoolmaps_PDFs crashmaps_dynamic crashmaps_dynamic_milwaukee crash_data_summaries

TOPS_data_process: R/TOPS_data_process.Rmd
	R -e 'library("rmarkdown"); old_path <- Sys.getenv("PATH"); Sys.setenv(PATH = paste(old_path, "/usr/local/bin", sep = ":")); rmarkdown::render(knit_root_dir = "../", output_dir = "./html", input = "./R/TOPS_data_process.Rmd", output_file = "./html/TOPS_data_process.html")'

schoolmaps_PDFs: R/schoolmaps_PDFs.Rmd
	R -e 'library("rmarkdown"); old_path <- Sys.getenv("PATH"); Sys.setenv(PATH = paste(old_path, "/usr/local/bin", sep = ":")); rmarkdown::render(knit_root_dir = "../", output_dir = "./html", input = "./R/schoolmaps_PDFs.Rmd", output_file = "./html/schoolmaps_PDFs.html")'

crashmaps_dynamic: R/dynamic_crash_map.Rmd
	R -e 'library("rmarkdown"); old_path <- Sys.getenv("PATH"); Sys.setenv(PATH = paste(old_path, "/usr/local/bin", sep = ":")); rmarkdown::render(knit_root_dir = "../", output_dir = "./html", input = "./R/dynamic_crash_map.Rmd", output_file = "./html/dynamic_crash_map.html")'

crashmaps_dynamic_milwaukee: R/dynamic_crash_map_milwaukee.Rmd
	R -e 'library("rmarkdown"); old_path <- Sys.getenv("PATH"); Sys.setenv(PATH = paste(old_path, "/usr/local/bin", sep = ":")); rmarkdown::render(knit_root_dir = "../", output_dir = "./html", input = "./R/dynamic_crash_map_milwaukee.Rmd", output_file = "./html/dynamic_crash_map_milwaukee.html")'

crash_data_summaries: R/crash_data_summaries.Rmd
	R -e 'library("rmarkdown"); old_path <- Sys.getenv("PATH"); Sys.setenv(PATH = paste(old_path, "/usr/local/bin", sep = ":")); rmarkdown::render(knit_root_dir = "../", output_dir = "./html", input = "./R/crash_data_summaries.Rmd", output_file = "./html/crash_data_summaries.html")'

MilWALKeeWalks: R/MilWALKeeWalks.Rmd
	R -e 'library("rmarkdown"); old_path <- Sys.getenv("PATH"); Sys.setenv(PATH = paste(old_path, "/usr/local/bin", sep = ":")); rmarkdown::render(knit_root_dir = "../", output_dir = "./html", input = "./R/MilWALKeeWalks.Rmd", output_file = "./html/MilWALKeeWalks.html")'

osrm-data:
	cd ./docker/osrm/; wget https://download.geofabrik.de/north-america/us/wisconsin-latest.osm.pbf -O ./data-raw/wisconsin-latest.osm.pbf
	cd ./docker/osrm/; docker run --rm -t -v "./data-foot:/data" -v "./data-raw/wisconsin-latest.osm.pbf:/data/wisconsin-latest.osm.pbf" osrm/osrm-backend osrm-extract -p /opt/foot.lua /data/wisconsin-latest.osm.pbf
	cd ./docker/osrm/; docker run --rm -t -v "./data-foot:/data" -v "./data-raw/wisconsin-latest.osm.pbf:/data/wisconsin-latest.osm.pbf" osrm/osrm-backend osrm-partition /data/wisconsin-latest.osrm
	cd ./docker/osrm/; docker run --rm -t -v "./data-foot:/data" -v "./data-raw/wisconsin-latest.osm.pbf:/data/wisconsin-latest.osm.pbf" osrm/osrm-backend osrm-customize /data/wisconsin-latest.osrm

osrm-containers: ./docker/osrm/docker-compose.yml
	cd ./docker/osrm/; docker compose up -d

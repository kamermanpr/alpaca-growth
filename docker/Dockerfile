#-- Build command --#
## cd to where the docker file is located.
# docker build -t kamermanpr/alpaca-growth:v1.1 .

#-- Push build to DockerHub --#
# docker push

#-- The build image can be downloaded from Docker Hub --#
# docker pull kamermanpr/alpaca-growth:v1.1

#-- Run command --#
# docker run --name alpaca -d -p 8787:8787 -e USER=user -e PASSWORD=password kamermanpr/alpaca-growth:v1.1

#-- Login to RStudio --#
# In your browser, navigate to: localhost:8787
# Username: user
# Password: password

#-- Get the verse rocker image --#

FROM rocker/tidyverse:4.0.4

MAINTAINER Peter Kamerman <peter.kamerman@gmail.com>

#-- Install extra packages --#

RUN Rscript -e "install.packages(c('performance', 'qqplotr', 'sjPlot', 'skimr', 'nlme', 'lme4', 'patchwork', 'ggrepel'), dependencies = TRUE)"

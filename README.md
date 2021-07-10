# Alpaca growth

[![DOI](https://zenodo.org/badge/383789985.svg)](https://zenodo.org/badge/latestdoi/383789985)

Investigation of body temperature and metabolic determinants of growth in alpacas.

## Bibliometric information


## Abstract


## Reproducibility

For reproducibility we have built a docker image with the environment used to run the scripts:  
[kamermanpr/alpaca-growth](https://hub.docker.com/repository/docker/kamermanpr/alpaca-growth)

### Using Docker to run the scripts

You need to have docker installed on your computer. To do so, go to [docker.com](https://www.docker.com/community-edition#/download) and follow the instructions for installing Docker for your operating system. Once Docker has been installed, follow the steps below, noting that Docker commands are entered in a terminal window (Linux and OSX/macOS) or command prompt window (Windows). 

#### Download the latest image

Enter: `docker pull kamermanpr/alpaca-growth:v1.1`

#### Run the container

Enter: `docker run --name alpaca -d -p 8787:8787 -e USER=user -e PASSWORD=password kamermanpr/alpaca-growth:v1.1`

#### Login to RStudio Server

- Open a web browser window and navigate to: `localhost:8787`

- Use the following login credentials: 
    - Username: _user_	
    - Password: _password_
    
#### Upload repository

- Go to the [alpaca growth](https://github.com/kamermanpr/alpaca-growth.git) repository on GitHub and select _Code_ and then _Download ZIP_.

- In the _Files_ tab on the lower right panel of RStudio, click **Upload**, located the zip file you downloaded and the click **OK**. The zip file will be uploaded and will automatically unzip, giving you access to all the content, including the analysis scripts, for the project.

- In the _Files_ tab, double-click the **alpaca-data.Rproj** file to ensure all the working directories are in order before running any of the scripts.

**Note:** The first time you _knit_ one of the _Rmd_ files, the generation of the PDF output will take some time as the system will install all the required _LaTeX_ packages for generating PDF documents. 

#### Shutting down

Once done, log out of RStudio Server and enter the following into a terminal to stop the docker container: `docker stop alpaca`. If you then want to remove the container, enter: `docker rm alpaca`. If you also want to remove the docker image you downloaded, enter: `docker rmi kamermanpr/alpaca-growth:v1.1`

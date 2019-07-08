NAME   := gcr.io/saation-palvelut/ajk-lomake
TAG    := $$(git log -1 --pretty=%h)
IMAGE  := ${NAME}:${TAG}
LATEST := ${NAME}:latest

docker-build:
	sudo docker build --build-arg EXECUTABLE=ajk-lomake-server -t ${IMAGE} .
	sudo docker tag ${IMAGE} ${LATEST}
 
docker-run:
	sudo docker run -it --publish 8080:8080 ${IMAGE}

docker-push:
	sudo docker push ${NAME}

docker-login:
	gcloud auth print-access-token | sudo docker login -u oauth2accesstoken --password-stdin https://gcr.io

deploy:
	gcloud beta run deploy ajk-lomake --image gcr.io/saation-palvelut/ajk-lomake --region us-central1

ghcid :
	ghcid -c 'cabal new-repl'

run :
	cabal new-run ajk-lomake-server

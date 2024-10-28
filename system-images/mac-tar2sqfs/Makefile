.DELETE_ON_ERROR:

all: docker-images/aarch64.tar docker-images/x86_64.tar

clean:
	rm -rf docker-images

docker-images:
	mkdir docker-images

docker-images/aarch64.tar: Dockerfile docker-images
	docker buildx build --tag start9/x_system/binfmt --platform=linux/arm64 -o type=docker,dest=docker-images/aarch64.tar .

docker-images/x86_64.tar: Dockerfile docker-images
	docker buildx build --tag start9/x_system/binfmt --platform=linux/amd64 -o type=docker,dest=docker-images/x86_64.tar .
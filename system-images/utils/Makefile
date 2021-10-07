.DELETE_ON_ERROR:

all: utils.tar

utils.tar: Dockerfile
	DOCKER_CLI_EXPERIMENTAL=enabled docker buildx build --tag start9/x_system/utils --platform=linux/arm64 -o type=docker,dest=utils.tar .

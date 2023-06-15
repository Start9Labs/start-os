
# Create the fake mounted drives
# Copy the files and folders to the fake mounted drives
(   
    cd ~/Projects/syncthing-wrapper &&
    docker run \
        --volume /tmp/start9:/start9 \
        --volume ~/Projects/start-os/libs/start_init:/start-init \
        --rm -it $(docker build -q .) sh \
)

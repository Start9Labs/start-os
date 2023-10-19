# StartOS Image Recipes

Code and `debos` recipes that are used to create the StartOS live and installer
images.

If you want to build a local image in the exact same environment used to build
official StartOS images, you can use the `run-local-build.sh` helper script:

```bash
# Prerequisites
sudo apt-get install -y debspawn
sudo mkdir -p /etc/debspawn/ && echo "AllowUnsafePermissions=true" | sudo tee /etc/debspawn/global.toml

# Get dpkg
mkdir -p overlays/startos/root
wget -O overlays/startos/root/startos_0.3.x-1_amd64.deb <dpkg_url>

# Build image
./run-local-build.sh
```

In order for the build to work properly, you will need debspawn >= 0.5.1, the
build may fail with prior versions.

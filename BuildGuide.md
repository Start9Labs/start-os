##### Initial Notes & Recommendations
* Due to issues to cross-compile the image from a desktop, this guide will take you step-by-step through the process of compiling EmbassyOS directly on a Raspberry Pi 4 (4GB or 8GB)
* This process will go faster if you have an SSD/NVMe USB drive available.
* This build guide does **not** require a large microSD card, especially if your final build wil be used on an SSD/NVMe USB drive.
* Basic know-how of linux commands and terminal use is recommended.
* Follow the guide carefully and do not skip any steps.

#### Build Guide
1. Flash [Raspberry Pi OS Lite](https://www.raspberrypi.org/software/operating-systems/) to a microSD and configure your raspi to boot from SSD/NVMe USB drive
   1. After flashing, create an empty text file called `ssh` in the `boot` partition of the microSD, then proceed with booting the raspi with the flashed microSD (check your router for the IP assigned to your raspi)
   1. Do the usual initial update/config
      ```
      sudo apt update
      sudo raspi-config
      ```
   1. Change `Advanced Options->Boot Order`
   1. Select `USB Boot` *(it will try to boot from microSD first if it's available)*
   1. Select `Finish`, then `Yes` to reboot
   1. After reboot, `sudo shutdown now` to power off the raspi and remove the microSD
  
2. Flash the *Raspi OS Lite* (from step 1) to your SSD/NVMe drive
   > :information_source: Don't worry about rootfs partition size (raspi will increase it for you on initial boot)
   
   > :information_source: Every time you re-flash your SSD/NVMe you need to first boot with a microSD and set *Boot Order* again

   1. Don't forget to create the empty `ssh` file
   1. Connect the drive (remember to remove the microSD) to the raspi and start it up
   1. Use `sudo raspi-config` to change the default password
   1. Optional: `sudo apt upgrade`
   1. Optional: `sudo nano /etc/apt/sources.list.d/vscode.list` comment the last line which contains `packages.microsoft.com`

3. Install GHC
   ```
   sudo apt update
   sudo apt install ghc
   
   #test:
   ghc --version
   
   #example of output:
   The Glorious Glasgow Haskell Compilation System, version 8.4.4
   ```

4. Compile Stack:
   1. Install Stack v2.1.3
      ```
      cd ~/
      wget -qO- https://raw.githubusercontent.com/commercialhaskell/stack/v2.1.3/etc/scripts/get-stack.sh | sh
      
      #test with
      stack --version
      
      #example output:
      Version 2.1.3, Git revision 636e3a759d51127df2b62f90772def126cdf6d1f (7735 commits) arm hpack-0.31.2
      ```
    
   1. Use current Stack to compile Stack v2.5.1:
      ```
      git clone --depth 1 --branch v2.5.1 https://github.com/commercialhaskell/stack.git
      cd stack

      #Build (>=3.5h total... You might need to use VPN to avoid timeout issues)
      #Note: Run it again if there were any "ConnectionFailure" messages
      stack build --stack-yaml=stack-ghc-84.yaml --system-ghc
      
      #Install
      stack install --stack-yaml=stack-ghc-84.yaml --system-ghc
      export PATH=~/.local/bin:$PATH
      ```

5. Clone EmbassyOS & *make* the `agent`:
   1. First attempt
      > :information_source: The first time you run **make** you'll get an error
      
      ```
      sudo apt install llvm-9 libgmp-dev
      cd ~/
      git clone https://github.com/Start9Labs/embassy-os.git
      cd embassy-os/
      
      #Note: This will install ghc-8.10.2, then attempt to build but will give errors (in next steps we deal with errors)
      make agent
      ```
   1. Confirm your cpu info
      ```
      cat /proc/cpuinfo
      ```
   1. If your "Hardware" is [BCM2711](https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2711/README.md) then:
      1. Change `C compiler flags` to `-marm -mcpu=cortex-a72` in the GHC settings:
         ```
         nano ~/.stack/programs/arm-linux/ghc-8.10.2/lib/ghc-8.10.2/settings
         ```
   1. To prevent gcc errors we delete the `setup-exe-src` folder
      ```
      rm -rf ~/.stack/setup-exe-src/
      ```
   1. Run `make agent` again (will take a few hours)
      > :information_source: Ignore warnings at the end (for example: "Warning: Failed to decode module interface") 

6. Install requirements for step 7
   1. Install NVM
      ```
      cd ~/
      curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
      #close and re-open connection to raspi and test:
      nvm --version
      ```
   1. Install Node.js & NPM
      ```
      nvm install node
      ```
   1. Install Ionic CLI
      ```
      npm install -g @ionic/cli
      ```
   1. Install Rust
      ```
      cd ~/
      curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs -o- | bash
      
      #Choose option 1
      source $HOME/.cargo/env

      #Check rust & cargo versions
      rustc --version
      cargo --version
      ```
   1. Install Dependencies
      ```
      sudo apt-get install -y build-essential openssl libssl-dev libc6-dev clang libclang-dev libavahi-client-dev upx
      ```

7. Finally, getting to build the **.img**
   1. Build `appmgr` & `lifeline`
      ```
      cd ~/embassy-os/appmgr
      cargo update
      
      cargo build --release --features=production
      /usr/bin/arm-linux-gnueabihf-strip ~/embassy-os/appmgr/target/release/appmgr
      
      cd ~/embassy-os/lifeline && cargo build --release
      /usr/bin/arm-linux-gnueabihf-strip ~/embassy-os/lifeline/target/release/lifeline
      ```

   1. Prep `make_image.sh`
      ```
      chmod +x ~/embassy-os/make_image.sh
      ```
   
   1. Build the `embassy.img`
      ```
      cd ~/embassy-os
      make
      
      #Wait for the "DONE!" message and take note of your product_key
      exit
      ```
8. Flash the `embassy.img` to a microSD
   1. Copy `embassy.img` from the raspi to your PC with scp
      ```
      scp pi@raspi_IP:~/embassy-os/embassy.img .
      ```
   1. Connect to raspi again to do `sudo shutdown now`, after a complete shutdown disconnect SSD/NVMe drive
   1. Flash `embassy.img` to a microSD (do this before flashing to the SSD/NVMe, to be sure it works)

9. Prepare for initial setup
   1. Boot raspi using flashed microSD
   1. Connect via SSH with user `pi` and password `raspberry` and use `sudo raspi-config` to change the password
   1. Prep & run `/root/setup.sh`
      > :information_source: Here we also remove line `passwd -l pi` (this will allow you to ssh into raspi in the event that setup failed)
      ```
      #We remove specific lines
      sudo grep -Ev 'systemctl disable setup.service|reboot|passwd -l pi' /root/setup.sh | sudo tee /root/setup.sh.tmp >/dev/null
      sudo cat /root/setup.sh.tmp | sudo tee /root/setup.sh >/dev/null
      
      #Run setup.sh
      sudo sh /root/setup.sh
      ```
   1. Enable services (unit files were only copied over and still need to be enabled)
      ```
      sudo systemctl enable lifeline.service
      sudo systemctl enable agent.service
      
      sudo reboot
      ```
   1. After a few minutes, the raspi should reboot itself and make it's first sounds.
      > :information_source: If needed, you can check the `agent` log with: `journalctl -u agent -ef`
      * Short Beep = Raspi finished reboot/startup sequence.
      * Mario "Coin" = EmbassyOS has started.
      * Mario "Death" = Raspi is about to Shutdown/Reboot.
      * Mario "Power Up" = EmbassyOS update sequence.
      * Beethoven = Update failed :(
   1. Proceed with the [initial setup process of EmbassyOS](https://docs.start9labs.com/user-manual/initial-setup.html)
   1. If all went well you can safely flash `embassy.img` to an SSD/NVMe and repeat step 9


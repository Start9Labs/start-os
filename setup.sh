#!/bin/bash
mkdir -p /root/volumes
mkdir -p /root/tmp/appmgr
mkdir -p /root/agent
mkdir -p /root/appmgr/tor
apt-get update -y
apt-get install -y tor
apt-get install -y iotop
apt-get install -y bmon
apt-get install -y libavahi-client3
apt-get install -y libsecp256k1-0
apt-get install -y docker.io needrestart-
mv /root/setup.sh /root/setup-s1.sh.done
cat <<EOT >> /root/setup-s2.sh
#!/bin/bash
apt-get update -y
apt-get install -y tor
apt-get install -y iotop
apt-get install -y bmon
apt-get install -y libavahi-client3
apt-get install -y libsecp256k1-0
apt-get install -y docker.io needrestart-
apt-get autoremove -y
systemctl enable lifeline
systemctl enable agent
systemctl enable ssh
systemctl enable avahi-daemon
passwd -l root
passwd -l pi
sync
systemctl disable setup
mv /root/setup-s2.sh /root/setup-s2.sh.done
reboot
EOT
chmod +x /root/setup-s2.sh
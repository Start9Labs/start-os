# SSH

Like other Linux distributions, StartOS allows you to go "under-the-hood" via Secure Shell Protocol (SSH).

> [!WARNING]
> Accessing your server via SSH is considered advanced. Please use caution, you can cause permanent damage to your server, potentially resulting in loss of data.

## Watch The Video

<div class="yt-video" data-id="qi5H_JzcRVk" data-title="SSH"></div>

## User and privileges

The SSH user is `start9`, not `root`. Root login is disabled. The `start9` user has `sudo` privileges, so commands requiring root should use `sudo`. There is no need to run `sudo -i` or `sudo su`.

## Using your StartOS Master Password

1.  Open a terminal on your client device and enter:

        ssh start9@SERVER-HOSTNAME

    Replace `SERVER-HOSTNAME` with your server's `your-server-name.local` address.

1.  The first time you connect, you will see something like this:

    ```
    The authenticity of host 'your-server-name.local (192.168.1.175)' can't be established.
    ED25519 key fingerprint is SHA256:BgYhzyIDbshm3annI1cfySd8C4/lh6Gfk2Oi3FdIVAa.
    This key is not known by any other names.
    Are you sure you want to continue connecting (yes/no/[fingerprint])?
    ```

    Type `yes` and hit Enter to start trusting the server's SSH public key.

1.  Enter your StartOS master password.

## Using SSH Keys

### Create an SSH key

If you don't already have an SSH key pair on your laptop or desktop, open a terminal and run:

```bash
ssh-keygen -t ed25519
```

Press Enter to accept the default file location, and optionally set a passphrase. Your public key will be at `~/.ssh/id_ed25519.pub`.

### Add your key to StartOS

1.  Copy your public key to clipboard:

        cat ~/.ssh/id_ed25519.pub

1.  In the StartOS UI, go to `System > SSH`

1.  Click `Add Key`, paste in your key and click `Save`

1.  Open a terminal on your client device and enter:

        ssh start9@SERVER-HOSTNAME

    Replace `SERVER-HOSTNAME` with your server's `your-server-name.local` address.

1.  Enter your key's passphrase (if any)

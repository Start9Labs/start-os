# Setting up clearnet for a service interface

NOTE: this guide is for HTTPS only! Other configurations may require a more bespoke setup depending on the service. Please consult the service documentation or the Start9 Community for help with non-HTTPS applications

## Initialize ACME certificate generation

The following command will register your device with an ACME certificate provider, such as letsencrypt

This only needs to be done once.

```
start-cli net acme init --provider=letsencrypt --contact="mailto:me@drbonez.dev"
```

- `provider` can be `letsencrypt`, `letsencrypt-staging` (useful if you're doing a lot of testing and want to avoid being rate limited), or the url of any provider that supports the [RFC8555](https://datatracker.ietf.org/doc/html/rfc8555) ACME api
- `contact` can be any valid contact url, typically `mailto:` urls. it can be specified multiple times to set multiple contacts

## Whitelist a domain for ACME certificate acquisition

The following command will tell the OS to use ACME certificates instead of system signed ones for the provided url. In this example, `testing.drbonez.dev`

This must be done for every domain you wish to host on clearnet.

```
start-cli net acme domain add "testing.drbonez.dev"
```

## Forward clearnet port

Go into your router settings, and map port 443 on your router to port 5443 on your start-os device. This one port should cover most use cases

## Add domain to service host

The following command will tell the OS to route https requests from the WAN to the provided hostname to the specified service. In this example, we are adding `testing.drbonez.dev` to the host `ui-multi` on the package `hello-world`. To see a list of available host IDs for a given package, run `start-cli package host <PACKAGE> list`

This must be done for every domain you wish to host on clearnet.

```
start-cli package host hello-world address ui-multi add testing.drbonez.dev
```

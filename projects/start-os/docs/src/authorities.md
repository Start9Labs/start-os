<!-- # Certificate Authorities

## Background

Your server provides its own Root Certificate Authority (Root CA) and uses it to issues SSL/TLS certificates. Devices that have downloaded and trusted your server's Root CA can access these service interfaces over encrypted, HTTPS connections.

## Use Case

If you want to host service interfaces on the public Internet without requiring visitors to download and trust your server's Root CA, you will need to obtain certificates from a 3rd-party Certificate Authority that is already trusted by their devices.

> [!NOTE]
> 3rd-party Certificate Authorities only issue certificates for clearnet (.com, .org, etc) domains. They _do not_ issue certificates for IP addresses, Local (.local) domains, or Tor (.onion) domains. Your server's Root CA will always be used to issue certificates for these types of addresses.

## Adding a Certificate Authority

StartOS uses the [Automatic Certificate Management Environment (ACME) protocol](https://en.wikipedia.org/wiki/Automatic_Certificate_Management_Environment) to obtain SSL/TLS certificates from 3rd-party Certificate Authorities, allowing visitors to access your domains over encrypted, HTTPS connections.

1. Navigate to `System > Certificate Authorities` and click "Add".

1. Select a Certificate Authority to add. StartOS has built-in support for `Let's Encrypt` and `Let's Encrypt (Staging)`. Advanced users may add a custom ACME provider.

> [!NOTE]
> - `Let's Encrypt` should only be used for production. If you use it frequently for testing, your IP address may get rate-limited, preventing you from obtaining certificates.
> - `Let's Encrypt (Staging)` should only be used for testing.

1. Provide a contact email address. This is required for the Certificate Authority to generate a certificate. -->

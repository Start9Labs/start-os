# Tor

Access your server over the Tor network using `.onion` addresses. Tor provides anonymous, censorship-resistant connections. How you use an onion address determines whether it functions as private or public access: keep the address secret and it's a private tunnel; publish it without associating your identity and you host anonymously; publish it with your identity and you host an identified but censorship-resistant service.

## WATCH THE VIDEO

<div class="yt-video" data-id="hQdexuNvshk" data-title="TOR"></div>

## Setting Up Tor

Tor is not included in StartOS by default. To use Tor, you must install the **Tor** service from the marketplace.

1. Go to the Marketplace and install the **Tor** service.

1. Start the Tor service and wait for it to become healthy.

## Creating and Deleting Onion Services

Once Tor is installed, a **Tor** addresses table appears for each service [interface](interfaces.md), under the **Interfaces** tab.

1. Navigate to the service you want to expose over Tor.

1. Open the **Interfaces** tab and expand the interface.

1. In the Tor table, click **Add Onion Service** to create a `.onion` address for that interface.

1. To delete an onion address, click the overflow menu on that row and select **Delete**.

> [!TIP]
> When creating an onion service, you can upload a private key to use a vanity address. For instructions on generating a vanity address, see [here](https://community.torproject.org/onion-services/advanced/vanity-addresses/).

## HTTP vs HTTPS (SSL)

When creating an onion service, you can choose whether to enable SSL (HTTPS). Because Tor is already an encrypted protocol, HTTP is perfectly safe and is the recommended default — it means neither you nor anyone you share the address with needs to [trust your server's Root CA](trust-ca.md). Only enable SSL if the client application naively requires HTTPS (for example, native Bitwarden apps that enforce HTTPS without considering Tor).

## Connecting over Tor

### Using a Tor Browser

You can connect to your server and installed services from anywhere in the world, privately and anonymously, by visiting their unique `http://....onion` URLs from any Tor-enabled browser.

> [!TIP]
> Recommended Browsers
>
> - Mac, Linux, Windows, Android/Graphene: [Tor Browser](https://torproject.org/download)
> - iOS: [Onion Browser](https://onionbrowser.com)

### Running Tor in the _Background_ on your Phone/Laptop

By running Tor in the background on your phone or laptop, certain apps can connect over Tor, even if the apps themselves do not natively support Tor.

For instructions specific to your device's operating system, use a search engine or AI. This capability is well documented.

## Public Hosting

Onion addresses can also serve as a public hosting method. Unlike clearnet domains, onion addresses do not require purchasing a domain, configuring DNS, or opening ports. Anyone with a Tor-enabled browser can reach them.

- **Censorship resistance**. Onion services cannot be taken down by domain registrars, DNS providers, or ISPs. As long as your server is running, the address is reachable.

- **Anonymity** (if done carefully). If you publish an onion address without associating it with your identity, observers can access your service but cannot determine who operates it. Achieving true anonymity requires careful operational security — for example, never linking the address to your real identity, and not leaking metadata that could be correlated.

- **No infrastructure dependencies**. You do not need a static IP, a domain name, or port forwarding. Tor handles routing entirely through its overlay network.

The difference between private and public Tor access is simply in how you use the addresses: keeping them secret vs. sharing them publicly.

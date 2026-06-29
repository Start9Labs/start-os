export default {
  "instance-name": {
    type: "string",
    name: "SearXNG Instance Name",
    description:
      "Enter a name for your SearXNG instance. This is the name that will be listed if you want to share your SearXNG engine publicly.",
    nullable: false,
    default: "My SearXNG Engine",
    placeholder: "Uncle Jim SearXNG Engine",
  },
  "tor-url": {
    name: "Enable Tor address as the base URL",
    description:
      "Activates the utilization of a .onion address as the primary URL, particularly beneficial for publicly hosted instances over the Tor network.",
    type: "boolean",
    default: false,
  },
  "enable-metrics": {
    name: "Enable Stats",
    description:
      "Your SearXNG instance will collect anonymous stats about its own usage and performance. You can view these metrics by appending `/stats` or `/stats/errors` to your SearXNG URL.",
    type: "boolean",
    default: true,
  }, //,
  // "email-address": {
  //   "type": "string",
  //   "name": "Email Address",
  //   "description": "Your Email address - required to create an SSL certificate.",
  //   "nullable": false,
  //   "default": "youremail@domain.com",
  // },
  // "public-host": {
  //   "type": "string",
  //   "name": "Public Domain Name",
  //   "description": "Enter a domain name here if you want to share your SearXNG engine publicly. You will also need to modify your domain name's DNS settings to point to your Start9 server.",
  //   "nullable": true,
  //   "placeholder": "https://search.mydomain.com"
  // }
}

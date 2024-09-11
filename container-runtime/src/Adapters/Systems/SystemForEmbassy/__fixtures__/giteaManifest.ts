export default {
  "eos-version": "0.3.5.1",
  id: "gitea",
  "git-hash": "91fada3edf30357a2e75c281d32f8888c87fcc2d\n",
  title: "Gitea",
  version: "1.22.0",
  description: {
    short: "A painless self-hosted Git service.",
    long: "Gitea is a community managed lightweight code hosting solution written in Go. It is published under the MIT license.\n",
  },
  assets: {
    license: "LICENSE",
    instructions: "instructions.md",
    icon: "icon.png",
    "docker-images": null,
    assets: null,
    scripts: null,
  },
  build: ["make"],
  "release-notes":
    "* Upstream code update\n* Fix deprecated config options\n* Full list of upstream changes available [here](https://github.com/go-gitea/gitea/compare/v1.21.8...v1.22.0)\n",
  license: "MIT",
  "wrapper-repo": "https://github.com/Start9Labs/gitea-startos",
  "upstream-repo": "https://github.com/go-gitea/gitea",
  "support-site": "https://docs.gitea.io/en-us/",
  "marketing-site": "https://gitea.io/en-us/",
  "donation-url": null,
  alerts: {
    install: null,
    uninstall: null,
    restore: null,
    start: null,
    stop: null,
  },
  main: {
    type: "docker",
    image: "main",
    system: false,
    entrypoint: "/usr/local/bin/docker_entrypoint.sh",
    args: [],
    inject: false,
    mounts: { main: "/data" },
    "io-format": null,
    "sigterm-timeout": null,
    "shm-size-mb": null,
    "gpu-acceleration": false,
  },
  "health-checks": {
    "user-signups-off": {
      name: "User Signups Off",
      "success-message": null,
      type: "script",
      args: [],
      timeout: null,
    },
    web: {
      name: "Web & Git HTTP Tor Interfaces",
      "success-message":
        "Gitea is ready to be visited in a web browser and git can be used with SSH over TOR.",
      type: "script",
      args: [],
      timeout: null,
    },
  },
  config: {
    get: { type: "script", args: [] },
    set: { type: "script", args: [] },
  },
  properties: { type: "script", args: [] },
  volumes: { main: { type: "data" } },
  interfaces: {
    main: {
      name: "Web UI / Git HTTPS/SSH",
      description:
        "Port 80: Browser Interface and HTTP Git Interface / Port 22: Git SSH Interface",
      "tor-config": { "port-mapping": { "22": "22", "80": "3000" } },
      "lan-config": { "443": { ssl: true, internal: 3000 } },
      ui: true,
      protocols: ["tcp", "http", "ssh", "git"],
    },
  },
  backup: {
    create: {
      type: "docker",
      image: "compat",
      system: true,
      entrypoint: "compat",
      args: ["duplicity", "create", "/mnt/backup", "/root/data"],
      inject: false,
      mounts: { BACKUP: "/mnt/backup", main: "/root/data" },
      "io-format": "yaml",
      "sigterm-timeout": null,
      "shm-size-mb": null,
      "gpu-acceleration": false,
    },
    restore: {
      type: "docker",
      image: "compat",
      system: true,
      entrypoint: "compat",
      args: ["duplicity", "restore", "/mnt/backup", "/root/data"],
      inject: false,
      mounts: { BACKUP: "/mnt/backup", main: "/root/data" },
      "io-format": "yaml",
      "sigterm-timeout": null,
      "shm-size-mb": null,
      "gpu-acceleration": false,
    },
  },
  migrations: {
    from: { "*": { type: "script", args: ["from"] } },
    to: { "*": { type: "script", args: ["to"] } },
  },
  actions: {},
  dependencies: {},
  containers: null,
  replaces: [],
  "hardware-requirements": {
    device: {},
    ram: null,
    arch: ["x86_64", "aarch64"],
  },
}
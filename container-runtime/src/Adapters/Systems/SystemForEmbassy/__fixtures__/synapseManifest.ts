export default {
  id: "synapse",
  title: "Synapse",
  version: "1.98.0",
  "release-notes":
    "* Upstream code update\n* Synapse Admin updated to the latest version - ([full changelog](https://github.com/Awesome-Technologies/synapse-admin/compare/0.8.7...0.9.1))\n* Instructions update\n* Updated package and upstream repositories links\n* Full list of upstream changes available [here](https://github.com/element-hq/synapse/compare/v1.95.1...v1.98.0)\n",
  license: "Apache-2.0",
  "wrapper-repo": "https://github.com/Start9Labs/synapse-startos",
  "upstream-repo": "https://github.com/element-hq/synapse",
  "support-site": "https://github.com/element-hq/synapse/issues",
  "marketing-site": "https://matrix.org/",
  build: ["make"],
  description: {
    short:
      "Synapse is a battle-tested implementation of the Matrix protocol, the killer of all messaging apps.",
    long: "Synapse is the battle-tested, reference implementation of the Matrix protocol. Matrix is a next-generation, federated, full-featured, encrypted, independent messaging system. There are no trusted third parties involved. (see matrix.org for details).",
  },
  assets: {
    license: "LICENSE",
    icon: "icon.png",
    instructions: "instructions.md",
  },
  main: {
    type: "docker",
    image: "main",
    entrypoint: "docker_entrypoint.sh",
    args: [],
    mounts: {
      main: "/data",
      cert: "/mnt/cert",
      "admin-cert": "/mnt/admin-cert",
    },
  },
  "health-checks": {
    federation: {
      name: "Federation",
      type: "docker",
      image: "main",
      system: false,
      entrypoint: "check-federation.sh",
      args: [],
      mounts: {},
      "io-format": "json",
      inject: true,
    },
    "synapse-admin": {
      name: "Admin interface",
      "success-message":
        "Synapse Admin is ready to be visited in a web browser.",
      type: "docker",
      image: "main",
      system: false,
      entrypoint: "check-ui.sh",
      args: [],
      mounts: {},
      "io-format": "yaml",
      inject: true,
    },
    "user-signups-off": {
      name: "User Signups Off",
      type: "docker",
      image: "main",
      system: false,
      entrypoint: "user-signups-off.sh",
      args: [],
      mounts: {},
      "io-format": "yaml",
      inject: true,
    },
  },
  config: {
    get: {
      type: "script",
    },
    set: {
      type: "script",
    },
  },
  properties: {
    type: "script",
  },
  volumes: {
    main: {
      type: "data",
    },
    cert: {
      type: "certificate",
      "interface-id": "main",
    },
    "admin-cert": {
      type: "certificate",
      "interface-id": "admin",
    },
  },
  alerts: {
    start:
      "After your first run, Synapse needs a little time to establish a stable TOR connection over federation. We kindly ask for your patience during this process. Remember, great things take time! 🕒",
  },
  interfaces: {
    main: {
      name: "Homeserver Address",
      description:
        "Used by clients and other servers to connect with your homeserver",
      "tor-config": {
        "port-mapping": {
          "80": "80",
          "443": "443",
          "8448": "8448",
        },
      },
      ui: false,
      protocols: ["tcp", "http", "matrix"],
    },
    admin: {
      name: "Admin Portal",
      description: "A web application for administering your Synapse server",
      "tor-config": {
        "port-mapping": {
          "80": "8080",
          "443": "4433",
        },
      },
      "lan-config": {
        "443": {
          ssl: true,
          internal: 8080,
        },
      },
      ui: true,
      protocols: ["tcp", "http"],
    },
  },
  dependencies: {},
  backup: {
    create: {
      type: "docker",
      image: "compat",
      system: true,
      entrypoint: "compat",
      args: ["duplicity", "create", "/mnt/backup", "/data"],
      mounts: {
        BACKUP: "/mnt/backup",
        main: "/data",
      },
    },
    restore: {
      type: "docker",
      image: "compat",
      system: true,
      entrypoint: "compat",
      args: ["duplicity", "restore", "/mnt/backup", "/data"],
      mounts: {
        BACKUP: "/mnt/backup",
        main: "/data",
      },
    },
  },
  actions: {
    "reset-first-user": {
      name: "Reset First User",
      description:
        "This action will reset the password of the first user in your database to a random value.",
      "allowed-statuses": ["stopped"],
      implementation: {
        type: "docker",
        image: "main",
        system: false,
        entrypoint: "docker_entrypoint.sh",
        args: ["reset-first-user"],
        mounts: {
          main: "/data",
        },
        "io-format": "json",
      },
    },
  },
  migrations: {
    from: {
      "*": {
        type: "script",
        args: ["from"],
      },
    },
    to: {
      "*": {
        type: "script",
        args: ["to"],
      },
    },
  },
}

import { DependencyErrorType, DockerIoFormat, Manifest, PackageDataEntry, PackageMainStatus, PackageState } from 'src/app/services/patch-db/data-model'
import { MarketplacePkg, Metric, NotificationLevel, RR, ServerNotifications } from './api.types'

export module Mock {

  export const MarketplaceEos: RR.GetMarketplaceEOSRes = {
    version: '0.3.1',
    headline: 'Our biggest release ever.',
    'release-notes': {
      '0.3.1': 'Some **Markdown** release _notes_ for 0.3.1',
      '0.3.0': 'Some **Markdown** release _notes_ from a prior version',
    },
  }

  export const ReleaseNotes: RR.GetReleaseNotesRes = {
    '0.19.2': 'Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a line in section 1.10.32.',
    '0.19.1': 'release notes for Bitcoin 0.19.1',
    '0.19.0': 'release notes for Bitcoin 0.19.0',
  }

  export const MockManifestBitcoind: Manifest = {
    id: 'bitcoind',
    title: 'Bitcoin Core',
    version: '0.21.0',
    description: {
      short: 'A Bitcoin full node by Bitcoin Core.',
      long: 'Bitcoin is a decentralized consensus protocol and settlement network.',
    },
    'release-notes': 'Taproot, Schnorr, and more.',
    license: 'MIT',
    'wrapper-repo': 'https://github.com/start9labs/bitcoind-wrapper',
    'upstream-repo': 'https://github.com/bitcoin/bitcoin',
    'support-site': 'https://bitcoin.org',
    'marketing-site': 'https://bitcoin.org',
    'donation-url': 'https://start9.com',
    alerts: {
      install: 'Bitcoin can take over a week to sync.',
      uninstall: 'Chain state will be lost, as will any funds stored on your Bitcoin Core waller that have not been backed up.',
      restore: null,
      start: null,
      stop: 'Stopping Bitcoin is bad for your health.',
    },
    main: {
      type: 'docker',
      image: '',
      system: true,
      entrypoint: '',
      args: [''],
      mounts: {},
      'io-format': DockerIoFormat.Yaml,
      inject: false,
      'shm-size': '',
    },
    'health-checks': {},
    config: null,
    volumes: {},
    'min-os-version': '0.2.12',
    interfaces: {
      ui: {
        name: 'Node Visualizer',
        description: 'Web application for viewing information about your node and the Bitcoin network.',
        ui: true,
        'tor-config': {
          'port-mapping': {},
        },
        'lan-config': {},
        protocols: [],
      },
      rpc: {
        name: 'RPC',
        description: 'Used by wallets to interact with your Bitcoin Core node.',
        ui: false,
        'tor-config': {
          'port-mapping': {},
        },
        'lan-config': {},
        protocols: [],
      },
      p2p: {
        name: 'P2P',
        description: 'Used by other Bitcoin nodes to communicate and interact with your node.',
        ui: false,
        'tor-config': {
          'port-mapping': {},
        },
        'lan-config': {},
        protocols: [],
      },
    },
    backup: {
      create: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [''],
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
      },
      restore: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [''],
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
      },
    },
    migrations: null,
    actions: {
      resync: {
        name: 'Resync Blockchain',
        description: 'Use this to resync the Bitcoin blockchain from genesis',
        warning: 'This will take a couple of days.',
        'allowed-statuses': [PackageMainStatus.Running, PackageMainStatus.Stopped],
        implementation: {
          type: 'docker',
          image: '',
          system: true,
          entrypoint: '',
          args: [''],
          mounts: {},
          'io-format': DockerIoFormat.Yaml,
          inject: false,
          'shm-size': '',
        },
        'input-spec': null,
      },
    },
    permissions: {},
    dependencies: {},
  }

  export const MockManifestLnd: Manifest = {
    id: 'lnd',
    title: 'LND',
    version: '0.11.1',
    description: {
      short: 'A bolt spec compliant client.',
      long: 'More info about LND. More info about LND. More info about LND.',
    },
    'release-notes': 'Dual funded channels!',
    license: 'MIT',
    'wrapper-repo': 'https://github.com/start9labs/lnd-wrapper',
    'upstream-repo': 'https://github.com/lightningnetwork/lnd',
    'support-site': 'https://lightning.engineering/',
    'marketing-site': 'https://lightning.engineering/',
    'donation-url': null,
    alerts: {
      install: null,
      uninstall: null,
      restore: 'If this is a duplicate instance of the same LND node, you may loose your funds.',
      start: 'Starting LND is good for your health.',
      stop: null,
    },
    main: {
      type: 'docker',
      image: '',
      system: true,
      entrypoint: '',
      args: [''],
      mounts: {},
      'io-format': DockerIoFormat.Yaml,
      inject: false,
      'shm-size': '',
    },
    'health-checks': {},
    config: null,
    volumes: {},
    'min-os-version': '0.2.12',
    interfaces: {
      rpc: {
        name: 'RPC interface',
        description: 'Good for connecting to your node at a distance.',
        ui: true,
        'tor-config': {
          'port-mapping': {},
        },
        'lan-config': {
          44: {
            ssl: true,
            mapping: 33,
          },
        },
        protocols: [],
      },
      grpc: {
        name: 'GRPC',
        description: 'Certain wallet use grpc.',
        ui: false,
        'tor-config': {
          'port-mapping': {},
        },
        'lan-config': {
          66: {
            ssl: true,
            mapping: 55,
          },
        },
        protocols: [],
      },
    },
    backup: {
      create: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [''],
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
      },
      restore: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [''],
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
      },
    },
    migrations: null,
    actions: {
      resync: {
        name: 'Resync Network Graph',
        description: 'Your node will resync its network graph.',
        warning: 'This will take a couple hours.',
        'allowed-statuses': [PackageMainStatus.Running],
        implementation: {
          type: 'docker',
          image: '',
          system: true,
          entrypoint: '',
          args: [''],
          mounts: {},
          'io-format': DockerIoFormat.Yaml,
          inject: false,
          'shm-size': '',
        },
        'input-spec': {
          label: {
            type: 'string',
            name: 'Name of Resync',
            nullable: false,
            masked: false,
            copyable: false,
          },
        },
      },
    },
    permissions: {},
    dependencies: {
      'bitcoind': {
        version: '=0.21.0',
        description: 'LND needs bitcoin to live.',
        optional: null,
        recommended: true,
        critical: true,
        config: {
          check: {
            type: 'docker',
            image: 'alpine',
            system: true,
            entrypoint: 'true',
            args: [],
            mounts: {},
            'io-format': DockerIoFormat.Cbor,
            inject: false,
            "shm-size": '10m'
          },
          "auto-configure": {
            type: 'docker',
            image: 'alpine',
            system: true,
            entrypoint: 'cat',
            args: [],
            mounts: {},
            'io-format': DockerIoFormat.Cbor,
            inject: false,
            "shm-size": '10m'
          }
        },
      },
      'bitcoin-proxy': {
        version: '>=0.2.2',
        description: 'As long as Bitcoin is pruned, LND needs Bitcoin Proxy to fetch block over the P2P network.',
        optional: null,
        recommended: true,
        critical: true,
        config: {
          check: {
            type: 'docker',
            image: 'alpine',
            system: true,
            entrypoint: 'true',
            args: [],
            mounts: {},
            'io-format': DockerIoFormat.Cbor,
            inject: false,
            "shm-size": '10m'
          },
          "auto-configure": {
            type: 'docker',
            image: 'alpine',
            system: true,
            entrypoint: 'cat',
            args: [],
            mounts: {},
            'io-format': DockerIoFormat.Cbor,
            inject: false,
            "shm-size": '10m'
          }
        },
      },
    },
  }

  export const MockManifestBitcoinProxy: Manifest = {
    id: 'bitcoin-proxy',
    title: 'Bitcoin Proxy',
    version: '0.2.2',
    description: {
      short: 'A super charger for your Bitcoin node.',
      long: 'More info about Bitcoin Proxy. More info about Bitcoin Proxy. More info about Bitcoin Proxy.',
    },
    'release-notes': 'Even better support for Bitcoin and wallets!',
    license: 'MIT',
    'wrapper-repo': 'https://github.com/start9labs/btc-rpc-proxy-wrapper',
    'upstream-repo': 'https://github.com/Kixunil/btc-rpc-proxy',
    'support-site': '',
    'marketing-site': '',
    'donation-url': 'https://start9.com',
    alerts: {
      install: null,
      uninstall: null,
      restore: null,
      start: null,
      stop: null,
    },
    main: {
      type: 'docker',
      image: '',
      system: true,
      entrypoint: '',
      args: [''],
      mounts: {},
      'io-format': DockerIoFormat.Yaml,
      inject: false,
      'shm-size': '',
    },
    'health-checks': {},
    config: null,
    volumes: {},
    'min-os-version': '0.2.12',
    interfaces: {
      rpc: {
        name: 'RPC interface',
        description: 'Good for connecting to your node at a distance.',
        ui: true,
        'tor-config': {
          'port-mapping': {},
        },
        'lan-config': {
          44: {
            ssl: true,
            mapping: 33,
          },
        },
        protocols: [],
      },
    },
    backup: {
      create: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [''],
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
      },
      restore: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [''],
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
      },
    },
    migrations: null,
    actions: {},
    permissions: {},
    dependencies: {
      'bitcoind': {
        version: '>=0.20.0',
        description: 'Bitcoin Proxy requires a Bitcoin node.',
        optional: null,
        recommended: true,
        critical: false,
        config: {
          check: {
            type: 'docker',
            image: 'alpine',
            system: true,
            entrypoint: 'true',
            args: [],
            mounts: {},
            'io-format': DockerIoFormat.Cbor,
            inject: false,
            "shm-size": '10m'
          },
          "auto-configure": {
            type: 'docker',
            image: 'alpine',
            system: true,
            entrypoint: 'cat',
            args: [],
            mounts: {},
            'io-format': DockerIoFormat.Cbor,
            inject: false,
            "shm-size": '10m'
          }
        },
      },
    },
  }

  export const MarketplacePkgs: {
    [id: string]: {
      [version: string]: MarketplacePkg
    }
  } = {
    'bitcoind': {
      '0.19.2': {
        icon: 'assets/img/service-icons/bitcoind.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.19.0',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': {},
      },
      '0.20.0': {
        icon: 'assets/img/service-icons/bitcoind.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.20.0',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': {},
      },
      '0.21.0': {
        icon: 'assets/img/service-icons/bitcoind.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.21.0',
          'release-notes': 'Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a line in section 1.10.32.Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a line in section 1.10.32.Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a line in section 1.10.32.Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a line in section 1.10.32.Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a line in section 1.10.32.',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': {},
      },
      'latest': {
        icon: 'assets/img/service-icons/bitcoind.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          'release-notes': 'Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a line in section 1.10.32.Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a line in section 1.10.32.Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a line in section 1.10.32.Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a line in section 1.10.32.Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, "Lorem ipsum dolor sit amet..", comes from a line in section 1.10.32.',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': {},
      },
    },
    'lnd': {
      '0.11.0': {
        icon: 'assets/img/service-icons/lnd.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestLnd,
          version: '0.11.0',
          'release-notes': 'release notes for LND 0.11.0',
        },
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        'dependency-metadata': {
          'bitcoind': {
            title: 'Bitcoin Core',
            icon: 'assets/img/service-icons/bitcoind.png',
          },
          'bitcoin-proxy': {
            title: 'Bitcoin Proxy',
            icon: 'assets/img/service-icons/bitcoin-proxy.png',
          },
        },
      },
      '0.11.1': {
        icon: 'assets/img/service-icons/lnd.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestLnd,
          version: '0.11.1',
          'release-notes': 'release notes for LND 0.11.0',
        },
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        'dependency-metadata': {
          'bitcoind': {
            title: 'Bitcoin Core',
            icon: 'assets/img/service-icons/bitcoind.png',
          },
          'bitcoin-proxy': {
            title: 'Bitcoin Proxy',
            icon: 'assets/img/service-icons/bitcoin-proxy.png',
          },
        },
      },
      'latest': {
        icon: 'assets/img/service-icons/lnd.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: Mock.MockManifestLnd,
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        'dependency-metadata': {
          'bitcoind': {
            title: 'Bitcoin Core',
            icon: 'assets/img/service-icons/bitcoind.png',
          },
          'bitcoin-proxy': {
            title: 'Bitcoin Proxy',
            icon: 'assets/img/service-icons/bitcoin-proxy.png',
          },
        },
      },
    },
    'bitcoin-proxy': {
      'latest': {
        icon: 'assets/img/service-icons/bitcoin-proxy.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: Mock.MockManifestBitcoinProxy,
        categories: ['bitcoin'],
        versions: ['0.2.2'],
        'dependency-metadata': {
          'bitcoind': {
            title: 'Bitcoin Core',
            icon: 'assets/img/service-icons/bitcoind.png',
          },
        },
      },
    },
  }

  export const MarketplacePkgsList: RR.GetMarketplacePackagesRes = Object.values(Mock.MarketplacePkgs).map(service => service['latest'])

  export const bitcoinproxy: PackageDataEntry = {
    state: PackageState.Installed,
    'static-files': {
      license: 'licenseUrl', // /public/package-data/bitcoinproxy/0.21.1/LICENSE.md,
      icon: 'assets/img/service-icons/bitcoin-proxy.png',
      instructions: 'instructionsUrl', // /public/package-data/bitcoinproxy/0.2.2/INSTRUCTIONS.md
    },
    manifest: MockManifestBitcoinProxy,
    installed: {
      status: {
        configured: true,
        main: {
          status: PackageMainStatus.Running,
          started: new Date().toISOString(),
          health: {},
        },
        'dependency-errors': {},
      },
      manifest: MockManifestBitcoinProxy,
      'interface-addresses': {
        rpc: {
          'tor-address': 'bitcoinproxy-rpc-address.onion',
          'lan-address': 'bitcoinproxy-rpc-address.local',
        },
      },
      'system-pointers': [],
      'current-dependents': {
        'lnd': {
          pointers: [],
          'health-checks': [],
        },
      },
      'current-dependencies': {
        'bitcoind': {
          pointers: [],
          'health-checks': [],
        },
      },
    },
    'install-progress': undefined,
  }

  export const Notifications: ServerNotifications = [
    {
      id: '123e4567-e89b-12d3-a456-426655440000',
      'package-id': null,
      'created-at': '2019-12-26T14:20:30.872Z',
      code: 1,
      level: NotificationLevel.Success,
      title: 'Backup Complete',
      message: 'Embassy and services have been successfully backed up.',
      data: {
        server: {
          attempted: true,
          error: null,
        },
        packages: {
          'bitcoind': {
            error: null,
          },
        },
      },
    },
    {
      id: '123e4567-e89b-12d3-a456-426655440001',
      'package-id': 'bitcoind',
      'created-at': '2019-12-26T14:20:30.872Z',
      code: 2,
      level: NotificationLevel.Warning,
      title: 'SSH Key Added',
      message: 'A new SSH key was added. If you did not do this, shit is bad.',
      data: null,
    },
    {
      id: '123e4567-e89b-12d3-a456-426655440002',
      'package-id': 'bitcoind',
      'created-at': '2019-12-26T14:20:30.872Z',
      code: 3,
      level: NotificationLevel.Info,
      title: 'SSH Key Removed',
      message: 'A SSH key was removed.',
      data: null,
    },
    {
      id: '123e4567-e89b-12d3-a456-426655440003',
      'package-id': 'bitcoind',
      'created-at': '2019-12-26T14:20:30.872Z',
      code: 4,
      level: NotificationLevel.Error,
      title: 'Service Crashed',
      message: 'Bitcoind has crashed.',
      data: null,
    },
  ]

  export function getServerMetrics () {
    return {
      'Group1': {
        'Metric1': {
          value: Math.random(),
          unit: 'mi/b',
        },
        'Metric2': {
          value: Math.random(),
          unit: '%',
        },
        'Metric3': {
          value: 10.1,
          unit: '%',
        },
      },
      'Group2': {
        'Hmmmm1': {
          value: 22.2,
          unit: 'mi/b',
        },
        'Hmmmm2': {
          value: 50,
          unit: '%',
        },
        'Hmmmm3': {
          value: 10.1,
          unit: '%',
        },
      },
      'Group3': {
        'Hmmmm1': {
          value: Math.random(),
          unit: 'mi/b',
        },
        'Hmmmm2': {
          value: 50,
          unit: '%',
        },
        'Hmmmm3': {
          value: 10.1,
          unit: '%',
        },
      },
      'Group4': {
        'Hmmmm1': {
          value: Math.random(),
          unit: 'mi/b',
        },
        'Hmmmm2': {
          value: 50,
          unit: '%',
        },
        'Hmmmm3': {
          value: 10.1,
          unit: '%',
        },
      },
      'Group5': {
        'Hmmmm1': {
          value: Math.random(),
          unit: 'mi/b',
        },
        'Hmmmm2': {
          value: 50,
          unit: '%',
        },
        'Hmmmm3': {
          value: 10.1,
          unit: '%',
        },
      },
    }
  }

  export function getAppMetrics () {
    const metr: Metric = {
      'Metric1': {
        value: Math.random(),
        unit: 'mi/b',
      },
      'Metric2': {
        value: Math.random(),
        unit: '%',
      },
      'Metric3': {
        value: 10.1,
        unit: '%',
      },
    }

    return metr
  }

  export const ServerLogs: RR.GetServerLogsRes = [
    {
      timestamp: '2019-12-26T14:20:30.872Z',
      log: '****** START *****',
    },
    {
      timestamp: '2019-12-26T14:21:30.872Z',
      log: 'ServerLogs ServerLogs ServerLogs ServerLogs ServerLogs',
    },
    {
      timestamp: '2019-12-26T14:22:30.872Z',
      log: '****** FINISH *****',
    },
  ]

  export const PackageLogs: RR.GetPackageLogsRes = [
    {
      timestamp: '2019-12-26T14:20:30.872Z',
      log: '****** START *****',
    },
    {
      timestamp: '2019-12-26T14:21:30.872Z',
      log: 'PackageLogs PackageLogs PackageLogs PackageLogs PackageLogs',
    },
    {
      timestamp: '2019-12-26T14:22:30.872Z',
      log: '****** FINISH *****',
    },
  ]

  export const Sessions: RR.GetSessionsRes = {
    current: 'b7b1a9cef4284f00af9e9dda6e676177',
    sessions: {
      '9513226517c54ddd8107d6d7b9d8aed7': {
        'last-active': '2021-07-14T20:49:17.774Z',
        'user-agent': 'AppleWebKit/{WebKit Rev} (KHTML, like Gecko)',
        metadata: {
          platforms: ['iphone', 'mobileweb', 'mobile', 'ios'],
        },
      },
      'b7b1a9cef4284f00af9e9dda6e676177': {
        'last-active': '2021-06-14T20:49:17.774Z',
        'user-agent': 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0',
        metadata: {
          platforms: ['desktop'],
        },
      },
    },
  }

  export const SshKeys: RR.GetSSHKeysRes = {
    '28:d2:7e:78:61:b4:bf:g2:de:24:15:96:4e:d4:15:53': {
      alg: 'ed25519',
      hostname: 'Matt Key',
      hash: 'VeryLongHashOfSSHKey1',
    },
    '12:f8:7e:78:61:b4:bf:e2:de:24:15:96:4e:d4:72:53': {
      alg: 'ed25519',
      hostname: 'Aiden Key',
      hash: 'VeryLongHashOfSSHKey2',
    },
  }

  export const SshKey: RR.AddSSHKeyRes = {
    '44:44:7e:78:61:b4:bf:g2:de:24:15:96:4e:d4:15:53': {
      alg: 'ed25519',
      hostname: 'Lucy Key',
      hash: 'VeryLongHashOfSSHKey3',
    },
  }

  export const Disks: RR.GetDisksRes = {
    '/dev/sda': {
      size: '32GB',
      description: 'Samsung',
      partitions: {
        'sdba2': {
          size: null,
          'is-mounted': false,
          label: 'Matt Stuff',
        },
      },
    },
    '/dev/sba': {
      size: '64GB',
      description: 'small USB stick',
      partitions: {
        'sdba2': {
          size: '16GB',
          'is-mounted': true,
          label: null,
        },
      },
    },
    '/dev/sbd': {
      size: '128GB',
      description: 'large USB stick',
      partitions: {
        'sdba1': {
          size: '32GB',
          'is-mounted': false,
          label: 'Partition 1',
        },
        'sdba2': {
          size: null,
          'is-mounted': true,
          label: 'Partition 2',
        },
      },
    },
  }

  export const PackageProperties: RR.GetPackagePropertiesRes<2> = {
    version: 2,
    data: {
      'Test': {
        type: 'string',
        description: 'This is some information about the thing.',
        copyable: true,
        qr: true,
        masked: false,
        value: 'lndconnect://udlyfq2mxa4355pt7cqlrdipnvk2tsl4jtsdw7zaeekenufwcev2wlad.onion:10009?cert=MIICJTCCAcugAwIBAgIRAOyq85fqAiA3U3xOnwhH678wCgYIKoZIzj0EAwIwODEfMB0GAkUEChMWbG5kIGF1dG9nZW5lcmF0ZWQgY2VydDEVMBMGA1UEAxMMNTc0OTkwMzIyYzZlMB4XDTIwMTAyNjA3MzEyN1oXDTIxMTIyMTA3MzEyN1owODEfMB0GA1UEChMWbG5kIGF1dG9nZW5lcmF0ZWQgY2VydDEVMBMGA1UEAxMMNTc0OTkwMzIyYzZlMFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEKqfhAMMZdY-eFnU5P4bGrQTSx0lo7m8u4V0yYkzUM6jlql_u31_mU2ovLTj56wnZApkEjoPl6fL2yasZA2wiy6OBtTCBsjAOBgNVHQ8BAf8EBAMCAqQwEwYDVR0lBAwwCgYIKwYBBQUHAwEwDwYDVR0TAQH_BAUwAwEB_zAdBgNVHQ4EFgQUYQ9uIO6spltnVCx4rLFL5BvBF9IwWwYDVR0RBFQwUoIMNTc0OTkwMzIyYzZlgglsb2NhbGhvc3SCBHVuaXiCCnVuaXhwYWNrZXSCB2J1ZmNvbm6HBH8AAAGHEAAAAAAAAAAAAAAAAAAAAAGHBKwSAAswCgYIKoZIzj0EAwIDSAAwRQIgVZH2Z2KlyAVY2Q2aIQl0nsvN-OEN49wreFwiBqlxNj4CIQD5_JbpuBFJuf81I5J0FQPtXY-4RppWOPZBb-y6-rkIUQ&macaroon=AgEDbG5kAusBAwoQuA8OUMeQ8Fr2h-f65OdXdRIBMBoWCgdhZGRyZXNzEgRyZWFkEgV3cml0ZRoTCgRpbmZvEgRyZWFkEgV3cml0ZRoXCghpbnZvaWNlcxIEcmVhZBIFd3JpdGUaFAoIbWFjYXJvb24SCGdlbmVyYXRlGhYKB21lc3NhZ2USBHJlYWQSBXdyaXRlGhcKCG9mZmNoYWluEgRyZWFkEgV3cml0ZRoWCgdvbmNoYWluEgRyZWFkEgV3cml0ZRoUCgVwZWVycxIEcmVhZBIFd3JpdGUaGAoGc2lnbmVyEghnZW5lcmF0ZRIEcmVhZAAABiCYsRUoUWuAHAiCSLbBR7b_qULDSl64R8LIU2aqNIyQfA',
      },
      'Nested': {
        type: 'object',
        description: 'This is a nested thing metric',
        value: {
          'Last Name': {
            type: 'string',
            description: 'The last name of the user',
            copyable: true,
            qr: true,
            masked: false,
            value: 'Hill',
          },
          'Age': {
            type: 'string',
            description: 'The age of the user',
            copyable: false,
            qr: false,
            masked: false,
            value: '35',
          },
          'Password': {
            type: 'string',
            description: 'A secret password',
            copyable: true,
            qr: false,
            masked: true,
            value: 'password123',
          },
        },
      },
      'Another Value': {
        type: 'string',
        description: 'Some more information about the service.',
        copyable: false,
        qr: true,
        masked: false,
        value: 'https://guessagain.com',
      },
    },
  } as any // @TODO why is this necessary?

  export const PackageConfig: RR.GetPackageConfigRes = {
    // config spec
    spec: {
      'testnet': {
        'name': 'Testnet',
        'type': 'boolean',
        'description': 'determines whether your node is running on testnet or mainnet',
        'change-warning': 'Chain will have to resync!',
        'default': true,
      },
      'objectList': {
        'name': 'Object List',
        'type': 'list',
        'subtype': 'object',
        'description': 'This is a list of objects, like users or something',
        'range': '[0,4]',
        'default': [
          {
            'firstName': 'Admin',
            'lastName': 'User',
            'age': 40,
          },
          {
            'firstName': 'Admin2',
            'lastName': 'User',
            'age': 40,
          },
        ],
        // the outer spec here, at the list level, says that what's inside (the inner spec) pertains to its inner elements.
        // it just so happens that ValueSpecObject's have the field { spec: ConfigSpec }
        // see 'unionList' below for a different example.
        'spec': {
          'unique-by': 'lastName',
          'display-as': `I'm {{lastName}}, {{firstName}} {{lastName}}`,
          'spec': {
            'firstName': {
              'name': 'First Name',
              'type': 'string',
              'description': 'User first name',
              'nullable': true,
              'default': null,
              'masked': false,
              'copyable': false,
            },
            'lastName': {
              'name': 'Last Name',
              'type': 'string',
              'description': 'User first name',
              'nullable': true,
              'default': {
                'charset': 'a-g,2-9',
                'len': 12,
              },
              'pattern': '^[a-zA-Z]+$',
              'pattern-description': 'must contain only letters.',
              'masked': false,
              'copyable': true,
            },
            'age': {
              'name': 'Age',
              'type': 'number',
              'description': 'The age of the user',
              'nullable': true,
              'default': null,
              'integral': false,
              'change-warning': 'User must be at least 18.',
              'range': '[18,*)',
            },
          },
        },
      },
      'unionList': {
        'name': 'Union List',
        'type': 'list',
        'subtype': 'union',
        'description': 'This is a sample list of unions',
        'change-warning': 'If you change this, things may work.',
        // a list of union selections. e.g. 'summer', 'winter',...
        'default': [
          'summer',
        ],
        'range': '[0, 2]',
        'spec': {
          'tag': {
            'id': 'preference',
            'name': 'Preferences',
            'variant-names': {
              'summer': 'Summer',
              'winter': 'Winter',
              'other': 'Other',
            },
          },
          // this default is used to make a union selection when a new list element is first created
          'default': 'summer',
          'variants': {
            'summer': {
              'favorite-tree': {
                'name': 'Favorite Tree',
                'type': 'string',
                'nullable': false,
                'description': 'What is your favorite tree?',
                'default': 'Maple',
                'masked': false,
                'copyable': false,
              },
              'favorite-flower': {
                'name': 'Favorite Flower',
                'type': 'enum',
                'description': 'Select your favorite flower',
                'value-names': {
                  'none': 'Hate Flowers',
                  'red': 'Red',
                  'blue': 'Blue',
                  'purple': 'Purple',
                },
                'values': [
                  'none',
                  'red',
                  'blue',
                  'purple',
                ],
                'default': 'none',
              },
            },
            'winter': {
              'like-snow': {
                'name': 'Like Snow?',
                'type': 'boolean',
                'description': 'Do you like snow or not?',
                'default': true,
              },
            },
          },
          'unique-by': 'preference',
        },
      },
      'randomEnum': {
        'name': 'Random Enum',
        'type': 'enum',
        'value-names': {
          'null': 'Null',
          'option1': 'One 1',
          'option2': 'Two 2',
          'option3': 'Three 3',
        },
        'default': 'null',
        'description': 'This is not even real.',
        'change-warning': 'Be careful chnaging this!',
        'values': [
          'null',
          'option1',
          'option2',
          'option3',
        ],
      },
      'favoriteNumber': {
        'name': 'Favorite Number',
        'type': 'number',
        'integral': false,
        'description': 'Your favorite number of all time',
        'change-warning': 'Once you set this number, it can never be changed without severe consequences.',
        'nullable': false,
        'default': 7,
        'range': '(-100,100]',
        'units': 'BTC',
      },
      'secondaryNumbers': {
        'name': 'Unlucky Numbers',
        'type': 'list',
        'subtype': 'number',
        'description': 'Numbers that you like but are not your top favorite.',
        'spec': {
          'integral': false,
          'range': '[-100,200)',
        },
        'range': '[0,10]',
        'default': [
          2,
          3,
        ],
      },
      'rpcsettings': {
        'name': 'RPC Settings',
        'type': 'object',
        'unique-by': null,
        'description': 'rpc username and password',
        'change-warning': 'Adding RPC users gives them special permissions on your node.',
        'spec': {
          'laws': {
            'name': 'Laws',
            'type': 'object',
            'unique-by': 'law1',
            'description': 'the law of the realm',
            'spec': {
              'law1': {
                'name': 'First Law',
                'type': 'string',
                'description': 'the first law',
                'nullable': true,
                'masked': false,
                'copyable': true,
              },
              'law2': {
                'name': 'Second Law',
                'type': 'string',
                'description': 'the second law',
                'nullable': true,
                'masked': false,
                'copyable': true,
              },
            },
          },
          'rulemakers': {
            'name': 'Rule Makers',
            'type': 'list',
            'subtype': 'object',
            'description': 'the people who make the rules',
            'range': '[0,2]',
            'default': [],
            'spec': {
              'unique-by': null,
              'spec': {
                'rulemakername': {
                  'name': 'Rulemaker Name',
                  'type': 'string',
                  'description': 'the name of the rule maker',
                  'nullable': false,
                  'default': {
                    'charset': 'a-g,2-9',
                    'len': 12,
                  },
                  'masked': false,
                  'copyable': false,
                },
                'rulemakerip': {
                  'name': 'Rulemaker IP',
                  'type': 'string',
                  'description': 'the ip of the rule maker',
                  'nullable': false,
                  'default': '192.168.1.0',
                  'pattern': '^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$',
                  'pattern-description': 'may only contain numbers and periods',
                  'masked': false,
                  'copyable': true,
                },
              },
            },
          },
          'rpcuser': {
            'name': 'RPC Username',
            'type': 'string',
            'description': 'rpc username',
            'nullable': false,
            'default': 'defaultrpcusername',
            'pattern': '^[a-zA-Z]+$',
            'pattern-description': 'must contain only letters.',
            'masked': false,
            'copyable': true,
          },
          'rpcpass': {
            'name': 'RPC User Password',
            'type': 'string',
            'description': 'rpc password',
            'nullable': false,
            'default': {
              'charset': 'a-z,A-Z,2-9',
              'len': 20,
            },
            'masked': true,
            'copyable': true,
          },
        },
      },
      'advanced': {
        'name': 'Advanced',
        'type': 'object',
        'unique-by': null,
        'description': 'Advanced settings',
        'spec': {
          'notifications': {
            'name': 'Notification Preferences',
            'type': 'list',
            'subtype': 'enum',
            'description': 'how you want to be notified',
            'range': '[1,3]',
            'default': [
              'email',
            ],
            'spec': {
              'value-names': {
                'email': 'EEEEmail',
                'text': 'Texxxt',
                'call': 'Ccccall',
                'push': 'PuuuusH',
                'webhook': 'WebHooookkeee',
              },
              'values': [
                'email',
                'text',
                'call',
                'push',
                'webhook',
              ],
            },
          },
        },
      },
      'bitcoinNode': {
        'name': 'Bitcoin Node Settings',
        'type': 'union',
        'unique-by': null,
        'description': 'The node settings',
        'default': 'internal',
        'change-warning': 'Careful changing this',
        'tag': {
          'id': 'type',
          'name': 'Type',
          'variant-names': {
            'internal': 'Internal',
            'external': 'External',
          },
        },
        'variants': {
          'internal': {
            'lan-address': {
              'name': 'LAN Address',
              'type': 'pointer',
              'subtype': 'app',
              'target': 'lan-address',
              'app-id': 'bitcoind',
              'description': 'the lan address',
            },
          },
          'external': {
            'public-domain': {
              'name': 'Public Domain',
              'type': 'string',
              'description': 'the public address of the node',
              'nullable': false,
              'default': 'bitcoinnode.com',
              'pattern': '.*',
              'pattern-description': 'anything',
              'masked': false,
              'copyable': true,
            },
          },
        },
      },
      'port': {
        'name': 'Port',
        'type': 'number',
        'integral': true,
        'description': 'the default port for your Bitcoin node. default: 8333, testnet: 18333, regtest: 18444',
        'nullable': false,
        'default': 8333,
        'range': '[0, 9999]',
      },
      'favoriteSlogan': {
        'name': 'Favorite Slogan',
        'type': 'string',
        'description': 'You most favorite slogan in the whole world, used for paying you.',
        'nullable': true,
        'masked': true,
        'copyable': true,
      },
      'rpcallowip': {
        'name': 'RPC Allowed IPs',
        'type': 'list',
        'subtype': 'string',
        'description': 'external ip addresses that are authorized to access your Bitcoin node',
        'change-warning': 'Any IP you allow here will have RPC access to your Bitcoin node.',
        'range': '[1,10]',
        'default': [
          '192.168.1.1',
        ],
        'spec': {
          'pattern': '((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|((^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$)|(^[a-z2-7]{16}\\.onion$)|(^([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)+[a-z0-9][a-z0-9-]{0,61}[a-z0-9]$))',
          'pattern-description': 'must be a valid ipv4, ipv6, or domain name',
        },
      },
      'rpcauth': {
        'name': 'RPC Auth',
        'type': 'list',
        'subtype': 'string',
        'description': 'api keys that are authorized to access your Bitcoin node.',
        'range': '[0,*)',
        'default': [],
        'spec': {},
      },
    },
    // actual config
    config: {
      testnet: false,
      objectList: [
        {
          'firstName': 'Admin',
          'lastName': 'User',
          'age': 40,
        },
        {
          'firstName': 'Admin2',
          'lastName': 'User',
          'age': 40,
        },
      ],
      unionList: undefined,
      randomEnum: 'option1',
      favoriteNumber: 8,
      secondaryNumbers: undefined,
      rpcsettings: {
        laws: {
          law1: 'The first law',
          law2: 'The second law',
        },
        rpcpass: null,
        rpcuser: '123',
        rulemakers: [],
      },
      advanced: {
        notifications: ['email'],
      },
      bitcoinNode: undefined,
      port: 5959,
      rpcallowip: undefined,
      rpcauth: ['matt: 8273gr8qwoidm1uid91jeh8y23gdio1kskmwejkdnm'],
    },
  }

  export const mockCupsDependentConfig = {
    randomEnum: 'option1',
    testnet: false,
    favoriteNumber: 8,
    secondaryNumbers: [13, 58, 20],
    objectList: [],
    unionList: [],
    rpcsettings: {
      laws: null,
      rpcpass: null,
      rpcuser: '123',
      rulemakers: [],
    },
    advanced: {
      notifications: [],
    },
    bitcoinNode: { type: 'internal' },
    port: 5959,
    rpcallowip: [],
    rpcauth: ['matt: 8273gr8qwoidm1uid91jeh8y23gdio1kskmwejkdnm'],
  }

  // export const bitcoind: PackageDataEntry = {
  //   state: PackageState.Installed,
  //   'static-files': {
  //     license: 'licenseUrl', // /public/package-data/bitcoind/0.21.1/LICENSE.md,
  //     icon: 'assets/img/service-icons/bitcoind.png',
  //     instructions: 'instructionsUrl', // /public/package-data/bitcoind/0.21.1/INSTRUCTIONS.md
  //   },
  //   manifest: {
  //     ...MockManifestBitcoind,
  //     version: '0.20.0',
  //   },
  //   installed: {
  //     status: {
  //       configured: true,
  //       main: {
  //         status: PackageMainStatus.Running,
  //         started: new Date().toISOString(),
  //         health: { },
  //       },
  //       'dependency-errors': { },
  //     },
  //     'interface-info': {
  //       ip: '10.0.0.1',
  //       addresses: {
  //         ui: {
  //           'tor-address': 'bitcoind-ui-address.onion',
  //           'lan-address': 'bitcoind-ui-address.local',
  //         },
  //         rpc: {
  //           'tor-address': 'bitcoind-rpc-address.onion',
  //           'lan-address': 'bitcoind-rpc-address.local',
  //         },
  //         p2p: {
  //           'tor-address': 'bitcoind-p2p-address.onion',
  //           'lan-address': 'bitcoind-p2p-address.local',
  //         },
  //       },
  //     },
  //     'system-pointers': [],
  //     'current-dependents': {
  //       'lnd': {
  //         pointers: [],
  //         'health-checks': [],
  //       },
  //     },
  //     'current-dependencies': { },
  //   },
  //   'install-progress': undefined,
  // }

  export const lnd: PackageDataEntry = {
    state: PackageState.Installed,
    'static-files': {
      license: 'licenseUrl', // /public/package-data/lnd/0.21.1/LICENSE.md,
      icon: 'assets/img/service-icons/lnd.png',
      instructions: 'instructionsUrl', // /public/package-data/lnd/0.21.1/INSTRUCTIONS.md
    },
    manifest: MockManifestLnd,
    installed: {
      status: {
        configured: true,
        main: {
          status: PackageMainStatus.Stopped,
        },
        'dependency-errors': {
          'bitcoin-proxy': {
            type: DependencyErrorType.NotInstalled,
            title: Mock.MockManifestBitcoinProxy.title,
            icon: 'assets/img/service-icons/bitcoin-proxy.png',
          },
        },
      },
      manifest: MockManifestLnd,
      'interface-addresses': {
        rpc: {
          'tor-address': 'lnd-rpc-address.onion',
          'lan-address': 'lnd-rpc-address.local',
        },
        grpc: {
          'tor-address': 'lnd-grpc-address.onion',
          'lan-address': 'lnd-grpc-address.local',
        },
      },
      'system-pointers': [],
      'current-dependents': {},
      'current-dependencies': {
        'bitcoind': {
          pointers: [],
          'health-checks': [],
        },
        'bitcoin-proxy': {
          pointers: [],
          'health-checks': [],
        },
      },
    },
    'install-progress': undefined,
  }

  // export const DbDump: RR.GetDumpRes = {
  //   id: 1,
  //   expireId: null,
  //   value: {
  //     'server-info': {
  //       id: 'start9-abcdefgmm',
  //       version: '1.0.0',
  //       status: ServerStatus.Running,
  //       'lan-address': 'start9-abcdefgh.local',
  //       'tor-address': 'myveryownspecialtoraddress.onion',
  //       wifi: {
  //         ssids: ['Goosers', 'Goosers5G'],
  //         selected: 'Goosers5G',
  //         connected: 'Goosers5G',
  //       },
  //       'eos-marketplace': 'https://registry.start9.com',
  //       'package-marketplace': 'https://registry.start9.com',
  //       'unread-notification-count': 4,
  //       specs: {
  //         CPU: 'Cortex-A72: 4 Cores @1500MHz',
  //         Disk: '1TB SSD',
  //         Memory: '8GB',
  //       },
  //       'connection-addresses': {
  //         tor: ['http://privacy34kn4ez3y3nijweec6w4g54i3g54sdv7r5mr6soma3w4begyd.onion'],
  //         clearnet: ['https://start9.com'],
  //       },
  //     },
  //     'package-data': {
  //       'bitcoind': bitcoind,
  //       'lnd': lnd,
  //     },
  //     ui: {
  //       'welcome-ack': '1.0.0',
  //       'auto-check-updates': true,
  //     },
  //   },
  // }
}

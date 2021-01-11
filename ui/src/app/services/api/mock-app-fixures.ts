import { AppStatus } from '../../models/app-model'
import { AppAvailablePreview, AppAvailableFull, AppInstalledPreview, AppDependency, BaseApp, AppInstalledFull, DependentBreakage, AppAvailableVersionSpecificInfo } from '../../models/app-types'
export function toAvailablePreview (f: AppAvailableFull): AppAvailablePreview {
  return {
    id: f.id,
    versionInstalled: f.versionInstalled,
    status: f.status,
    title: f.title,
    descriptionShort: f.descriptionShort,
    iconURL: f.iconURL,
    versionLatest: f.versionLatest,
  }
}

export function toInstalledPreview (f: AppInstalledFull): AppInstalledPreview {
  return {
    id: f.id,
    versionInstalled: f.versionInstalled,
    status: f.status,
    title: f.title,
    iconURL: f.iconURL,
    torAddress: f.torAddress,
    ui: f.ui,
  }
}

export function toServiceRequirement (f: BaseApp, o: Omit<AppDependency, keyof BaseApp>): AppDependency {
  return {
    id: f.id,
    title: f.title,
    iconURL: f.iconURL,
    ...o,
  }
}

export function toServiceBreakage (f: BaseApp): DependentBreakage {
  return {
    id: f.id,
    title: f.title,
    iconURL: f.iconURL,
  }
}

export const bitcoinI: AppInstalledFull = {
  id: 'bitcoind',
  versionInstalled: '0.18.1',
  title: 'Bitcoin Core',
  torAddress: 'sample-bitcoin-tor-address-and-some-more-tor-address.onion',
  status: AppStatus.STOPPED,
  iconURL: 'assets/img/service-icons/bitcoind.png',
  instructions: 'some instructions',
  lastBackup: new Date().toISOString(),
  configuredRequirements: [],
  hasFetchedFull: true,
  ui: false,
}

export const lightningI: AppInstalledFull = {
  id: 'c-lightning',
  status: AppStatus.RUNNING,
  title: 'C Lightning',
  versionInstalled: '1.0.0',
  torAddress: 'sample-bitcoin-tor-address-and-some-more-tor-address.onion',
  iconURL: 'assets/img/service-icons/bitwarden.png',
  instructions: 'some instructions',
  lastBackup: new Date().toISOString(),
  configuredRequirements: [
    toServiceRequirement(bitcoinI,
      {
        optional: 'you don\'t reeeeelly need this',
        default: true,
        versionSpec: '>= 0.1.2',
        description: 'lightning needs bitcoin',
        violation: null,
      }),
  ],
  hasFetchedFull: true,
  ui: true,
}

export const cupsI: AppInstalledFull = {
  id: 'cups',
  versionInstalled: '2.1.0',
  title: 'Cups Messenger',
  torAddress: 'sample-cups-tor-address.onion',
  status: AppStatus.BROKEN_DEPENDENCIES,
  iconURL: 'assets/img/service-icons/cups.png',

  instructions: 'some instructions',
  lastBackup: new Date().toISOString(),
  ui: true,
  configuredRequirements: [
    toServiceRequirement(lightningI,
      {
        optional: 'you don\'t reeeeelly need this',
        default: true,

        versionSpec: '>= 0.1.2',
        description: 'lightning needs bitcoin',
        violation: { name: 'incompatible-version' },
      }),
    toServiceRequirement(lightningI,
      {
        optional: 'you don\'t reeeeelly need this',
        default: true,

        versionSpec: '>= 0.1.2',
        description: 'lightning needs bitcoin',
        violation: { name: 'incompatible-status', status: AppStatus.INSTALLING },
      }),
    toServiceRequirement(lightningI,
      {
        optional: 'you don\'t reeeeelly need this',
        default: true,

        versionSpec: '>= 0.1.2',
        description: 'lightning needs bitcoin',
        violation: { name: 'incompatible-config', ruleViolations: ['bro', 'seriously', 'fix this'] },
      }),
  ],
  hasFetchedFull: true,
}

export const bitcoinA: AppAvailableFull = {
  id: 'bitcoind',
  versionLatest: '0.19.1.1',
  versionInstalled: '0.19.0',
  status: AppStatus.UNKNOWN,
  title: 'Bitcoin Core',
  descriptionShort: 'Bitcoin is an innovative payment network and new kind of money.',
  iconURL: 'assets/img/service-icons/bitcoind.png',
  releaseNotes: 'Bitcoin is an innovative payment network and new kind of money. Bitcoin utilizes a robust p2p network to garner decentralized consensus. Bitcoin is an innovative payment network and new kind of money. Bitcoin utilizes a robust p2p network to garner decentralized consensus. Bitcoin is an innovative payment network and new kind of money. Bitcoin utilizes a robust p2p network to garner decentralized consensus. Segit and more cool things!',
  descriptionLong: 'Bitcoin is an innovative payment network and new kind of money. Bitcoin utilizes a robust p2p network to garner decentralized consensus.',
  versions: ['0.19.1.1', '0.19.1', '0.19.0', '0.18.1', '0.17.0'],
  versionViewing: '0.19.1',
  serviceRequirements: [],
}

export const lightningA: AppAvailableFull = {
  id: 'c-lightning',
  versionLatest: '1.0.1',
  versionInstalled: null,
  status: AppStatus.UNKNOWN,
  title: 'C Lightning',
  descriptionShort: 'Lightning is quick money things.',
  iconURL: 'assets/img/service-icons/bitcoind.png',
  releaseNotes: 'Finally it works',
  descriptionLong: 'Lightning is an innovative payment network and new kind of money. Lightning utilizes a robust p2p network to garner decentralized consensus.',
  versions: ['0.0.1', '0.8.0', '0.8.1', '1.0.0', '1.0.1'],
  versionViewing: '1.0.1',
  serviceRequirements: [
    toServiceRequirement(bitcoinA, {
      optional: null,
      default: true,
      versionSpec: '>=0.19.0',
      description: 'Lightning uses bitcoin under the hood',
      violation: null,
    }),
  ],
}

export const btcPayA: AppAvailableFull = {
  id: 'btcPay',
  versionLatest: '1.0.1',
  versionInstalled: '1.0.1',
  status: AppStatus.INSTALLING,
  title: 'BTC Pay',
  descriptionShort: 'BTC Pay is quick payment money things',
  iconURL: 'assets/img/service-icons/bitcoind.png',
  releaseNotes: 'Finally pay us Finally pay us Finally pay us Finally pay us Finally pay usFinally pay us',
  descriptionLong: 'Btc Pay is an innovative payment network and new kind of money. Btc Pay utilizes a robust p2p network to garner decentralized consensus.',
  versions: ['0.8.0', '0.8.1', '1.0.0', '1.0.1'],
  versionViewing: '1.0.1',
  serviceRequirements: [
    toServiceRequirement(bitcoinA, {
      optional: null,
      default: true,
      versionSpec: '>0.19.0',
      description: 'Lightning uses bitcoin under the hood',
      violation: { name: 'incompatible-version' },
    }),
  ],
}

export const thunderA: AppAvailableFull = {
  id: 'thunder',
  versionLatest: '1.0.1',
  versionInstalled: null,
  status: AppStatus.UNKNOWN,
  title: 'Thunder',
  descriptionShort: 'Thunder is quick payment money things',
  iconURL: 'assets/img/service-icons/bitcoind.png',
  releaseNotes: 'Finally pay us',
  descriptionLong: 'Thunder is an innovative payment network and new kind of money. Thunder utilizes a robust p2p network to garner decentralized consensus.',
  versions: ['0.8.0', '0.8.1', '1.0.0', '1.0.1'],
  versionViewing: '1.0.1',
  serviceRequirements: [
    toServiceRequirement(bitcoinA, {
      optional: null,
      default: true,
      versionSpec: '>0.19.0',
      description: 'Thunder uses bitcoin under the hood',
      violation: { name: 'incompatible-version' },
    }),
    toServiceRequirement(lightningA, {
      optional: null,
      default: true,
      versionSpec: '>=1.0.1',
      description: 'Thunder uses lightning under the hood',
      violation: { name: 'incompatible-version' },
    }),
    toServiceRequirement(btcPayA, {
      optional: 'Can be configured to use chase bank instead',
      default: true,
      versionSpec: '>=1.0.1',
      description: 'Thunder can use btcpay under the hood',
      violation: { name: 'missing' },
    }),
    toServiceRequirement(btcPayA, {
      optional: 'Can be configured to use chase bank instead',
      default: true,
      versionSpec: '>=1.0.1',
      description: 'Thunder can use btcpay under the hood',
      violation: { name: 'incompatible-status', status: AppStatus.INSTALLING },
    }),
  ],
}

export const cupsA: AppAvailableFull = {
  id: 'cups',
  versionLatest: '2.1.0',
  versionInstalled: '2.1.0',
  status: AppStatus.RUNNING,
  title: 'Cups Messenger',
  descriptionShort: 'P2P encrypted messaging over Tor.',
  iconURL: 'assets/img/service-icons/cups.png',
  releaseNotes: 'Segit and more cool things!',
  descriptionLong: 'Bitcoin is an innovative payment network and new kind of money. Bitcoin utilizes a robust p2p network to garner decentralized consensus.',
  versions: ['0.1.0', '0.1.1', '0.1.2', '1.0.0', '2.0.0', '2.1.0'],
  versionViewing: '2.1.0',
  serviceRequirements: [],
}

export const bitwardenA: AppAvailableFull = {
  id: 'bitwarden',
  versionLatest: '0.1.1',
  versionInstalled: null,
  status: null,
  title: 'Bitwarden',
  descriptionShort: `Self-hosted password manager`,
  iconURL: 'assets/img/service-icons/bitwarden.png',
  releaseNotes: 'Passwords and shite!',
  descriptionLong: 'Bitwarden is fun.',
  versions: ['0.19.0', '0.18.1', '0.17.0'],
  versionViewing: '0.1.1',
  serviceRequirements: [
    toServiceRequirement(cupsA, {
      optional: 'Can be configured to use chase bank instead',
      default: true,
      versionSpec: '>=1.0.0',
      description: 'cups does great stuff for bitwarden',
      violation: { name: 'incompatible-config', ruleViolations: ['change this value to that value', 'change this second value to something better']},
    }),
  ],
}

export const mockApiAppAvailableFull: { [appId: string]: AppAvailableFull; } = {
  bitcoind: bitcoinA,
  lightning: lightningA,
  btcPay: btcPayA,
  thunder: thunderA,
  cups: cupsA,
  bitwarden: bitwardenA,
}

export const mockApiAppInstalledFull: { [appId: string]: AppInstalledFull; } = {
  bitcoind: bitcoinI,
  cups: cupsI,
  lightning: lightningI,
}

export const mockApiAppAvailableVersionInfo: AppAvailableVersionSpecificInfo = {
  releaseNotes: 'Some older release notes that are not super important anymore.',
  serviceRequirements: [],
  versionViewing: '0.2.0',
}

export const mockAppDependentBreakages: { breakages: DependentBreakage[] } = {
  breakages: [
    toServiceBreakage(bitcoinI),
    toServiceBreakage(cupsA),
  ],
}
import { AppStatus } from '../../models/app-model'
import { AppAvailablePreview, AppAvailableFull, AppInstalledPreview, AppDependency, BaseApp, AppInstalledFull, DependentBreakage, AppAvailableVersionSpecificInfo } from '../../models/app-types'
import { modulateTime } from 'src/app/util/misc.util'
import { ApiAppInstalledFull } from './api-types'

export function toAvailablePreview (f: AppAvailableFull): AppAvailablePreview {
  return {
    id: f.id,
    versionInstalled: f.versionInstalled,
    status: f.status,
    title: f.title,
    descriptionShort: f.descriptionShort,
    iconURL: f.iconURL,
    versionLatest: f.versionLatest,
    latestVersionTimestamp: f.latestVersionTimestamp,
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
    lanAddress: f.lanAddress,
    lanUi: f.lanUi,
    torUi: f.torUi,
    hasUI: f.hasUI,
    launchable: f.launchable,
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

export const bitcoinI: ApiAppInstalledFull = {
  id: 'bitcoind',
  versionInstalled: '0.18.1',
  lanAddress: undefined,
  title: 'Bitcoin Core',
  licenseName: 'MIT',
  licenseLink: 'https://github.com/bitcoin/bitcoin/blob/master/COPYING',
  torAddress: '4acth47i6kxnvkewtm6q7ib2s3ufpo5sqbsnzjpbi7utijcltosqemad.onion',
  startAlert: 'Bitcoind could take a loooooong time to start. Please be patient.',
  status: AppStatus.STOPPED,
  iconURL: 'assets/img/service-icons/bitcoind.png',
  instructions: 'some instructions',
  lastBackup: new Date().toISOString(),
  configuredRequirements: [],
  lanUi: false,
  torUi: false,
  restoreAlert: 'if you restore this app horrible things will happen to the people you love.',
  actions: [
    { id: 'sync-chain', name: 'Sync Chain', description: 'this will sync with the chain like from Avatar', allowedStatuses: [ AppStatus.RUNNING, AppStatus.RUNNING, AppStatus.RUNNING, AppStatus.RUNNING ]},
    { id: 'single-status-action', name: 'Single Status Action', description: 'This action has only one allowed status', allowedStatuses: [ AppStatus.RUNNING ]},
    { id: 'off-sync-chain', name: 'Off Sync Chain', description: 'this will off sync with the chain like from Avatar', allowedStatuses: [ AppStatus.STOPPED ]},
  ],
}

export const lightningI: ApiAppInstalledFull = {
  id: 'c-lightning',
  lanAddress: 'lightningLan.local',
  status: AppStatus.RUNNING,
  title: 'C Lightning',
  versionInstalled: '1.0.0',
  torAddress: '4acth47i6kxnvkewtm6q7ib2s3ufpo5sqbsnzjpbi7utijcltosqemad.onion',
  iconURL: 'assets/img/service-icons/c-lightning.png',
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
  lanUi: false,
  torUi: true,
  actions: [],
}

export const cupsI: ApiAppInstalledFull = {
  id: 'cups',
  lanAddress: 'cupsLan.local',
  versionInstalled: '2.1.0',
  title: 'Cups Messenger',
  torAddress: 'sample-cups-tor-address.onion',
  status: AppStatus.BROKEN_DEPENDENCIES,
  iconURL: 'assets/img/service-icons/cups.png',

  instructions: 'some instructions',
  lastBackup: new Date().toISOString(),
  uninstallAlert: 'This is A GREAT APP man, I just don\'t know',
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
  lanUi: true,
  torUi: true,
  actions: [],
}

export const bitcoinA: AppAvailableFull = {
  latestVersionTimestamp: modulateTime(new Date(), 5 , 'seconds'),
  id: 'bitcoind',
  versionLatest: '0.19.1.1',
  versionInstalled: '0.19.0',
  licenseName: 'MIT',
  licenseLink: 'https://github.com/bitcoin/bitcoin/blob/master/COPYING',
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
  latestVersionTimestamp: modulateTime(new Date(), 4 , 'seconds'),
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
  latestVersionTimestamp: modulateTime(new Date(), 3 , 'seconds'),
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
  latestVersionTimestamp: modulateTime(new Date(), 2 , 'seconds'),
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
  installAlert: 'Oooooh you really might want to think twice about installing this...',
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
  latestVersionTimestamp: new Date(),
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
  latestVersionTimestamp: modulateTime(new Date(), 1 , 'seconds'),
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
  'c-lightning': lightningA,
  btcPay: btcPayA,
  thunder: thunderA,
  cups: cupsA,
  bitwarden: bitwardenA,
}

export const mockApiAppInstalledFull: { [appId: string]: ApiAppInstalledFull; } = {
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

import { AppStatus } from './app-model'

/** APPS **/

export interface BaseApp {
  id: string
  title: string
  status: AppStatus | null
  versionInstalled: string | null
  iconURL: string
}

// available
export interface AppAvailablePreview extends BaseApp {
  versionLatest: string
  descriptionShort: string
  latestVersionTimestamp: Date //used for sorting AAL
}

export type AppAvailableFull =
  AppAvailablePreview  &
  { descriptionLong: string
    versions: string[]
  } &
  AppAvailableVersionSpecificInfo


export interface AppAvailableVersionSpecificInfo {
  releaseNotes: string
  serviceRequirements: AppDependency[]
  versionViewing: string
  installAlert?: string
}
// installed

export interface AppInstalledPreview extends BaseApp {
  torAddress: string
  versionInstalled: string
  ui: boolean
}

export interface AppInstalledFull extends AppInstalledPreview {
  instructions: string | null
  lastBackup: string | null
  configuredRequirements: AppDependency[] | null // null if not yet configured
  hasFetchedFull: boolean
  uninstallAlert?: string
  restoreAlert?: string
}
// dependencies

export interface AppDependency extends InstalledAppDependency {
  // explanation of why it *is* optional. null represents it is required.
  optional: string | null
  // whether it comes as defualt in the config. This will not be present on an installed app, as we only care
  default: boolean
}

export interface InstalledAppDependency extends Omit<BaseApp, 'versionInstalled' | 'status'> {
  // semver specification
  versionSpec: string

  // an optional description of how this dependency is utlitized by the host app
  description: string | null

  // how the requirement is failed, null means satisfied. If the dependency is optional, this should still be set as though it were required.
  // This way I can say "it's optional, but also you would need to upgrade it to versionSpec" or "it's optional, but you don't even have it"
  // Said another way, if violaion === null, then this thing as a requirement is straight up satisfied.
  violation: DependencyViolation | null
}

export enum DependencyViolationSeverity {
  NONE = 0,
  OPTIONAL = 1,
  RECOMMENDED = 2,
  REQUIRED = 3,
}
export function getViolationSeverity (r: AppDependency): DependencyViolationSeverity {
  if (!r.optional && r.violation) return DependencyViolationSeverity.REQUIRED
  if (r.optional && r.default && r.violation) return DependencyViolationSeverity.RECOMMENDED
  if (isOptional(r) && r.violation) return DependencyViolationSeverity.OPTIONAL
  return DependencyViolationSeverity.NONE
}

// optional not recommended
export function isOptional (r: AppDependency): boolean {
  return r.optional && !r.default
}

export function isRecommended (r: AppDependency): boolean {
  return r.optional && r.default
}

export function isMissing (r: AppDependency) {
  return r.violation && r.violation.name === 'missing'
}

export function isMisconfigured (r: AppDependency) {
  return r.violation && r.violation.name === 'incompatible-config'
}

export function isNotRunning (r: AppDependency) {
  return r.violation && r.violation.name === 'incompatible-status'
}

export function isVersionMismatch (r: AppDependency) {
  return r.violation && r.violation.name === 'incompatible-version'
}

export function isInstalling (r: AppDependency) {
  return r.violation && r.violation.name === 'incompatible-status' && r.violation.status === AppStatus.INSTALLING
}


// both or none
export function getInstalledViolationSeverity (r: InstalledAppDependency): DependencyViolationSeverity {
  if (r.violation) return DependencyViolationSeverity.REQUIRED
  return DependencyViolationSeverity.NONE
}
// e.g. of I try to uninstall a thing, and some installed apps break, those apps will be returned as instances of this type.
export type DependentBreakage = Omit<BaseApp, 'versionInstalled' | 'status'>

export type DependencyViolation =
  { name: 'missing' } |
  { name: 'incompatible-version' } |
  { name: 'incompatible-config'; ruleViolations: string[]; } |
  { name: 'incompatible-status'; status: AppStatus; }

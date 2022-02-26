import { PackageState } from '@start9labs/shared'

export type LocalPackages = Record<
  string,
  {
    state: PackageState
    manifest: { version: string }
  }
>

import { DependentInfo } from 'src/app/types/dependent-info'

export interface PackageConfigData {
  readonly pkgId: string
  readonly dependentInfo?: DependentInfo
}

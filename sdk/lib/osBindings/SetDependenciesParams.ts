// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { DependencyRequirement } from "./DependencyRequirement"
import type { Guid } from "./Guid"

export type SetDependenciesParams = {
  procedureId: Guid
  dependencies: Array<DependencyRequirement>
}
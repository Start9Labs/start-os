import { VersionRange } from "./exver"

export class Dependency {
  constructor(
    readonly data:
      | {
          type: "running"
          versionRange: VersionRange
          registryUrl: string
          healthChecks: string[]
        }
      | {
          type: "exists"
          versionRange: VersionRange
          registryUrl: string
        },
  ) {}
}

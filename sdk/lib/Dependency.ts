import { Checker } from "./emverLite/mod"

export class Dependency {
  constructor(
    readonly data:
      | {
          type: "running"
          versionSpec: Checker
          registryUrl: string
          healthChecks: string[]
        }
      | {
          type: "exists"
          versionSpec: Checker
          registryUrl: string
        },
  ) {}
}

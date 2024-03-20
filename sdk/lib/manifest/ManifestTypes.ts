import { ValidEmVer } from "../emverLite/mod"
import { ActionMetadata } from "../types"

export interface Container {
  /** This should be pointing to a docker container name */
  image: string
  /** These should match the manifest data volumes */
  mounts: Record<string, string>
  /** Default is 64mb */
  shmSizeMb?: `${number}${"mb" | "gb" | "b" | "kb"}`
  /** if more than 30s to shutdown */
  sigtermTimeout?: `${number}${"s" | "m" | "h"}`
}

export type ManifestVersion = ValidEmVer

export type SDKManifest = {
  /**  The package identifier used by the OS. This must be unique amongst all other known packages */
  readonly id: string
  /** A human readable service title */
  readonly title: string
  /** Service version - accepts up to four digits, where the last confirms to revisions necessary for StartOs
   * - see documentation: https://github.com/Start9Labs/emver-rs. This value will change with each release of
   * the service
   */
  readonly version: ManifestVersion
  /** Release notes for the update - can be a string, paragraph or URL */
  readonly releaseNotes: string
  /** The type of license for the project. Include the LICENSE in the root of the project directory. A license is required for a Start9 package.*/
  readonly license: string // name of license
  /** A list of normie (hosted, SaaS, custodial, etc) services this services intends to replace */
  readonly replaces: Readonly<string[]>
  /** The Start9 wrapper repository URL for the package. This repo contains the manifest file (this),
   * any scripts necessary for configuration, backups, actions, or health checks (more below). This key
   * must exist. But could be embedded into the source repository
   */
  readonly wrapperRepo: string
  /** The original project repository URL. There is no upstream repo in this example */
  readonly upstreamRepo: string
  /**  URL to the support site / channel for the project. This key can be omitted if none exists, or it can link to the original project repository issues */
  readonly supportSite: string
  /** URL to the marketing site for the project. If there is no marketing site, it can link to the original project repository */
  readonly marketingSite: string
  /** URL where users can donate to the upstream project */
  readonly donationUrl: string | null
  /**Human readable descriptions for the service. These are used throughout the StartOS user interface, primarily in the marketplace. */
  readonly description: {
    /**This is the first description visible to the user in the marketplace */
    readonly short: string
    /** This description will display with additional details in the service's individual marketplace page */
    readonly long: string
  }

  /** Defines the os images needed to run the container processes */
  readonly images: string[]
  /** This denotes readonly asset directories that should be available to mount to the container.
   * Assuming that there will be three files with names along the lines:
   * icon.* : the icon that will be this packages icon on the ui
   * LICENSE : What the license is for this service
   * Instructions : to be seen in the ui section of the package
   * */
  readonly assets: string[]
  /** This denotes any data volumes that should be available to mount to the container */
  readonly volumes: string[]

  readonly alerts: {
    readonly install: string | null
    readonly update: string | null
    readonly uninstall: string | null
    readonly restore: string | null
    readonly start: string | null
    readonly stop: string | null
  }
  readonly dependencies: Readonly<Record<string, ManifestDependency>>
}

export interface ManifestDependency {
  /** The range of versions that would satisfy the dependency
   *
   * ie: >=3.4.5 <4.0.0
   */
  version: string
  /**
   * A human readable explanation on what the dependency is used for
   */
  description: string | null
  requirement:
    | {
        type: "opt-in"
        /**
         * The human readable explanation on how to opt-in to the dependency
         */
        how: string
      }
    | {
        type: "opt-out"
        /**
         * The human readable explanation on how to opt-out to the dependency
         */
        how: string
      }
    | {
        type: "required"
      }
}

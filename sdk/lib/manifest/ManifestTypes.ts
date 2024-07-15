import { ValidExVer } from "../emverLite/mod"
import {
  ActionMetadata,
  HardwareRequirements,
  ImageConfig,
  ImageId,
  ImageSource,
} from "../types"

export type ManifestVersion = ValidExVer

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
  readonly satisfies?: ManifestVersion[]
  /** Release notes for the update - can be a string, paragraph or URL */
  readonly releaseNotes: string
  /** The type of license for the project. Include the LICENSE in the root of the project directory. A license is required for a Start9 package.*/
  readonly license: string // name of license
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
  readonly images: Record<ImageId, SDKImageConfig>
  /** This denotes readonly asset directories that should be available to mount to the container.
   * These directories are expected to be found in `assets/<id>` at pack time.
   **/
  readonly assets: string[]
  /** This denotes any data volumes that should be available to mount to the container */
  readonly volumes: string[]

  readonly alerts?: {
    readonly install?: string | null
    readonly update?: string | null
    readonly uninstall?: string | null
    readonly restore?: string | null
    readonly start?: string | null
    readonly stop?: string | null
  }
  readonly hasConfig?: boolean
  readonly dependencies: Readonly<Record<string, ManifestDependency>>
  readonly hardwareRequirements?: {
    readonly device?: { display?: RegExp; processor?: RegExp }
    readonly ram?: number | null
    readonly arch?: string[] | null
  }
}

export type SDKImageConfig = {
  source: Exclude<ImageSource, "packed">
  arch?: string[]
  emulateMissingAs?: string | null
}

export type ManifestDependency = {
  /**
   * A human readable explanation on what the dependency is used for
   */
  readonly description: string | null
  /**
   * Determines if the dependency is optional or not. Times that optional that are good include such situations
   * such as being able to toggle other services or to use a different service for the same purpose.
   */
  readonly optional: boolean
  /**
   * A url or local path for an s9pk that satisfies this dependency
   */
  readonly s9pk: string
}

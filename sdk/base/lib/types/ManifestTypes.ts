import { T } from ".."
import { ImageId, ImageSource } from "../types"

export type SDKManifest = {
  /**
   * The package identifier used by StartOS. This must be unique amongst all other known packages.
   * @example nextcloud
   * */
  readonly id: string
  /**
   * The human readable name of your service
   * @example Nextcloud
   * */
  readonly title: string
  /**
   * The name of the software license for this project. The license itself should be included in the root of the project directory.
   * @example MIT
   */
  readonly license: string
  /**
   * URL of the StartOS package repository
   * @example `https://github.com/Start9Labs/nextcloud-startos`
   */
  readonly wrapperRepo: string
  /**
   * URL of the upstream service repository
   * @example `https://github.com/nextcloud/docker`
   */
  readonly upstreamRepo: string
  /**
   * URL where users can get help using the upstream service
   * @example `https://github.com/nextcloud/docker/issues`
   */
  readonly supportSite: string
  /**
   * URL where users can learn more about the upstream service
   * @example `https://nextcloud.com`
   */
  readonly marketingSite: string
  /**
   * (optional) URL where users can donate to the upstream project
   * @example `https://nextcloud.com/contribute/`
   */
  readonly donationUrl: string | null
  /**
   * URL where users can find instructions on how to use the service
   */
  readonly docsUrl: string
  readonly description: {
    /** Short description to display on the marketplace list page. Max length 80 chars. */
    readonly short: string
    /** Long description to display on the marketplace details page for this service. Max length 500 chars. */
    readonly long: string
  }
  /**
   * override the StartOS version this package was made for
   * defaults to the version the SDK was built for
   */
  osVersion?: string
  /**
   * @description A mapping of OS images needed to run the container processes. Each image ID is a unique key.
   * @example
   * Using dockerTag...
   *
   * ```
    images: {
      main: {
        source: {
          dockerTag: 'start9/hello-world',
        },
      },
    },
   * ```
   * @example
   * Using dockerBuild...
   *
   * ```
    images: {
      main: {
        source: {
          dockerBuild: {
            dockerFile: '../Dockerfile',
            workdir: '.',
          },
        },
      },
    },
   * ```
   */
  readonly images: Record<ImageId, SDKImageInputSpec>
  /**
   * @description A list of data volumes that will mount to the container. Must contain at least one volume.
   * @example ['main']
   */
  readonly volumes: string[]

  readonly alerts?: {
    /** An warning alert requiring user confirmation before proceeding with initial installation of this service. */
    readonly install?: string | null
    /** An warning alert requiring user confirmation before updating this service. */
    readonly update?: string | null
    /** An warning alert requiring user confirmation before uninstalling this service. */
    readonly uninstall?: string | null
    /** An warning alert requiring user confirmation before restoring this service from backup. */
    readonly restore?: string | null
    /** An warning alert requiring user confirmation before starting this service. */
    readonly start?: string | null
    /** An warning alert requiring user confirmation before stopping this service. */
    readonly stop?: string | null
  }
  /**
   * @description A mapping of service dependencies to be displayed to users when viewing the Marketplace
   * @property {string} description - An explanation of why this service is a dependency.
   * @property {boolean} optional - Whether or not this dependency is required or contingent on user configuration.
   * @property {string} s9pk - A path or url to an s9pk of the dependency to extract metadata at build time
   * @example
   * ```
    dependencies: {
      'hello-world': {
        description: 'A moon needs a world',
        optional: false,
        s9pk: '',
      },
    },
   * ```
   */
  readonly dependencies: Record<string, ManifestDependency>
  /**
   * @description (optional) A set of hardware requirements for this service. If the user's machine
   *   does not meet these requirements, they will not be able to install this service.
   * @property {object[]} devices - List of required devices (display or processor).
   *    `pattern` refers to a regular expression that at least one device of the specified class must match
   *    `patternDescription` is what will be displayed to the user about what kind of device is required
   * @property {number} ram - Minimum RAM requirement (in megabytes MB)
   * @property {string[]} arch - List of supported arches
   * @example
   * ```
    hardwareRequirements: {
      devices: [
        { class: 'display', pattern: 'CometLake', patternDescription: 'A CometLake (10th generation) Intel Integrated GPU' },
        { class: 'processor', pattern: 'i[3579]-10[0-9]{3}U CPU', patternDescription: 'A 10th Generation Intel i-Series processor' },
      ],
      ram: 8192,
      arch: ['x86-64'],
    },
   * ```
   */
  readonly hardwareRequirements?: {
    readonly device?: T.DeviceFilter[]
    readonly ram?: number | null
    readonly arch?: string[] | null
  }

  /**
   * @description Enable access to hardware acceleration devices (such as /dev/dri, or /dev/nvidia*)
   */
  readonly hardwareAcceleration?: boolean
}

// this is hacky but idk a more elegant way
type ArchOptions = {
  0: ["x86_64", "aarch64", "riscv64"]
  1: ["aarch64", "x86_64", "riscv64"]
  2: ["x86_64", "riscv64", "aarch64"]
  3: ["aarch64", "riscv64", "x86_64"]
  4: ["riscv64", "x86_64", "aarch64"]
  5: ["riscv64", "aarch64", "x86_64"]
  6: ["x86_64", "aarch64"]
  7: ["aarch64", "x86_64"]
  8: ["x86_64", "riscv64"]
  9: ["aarch64", "riscv64"]
  10: ["riscv64", "aarch64"]
  11: ["riscv64", "x86_64"]
  12: ["x86_64"]
  13: ["aarch64"]
  14: ["riscv64"]
}
export type SDKImageInputSpec = {
  [A in keyof ArchOptions]: {
    source: Exclude<ImageSource, "packed">
    arch?: ArchOptions[A]
    emulateMissingAs?: ArchOptions[A][number] | null
  }
}[keyof ArchOptions]

export type ManifestDependency = T.Manifest["dependencies"][string]

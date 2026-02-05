/**
 * @module ManifestTypes
 *
 * Defines the type for the service manifest, which contains all metadata about
 * a StartOS package including its name, description, images, volumes, dependencies,
 * and other configuration.
 *
 * The manifest is defined in your package and exported as the `manifest` constant.
 * It's used by the SDK for type checking and by StartOS for package management.
 *
 * @example
 * ```typescript
 * import { sdk } from './sdk'
 *
 * export const manifest = sdk.Manifest({
 *   id: 'myservice',
 *   title: 'My Service',
 *   license: 'MIT',
 *   images: { main: { source: { dockerTag: 'myimage:latest' } } },
 *   volumes: ['main'],
 *   dependencies: {},
 *   // ... other required fields
 * })
 * ```
 */
import { T } from ".."
import { ImageId, ImageSource } from "../types"

/**
 * The manifest type for StartOS service packages.
 *
 * This is the primary type used to describe a service package. All fields provide
 * metadata used by StartOS for installation, marketplace display, and runtime configuration.
 *
 * Required fields include package identification (id, title), licensing info,
 * repository URLs, descriptions, and technical specifications (images, volumes).
 */
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
    readonly short: T.LocaleString
    /** Long description to display on the marketplace details page for this service. Max length 500 chars. */
    readonly long: T.LocaleString
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
    readonly install?: T.LocaleString | null
    /** An warning alert requiring user confirmation before updating this service. */
    readonly update?: T.LocaleString | null
    /** An warning alert requiring user confirmation before uninstalling this service. */
    readonly uninstall?: T.LocaleString | null
    /** An warning alert requiring user confirmation before restoring this service from backup. */
    readonly restore?: T.LocaleString | null
    /** An warning alert requiring user confirmation before starting this service. */
    readonly start?: T.LocaleString | null
    /** An warning alert requiring user confirmation before stopping this service. */
    readonly stop?: T.LocaleString | null
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
   * @example
   * ```
    hardwareRequirements: {
      devices: [
        { class: 'display', pattern: 'CometLake', patternDescription: 'A CometLake (10th generation) Intel Integrated GPU' },
        { class: 'processor', pattern: 'i[3579]-10[0-9]{3}U CPU', patternDescription: 'A 10th Generation Intel i-Series processor' },
      ],
      ram: 8192,
    },
   * ```
   */
  readonly hardwareRequirements?: {
    readonly device?: T.DeviceFilter[]
    readonly ram?: number | null
  }

  /**
   * @description Enable access to hardware acceleration devices (such as /dev/dri, or /dev/nvidia*)
   */
  readonly hardwareAcceleration?: boolean
}

/**
 * @internal
 * Helper type for generating all valid architecture combinations.
 * Allows specifying one, two, or three target architectures in any order.
 */
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

/**
 * Configuration for a Docker image used by the service.
 *
 * Specifies where to get the image (Docker Hub, local build) and
 * which CPU architectures it supports.
 *
 * @example
 * ```typescript
 * // Using a pre-built Docker Hub image
 * {
 *   source: { dockerTag: 'nginx:latest' },
 *   arch: ['x86_64', 'aarch64']
 * }
 *
 * // Building from a local Dockerfile
 * {
 *   source: {
 *     dockerBuild: {
 *       dockerFile: './Dockerfile',
 *       workdir: '.'
 *     }
 *   },
 *   arch: ['x86_64']
 * }
 *
 * // With NVIDIA GPU support
 * {
 *   source: { dockerTag: 'tensorflow/tensorflow:latest-gpu' },
 *   arch: ['x86_64'],
 *   nvidiaContainer: true
 * }
 * ```
 */
export type SDKImageInputSpec = {
  [A in keyof ArchOptions]: {
    /** Where to get the image (Docker tag or local build) */
    source: Exclude<ImageSource, "packed">
    /** CPU architectures this image supports */
    arch?: ArchOptions[A]
    /** If architecture is missing, use this architecture with emulation */
    emulateMissingAs?: ArchOptions[A][number] | null
    /** Enable NVIDIA container runtime for GPU acceleration */
    nvidiaContainer?: boolean
  }
}[keyof ArchOptions]

/**
 * Configuration for a service dependency.
 * Extracted from the main Manifest type for dependency declarations.
 */
export type ManifestDependency = T.Manifest["dependencies"][string]

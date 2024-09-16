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
  readonly description: {
    /** Short description to display on the marketplace list page. Max length 80 chars. */
    readonly short: string
    /** Long description to display on the marketplace details page for this service. Max length 500 chars. */
    readonly long: string
  }
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
   * @description A list of readonly asset directories that will mount to the container. Each item here must
   *   correspond to a directory in the /assets directory of this project.
   *
   *   Most projects will not make use of this.
   * @example []
   */
  readonly assets: string[]
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
   * @property {boolean} optional - Whether or not this dependency is required or contingent on user inputSpecuration.
   * @property {string} s9pk - TODO Aiden what goes here?
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
   * @property {object[]} devices - TODO Aiden confirm type on the left. List of required devices (displays or processors).
   * @property {number} ram - Minimum RAM requirement (in megabytes MB)
   * @property {string[]} arch - List of supported arches
   * @example
   * ```
    TODO Aiden verify below and provide examples for devices
    hardwareRequirements: {
      devices: [
        { class: 'display', value: '' },
        { class: 'processor', value: '' },
      ],
      ram: 8192,
      arch: ['x86-64'],
    },
   * ```
   */
  readonly hardwareRequirements?: {
    readonly device?: { display?: RegExp; processor?: RegExp }
    readonly ram?: number | null
    readonly arch?: string[] | null
  }
}

export type SDKImageInputSpec = {
  source: Exclude<ImageSource, "packed">
  arch?: string[]
  emulateMissingAs?: string | null
}

export type ManifestDependency = {
  readonly description: string | null
  readonly optional: boolean
  readonly s9pk: string
}

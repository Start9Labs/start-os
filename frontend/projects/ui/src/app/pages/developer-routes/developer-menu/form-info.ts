import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { DevProjectData } from 'src/app/services/patch-db/data-model'

export type BasicInfo = {
  id: string
  title: string
  'service-version-number': string
  'release-notes': string
  license: string
  'wrapper-repo': string
  'upstream-repo'?: string
  'support-site'?: string
  'marketing-site'?: string
  description: {
    short: string
    long: string
  }
}

export function getBasicInfoSpec(devData: DevProjectData): ConfigSpec {
  const basicInfo = devData['basic-info']
  return {
    id: {
      type: 'string',
      name: 'ID',
      description: 'The package identifier used by the OS',
      placeholder: 'e.g. bitcoind',
      nullable: false,
      masked: false,
      copyable: false,
      pattern: '^([a-z][a-z0-9]*)(-[a-z0-9]+)*$',
      'pattern-description': 'Must be kebab case',
      default: basicInfo?.id,
    },
    title: {
      type: 'string',
      name: 'Service Name',
      description: 'A human readable service title',
      placeholder: 'e.g. Bitcoin Core',
      nullable: false,
      masked: false,
      copyable: false,
      default: basicInfo ? basicInfo.title : devData.name,
    },
    'service-version-number': {
      type: 'string',
      name: 'Service Version',
      description:
        'Service version - accepts up to four digits, where the last confirms to revisions necessary for embassyOS - see documentation: https://github.com/Start9Labs/emver-rs. This value will change with each release of the service',
      placeholder: 'e.g. 0.1.2.3',
      nullable: false,
      masked: false,
      copyable: false,
      pattern: '^([0-9]+).([0-9]+).([0-9]+).([0-9]+)$',
      'pattern-description': 'Must be valid Emver version',
      default: basicInfo?.['service-version-number'],
    },
    description: {
      type: 'object',
      name: 'Marketplace Descriptions',
      spec: {
        short: {
          type: 'string',
          name: 'Short Description',
          description:
            'This is the first description visible to the user in the marketplace',
          nullable: false,
          masked: false,
          copyable: false,
          textarea: true,
          default: basicInfo?.description?.short,
          pattern: '^.{1,320}$',
          'pattern-description': 'Must be shorter than 320 characters',
        },
        long: {
          type: 'string',
          name: 'Long Description',
          description: `This description will display with additional details in the service's individual marketplace page`,
          nullable: false,
          masked: false,
          copyable: false,
          textarea: true,
          default: basicInfo?.description?.long,
          pattern: '^.{1,5000}$',
          'pattern-description': 'Must be shorter than 5000 characters',
        },
      },
    },
    'release-notes': {
      type: 'string',
      name: 'Release Notes',
      description:
        'Markdown supported release notes for this version of this service.',
      placeholder: 'e.g. Markdown _release notes_ for **Bitcoin Core**',
      nullable: false,
      masked: false,
      copyable: false,
      textarea: true,
      default: basicInfo?.['release-notes'],
    },
    license: {
      type: 'enum',
      name: 'License',
      values: [
        'gnu-agpl-v3',
        'gnu-gpl-v3',
        'gnu-lgpl-v3',
        'mozilla-public-license-2.0',
        'apache-license-2.0',
        'mit',
        'boost-software-license-1.0',
        'the-unlicense',
        'custom',
      ],
      'value-names': {
        'gnu-agpl-v3': 'GNU AGPLv3',
        'gnu-gpl-v3': 'GNU GPLv3',
        'gnu-lgpl-v3': 'GNU LGPLv3',
        'mozilla-public-license-2.0': 'Mozilla Public License 2.0',
        'apache-license-2.0': 'Apache License 2.0',
        mit: 'mit',
        'boost-software-license-1.0': 'Boost Software License 1.0',
        'the-unlicense': 'The Unlicense',
        custom: 'Custom',
      },
      description: 'Example description for enum select',
      default: 'mit',
    },
    'wrapper-repo': {
      type: 'string',
      name: 'Wrapper Repo',
      description:
        'The Start9 wrapper repository URL for the package. This repo contains the manifest file (this), any scripts necessary for configuration, backups, actions, or health checks',
      placeholder: 'e.g. www.github.com/example',
      nullable: false,
      masked: false,
      copyable: false,
      default: basicInfo?.['wrapper-repo'],
    },
    'upstream-repo': {
      type: 'string',
      name: 'Upstream Repo',
      description: 'The original project repository URL',
      placeholder: 'e.g. www.github.com/example',
      nullable: true,
      masked: false,
      copyable: false,
      default: basicInfo?.['upstream-repo'],
    },
    'support-site': {
      type: 'string',
      name: 'Support Site',
      description: 'URL to the support site / channel for the project',
      placeholder: 'e.g. www.start9labs.com',
      nullable: true,
      masked: false,
      copyable: false,
      default: basicInfo?.['support-site'],
    },
    'marketing-site': {
      type: 'string',
      name: 'Marketing Site',
      description: 'URL to the marketing site / channel for the project',
      placeholder: 'e.g. www.start9labs.com',
      nullable: true,
      masked: false,
      copyable: false,
      default: basicInfo?.['marketing-site'],
    },
  }
}

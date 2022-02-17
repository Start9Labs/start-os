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
      placeholder: 'e.g. Bitcoind',
      nullable: false,
      masked: false,
      copyable: true,
      pattern: '^([a-z][a-z0-9]*)(-[a-z0-9]+)*$',
      'pattern-description': 'Must be kebab case',
      default: basicInfo?.id,
    },
    title: {
      type: 'string',
      name: 'Title',
      description: 'A human readable service title',
      placeholder: 'e.g. Bitcoin Core',
      nullable: false,
      masked: false,
      copyable: true,
      default: basicInfo ? basicInfo.title : devData.name,
    },
    'service-version-number': {
      type: 'string',
      name: 'Service Version Number',
      description:
        'Service version - accepts up to four digits, where the last confirms to revisions necessary for EmbassyOS - see documentation: https://github.com/Start9Labs/emver-rs. This value will change with each release of the service',
      placeholder: 'e.g. 0.1.2.3',
      nullable: false,
      masked: false,
      copyable: true,
      pattern: '^([0-9]+).([0-9]+).([0-9]+).([0-9]+)$',
      'pattern-description': 'Must be valid Emver version',
      default: basicInfo?.['service-version-number'],
    },
    'release-notes': {
      type: 'string',
      name: 'Release Notes',
      description: 'A human readable service title',
      placeholder: 'e.g. Bitcoin Core',
      nullable: false,
      masked: false,
      copyable: true,
      long: true,
      default: basicInfo?.['release-notes'],
    },
    license: {
      type: 'string',
      name: 'License',
      description:
        'The type of license for the project. Include the LICENSE in the root of the project directory',
      placeholder: 'e.g. mit',
      nullable: false,
      masked: false,
      copyable: true,
      default: basicInfo?.license,
    },
    'wrapper-repo': {
      type: 'string',
      name: 'Wrapper Repo',
      description:
        'The Start9 wrapper repository URL for the package. This repo contains the manifest file (this), any scripts necessary for configuration, backups, actions, or health checks',
      placeholder: 'e.g. www.github.com/example',
      nullable: false,
      masked: false,
      copyable: true,
      default: basicInfo?.['wrapper-repo'],
    },
    'upstream-repo': {
      type: 'string',
      name: 'Upstream Repo',
      description: 'The original project repository URL',
      placeholder: 'e.g. www.github.com/example',
      nullable: true,
      masked: false,
      copyable: true,
      default: basicInfo?.['upstream-repo'],
    },
    'support-site': {
      type: 'string',
      name: 'Support Site',
      description: 'URL to the support site / channel for the project',
      placeholder: 'e.g. www.start9labs.com',
      nullable: true,
      masked: false,
      copyable: true,
      default: basicInfo?.['support-site'],
    },
    'marketing-site': {
      type: 'string',
      name: 'Marketing Site',
      description: 'URL to the marketing site / channel for the project',
      placeholder: 'e.g. www.start9labs.com',
      nullable: true,
      masked: false,
      copyable: true,
      default: basicInfo?.['marketing-site'],
    },
    description: {
      name: 'Description',
      type: 'object',
      'unique-by': null,
      description:
        'Human readable descriptors for the service. These are used throughout the EmbassyOS user interface, primarily in the marketplace',
      spec: {
        short: {
          type: 'string',
          name: 'Short',
          description:
            'This is the first description visible to the user in the marketplace',
          nullable: false,
          masked: false,
          copyable: false,
          long: true,
          default: basicInfo?.description?.short,
        },
        long: {
          type: 'string',
          name: 'Long',
          description: `This description will display with additional details in the service's individual marketplace page`,
          nullable: false,
          masked: false,
          copyable: false,
          long: true,
          default: basicInfo?.description?.long,
        },
      },
    },
  }
}

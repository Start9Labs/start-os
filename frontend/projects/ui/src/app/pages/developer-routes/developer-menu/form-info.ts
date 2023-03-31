import { InputSpec } from 'start-sdk/lib/config/config-types'
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

export function getBasicInfoSpec(devData: DevProjectData): InputSpec {
  const basicInfo = devData['basic-info']
  return {
    id: {
      type: 'string',
      inputmode: 'text',
      name: 'ID',
      description: 'The package identifier used by the OS',
      placeholder: 'e.g. bitcoind',
      required: true,
      masked: false,
      pattern: '^([a-z][a-z0-9]*)(-[a-z0-9]+)*$',
      patternDescription: 'Must be kebab case',
      default: basicInfo?.id || '',
      warning: null,
    },
    title: {
      type: 'string',
      inputmode: 'text',
      name: 'Service Name',
      description: 'A human readable service title',
      placeholder: 'e.g. Bitcoin Core',
      required: true,
      masked: false,
      pattern: null,
      patternDescription: null,
      default: basicInfo ? basicInfo.title : devData.name,
      warning: null,
    },
    'service-version-number': {
      type: 'string',
      inputmode: 'text',
      name: 'Service Version',
      description:
        'Service version - accepts up to four digits, where the last confirms to revisions necessary for StartOS - see documentation: https://github.com/Start9Labs/emver-rs. This value will change with each release of the service',
      placeholder: 'e.g. 0.1.2.3',
      required: true,
      masked: false,
      pattern: '^([0-9]+).([0-9]+).([0-9]+).([0-9]+)$',
      patternDescription: 'Must be valid Emver version',
      default: basicInfo?.['service-version-number'] || '',
      warning: null,
    },
    description: {
      type: 'object',
      name: 'Marketplace Descriptions',
      description: null,
      warning: null,
      spec: {
        short: {
          type: 'string',
          inputmode: 'text',
          name: 'Short Description',
          description:
            'This is the first description visible to the user in the marketplace',
          placeholder: null,
          required: true,
          masked: false,
          default: basicInfo?.description?.short || '',
          pattern: '^.{1,320}$',
          patternDescription: 'Must be shorter than 320 characters',
          warning: null,
        },
        long: {
          type: 'textarea',
          name: 'Long Description',
          description: `This description will display with additional details in the service's individual marketplace page`,
          placeholder: null,
          required: true,
          warning: null,
        },
      },
    },
    'release-notes': {
      type: 'string',
      inputmode: 'text',
      name: 'Release Notes',
      description:
        'Markdown supported release notes for this version of this service.',
      placeholder: 'e.g. Markdown _release notes_ for **Bitcoin Core**',
      required: true,
      masked: false,
      pattern: null,
      patternDescription: null,
      default: basicInfo?.['release-notes'] || '',
      warning: null,
    },
    license: {
      type: 'select',
      name: 'License',
      warning: null,
      values: {
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
      description: 'Example description for select',
      required: true,
      default: 'mit',
    },
    'wrapper-repo': {
      type: 'string',
      inputmode: 'url',
      name: 'Wrapper Repo',
      description:
        'The Start9 wrapper repository URL for the package. This repo contains the manifest file (this), any scripts necessary for configuration, backups, actions, or health checks',
      placeholder: 'e.g. www.github.com/example',
      pattern: null,
      patternDescription: null,
      required: true,
      masked: false,
      default: basicInfo?.['wrapper-repo'] || '',
      warning: null,
    },
    'upstream-repo': {
      type: 'string',
      inputmode: 'url',
      name: 'Upstream Repo',
      description: 'The original project repository URL',
      placeholder: 'e.g. www.github.com/example',
      pattern: null,
      patternDescription: null,
      required: false,
      masked: false,
      default: basicInfo?.['upstream-repo'] || '',
      warning: null,
    },
    'support-site': {
      type: 'string',
      inputmode: 'url',
      name: 'Support Site',
      description: 'URL to the support site / channel for the project',
      placeholder: 'e.g. start9.com/support',
      pattern: null,
      patternDescription: null,
      required: false,
      masked: false,
      default: basicInfo?.['support-site'] || '',
      warning: null,
    },
    'marketing-site': {
      type: 'string',
      inputmode: 'url',
      name: 'Website',
      description: 'URL to the marketing site / channel for the project',
      placeholder: 'e.g. start9.com',
      pattern: null,
      patternDescription: null,
      required: false,
      masked: false,
      default: basicInfo?.['marketing-site'] || '',
      warning: null,
    },
  }
}

import { ValueSpecObject } from '@start9labs/start-sdk/cjs/sdk/lib/config/configTypes'
import { TuiDialogOptions } from '@taiga-ui/core'
import { TuiPromptData } from '@taiga-ui/kit'

export function getMarketplaceValueSpec(): ValueSpecObject {
  return {
    type: 'object',
    name: 'Add Custom Registry',
    description: null,
    warning: null,
    spec: {
      url: {
        type: 'text',
        name: 'URL',
        description: 'A fully-qualified URL of the custom registry',
        inputmode: 'url',
        required: true,
        masked: false,
        minLength: null,
        maxLength: null,
        patterns: [
          {
            regex: `https?:\/\/[a-zA-Z0-9][a-zA-Z0-9-\.]+[a-zA-Z0-9]\.[^\s]{2,}`,
            description: 'Must be a valid URL',
          },
        ],
        placeholder: 'e.g. https://example.org',
        default: null,
        warning: null,
        disabled: false,
        immutable: false,
        generate: null,
      },
    },
  }
}

export function getPromptOptions(
  name: string,
): Partial<TuiDialogOptions<TuiPromptData>> {
  return {
    label: 'Confirm',
    size: 's',
    data: {
      content: `Are you sure you want to delete ${name}?`,
      yes: 'Delete',
      no: 'Cancel',
    },
  }
}

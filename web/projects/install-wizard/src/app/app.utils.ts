import { DiskInfo } from '@start9labs/shared'
import { TuiDialogOptions } from '@taiga-ui/core'
import { TuiPromptData } from '@taiga-ui/kit'

export const SUCCESS: Partial<TuiDialogOptions<any>> = {
  label: 'Install Success',
  closeable: false,
  dismissible: false,
  size: 's',
  data: { button: 'Reboot' },
}

export function toWarning(
  disk: DiskInfo | null,
): Partial<TuiDialogOptions<TuiPromptData>> {
  return {
    label: 'Warning',
    size: 's',
    data: {
      content: `This action will COMPLETELY erase the disk ${
        disk?.vendor || 'Unknown Vendor'
      } - ${disk?.model || 'Unknown Model'} and install StartOS in its place`,
      yes: 'Continue',
      no: 'Cancel',
    },
  }
}

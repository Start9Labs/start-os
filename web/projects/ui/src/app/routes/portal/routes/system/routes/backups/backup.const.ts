import { TuiDialogOptions } from '@taiga-ui/core'
import { PromptOptions } from 'src/app/routes/portal/modals/prompt.component'

export const PASSWORD_OPTIONS: Partial<TuiDialogOptions<PromptOptions>> = {
  label: 'Master Password Needed',
  data: {
    message: 'Enter your master password to encrypt this backup.',
    label: 'Master Password',
    placeholder: 'Enter master password',
    useMask: true,
    buttonText: 'Create Backup',
  },
}

export const OLD_OPTIONS: Partial<TuiDialogOptions<PromptOptions>> = {
  label: 'Original Password Needed',
  data: {
    message:
      'This backup was created with a different password. Enter the ORIGINAL password that was used to encrypt this backup.',
    label: 'Original Password',
    placeholder: 'Enter original password',
    useMask: true,
    buttonText: 'Create Backup',
  },
}

export const RESTORE_OPTIONS: Partial<TuiDialogOptions<PromptOptions>> = {
  label: 'Password Required',
  data: {
    message: `Enter the master password that was used to encrypt this backup. On the next screen, you will select the individual services you want to restore.`,
    label: 'Master Password',
    placeholder: 'Enter master password',
    useMask: true,
  },
}

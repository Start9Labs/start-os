export type WizardAction =
  | 'update'
  | 'downgrade'
  | 'uninstall'
  | 'stop'
  | 'configure'

export interface BaseSlide {
  load: () => Promise<void>
  loading: boolean
}

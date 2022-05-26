export interface InstallProgress {
  readonly size: number | null
  readonly downloaded: number
  readonly 'download-complete': boolean
  readonly validated: number
  readonly 'validation-complete': boolean
  readonly unpacked: number
  readonly 'unpack-complete': boolean
}

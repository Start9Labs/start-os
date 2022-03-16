export interface InstallProgress {
  size: number | null
  downloaded: number
  'download-complete': boolean
  validated: number
  'validation-complete': boolean
  unpacked: number
  'unpack-complete': boolean
}

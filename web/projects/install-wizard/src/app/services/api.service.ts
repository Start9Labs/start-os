import { DiskInfo } from '@start9labs/shared'

export abstract class ApiService {
  abstract getDisks(): Promise<GetDisksRes> // install.disk.list
  abstract install(params: InstallReq): Promise<void> // install.execute
  abstract reboot(): Promise<void> // install.reboot
}

export type GetDisksRes = DiskInfo[]

export type InstallReq = {
  logicalname: string
  overwrite: boolean
}

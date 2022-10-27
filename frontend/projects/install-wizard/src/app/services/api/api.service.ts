export abstract class ApiService {
  abstract getDisks(): Promise<GetDisksRes> // install.status
  abstract install(params: InstallReq): Promise<void> // install.execute
}

export type GetDisksRes = Disk[]
export type Disk = {
  logicalname: string
  'embassy-data': boolean
}

export type InstallReq = {
  logicalname: string
  overwrite: boolean
}

import { Injectable } from '@angular/core'

@Injectable({
  providedIn: 'root',
})
export abstract class ApiService {
  // auth
  abstract login(params: LoginReq): Promise<null>
  abstract logout(): Promise<null>
  // exec
  abstract exec(params: ExecReq): Promise<ExecRes>
  // file
  abstract getFile(params: GetFileReq): Promise<GetFileRes>
  abstract setFile(params: GetFileRes): Promise<null>
  // uci
  abstract getUci(params: GetUciReq): Promise<GetUciRes>
  abstract setUci(params: SetUciReq): Promise<null>
}

export type UciSectionType = 'rule'

export type UciSection = {
  type: UciSectionType
  name: string | null
  options: Record<string, string>
  lists: Record<string, string[]>
}

export type UciFile = {
  sections: UciSection[]
  modified: string
}

export type LoginReq = { password: string }

export type ExecReq = {
  command: string
  args: string[]
  timeout: number
}

export type ExecRes = {
  stdout: string
  stderr: string
  exitCode: number
}

export type GetFileReq = {
  path: string
}

export type GetFileRes = {
  contents: string
  modified: string
}

export type SetFileReq = GetFileReq & GetFileRes

export type GetUciReq = {
  name: string
}

export type GetUciRes = UciFile

export type SetUciReq = GetUciReq & GetUciRes

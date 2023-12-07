import fs from "fs/promises"
import * as T from "@start9labs/start-sdk/lib/types"
import * as oet from "./oldEmbassyTypes"
import { HostSystemStartOs } from "../../HostSystemStartOs"

export class PolyfillEffects implements oet.Effects {
  constructor(readonly effects: HostSystemStartOs) {}
  writeFile(input: {
    path: string
    volumeId: string
    toWrite: string
  }): Promise<void> {
    throw new Error("Method not implemented.")
  }
  readFile(input: { volumeId: string; path: string }): Promise<string> {
    throw new Error("Method not implemented.")
  }
  metadata(input: { volumeId: string; path: string }): Promise<oet.Metadata> {
    throw new Error("Method not implemented.")
  }
  createDir(input: { volumeId: string; path: string }): Promise<string> {
    throw new Error("Method not implemented.")
  }
  readDir(input: { volumeId: string; path: string }): Promise<string[]> {
    throw new Error("Method not implemented.")
  }
  removeDir(input: { volumeId: string; path: string }): Promise<string> {
    throw new Error("Method not implemented.")
  }
  removeFile(input: { volumeId: string; path: string }): Promise<void> {
    throw new Error("Method not implemented.")
  }
  writeJsonFile(input: {
    volumeId: string
    path: string
    toWrite: Record<string, unknown>
  }): Promise<void> {
    throw new Error("Method not implemented.")
  }
  readJsonFile(input: {
    volumeId: string
    path: string
  }): Promise<Record<string, unknown>> {
    throw new Error("Method not implemented.")
  }
  runCommand(input: {
    command: string
    args?: string[] | undefined
    timeoutMillis?: number | undefined
  }): Promise<oet.ResultType<string>> {
    throw new Error("Method not implemented.")
  }
  runDaemon(input: { command: string; args?: string[] | undefined }): {
    wait(): Promise<oet.ResultType<string>>
    term(): Promise<void>
  } {
    throw new Error("Method not implemented.")
  }
  chown(input: { volumeId: string; path: string; uid: string }): Promise<null> {
    throw new Error("Method not implemented.")
  }
  chmod(input: {
    volumeId: string
    path: string
    mode: string
  }): Promise<null> {
    throw new Error("Method not implemented.")
  }
  sleep(timeMs: number): Promise<null> {
    throw new Error("Method not implemented.")
  }
  trace(whatToPrint: string): void {
    throw new Error("Method not implemented.")
  }
  warn(whatToPrint: string): void {
    throw new Error("Method not implemented.")
  }
  error(whatToPrint: string): void {
    throw new Error("Method not implemented.")
  }
  debug(whatToPrint: string): void {
    throw new Error("Method not implemented.")
  }
  info(whatToPrint: string): void {
    throw new Error("Method not implemented.")
  }
  is_sandboxed(): boolean {
    throw new Error("Method not implemented.")
  }
  exists(input: { volumeId: string; path: string }): Promise<boolean> {
    throw new Error("Method not implemented.")
  }
  bindLocal(options: {
    internalPort: number
    name: string
    externalPort: number
  }): Promise<string> {
    throw new Error("Method not implemented.")
  }
  bindTor(options: {
    internalPort: number
    name: string
    externalPort: number
  }): Promise<string> {
    throw new Error("Method not implemented.")
  }
  fetch(
    url: string,
    options?:
      | {
          method?:
            | "GET"
            | "POST"
            | "PUT"
            | "DELETE"
            | "HEAD"
            | "PATCH"
            | undefined
          headers?: Record<string, string> | undefined
          body?: string | undefined
        }
      | undefined,
  ): Promise<{
    method: string
    ok: boolean
    status: number
    headers: Record<string, string>
    body?: string | null | undefined
    text(): Promise<string>
    json(): Promise<unknown>
  }> {
    throw new Error("Method not implemented.")
  }
  runRsync(options: {
    srcVolume: string
    dstVolume: string
    srcPath: string
    dstPath: string
    options: oet.BackupOptions
  }): {
    id: () => Promise<string>
    wait: () => Promise<null>
    progress: () => Promise<number>
  } {
    throw new Error("Method not implemented.")
  }
}

import * as fs from "fs/promises"
import * as T from "@start9labs/start-sdk/lib/types"
import * as oet from "./oldEmbassyTypes"
import { HostSystemStartOs } from "../../HostSystemStartOs"
import { Volume } from "../../../Models/Volume"
import * as child_process from "child_process"
import { promisify } from "util"
import { createUtils, Utils } from "@start9labs/start-sdk/lib/util/utils"
import { Manifest } from "./matchManifest"

const fetcher = import("node-fetch")
const execFile = promisify(child_process.execFile)

export class PolyfillEffects implements oet.Effects {
  private utils: Utils<any, any>
  constructor(readonly effects: HostSystemStartOs, private manifest: Manifest) {
    this.utils = createUtils(effects)
  }
  async writeFile(input: {
    path: string
    volumeId: string
    toWrite: string
  }): Promise<void> {
    await fs.writeFile(
      new Volume(input.volumeId, input.path).path,
      input.toWrite,
    )
  }
  async readFile(input: { volumeId: string; path: string }): Promise<string> {
    return (
      await fs.readFile(new Volume(input.volumeId, input.path).path)
    ).toString()
  }
  async metadata(input: {
    volumeId: string
    path: string
  }): Promise<oet.Metadata> {
    const stats = await fs.stat(new Volume(input.volumeId, input.path).path)
    return {
      fileType: stats.isFile() ? "file" : "directory",
      gid: stats.gid,
      uid: stats.uid,
      mode: stats.mode,
      isDir: stats.isDirectory(),
      isFile: stats.isFile(),
      isSymlink: stats.isSymbolicLink(),
      len: stats.size,
      readonly: (stats.mode & 0o200) > 0,
    }
  }
  async createDir(input: { volumeId: string; path: string }): Promise<string> {
    const path = new Volume(input.volumeId, input.path).path
    await fs.mkdir(path, { recursive: true })
    return path
  }
  async readDir(input: { volumeId: string; path: string }): Promise<string[]> {
    return fs.readdir(new Volume(input.volumeId, input.path).path)
  }
  async removeDir(input: { volumeId: string; path: string }): Promise<string> {
    const path = new Volume(input.volumeId, input.path).path
    await fs.rmdir(new Volume(input.volumeId, input.path).path, {
      recursive: true,
    })
    return path
  }
  removeFile(input: { volumeId: string; path: string }): Promise<void> {
    return fs.rm(new Volume(input.volumeId, input.path).path)
  }
  async writeJsonFile(input: {
    volumeId: string
    path: string
    toWrite: Record<string, unknown>
  }): Promise<void> {
    await fs.writeFile(
      new Volume(input.volumeId, input.path).path,
      JSON.stringify(input.toWrite),
    )
  }
  async readJsonFile(input: {
    volumeId: string
    path: string
  }): Promise<Record<string, unknown>> {
    return JSON.parse(
      (
        await fs.readFile(new Volume(input.volumeId, input.path).path)
      ).toString(),
    )
  }
  runCommand({
    command,
    args,
    timeoutMillis,
  }: {
    command: string
    args?: string[] | undefined
    timeoutMillis?: number | undefined
  }): Promise<oet.ResultType<string>> {
    return this.utils
      .runCommand(this.manifest.main.image, [command, ...(args || [])], {})
      .then((x) => ({
        stderr: x.stderr.toString(),
        stdout: x.stdout.toString(),
      }))
      .then((x) => (!!x.stderr ? { error: x.stderr } : { result: x.stdout }))
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
    return new Promise((resolve) => setTimeout(resolve, timeMs))
  }
  trace(whatToPrint: string): void {
    console.trace(whatToPrint)
  }
  warn(whatToPrint: string): void {
    console.warn(whatToPrint)
  }
  error(whatToPrint: string): void {
    console.error(whatToPrint)
  }
  debug(whatToPrint: string): void {
    console.debug(whatToPrint)
  }
  info(whatToPrint: string): void {
    console.log(false)
  }
  is_sandboxed(): boolean {
    return false
  }
  exists(input: { volumeId: string; path: string }): Promise<boolean> {
    return this.metadata(input)
      .then(() => true)
      .catch(() => false)
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
  async fetch(
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
    const fetch: any = await fetcher
    const fetched = await fetch(url, options)
    return {
      method: fetched.type,
      ok: fetched.ok,
      status: fetched.status,
      headers: Object.fromEntries(
        Object.entries<string[]>(fetched.headers.raw()).map(([k, v]) => [
          k,
          v.join(", "),
        ]),
      ),
      body: await fetched.text(),
      text: () => fetched.text(),
      json: () => fetched.json(),
    }
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

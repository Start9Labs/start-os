import * as matches from "ts-matches"
import * as YAML from "yaml"
import * as TOML from "@iarna/toml"
import * as T from "../../../base/lib/types"
import * as fs from "node:fs/promises"
import { asError, partialDiff } from "../../../base/lib/util"

const previousPath = /(.+?)\/([^/]*)$/

const exists = (path: string) =>
  fs.access(path).then(
    () => true,
    () => false,
  )

async function onCreated(path: string) {
  if (path === "/") return
  if (!path.startsWith("/")) path = `${process.cwd()}/${path}`
  if (await exists(path)) {
    return
  }
  const split = path.split("/")
  const filename = split.pop()
  const parent = split.join("/")
  await onCreated(parent)
  const ctrl = new AbortController()
  const watch = fs.watch(parent, { persistent: false, signal: ctrl.signal })
  if (
    await fs.access(path).then(
      () => true,
      () => false,
    )
  ) {
    ctrl.abort("finished")
    return
  }
  for await (let event of watch) {
    if (event.filename === filename) {
      ctrl.abort("finished")
      return
    }
  }
}

function fileMerge(...args: any[]): any {
  let res = args.shift()
  for (const arg of args) {
    if (res === arg) continue
    else if (
      typeof res === "object" &&
      typeof arg === "object" &&
      !Array.isArray(res) &&
      !Array.isArray(arg)
    ) {
      for (const key of Object.keys(arg)) {
        res[key] = fileMerge(res[key], arg[key])
      }
    } else res = arg
  }
  return res
}

/**
 * @description Use this class to read/write an underlying configuration file belonging to the upstream service.
 *
 *   These type definitions should reflect the underlying file as closely as possible. For example, if the service does not require a particular value, it should be marked as optional(), even if your package requires it.
 *
 *   It is recommended to use onMismatch() whenever possible. This provides an escape hatch in case the user edits the file manually and accidentally sets a value to an unsupported type.
 *
 *   Officially supported file types are json, yaml, and toml. Other files types can use "raw"
 *
 *   Choose between officially supported file formats (), or a custom format (raw).
 *
 * @example
 * Below are a few examples
 *
 * ```
 * import { matches, FileHelper } from '@start9labs/start-sdk'
 * const { arrayOf, boolean, literal, literals, object, natural, string } = matches
 *
 * export const jsonFile = FileHelper.json('./inputSpec.json', object({
 *   passwords: arrayOf(string).onMismatch([])
 *   type: literals('private', 'public').optional().onMismatch(undefined)
 * }))
 *
 * export const tomlFile = FileHelper.toml('./inputSpec.toml', object({
 *   url: literal('https://start9.com').onMismatch('https://start9.com')
 *   public: boolean.onMismatch(true)
 * }))
 *
 * export const yamlFile = FileHelper.yaml('./inputSpec.yml', object({
 *   name: string.optional().onMismatch(undefined)
 *   age: natural.optional().onMismatch(undefined)
 * }))
 *
 * export const bitcoinConfFile = FileHelper.raw(
 *   './service.conf',
 *   (obj: CustomType) => customConvertObjToFormattedString(obj),
 *   (str) => customParseStringToTypedObj(str),
 * )
 * ```
 */
export class FileHelper<A> {
  private consts: (() => void)[] = []
  protected constructor(
    readonly path: string,
    readonly writeData: (dataIn: A) => string,
    readonly readData: (stringValue: string) => unknown,
    readonly validate: (value: unknown) => A,
  ) {}

  private async writeFileRaw(data: string): Promise<null> {
    const parent = previousPath.exec(this.path)
    if (parent) {
      await fs.mkdir(parent[1], { recursive: true })
    }

    await fs.writeFile(this.path, data)

    return null
  }

  /**
   * Accepts structured data and overwrites the existing file on disk.
   */
  private async writeFile(data: A): Promise<null> {
    return await this.writeFileRaw(this.writeData(data))
  }

  private async readFileRaw(): Promise<string | null> {
    if (!(await exists(this.path))) {
      return null
    }
    return await fs.readFile(this.path).then((data) => data.toString("utf-8"))
  }

  private async readFile(): Promise<unknown> {
    const raw = await this.readFileRaw()
    if (raw === null) {
      return raw
    }
    return this.readData(raw)
  }

  /**
   * Reads the file from disk and converts it to structured data.
   */
  private async readOnce(): Promise<A | null> {
    const data = await this.readFile()
    if (!data) return null
    return this.validate(data)
  }

  private async readConst(effects: T.Effects): Promise<A | null> {
    const watch = this.readWatch()
    const res = await watch.next()
    if (!this.consts.includes(effects.constRetry))
      this.consts.push(effects.constRetry)
    watch.next().then(() => {
      this.consts = this.consts.filter((a) => a === effects.constRetry)
      effects.constRetry
    })
    return res.value
  }

  private async *readWatch() {
    let res
    while (true) {
      if (await exists(this.path)) {
        const ctrl = new AbortController()
        const watch = fs.watch(this.path, {
          persistent: false,
          signal: ctrl.signal,
        })
        res = await this.readOnce()
        const listen = Promise.resolve()
          .then(async () => {
            for await (const _ of watch) {
              ctrl.abort("finished")
              return null
            }
          })
          .catch((e) => console.error(asError(e)))
        yield res
        await listen
      } else {
        yield null
        await onCreated(this.path).catch((e) => console.error(asError(e)))
      }
    }
    return null
  }

  private readOnChange(
    callback: (value: A | null, error?: Error) => void | Promise<void>,
  ) {
    ;(async () => {
      for await (const value of this.readWatch()) {
        try {
          await callback(value)
        } catch (e) {
          console.error(
            "callback function threw an error @ FileHelper.read.onChange",
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          "callback function threw an error @ FileHelper.read.onChange",
          e,
        ),
      )
  }

  get read() {
    return {
      once: () => this.readOnce(),
      const: (effects: T.Effects) => this.readConst(effects),
      watch: () => this.readWatch(),
      onChange: (
        callback: (value: A | null, error?: Error) => void | Promise<void>,
      ) => this.readOnChange(callback),
    }
  }

  /**
   * Accepts full structured data and overwrites the existing file on disk if it exists.
   */
  async write(effects: T.Effects, data: A) {
    await this.writeFile(this.validate(data))
    if (this.consts.includes(effects.constRetry))
      throw new Error(`Canceled: write after const: ${this.path}`)
    return null
  }

  /**
   * Accepts partial structured data and performs a merge with the existing file on disk.
   */
  async merge(effects: T.Effects, data: T.DeepPartial<A>) {
    const fileDataRaw = await this.readFileRaw()
    let fileData: any = fileDataRaw === null ? null : this.readData(fileDataRaw)
    try {
      fileData = this.validate(fileData)
    } catch (_) {}
    const mergeData = this.validate(fileMerge({}, fileData, data))
    const toWrite = this.writeData(mergeData)
    if (toWrite !== fileDataRaw) {
      this.writeFile(mergeData)
      if (this.consts.includes(effects.constRetry)) {
        const diff = partialDiff(fileData, mergeData as any)
        if (!diff) {
          return null
        }
        throw new Error(`Canceled: write after const: ${this.path}`)
      }
    }
    return null
  }

  /**
   * We wanted to be able to have a fileHelper, and just modify the path later in time.
   * Like one behavior of another dependency or something similar.
   */
  withPath(path: string) {
    return new FileHelper<A>(path, this.writeData, this.readData, this.validate)
  }

  /**
   * Create a File Helper for an arbitrary file type.
   *
   * Provide custom functions for translating data to/from the file format.
   */
  static raw<A>(
    path: string,
    toFile: (dataIn: A) => string,
    fromFile: (rawData: string) => unknown,
    validate: (data: unknown) => A,
  ) {
    return new FileHelper<A>(path, toFile, fromFile, validate)
  }
  /**
   * Create a File Helper for a .json file.
   */
  static json<A>(path: string, shape: matches.Validator<unknown, A>) {
    return new FileHelper<A>(
      path,
      (inData) => JSON.stringify(inData, null, 2),
      (inString) => JSON.parse(inString),
      (data) => shape.unsafeCast(data),
    )
  }
  /**
   * Create a File Helper for a .toml file
   */
  static toml<A extends Record<string, unknown>>(
    path: string,
    shape: matches.Validator<unknown, A>,
  ) {
    return new FileHelper<A>(
      path,
      (inData) => TOML.stringify(inData as any),
      (inString) => TOML.parse(inString),
      (data) => shape.unsafeCast(data),
    )
  }
  /**
   * Create a File Helper for a .yaml file
   */
  static yaml<A extends Record<string, unknown>>(
    path: string,
    shape: matches.Validator<unknown, A>,
  ) {
    return new FileHelper<A>(
      path,
      (inData) => YAML.stringify(inData, null, 2),
      (inString) => YAML.parse(inString),
      (data) => shape.unsafeCast(data),
    )
  }
}

export default FileHelper

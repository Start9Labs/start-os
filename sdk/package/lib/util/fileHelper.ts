import * as matches from "ts-matches"
import * as YAML from "yaml"
import * as TOML from "@iarna/toml"
import * as INI from "ini"
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

function filterUndefined<A>(a: A): A {
  if (a && typeof a === "object") {
    if (Array.isArray(a)) {
      return a.map(filterUndefined) as A
    }
    return Object.entries(a).reduce<Record<string, any>>((acc, [k, v]) => {
      if (v !== undefined) {
        acc[k] = filterUndefined(v)
      }
      return acc
    }, {}) as A
  }
  return a
}

export type Transformers<Raw = unknown, Transformed = unknown> = {
  onRead: (value: Raw) => Transformed
  onWrite: (value: Transformed) => Raw
}

type Validator<T, U> = matches.Validator<T, U> | matches.Validator<unknown, U>

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
    const watch = this.readWatch(effects)
    const res = await watch.next()
    if (effects.constRetry) {
      if (!this.consts.includes(effects.constRetry))
        this.consts.push(effects.constRetry)
      watch.next().then(() => {
        this.consts = this.consts.filter((a) => a === effects.constRetry)
        effects.constRetry && effects.constRetry()
      })
    }
    return res.value
  }

  private async *readWatch(effects: T.Effects) {
    let res
    while (effects.isInContext) {
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
    effects: T.Effects,
    callback: (value: A | null, error?: Error) => void | Promise<void>,
  ) {
    ;(async () => {
      for await (const value of this.readWatch(effects)) {
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
      watch: (effects: T.Effects) => this.readWatch(effects),
      onChange: (
        effects: T.Effects,
        callback: (value: A | null, error?: Error) => void | Promise<void>,
      ) => this.readOnChange(effects, callback),
    }
  }

  /**
   * Accepts full structured data and overwrites the existing file on disk if it exists.
   */
  async write(effects: T.Effects, data: T.AllowReadonly<A> | A) {
    await this.writeFile(this.validate(data))
    if (effects.constRetry && this.consts.includes(effects.constRetry))
      throw new Error(`Canceled: write after const: ${this.path}`)
    return null
  }

  /**
   * Accepts partial structured data and performs a merge with the existing file on disk.
   */
  async merge(effects: T.Effects, data: T.AllowReadonly<T.DeepPartial<A>>) {
    const fileDataRaw = await this.readFileRaw()
    let fileData: any = fileDataRaw === null ? null : this.readData(fileDataRaw)
    try {
      fileData = this.validate(fileData)
    } catch (_) {}
    const mergeData = this.validate(fileMerge({}, fileData, data))
    const toWrite = this.writeData(mergeData)
    if (toWrite !== fileDataRaw) {
      this.writeFile(mergeData)
      if (effects.constRetry && this.consts.includes(effects.constRetry)) {
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

  private static rawTransformed<A extends Transformed, Raw, Transformed>(
    path: string,
    toFile: (dataIn: Raw) => string,
    fromFile: (rawData: string) => Raw,
    validate: (data: Transformed) => A,
    transformers: Transformers<Raw, Transformed> | undefined,
  ) {
    return new FileHelper<A>(
      path,
      (inData) => {
        if (transformers) {
          return toFile(transformers.onWrite(inData))
        }
        return toFile(inData as any as Raw)
      },
      fromFile,
      validate as (a: unknown) => A,
    )
  }

  /**
   * Create a File Helper for a text file
   */
  static string(path: string): FileHelper<string>
  static string<A extends string>(
    path: string,
    shape: Validator<string, A>,
  ): FileHelper<A>
  static string<A extends Transformed, Transformed = string>(
    path: string,
    shape: Validator<Transformed, A>,
    transformers: Transformers<string, Transformed>,
  ): FileHelper<A>
  static string<A extends Transformed, Transformed = string>(
    path: string,
    shape?: Validator<Transformed, A>,
    transformers?: Transformers<string, Transformed>,
  ) {
    return FileHelper.rawTransformed<A, string, Transformed>(
      path,
      (inData) => inData,
      (inString) => inString,
      (data) =>
        (shape || (matches.string as Validator<Transformed, A>)).unsafeCast(
          data,
        ),
      transformers,
    )
  }

  /**
   * Create a File Helper for a .json file.
   */
  static json<A>(
    path: string,
    shape: Validator<unknown, A>,
    transformers?: Transformers,
  ) {
    return FileHelper.rawTransformed(
      path,
      (inData) => JSON.stringify(inData, null, 2),
      (inString) => JSON.parse(inString),
      (data) => shape.unsafeCast(data),
      transformers,
    )
  }

  /**
   * Create a File Helper for a .yaml file
   */
  static yaml<A extends Record<string, unknown>>(
    path: string,
    shape: Validator<Record<string, unknown>, A>,
  ): FileHelper<A>
  static yaml<A extends Transformed, Transformed = Record<string, unknown>>(
    path: string,
    shape: Validator<Transformed, A>,
    transformers: Transformers<Record<string, unknown>, Transformed>,
  ): FileHelper<A>
  static yaml<A extends Transformed, Transformed = Record<string, unknown>>(
    path: string,
    shape: Validator<Transformed, A>,
    transformers?: Transformers<Record<string, unknown>, Transformed>,
  ) {
    return FileHelper.rawTransformed<A, Record<string, unknown>, Transformed>(
      path,
      (inData) => YAML.stringify(inData, null, 2),
      (inString) => YAML.parse(inString),
      (data) => shape.unsafeCast(data),
      transformers,
    )
  }

  /**
   * Create a File Helper for a .toml file
   */
  static toml<A extends TOML.JsonMap>(
    path: string,
    shape: Validator<TOML.JsonMap, A>,
  ): FileHelper<A>
  static toml<A extends Transformed, Transformed = TOML.JsonMap>(
    path: string,
    shape: Validator<Transformed, A>,
    transformers: Transformers<TOML.JsonMap, Transformed>,
  ): FileHelper<A>
  static toml<A extends Transformed, Transformed = TOML.JsonMap>(
    path: string,
    shape: Validator<Transformed, A>,
    transformers?: Transformers<TOML.JsonMap, Transformed>,
  ) {
    return FileHelper.rawTransformed<A, TOML.JsonMap, Transformed>(
      path,
      (inData) => TOML.stringify(inData),
      (inString) => TOML.parse(inString),
      (data) => shape.unsafeCast(data),
      transformers,
    )
  }

  static ini<A extends Record<string, unknown>>(
    path: string,
    shape: Validator<Record<string, unknown>, A>,
    options?: INI.EncodeOptions & INI.DecodeOptions,
  ): FileHelper<A>
  static ini<A extends Transformed, Transformed = Record<string, unknown>>(
    path: string,
    shape: Validator<Transformed, A>,
    options: INI.EncodeOptions & INI.DecodeOptions,
    transformers: Transformers<Record<string, unknown>, Transformed>,
  ): FileHelper<A>
  static ini<A extends Transformed, Transformed = Record<string, unknown>>(
    path: string,
    shape: Validator<Transformed, A>,
    options?: INI.EncodeOptions & INI.DecodeOptions,
    transformers?: Transformers<Record<string, unknown>, Transformed>,
  ): FileHelper<A> {
    return FileHelper.rawTransformed<A, Record<string, unknown>, Transformed>(
      path,
      (inData) => INI.stringify(filterUndefined(inData), options),
      (inString) => INI.parse(inString, options),
      (data) => shape.unsafeCast(data),
      transformers,
    )
  }

  static env<A extends Record<string, string>>(
    path: string,
    shape: Validator<Record<string, string>, A>,
  ): FileHelper<A>
  static env<A extends Transformed, Transformed = Record<string, string>>(
    path: string,
    shape: Validator<Transformed, A>,
    transformers: Transformers<Record<string, string>, Transformed>,
  ): FileHelper<A>
  static env<A extends Transformed, Transformed = Record<string, string>>(
    path: string,
    shape: Validator<Transformed, A>,
    transformers?: Transformers<Record<string, string>, Transformed>,
  ) {
    return FileHelper.rawTransformed<A, Record<string, string>, Transformed>(
      path,
      (inData) =>
        Object.entries(inData)
          .map(([k, v]) => `${k}=${v}`)
          .join("\n"),
      (inString) =>
        Object.fromEntries(
          inString
            .split("\n")
            .map((line) => line.trim())
            .filter((line) => !line.startsWith("#") && line.includes("="))
            .map((line) => line.split("=", 2)),
        ),
      (data) => shape.unsafeCast(data),
      transformers,
    )
  }
}

export default FileHelper

import * as matches from "ts-matches"
import * as YAML from "yaml"
import * as TOML from "@iarna/toml"
import merge from "lodash.merge"
import * as T from "../../../base/lib/types"
import * as fs from "node:fs/promises"
import { asError } from "../../../base/lib/util"

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

/**
 * @description Use this class to read/write an underlying configuration file belonging to the upstream service.
 *
 * Using the static functions, choose between officially supported file formats (json, yaml, toml), or a custom format (raw).
 * @example
 * Below are a few examples
 *
 * ```
 * import { matches, FileHelper } from '@start9labs/start-sdk'
 * const { arrayOf, boolean, literal, literals, object, oneOf, natural, string } = matches
 *
 * export const jsonFile = FileHelper.json('./inputSpec.json', object({
 *   passwords: arrayOf(string)
 *   type: oneOf(literals('private', 'public'))
 * }))
 *
 * export const tomlFile = FileHelper.toml('./inputSpec.toml', object({
 *   url: literal('https://start9.com')
 *   public: boolean
 * }))
 *
 * export const yamlFile = FileHelper.yaml('./inputSpec.yml', object({
 *   name: string
 *   age: natural
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
  protected constructor(
    readonly path: string,
    readonly writeData: (dataIn: A) => string,
    readonly readData: (stringValue: string) => A,
  ) {}

  /**
   * Accepts structured data and overwrites the existing file on disk.
   */
  async write(data: A) {
    const parent = previousPath.exec(this.path)
    if (parent) {
      await fs.mkdir(parent[1], { recursive: true })
    }

    await fs.writeFile(this.path, this.writeData(data))
  }

  /**
   * Reads the file from disk and converts it to structured data.
   */
  async read() {
    if (!(await exists(this.path))) {
      return null
    }
    return this.readData(
      await fs.readFile(this.path).then((data) => data.toString("utf-8")),
    )
  }

  async const(effects: T.Effects) {
    const watch = this.watch()
    const res = await watch.next()
    watch.next().then(effects.constRetry)
    return res.value
  }

  async *watch() {
    let res
    while (true) {
      if (await exists(this.path)) {
        const ctrl = new AbortController()
        const watch = fs.watch(this.path, {
          persistent: false,
          signal: ctrl.signal,
        })
        res = await this.read()
        const listen = Promise.resolve()
          .then(async () => {
            for await (const _ of watch) {
              ctrl.abort("finished")
              return
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
  }

  /**
   * Accepts structured data and performs a merge with the existing file on disk.
   */
  async merge(data: A) {
    const fileData = (await this.read().catch(() => ({}))) || {}
    const mergeData = merge({}, fileData, data)
    return await this.write(mergeData)
  }
  /**
   * Create a File Helper for an arbitrary file type.
   *
   * Provide custom functions for translating data to/from the file format.
   */
  static raw<A>(
    path: string,
    toFile: (dataIn: A) => string,
    fromFile: (rawData: string) => A,
  ) {
    return new FileHelper<A>(path, toFile, fromFile)
  }
  /**
   * Create a File Helper for a .json file.
   */
  static json<A>(path: string, shape: matches.Validator<unknown, A>) {
    return new FileHelper<A>(
      path,
      (inData) => {
        return JSON.stringify(inData, null, 2)
      },
      (inString) => {
        return shape.unsafeCast(JSON.parse(inString))
      },
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
      (inData) => {
        return TOML.stringify(inData as any)
      },
      (inString) => {
        return shape.unsafeCast(TOML.parse(inString))
      },
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
      (inData) => {
        return YAML.stringify(inData, null, 2)
      },
      (inString) => {
        return shape.unsafeCast(YAML.parse(inString))
      },
    )
  }
}

export default FileHelper
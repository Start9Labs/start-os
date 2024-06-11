import * as matches from "ts-matches"
import * as YAML from "yaml"
import * as TOML from "@iarna/toml"
import _ from "lodash"
import * as T from "../types"
import * as fs from "node:fs/promises"

const previousPath = /(.+?)\/([^/]*)$/

/**
 * Used in the get config and the set config exported functions.
 * The idea is that we are going to be reading/ writing to a file, or multiple files. And then we use this tool
 * to keep the same path on the read and write, and have methods for helping with structured data.
 * And if we are not using a structured data, we can use the raw method which forces the construction of a BiMap
 * ```ts
        import {InputSpec} from './InputSpec.ts'
        import {matches, T} from '../deps.ts';
        const { object, string, number, boolean, arrayOf, array, anyOf, allOf } = matches
        const someValidator = object({
        data: string
        })
        const jsonFile = FileHelper.json({
        path: 'data.json',
        validator: someValidator,
        volume: 'main'
        })
        const  tomlFile = FileHelper.toml({
        path: 'data.toml',
        validator: someValidator,
        volume: 'main'
        })
        const rawFile = FileHelper.raw({
        path: 'data.amazingSettings',
        volume: 'main'
        fromData(dataIn: Data): string {
            return `myDatais ///- ${dataIn.data}`
        },
        toData(rawData: string): Data {
        const [,data] = /myDatais \/\/\/- (.*)/.match(rawData)
        return {data}
        }
        })

        export const setConfig : T.ExpectedExports.setConfig= async (effects, config) => {
        await  jsonFile.write({ data: 'here lies data'}, effects)
        }

        export const getConfig: T.ExpectedExports.getConfig = async (effects, config) => ({
        spec: InputSpec,
        config: nullIfEmpty({
            ...jsonFile.get(effects)
        })
    ```
 */
export class FileHelper<A> {
  protected constructor(
    readonly path: string,
    readonly writeData: (dataIn: A) => string,
    readonly readData: (stringValue: string) => A,
  ) {}
  async write(data: A, effects: T.Effects) {
    const parent = previousPath.exec(this.path)
    if (parent) {
      await fs.mkdir(parent[1], { recursive: true })
    }

    await fs.writeFile(this.path, this.writeData(data))
  }
  async read(effects: T.Effects) {
    if (
      !(await fs.access(this.path).then(
        () => true,
        () => false,
      ))
    ) {
      return null
    }
    return this.readData(
      await fs.readFile(this.path).then((data) => data.toString("utf-8")),
    )
  }

  async merge(data: A, effects: T.Effects) {
    const fileData = (await this.read(effects).catch(() => ({}))) || {}
    const mergeData = _.merge({}, fileData, data)
    return await this.write(mergeData, effects)
  }
  /**
   * Create a File Helper for an arbitrary file type.
   *
   * Provide custom functions for translating data to the file format and visa versa.
   */
  static raw<A>(
    path: string,
    toFile: (dataIn: A) => string,
    fromFile: (rawData: string) => A,
  ) {
    return new FileHelper<A>(path, toFile, fromFile)
  }
  /**
   * Create a File Helper for a .json file
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

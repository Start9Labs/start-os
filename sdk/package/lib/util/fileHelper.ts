import { z } from 'zod'
import * as YAML from 'yaml'
import * as TOML from '@iarna/toml'
import * as INI from 'ini'
import * as T from '../../../base/lib/types'
import * as fs from 'node:fs/promises'
import { asError, deepEqual } from '../../../base/lib/util'
import { DropGenerator, DropPromise } from '../../../base/lib/util/Drop'
import { PathBase } from './Volume'

const previousPath = /(.+?)\/([^/]*)$/

const exists = (path: string) =>
  fs.access(path).then(
    () => true,
    () => false,
  )

async function onCreated(path: string) {
  if (path === '/') return
  if (!path.startsWith('/')) path = `${process.cwd()}/${path}`
  if (await exists(path)) {
    return
  }
  const split = path.split('/')
  const filename = split.pop()
  const parent = split.join('/')
  await onCreated(parent)
  const ctrl = new AbortController()
  const watch = fs.watch(parent, { persistent: false, signal: ctrl.signal })
  if (await exists(path)) {
    ctrl.abort()
    return
  }
  if (
    await fs.access(path).then(
      () => true,
      () => false,
    )
  ) {
    ctrl.abort()
    return
  }
  for await (let event of watch) {
    if (event.filename === filename) {
      ctrl.abort('finished')
      return
    }
  }
}

function fileMerge(...args: any[]): any {
  let res = args.shift()
  for (const arg of args) {
    if (res === arg) continue
    else if (
      res &&
      arg &&
      typeof res === 'object' &&
      typeof arg === 'object' &&
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
  if (a && typeof a === 'object') {
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

/**
 * Bidirectional transformers for converting between the raw file format and
 * the application-level data type. Used with FileHelper factory methods.
 *
 * @typeParam Raw - The native type the file format parses to (e.g. `Record<string, unknown>` for JSON)
 * @typeParam Transformed - The application-level type after transformation
 */
export type Transformers<
  Raw = unknown,
  Transformed = unknown,
  Validated extends Transformed = Transformed,
> = {
  /** Transform raw parsed data into the application type */
  onRead: (value: Raw) => Transformed
  /** Transform application data back into the raw format for writing */
  onWrite: (value: Validated) => Raw
}

type ToPath = string | { base: PathBase; subpath: string }
function toPath(path: ToPath): string {
  if (typeof path === 'string') {
    return path
  }
  return path.base.subpath(path.subpath)
}

type Validator<_T, U> = z.ZodType<U>

type ReadType<A> = {
  once: () => Promise<A | null>
  const: (effects: T.Effects) => Promise<A | null>
  watch: (
    effects: T.Effects,
    abort?: AbortSignal,
  ) => AsyncGenerator<A | null, never, unknown>
  onChange: (
    effects: T.Effects,
    callback: (
      value: A | null,
      error?: Error,
    ) => { cancel: boolean } | Promise<{ cancel: boolean }>,
  ) => void
  waitFor: (
    effects: T.Effects,
    pred: (value: A | null) => boolean,
  ) => Promise<A | null>
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
  private consts: [
    () => void,
    any,
    (a: any) => any,
    (left: any, right: any) => any,
  ][] = []
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
    return await fs.readFile(this.path).then((data) => data.toString('utf-8'))
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
  private async readOnce<B>(map: (value: A) => B): Promise<B | null> {
    const data = await this.readFile()
    if (!data) return null
    return map(this.validate(data))
  }

  private async readConst<B>(
    effects: T.Effects,
    map: (value: A) => B,
    eq: (left: B | null | undefined, right: B | null) => boolean,
  ): Promise<B | null> {
    const watch = this.readWatch(effects, map, eq)
    const res = await watch.next()
    if (effects.constRetry) {
      const record: (typeof this.consts)[number] = [
        effects.constRetry,
        res.value,
        map,
        eq,
      ]
      this.consts.push(record)
      watch
        .next()
        .then(() => {
          this.consts = this.consts.filter((r) => r !== record)
          effects.constRetry && effects.constRetry()
        })
        .catch()
    }
    return res.value
  }

  private async *readWatch<B>(
    effects: T.Effects,
    map: (value: A) => B,
    eq: (left: B | null | undefined, right: B | null) => boolean,
    abort?: AbortSignal,
  ) {
    let prev: { value: B | null } | null = null
    while (effects.isInContext && !abort?.aborted) {
      if (await exists(this.path)) {
        const ctrl = new AbortController()
        abort?.addEventListener('abort', () => ctrl.abort())
        const watch = fs.watch(this.path, {
          persistent: false,
          signal: ctrl.signal,
        })
        const newRes = await this.readOnce(map)
        const listen = Promise.resolve()
          .then(async () => {
            for await (const _ of watch) {
              ctrl.abort()
              return null
            }
          })
          .catch((e) => console.error(asError(e)))
        if (!prev || !eq(prev.value, newRes)) {
          console.error('yielding', JSON.stringify({ prev: prev, newRes }))
          yield newRes
        }
        prev = { value: newRes }
        await listen
      } else {
        yield null
        await onCreated(this.path).catch((e) => console.error(asError(e)))
      }
    }
    return new Promise<never>((_, rej) => rej(new Error('aborted')))
  }

  private readOnChange<B>(
    effects: T.Effects,
    callback: (
      value: B | null,
      error?: Error,
    ) => { cancel: boolean } | Promise<{ cancel: boolean }>,
    map: (value: A) => B,
    eq: (left: B | null | undefined, right: B | null) => boolean,
  ) {
    ;(async () => {
      const ctrl = new AbortController()
      for await (const value of this.readWatch(effects, map, eq, ctrl.signal)) {
        try {
          const res = await callback(value)
          if (res.cancel) ctrl.abort()
        } catch (e) {
          console.error(
            'callback function threw an error @ FileHelper.read.onChange',
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          'callback function threw an error @ FileHelper.read.onChange',
          e,
        ),
      )
  }

  private readWaitFor<B>(
    effects: T.Effects,
    pred: (value: B | null, error?: Error) => boolean,
    map: (value: A) => B,
  ): Promise<B | null> {
    const ctrl = new AbortController()
    return DropPromise.of(
      Promise.resolve().then(async () => {
        const watch = this.readWatch(effects, map, (_) => false, ctrl.signal)
        while (true) {
          try {
            const res = await watch.next()
            if (pred(res.value)) {
              ctrl.abort()
              return res.value
            }
            if (res.done) {
              break
            }
          } catch (e) {
            if (pred(null, e as Error)) {
              break
            }
          }
        }
        ctrl.abort()
        return null
      }),
      () => ctrl.abort(),
    )
  }

  /**
   * Create a reactive reader for this file.
   *
   * Returns an object with multiple read strategies:
   * - `once()` - Read the file once and return the parsed value
   * - `const(effects)` - Read once but re-read when the file changes (for use with constRetry)
   * - `watch(effects)` - Async generator yielding new values on each file change
   * - `onChange(effects, callback)` - Fire a callback on each file change
   * - `waitFor(effects, predicate)` - Block until the file value satisfies a predicate
   *
   * @param map - Optional transform function applied after validation
   * @param eq - Optional equality function to deduplicate watch emissions
   */
  read(): ReadType<A>
  read<B>(
    map: (value: A) => B,
    eq?: (left: B | null | undefined, right: B | null) => boolean,
  ): ReadType<B>
  read(
    map?: (value: A) => any,
    eq?: (left: any, right: any) => boolean,
  ): ReadType<any> {
    map = map ?? ((a: A) => a)
    eq = eq ?? deepEqual
    return {
      once: () => this.readOnce(map),
      const: (effects: T.Effects) => this.readConst(effects, map, eq),
      watch: (effects: T.Effects, abort?: AbortSignal) => {
        const ctrl = new AbortController()
        abort?.addEventListener('abort', () => ctrl.abort())
        return DropGenerator.of(
          this.readWatch(effects, map, eq, ctrl.signal),
          () => ctrl.abort(),
        )
      },
      onChange: (
        effects: T.Effects,
        callback: (
          value: A | null,
          error?: Error,
        ) => { cancel: boolean } | Promise<{ cancel: boolean }>,
      ) => this.readOnChange(effects, callback, map, eq),
      waitFor: (effects: T.Effects, pred: (value: A | null) => boolean) =>
        this.readWaitFor(effects, pred, map),
    }
  }

  /**
   * Accepts full structured data and overwrites the existing file on disk if it exists.
   */
  async write(
    effects: T.Effects,
    data: T.AllowReadonly<A> | A,
    options: { allowWriteAfterConst?: boolean } = {},
  ) {
    const newData = this.validate(data)
    await this.writeFile(newData)
    if (!options.allowWriteAfterConst && effects.constRetry) {
      const records = this.consts.filter(([c]) => c === effects.constRetry)
      for (const record of records) {
        const [_, prev, map, eq] = record
        if (!eq(prev, map(newData))) {
          throw new Error(`Canceled: write after const: ${this.path}`)
        }
      }
    }
    return null
  }

  /**
   * Accepts partial structured data and performs a merge with the existing file on disk.
   */
  async merge(
    effects: T.Effects,
    data: T.AllowReadonly<T.DeepPartial<A>>,
    options: { allowWriteAfterConst?: boolean } = {},
  ) {
    const fileDataRaw = await this.readFileRaw()
    let fileData: any = fileDataRaw === null ? null : this.readData(fileDataRaw)
    try {
      fileData = this.validate(fileData)
    } catch (_) {}
    const mergeData = this.validate(fileMerge({}, fileData, data))
    const toWrite = this.writeData(mergeData)
    if (toWrite !== fileDataRaw) {
      this.writeFile(mergeData)
      if (!options.allowWriteAfterConst && effects.constRetry) {
        const records = this.consts.filter(([c]) => c === effects.constRetry)
        for (const record of records) {
          const [_, prev, map, eq] = record
          if (!eq(prev, map(mergeData))) {
            throw new Error(`Canceled: write after const: ${this.path}`)
          }
        }
      }
    }
    return null
  }

  /**
   * We wanted to be able to have a fileHelper, and just modify the path later in time.
   * Like one behavior of another dependency or something similar.
   */
  withPath(path: ToPath) {
    return new FileHelper<A>(
      toPath(path),
      this.writeData,
      this.readData,
      this.validate,
    )
  }

  /**
   * Create a File Helper for an arbitrary file type.
   *
   * Provide custom functions for translating data to/from the file format.
   */
  static raw<A>(
    path: ToPath,
    toFile: (dataIn: A) => string,
    fromFile: (rawData: string) => unknown,
    validate: (data: unknown) => A,
  ) {
    return new FileHelper<A>(toPath(path), toFile, fromFile, validate)
  }

  private static rawTransformed<A extends Transformed, Raw, Transformed>(
    path: ToPath,
    toFile: (dataIn: Raw) => string,
    fromFile: (rawData: string) => Raw,
    validate: (data: Transformed) => A,
    transformers: Transformers<Raw, Transformed, A> | undefined,
  ) {
    return FileHelper.raw<A>(
      path,
      (inData) => {
        if (transformers) {
          return toFile(transformers.onWrite(inData))
        }
        return toFile(inData as any as Raw)
      },
      (fileData) => {
        if (transformers) {
          return transformers.onRead(fromFile(fileData))
        }
        return fromFile(fileData)
      },
      validate as (a: unknown) => A,
    )
  }

  /**
   * Create a File Helper for a text file
   */
  static string(path: ToPath): FileHelper<string>
  static string<A extends string>(
    path: ToPath,
    shape: Validator<string, A>,
  ): FileHelper<A>
  static string<A extends Transformed, Transformed = string>(
    path: ToPath,
    shape: Validator<Transformed, A>,
    transformers: Transformers<string, Transformed, A>,
  ): FileHelper<A>
  static string<A extends Transformed, Transformed = string>(
    path: ToPath,
    shape?: Validator<Transformed, A>,
    transformers?: Transformers<string, Transformed, A>,
  ) {
    return FileHelper.rawTransformed<A, string, Transformed>(
      path,
      (inData) => inData,
      (inString) => inString,
      (data) =>
        (shape || (z.string() as unknown as Validator<Transformed, A>)).parse(
          data,
        ),
      transformers,
    )
  }

  /**
   * Create a File Helper for a .json file.
   */
  static json<A>(path: ToPath, shape: Validator<unknown, A>): FileHelper<A>
  static json<A extends Transformed, Transformed = unknown>(
    path: ToPath,
    shape: Validator<unknown, A>,
    transformers: Transformers<unknown, Transformed, A>,
  ): FileHelper<A>
  static json<A extends Transformed, Transformed = unknown>(
    path: ToPath,
    shape: Validator<unknown, A>,
    transformers?: Transformers<unknown, Transformed, A>,
  ) {
    return FileHelper.rawTransformed(
      path,
      (inData) => JSON.stringify(inData, null, 2),
      (inString) => JSON.parse(inString),
      (data) => shape.parse(data),
      transformers,
    )
  }

  /**
   * Create a File Helper for a .yaml file
   */
  static yaml<A extends Record<string, unknown>>(
    path: ToPath,
    shape: Validator<Record<string, unknown>, A>,
  ): FileHelper<A>
  static yaml<A extends Transformed, Transformed = Record<string, unknown>>(
    path: ToPath,
    shape: Validator<Transformed, A>,
    transformers: Transformers<Record<string, unknown>, Transformed, A>,
  ): FileHelper<A>
  static yaml<A extends Transformed, Transformed = Record<string, unknown>>(
    path: ToPath,
    shape: Validator<Transformed, A>,
    transformers?: Transformers<Record<string, unknown>, Transformed, A>,
  ) {
    return FileHelper.rawTransformed<A, Record<string, unknown>, Transformed>(
      path,
      (inData) => YAML.stringify(inData, null, 2),
      (inString) => YAML.parse(inString),
      (data) => shape.parse(data),
      transformers,
    )
  }

  /**
   * Create a File Helper for a .toml file
   */
  static toml<A extends Record<string, unknown>>(
    path: ToPath,
    shape: Validator<Record<string, unknown>, A>,
  ): FileHelper<A>
  static toml<A extends Transformed, Transformed = Record<string, unknown>>(
    path: ToPath,
    shape: Validator<Transformed, A>,
    transformers: Transformers<Record<string, unknown>, Transformed, A>,
  ): FileHelper<A>
  static toml<A extends Transformed, Transformed = Record<string, unknown>>(
    path: ToPath,
    shape: Validator<Transformed, A>,
    transformers?: Transformers<Record<string, unknown>, Transformed, A>,
  ) {
    return FileHelper.rawTransformed<A, Record<string, unknown>, Transformed>(
      path,
      (inData) => TOML.stringify(inData as TOML.JsonMap),
      (inString) => TOML.parse(inString),
      (data) => shape.parse(data),
      transformers,
    )
  }

  /**
   * Create a File Helper for a .ini file.
   *
   * Supports optional encode/decode options and custom transformers.
   */
  static ini<A extends Record<string, unknown>>(
    path: ToPath,
    shape: Validator<Record<string, unknown>, A>,
    options?: INI.EncodeOptions & INI.DecodeOptions,
  ): FileHelper<A>
  static ini<A extends Transformed, Transformed = Record<string, unknown>>(
    path: ToPath,
    shape: Validator<Transformed, A>,
    options: INI.EncodeOptions & INI.DecodeOptions,
    transformers: Transformers<Record<string, unknown>, Transformed, A>,
  ): FileHelper<A>
  static ini<A extends Transformed, Transformed = Record<string, unknown>>(
    path: ToPath,
    shape: Validator<Transformed, A>,
    options?: INI.EncodeOptions & INI.DecodeOptions,
    transformers?: Transformers<Record<string, unknown>, Transformed, A>,
  ): FileHelper<A> {
    return FileHelper.rawTransformed<A, Record<string, unknown>, Transformed>(
      path,
      (inData) => INI.stringify(filterUndefined(inData), options),
      (inString) => INI.parse(inString, options),
      (data) => shape.parse(data),
      transformers,
    )
  }

  /**
   * Create a File Helper for a .env file (KEY=VALUE format, one per line).
   *
   * Lines starting with `#` are treated as comments and ignored on read.
   */
  static env<A extends Record<string, string>>(
    path: ToPath,
    shape: Validator<Record<string, string>, A>,
  ): FileHelper<A>
  static env<A extends Transformed, Transformed = Record<string, string>>(
    path: ToPath,
    shape: Validator<Transformed, A>,
    transformers: Transformers<Record<string, string>, Transformed, A>,
  ): FileHelper<A>
  static env<A extends Transformed, Transformed = Record<string, string>>(
    path: ToPath,
    shape: Validator<Transformed, A>,
    transformers?: Transformers<Record<string, string>, Transformed, A>,
  ) {
    return FileHelper.rawTransformed<A, Record<string, string>, Transformed>(
      path,
      (inData) =>
        Object.entries(inData)
          .map(([k, v]) => `${k}=${v}`)
          .join('\n'),
      (inString) =>
        Object.fromEntries(
          inString
            .split('\n')
            .map((line) => line.trim())
            .filter((line) => !line.startsWith('#') && line.includes('='))
            .map((line) => {
              const pos = line.indexOf('=')
              return [line.slice(0, pos), line.slice(pos + 1)]
            }),
        ),
      (data) => shape.parse(data),
      transformers,
    )
  }
}

export default FileHelper

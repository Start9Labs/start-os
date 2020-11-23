import {
  ValueSpec, ConfigSpec, UniqueBy, ValueSpecOf, ValueType
} from './config-types'
import * as pointer from 'json-pointer'
import * as handlebars from 'handlebars'
import { Annotations, getDefaultObject, getDefaultUnion, listInnerSpec, mapConfigSpec, Range } from './config-utilities'

export class ConfigCursor<T extends ValueType> {
  private cachedSpec?: ValueSpecOf<T>

  constructor (
    private readonly rootSpec: ConfigSpec,
    private readonly rootOldConfig: object,
    private readonly rootMappedConfig: object = null,
    private readonly rootConfig: object = null,
    private readonly ptr: string = '',
  ) {
    if (!this.rootOldConfig) {
      this.rootOldConfig = getDefaultObject(this.rootSpec)
    }
    if (!this.rootMappedConfig) {
      this.rootMappedConfig = JSON.parse(JSON.stringify(this.rootOldConfig))
      mapConfigSpec(this.rootSpec, this.rootMappedConfig)
    }
    if (!this.rootConfig) {
      this.rootConfig = JSON.parse(JSON.stringify(this.rootMappedConfig))
    }
  }

  seek<S extends ValueType> (ptr: string): ConfigCursor<S> {
    return new ConfigCursor(
      this.rootSpec,
      this.rootOldConfig,
      this.rootMappedConfig,
      this.rootConfig,
      pointer.compile(
        pointer.parse(this.ptr)
          .concat(pointer.parse(ptr)),
      ),
    )
  }

  seekNext<S extends ValueType> (key: string | number): ConfigCursor<S> {
    return this.seek(pointer.compile([`${key}`]))
  }

  unseek<S extends ValueType> (levels?: number): ConfigCursor<S> {
    let ptr: string
    if (levels === undefined) {
      ptr = ''
    } else {
      // TODO, delete or make use of, it isn't being used so far
      // This is not being used so far
      let ptr_arr = pointer.parse(this.ptr)
      for (let i = 0; i < levels; i++) {
        ptr_arr.pop()
      }
      ptr = pointer.compile(ptr_arr)
    }
    return new ConfigCursor(
      this.rootSpec,
      this.rootOldConfig,
      this.rootMappedConfig,
      this.rootConfig,
      ptr,
    )
  }

  key (): string {
    return pointer.parse(this.ptr).pop()
  }

  oldConfig (): any {
    if (pointer.has(this.rootOldConfig, this.ptr)) {
      return pointer.get(this.rootOldConfig, this.ptr)
    } else {
      return undefined
    }
  }

  mappedConfig (): any {
    if (pointer.has(this.rootMappedConfig, this.ptr)) {
      return pointer.get(this.rootMappedConfig, this.ptr)
    } else {
      return undefined
    }
  }

  toString (): string {
    const spec: ValueSpec = this.spec()
    const config = this.config()
    switch (spec.type) {
      case 'string':
        return config
      case 'number':
        return `${config}${spec.units ? ' ' + spec.units : ''}`
      case 'object':
        return spec.displayAs ? handlebars.compile(spec.displayAs)(config) : ''
      case 'union':
        return spec.displayAs ? handlebars.compile(spec.displayAs)(config) : config[spec.tag.id]
      case 'pointer':
        return 'System Defined'
      default:
        return ''
    }
  }

  // if (config : T) then (spec : ValueSpecOf<T>)
  config (): any {
    if (pointer.has(this.rootConfig, this.ptr)) {
      return pointer.get(this.rootConfig, this.ptr)
    } else {
      return undefined
    }
  }

  // if (config : T) then (spec : ValueSpecOf<T>)
  spec (): ValueSpecOf<T> {
    if (this.cachedSpec) return this.cachedSpec
    const parsed = pointer.parse(this.ptr)

    // We elevate the rootSpec (ConfigSpec) to a dummy ValueSpecObject
    let ret: ValueSpec = {
      type: 'object',
      spec: this.rootSpec,
      nullable: false,
      nullByDefault: false,
      name: 'Config',
      displayAs: 'Config',
      uniqueBy: null,
    }
    let ptr = []
    for (let seg of parsed) {
      switch (ret.type) {
        case 'object':
          ret = ret.spec[seg]
          break
        case 'union':
          if (seg === ret.tag.id) {
            ret = {
              type: 'enum',
              default: ret.default,
              values: Object.keys(ret.variants),
              name: ret.tag.name,
              description: ret.tag.description,
              valueNames: ret.tag.variantNames,
            }
          } else {
            const cfg = this.unseek().seek(pointer.compile(ptr))
            ret = ret.variants[cfg.config()[ret.tag.id]][seg]
          }
          break
        case 'list':
          //in essence, for a list we replace the list typed ValueSpecOf with it's internal ListValueSpec, a ValueSpecOf<T> where config @ ptr is of type T[].
          // we also append default values to it.
          // note also that jsonKey is not used. jsonKey in this case is an index of an array, like 0, 1, etc.
          // this implies that every index of a list has an identical inner spec
          ret = listInnerSpec(ret)
          break
        default:
          return undefined
      }
      if (ret === undefined) break
      ptr.push(seg)
    }
    this.cachedSpec = ret as ValueSpecOf<T>
    return this.cachedSpec
  }

  checkInvalid (): string | null { // null if valid
    const spec: ValueSpec = this.spec()
    const cfg = this.config()
    switch (spec.type) {
      case 'string':
        if (!cfg) {
          return spec.nullable ? null : `${spec.name} is missing.`
        } else if (typeof cfg === 'string') {
          if (!spec.pattern || new RegExp(spec.pattern).test(cfg)) {
            return null
          } else {
            return spec.patternDescription
          }
        } else {
          throw new TypeError(`${this.ptr}: expected string, got ${Array.isArray(cfg) ? 'array' : typeof cfg}`)
        }
      case 'number':
        if (!cfg) {
          return spec.nullable ? null : `${spec.name} is missing.`
        } else if (typeof cfg === 'number') {
          if (spec.integral && cfg != Math.trunc(cfg)) {
            return `${spec.name} must be an integer.`
          }
          try {
            Range.from(spec.range).checkIncludes(cfg)
            return null
          } catch (e) {
            return e.message
          }
        } else {
          throw new TypeError(`${this.ptr}: expected number, got ${Array.isArray(cfg) ? 'array' : typeof cfg}`)
        }
      case 'boolean':
        if (typeof cfg === 'boolean') {
          return null
        } else {
          throw new TypeError(`${this.ptr}: expected boolean, got ${Array.isArray(cfg) ? 'array' : typeof cfg}`)
        }
      case 'enum':
        if (typeof cfg === 'string') {
          return spec.values.includes(cfg) ? null : `${cfg} is not a valid selection.`
        } else {
          throw new TypeError(`${this.ptr}: expected string, got ${Array.isArray(cfg) ? 'array' : typeof cfg}`)
        }
      case 'list':
        if (Array.isArray(cfg)) {
          const range = Range.from(spec.range)
          const min = range.integralMin()
          const max = range.integralMax()
          const length = cfg.length
          if (min && length < min) {
            return spec.subtype === 'enum' ? 'Not enough options selected.' : 'List is too short.'
          }
          if (max && length > max) {
            return spec.subtype === 'enum' ? 'Too many options selected.' : 'List is too long.'
          }
          for (let idx in cfg) {
            let cursor = this.seekNext(idx)
            if (cursor.checkInvalid()) {
              return `Item #${idx + 1} is invalid. ${cursor.checkInvalid()}`
            }
            for (let idx2 in cfg) {
              if (idx !== idx2 && cursor.equals(this.seekNext(idx2))) {
                return `Item #${idx + 1} is not unique.`
              }
            }
          }
          return null
        } else {
          throw new TypeError(`${this.ptr}: expected array, got ${Array.isArray(cfg) ? 'array' : typeof cfg}`)
        }
      case 'object':
        if (!cfg) {
          return spec.nullable ? null : `${spec.name} is missing.`
        } else if (typeof cfg === 'object' && !Array.isArray(cfg)) {
          for (let idx in spec.spec) {
            if (this.seekNext(idx).checkInvalid()) {
              return `${spec.spec[idx].name} is invalid.`
            }
          }
          return null
        } else {
          throw new TypeError(`${this.ptr}: expected object, got ${Array.isArray(cfg) ? 'array' : typeof cfg}`)
        }
      case 'pointer':
        return null
      case 'union':
        if (typeof cfg === 'object' && !Array.isArray(cfg)) {
          if (typeof cfg[spec.tag.id] === 'string') {
            for (let idx in spec.variants[cfg[spec.tag.id]]) {
              if (this.seekNext(idx).checkInvalid()) {
                return `${spec.variants[cfg[spec.tag.id]][idx].name} is invalid.`
              }
            }
            return null
          } else {
            throw new TypeError(`${this.ptr}/${spec.tag.id}: expected string, got ${Array.isArray(cfg) ? 'array' : typeof cfg}`)
          }
        } else {
          throw new TypeError(`${this.ptr}: expected object, got ${Array.isArray(cfg) ? 'array' : typeof cfg}`)
        }
    }
  }

  isNew (): boolean {
    const oldCfg = this.oldConfig()
    const mappedCfg = this.mappedConfig()
    if (mappedCfg && oldCfg && typeof mappedCfg === 'object' && typeof oldCfg === 'object') {
      for (let key in mappedCfg) {
        if (this.seekNext(key).isNew()) return true
      }
      return false
    } else {
      return mappedCfg !== oldCfg
    }
  }

  isEdited (): boolean {
    const cfg = this.config()
    const mappedCfg = this.mappedConfig()
    if (cfg && mappedCfg && typeof cfg === 'object' && typeof mappedCfg === 'object') {
      const spec = this.spec()
      let allKeys
      if (spec.type === 'union') {
        let unionSpec = spec as  ValueSpecOf<'union'>
        const labelForSelection = unionSpec.tag.id
        allKeys = new Set([...Object.keys(unionSpec.variants[cfg[labelForSelection]])])
      } else {
        allKeys = new Set([...Object.keys(cfg), ...Object.keys(mappedCfg)])
      }

      for (let key of allKeys) {
        if (this.seekNext(key).isEdited()) return true
      }
      return false
    } else {
      return cfg !== mappedCfg
    }
  }

  equals (cursor: ConfigCursor<T>): boolean {
    const lhs = this.config()
    const rhs = cursor.config()
    const spec: ValueSpec = this.spec()

    switch (spec.type) {
      case 'string':
      case 'number':
      case 'boolean':
      case 'enum':
        return lhs === rhs
      case 'object':
      case 'union':
        return isEqual(spec.uniqueBy, this as ConfigCursor<'object' | 'union'>, cursor as ConfigCursor<'object' | 'union'>)
      case 'list':
        if (lhs.length !== rhs.length) {
          return false
        }
        for (let idx = 0; idx < lhs.length; idx++) {
          if (!this.seekNext(`${idx}`).equals(cursor.seekNext(`${idx}`))) {
            return false
          }
        }
        return true
      default:
        return false
    }
  }

  getAnnotations (): Annotations<T> {
    const spec: ValueSpec = this.spec()
    switch (spec.type) {
      case 'object': {
        const ret: Annotations<'object'> = {
          self: {
            invalid: this.checkInvalid(),
            edited: this.isEdited(),
            added: this.isNew(),
          },
          members: { },
        }
        for (let key in spec.spec) {
          let annotation: any = this.seekNext(key).getAnnotations()
          if ('self' in annotation) {
            annotation = annotation.self
          }
          ret.members[key] = annotation
        }
        return ret as Annotations<T>
      }
      case 'union': {
        const ret: Annotations<'union'> = {
          self: {
            invalid: this.checkInvalid(),
            edited: this.isEdited(),
            added: this.isNew(),
          },
          members: {
            [spec.tag.id]: this.seekNext<'enum'>(spec.tag.id).getAnnotations(),
          },
        }
        for (let key in spec.variants[this.config()[spec.tag.id]]) {
          let annotation: any = this.seekNext(key).getAnnotations()
          if ('self' in annotation) {
            annotation = annotation.self
          }
          ret.members[key] = annotation
        }
        return ret as Annotations<T>
      }
      case 'list': {
        const ret: Annotations<'list'> = {
          self: {
            invalid: this.checkInvalid(),
            edited: this.isEdited(),
            added: this.isNew(),
          },
          members: [],
        }
        for (let key in this.config()) {
          let annotation: any = this.seekNext(key).getAnnotations()
          if ('self' in annotation) {
            annotation = annotation.self
          }
          ret.members[key] = annotation
        }
        return ret as Annotations<T>
      }
      default:
        return {
          invalid: this.checkInvalid(),
          edited: this.isEdited(),
          added: this.isNew(),
        } as Annotations<T>
    }
  }

  async createFirstEntryForList () {
    const spec: ValueSpec = this.spec()

    if (spec.type === 'object' && !this.config())  {
      pointer.set(this.rootConfig, this.ptr, getDefaultObject(spec.spec))
    }

    if (spec.type === 'union' && !this.config())  {
      pointer.set(this.rootConfig, this.ptr, getDefaultUnion(spec))
    }
  }

  injectModalData (res: { data?: any }): void {
    if (res.data !== undefined) {
      pointer.set(this.rootConfig, this.ptr, res.data)
    }
  }
}

function isEqual (uniqueBy: UniqueBy, lhs: ConfigCursor<'object'>, rhs: ConfigCursor<'object'>): boolean {
  if (uniqueBy === null) {
    return false
  } else if (typeof uniqueBy === 'string') {
    return lhs.seekNext(uniqueBy).equals(rhs.seekNext(uniqueBy))
  } else if ('any' in uniqueBy) {
    for (let subSpec of uniqueBy.any) {
      if (isEqual(subSpec, lhs, rhs)) {
        return true
      }
    }
    return false
  } else if ('all' in uniqueBy) {
    for (let subSpec of uniqueBy.all) {
      if (!isEqual(subSpec, lhs, rhs)) {
        return false
      }
    }
    return true
  }
}
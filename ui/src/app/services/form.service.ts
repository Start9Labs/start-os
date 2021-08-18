import { Injectable } from '@angular/core'
import { AbstractControl, FormArray, FormBuilder, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms'
import { ConfigSpec, isValueSpecListOf, ListValueSpecNumber, ListValueSpecObject, ListValueSpecOf, ListValueSpecString, ListValueSpecUnion, UniqueBy, ValueSpec, ValueSpecEnum, ValueSpecList, ValueSpecListOf, ValueSpecNumber, ValueSpecObject, ValueSpecString, ValueSpecUnion } from '../pkg-config/config-types'
import { getDefaultString, Range } from '../pkg-config/config-utilities'
const Mustache = require('mustache')

@Injectable({
  providedIn: 'root',
})
export class FormService {

  constructor (
    private readonly formBuilder: FormBuilder,
  ) { }

  createForm (config: ConfigSpec, current: { [key: string]: any } = { }): FormGroup {
    return this.getFormGroup(config, [], current)
  }

  getListItemValidators (spec: ValueSpecList) {
    if (isValueSpecListOf(spec, 'string')) {
      return this.stringValidators(spec.spec)
    } else if (isValueSpecListOf(spec, 'number')) {
      return this.numberValidators(spec.spec)
    }
  }

  getUnionObject (spec: ValueSpecUnion | ListValueSpecUnion, selection: string, current?: { [key: string]: any }): FormGroup {
    const { variants, tag } = spec
    const { name, description, 'change-warning' : changeWarning } = isFullUnion(spec) ? spec : { ...spec.tag, 'change-warning': undefined }

    const enumSpec: ValueSpecEnum = {
      type: 'enum',
      name,
      description,
      'change-warning': changeWarning,
      default: selection,
      values: Object.keys(variants),
      'value-names': tag['variant-names'],
    }
    return this.getFormGroup({ [spec.tag.id]: enumSpec, ...spec.variants[selection] }, [], current)
  }

  getFormGroup (config: ConfigSpec, validators: ValidatorFn[] = [], current: { [key: string]: any } = { }): FormGroup {
    let group = { }
    Object.entries(config).map(([key, spec]) => {
      if (spec.type === 'pointer') return
      group[key] = this.getFormEntry(key, spec, current ? current[key] : { })
    })
    return this.formBuilder.group(group, { validators } )
  }

  getListItem (spec: ValueSpecList, entry: any) {
    const listItemValidators = this.getListItemValidators(spec)
    if (isValueSpecListOf(spec, 'string')) {
      return this.formBuilder.control(entry, listItemValidators)
    } else if (isValueSpecListOf(spec, 'number')) {
      return this.formBuilder.control(entry, listItemValidators)
    } else if (isValueSpecListOf(spec, 'enum')) {
      return this.formBuilder.control(entry)
    } else if (isValueSpecListOf(spec, 'object')) {
      return this.getFormGroup(spec.spec.spec, listItemValidators, entry)
    } else if (isValueSpecListOf(spec, 'union')) {
      return this.getUnionObject(spec.spec, spec.spec.default, entry)
    }
  }

  private getFormEntry (key: string, spec: ValueSpec, currentValue: any): FormGroup | FormArray | FormControl {
    let validators: ValidatorFn[]
    let value: any
    switch (spec.type) {
      case 'string':
        validators = this.stringValidators(spec)
        if (currentValue !== undefined) {
          value = currentValue
        } else {
          value = spec.default ? getDefaultString(spec.default) : null
        }
        return this.formBuilder.control(value, validators)
      case 'number':
        validators = this.numberValidators(spec)
        if (currentValue !== undefined) {
          value = currentValue
        } else {
          value = spec.default || null
        }
        return this.formBuilder.control(value, validators)
      case 'object':
        return this.getFormGroup(spec.spec, [], currentValue)
      case 'list':
        validators = this.listValidators(spec)
        const mapped = (Array.isArray(currentValue) ? currentValue : spec.default as any[]).map((entry: any, index) => {
          return this.getListItem(spec, entry)
        })
        return this.formBuilder.array(mapped, validators)
      case 'union':
        return this.getUnionObject(spec, currentValue?.[spec.tag.id] || spec.default, currentValue)
      case 'boolean':
      case 'enum':
        value = currentValue === undefined ? spec.default : currentValue
        return this.formBuilder.control(value)
    }
  }

  private stringValidators (spec: ValueSpecString | ListValueSpecString): ValidatorFn[] {
    const validators: ValidatorFn[] = []

    if (!(spec as ValueSpecString).nullable) {
      validators.push(Validators.required)
    }

    if (spec.pattern) {
      validators.push(Validators.pattern(spec.pattern))
    }

    return validators
  }

  private numberValidators (spec: ValueSpecNumber | ListValueSpecNumber): ValidatorFn[] {
    const validators: ValidatorFn[] = []

    if (!(spec as ValueSpecNumber).nullable) {
      validators.push(Validators.required)
    }

    if (spec.integral) {
      validators.push(isInteger())
    }

    validators.push(numberInRange(spec.range))

    return validators
  }

  private listValidators (spec: ValueSpecList): ValidatorFn[] {
    const validators: ValidatorFn[] = []

    validators.push(listInRange(spec.range))

    if (!isValueSpecListOf(spec, 'enum')) {
      validators.push(listUnique(spec))
    }

    return validators
  }
}

function isFullUnion (spec: ValueSpecUnion | ListValueSpecUnion): spec is ValueSpecUnion {
  return !!(spec as ValueSpecUnion).name
}

export function numberInRange (stringRange: string): ValidatorFn {
  return (control: AbstractControl): ValidationErrors | null => {
    try {
      Range.from(stringRange).checkIncludes(control.value)
      return null
    } catch (e) {
      return { numberNotInRange: { value: control.value } }
    }
  }
}

export function isInteger (): ValidatorFn {
  return (control: AbstractControl): ValidationErrors | null => {
    return control.value == Math.trunc(control.value) ?
      null :
      { numberNotInteger: { value: control.value } }
  }
}

export function listInRange (stringRange: string): ValidatorFn {
  return (control: AbstractControl): ValidationErrors | null => {
    const range = Range.from(stringRange)
    const min = range.integralMin()
    const max = range.integralMax()
    const length = control.value.length
    if ((min && length < min) || (max && length > max)) {
      return { listNotInRange: { value: control.value } }
    } else {
      return null
    }
  }
}

export function listUnique (spec: ValueSpecList): ValidatorFn {
  return (control: AbstractControl): ValidationErrors | null => {
    for (let idx = 0; idx < control.value.length; idx++) {
      for (let idx2 = idx + 1; idx2 < control.value.length; idx2++) {
        if (listItemEquals(spec, control.value[idx], control.value[idx2])) {
          let display1: string
          let display2: string
          let uniqueMessage = isObjectOrUnion(spec.spec) ? uniqueByMessageWrapper(spec.spec['unique-by'], spec.spec, control.value[idx]) : ''


          if (isObjectOrUnion(spec.spec) && spec.spec['display-as']) {
            display1 = `"${(Mustache as any).render(spec.spec['display-as'], control.value[idx])}"`
            display2 = `"${(Mustache as any).render(spec.spec['display-as'], control.value[idx2])}"`
          } else {
            display1 = `Entry ${idx + 1}`
            display2 = `Entry ${idx2 + 1}`
          }

          return { listNotUnique: { value: `${display1} and ${display2} are not unique.${uniqueMessage}` } }
        }
      }
    }
    return null
  }
}

function listItemEquals (spec: ValueSpecList, val1: any, val2: any): boolean {
  switch (spec.subtype) {
    case 'string':
    case 'number':
    case 'enum':
      return val1 == val2
    case 'object':
      return listObjEquals(spec.spec['unique-by'], (spec.spec as ListValueSpecObject), val1, val2)
    case 'union':
      return unionEquals(spec.spec['unique-by'], spec.spec as ListValueSpecUnion, val1, val2)
    default:
      return false
  }
}

function itemEquals (spec: ValueSpec, val1: any, val2: any): boolean {
  switch (spec.type) {
    case 'string':
    case 'number':
    case 'boolean':
    case 'enum':
      return val1 == val2
    case 'object':
      return objEquals(spec['unique-by'], (spec as ValueSpecObject), val1, val2)
    case 'union':
      return unionEquals(spec['unique-by'], (spec as ValueSpecUnion), val1, val2)
    case 'list':
      if (val1.length !== val2.length) {
        return false
      }
      for (let idx = 0; idx < val1.length; idx++) {
        if (listItemEquals(spec, val1[idx], val2[idx])) {
          return false
        }
      }
      return true
    default:
      return false
  }
}

function listObjEquals (uniqueBy: UniqueBy, spec: ListValueSpecObject, val1: any, val2: any): boolean {
  if (uniqueBy === null) {
    return false
  } else if (typeof uniqueBy === 'string') {
    return itemEquals(spec.spec[uniqueBy], val1[uniqueBy], val2[uniqueBy])
  } else if ('any' in uniqueBy) {
    for (let subSpec of uniqueBy.any) {
      if (listObjEquals(subSpec, spec, val1, val2)) {
        return true
      }
    }
    return false
  } else if ('all' in uniqueBy) {
    for (let subSpec of uniqueBy.all) {
      if (!listObjEquals(subSpec, spec, val1, val2)) {
        return false
      }
    }
    return true
  }
}

function objEquals (uniqueBy: UniqueBy, spec: ValueSpecObject, val1: any, val2: any): boolean {
  if (uniqueBy === null) {
    return false
  } else if (typeof uniqueBy === 'string') {
    return itemEquals(spec[uniqueBy], val1[uniqueBy], val2[uniqueBy])
  } else if ('any' in uniqueBy) {
    for (let subSpec of uniqueBy.any) {
      if (objEquals(subSpec, spec, val1, val2)) {
        return true
      }
    }
    return false
  } else if ('all' in uniqueBy) {
    for (let subSpec of uniqueBy.all) {
      if (!objEquals(subSpec, spec, val1, val2)) {
        return false
      }
    }
    return true
  }
}

function unionEquals (uniqueBy: UniqueBy, spec: ValueSpecUnion | ListValueSpecUnion, val1: any, val2: any): boolean {
  const tagId = spec.tag.id
  const variant = spec.variants[val1[tagId]]
  if (uniqueBy === null) {
    return false
  } else if (typeof uniqueBy === 'string') {
    if (uniqueBy === tagId) {
      return val1[tagId] === val2[tagId]
    } else {
      return itemEquals(variant[uniqueBy], val1[uniqueBy], val2[uniqueBy])
    }
  } else if ('any' in uniqueBy) {
    for (let subSpec of uniqueBy.any) {
      if (unionEquals(subSpec, spec, val1, val2)) {
        return true
      }
    }
    return false
  } else if ('all' in uniqueBy) {
    for (let subSpec of uniqueBy.all) {
      if (!unionEquals(subSpec, spec, val1, val2)) {
        return false
      }
    }
    return true
  }
}

function uniqueByMessageWrapper (uniqueBy: UniqueBy, spec: ListValueSpecObject | ListValueSpecUnion, obj: object) {
  let configSpec: ConfigSpec
  if (isUnion(spec)) {
    const variantKey = obj[spec.tag.id]
    configSpec = spec.variants[variantKey]
  } else {
    configSpec = spec.spec
  }

  const message = uniqueByMessage(uniqueBy, configSpec)
  if (message) {
    return ' Must be unique by: ' + message + '.'
  }
}

function uniqueByMessage (uniqueBy: UniqueBy, configSpec: ConfigSpec, outermost = true): string {
  let joinFunc
  const subSpecs = []
  if (uniqueBy === null) {
    return null
  } else if (typeof uniqueBy === 'string') {
    return configSpec[uniqueBy] ? configSpec[uniqueBy].name : uniqueBy
  } else if ('any' in uniqueBy) {
    joinFunc = ' OR '
    for (let subSpec of uniqueBy.any) {
      subSpecs.push(uniqueByMessage(subSpec, configSpec, false))
    }
  } else if ('all' in uniqueBy) {
    joinFunc = ' AND '
    for (let subSpec of uniqueBy.all) {
      subSpecs.push(uniqueByMessage(subSpec, configSpec, false))
    }
  }
  const ret = subSpecs.filter(ss => ss).join(joinFunc)
  return outermost || subSpecs.filter(ss => ss).length === 1 ? ret : '(' + ret + ')'
}

function isObjectOrUnion (spec: any): spec is ListValueSpecObject | ListValueSpecUnion {
  // only lists of objects and unions have unique-by
  return spec['unique-by'] !== undefined
}

function isUnion (spec: any): spec is ListValueSpecUnion {
  // only unions have tag
  return !!spec.tag
}

const sampleUniqueBy: UniqueBy = {
  all: [
    'last name',
    { any: [
      'favorite color',
      null,
    ] },
    { any: [
      'favorite color',
      'first name',
      null,
    ] },
  ],
}

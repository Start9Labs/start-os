import { Injectable } from '@angular/core'
import { AbstractControl, FormArray, FormBuilder, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms'
import { By } from '@angular/platform-browser'
import { ConfigSpec, isValueSpecListOf, ListValueSpecNumber, ListValueSpecObject, ListValueSpecOf, ListValueSpecString, ListValueSpecUnion, UniqueBy, ValueSpec, ValueSpecEnum, ValueSpecList, ValueSpecNumber, ValueSpecObject, ValueSpecString, ValueSpecUnion } from '../pkg-config/config-types'
import { getDefaultString, Range } from '../pkg-config/config-utilities'

@Injectable({
  providedIn: 'root',
})
export class FormService {
  validationMessages: { [key: string]: { type: string, message: string }[] } = { }

  constructor (
    private readonly formBuilder: FormBuilder,
  ) { }

  createForm (config: ConfigSpec, current: { [key: string]: any } = { }): FormGroup {
    return this.getFormGroup(config, [], current)
  }

  getListItemValidators (spec: ValueSpecList, key: string,  index: number) {
    const listKey = `${key}/${index}`
    this.validationMessages[listKey] = []
    if (isValueSpecListOf(spec, 'string')) {
      return this.stringValidators(listKey, spec.spec)
    } else if (isValueSpecListOf(spec, 'number')) {
      return this.numberValidators(listKey, spec.spec)
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

  getListItem (key: string, index: number, spec: ValueSpecList, entry: any) {
    const listItemValidators = this.getListItemValidators(spec, key, index)
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
    this.validationMessages[key] = []
    let validators: ValidatorFn[]
    let value: any
    switch (spec.type) {
      case 'string':
        validators = this.stringValidators(key, spec)
        if (currentValue !== undefined) {
          value = currentValue
        } else {
          value = spec.default ? getDefaultString(spec.default) : null
        }
        return this.formBuilder.control(value, validators)
      case 'number':
        validators = this.numberValidators(key, spec)
        if (currentValue !== undefined) {
          value = currentValue
        } else {
          value = spec.default || null
        }
        return this.formBuilder.control(value, validators)
      case 'object':
        return this.getFormGroup(spec.spec, [], currentValue)
      case 'list':
        validators = this.listValidators(key, spec)
        const mapped = (Array.isArray(currentValue) ? currentValue : spec.default as any[]).map((entry: any, index) => {
          return this.getListItem(key, index, spec, entry)
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

  private stringValidators (key: string, spec: ValueSpecString | ListValueSpecString): ValidatorFn[] {
    const validators: ValidatorFn[] = []

    if (!(spec as ValueSpecString).nullable) {
      validators.push(Validators.required)
      this.validationMessages[key].push({
        type: 'required',
        message: 'Cannot be blank.',
      })
    }

    if (spec.pattern) {
      validators.push(Validators.pattern(spec.pattern))
      this.validationMessages[key].push({
        type: 'pattern',
        message: spec['pattern-description'],
      })
    }

    return validators
  }

  private numberValidators (key: string, spec: ValueSpecNumber | ListValueSpecNumber): ValidatorFn[] {
    const validators: ValidatorFn[] = []

    if (!(spec as ValueSpecNumber).nullable) {
      validators.push(Validators.required)
      this.validationMessages[key].push({
        type: 'required',
        message: 'Cannot be blank.',
      })
    }

    if (spec.integral) {
      validators.push(isInteger())
      this.validationMessages[key].push({
        type: 'numberNotInteger',
        message: 'Number must be an integer.',
      })
    }

    validators.push(numberInRange(spec.range))
    this.validationMessages[key].push({
      type: 'numberNotInRange',
      message: 'Number not in range.',
    })

    return validators
  }

  private listValidators (key: string, spec: ValueSpecList): ValidatorFn[] {
    const validators: ValidatorFn[] = []

    validators.push(listInRange(spec.range))
    this.validationMessages[key].push({
      type: 'listNotInRange',
      message: 'List not in range.',
    })

    if (!isValueSpecListOf(spec, 'enum')) {
      validators.push(listUnique(spec))
      this.validationMessages[key].push({
        type: 'listNotUnique',
        message: 'List contains duplicate entries.',
      })
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
          return { listNotUnique: { value: control.value } }
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
    case 'union':
      return listObjEquals(spec.spec['unique-by'], (spec.spec as ListValueSpecObject), val1, val2)
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
    case 'union':
      return objEquals(spec['unique-by'], (spec as ValueSpec), val1, val2)
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

function objEquals (uniqueBy: UniqueBy, spec: ValueSpec, val1: any, val2: any): boolean {
  if (uniqueBy === null) {
    return false
  } else if (typeof uniqueBy === 'string') {
    return itemEquals(spec, val1, val2)
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
import { Injectable } from '@angular/core'
import {
  FormArray,
  FormBuilder,
  FormControl,
  FormGroup,
  ValidatorFn,
  Validators,
} from '@angular/forms'
import {
  ConfigSpec,
  isValueSpecListOf,
  ListValueSpecNumber,
  ListValueSpecObject,
  ListValueSpecOf,
  ListValueSpecString,
  ListValueSpecUnion,
  UniqueBy,
  ValueSpec,
  ValueSpecEnum,
  ValueSpecList,
  ValueSpecNumber,
  ValueSpecObject,
  ValueSpecString,
  ValueSpecUnion,
} from 'src/app/pkg-config/config-types'
import { getDefaultString, Range } from '../pkg-config/config-utilities'
const Mustache = require('mustache')

@Injectable({
  providedIn: 'root',
})
export class FormService {
  constructor(private readonly formBuilder: FormBuilder) {}

  createForm(
    spec: ConfigSpec,
    current: { [key: string]: any } = {},
  ): FormGroup {
    return this.getFormGroup(spec, [], current)
  }

  getUnionObject(
    spec: ValueSpecUnion | ListValueSpecUnion,
    selection: string,
    current?: { [key: string]: any } | null,
  ): FormGroup {
    const { variants, tag } = spec
    const { name, description, warning } = isFullUnion(spec)
      ? spec
      : { ...spec.tag, warning: undefined }

    const enumSpec: ValueSpecEnum = {
      type: 'enum',
      name,
      description,
      warning,
      default: selection,
      values: Object.keys(variants),
      'value-names': tag['variant-names'],
    }
    return this.getFormGroup(
      { [spec.tag.id]: enumSpec, ...spec.variants[selection] },
      [],
      current,
    )
  }

  getListItem(spec: ValueSpecList, entry: any) {
    const listItemValidators = getListItemValidators(spec)
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

  private getFormGroup(
    config: ConfigSpec,
    validators: ValidatorFn[] = [],
    current?: { [key: string]: any } | null,
  ): FormGroup {
    let group: Record<string, FormGroup | FormArray | FormControl> = {}
    Object.entries(config).map(([key, spec]) => {
      if (spec.type === 'pointer') return
      group[key] = this.getFormEntry(spec, current ? current[key] : undefined)
    })
    return this.formBuilder.group(group, { validators })
  }

  private getFormEntry(
    spec: ValueSpec,
    currentValue?: any,
  ): FormGroup | FormArray | FormControl {
    let validators: ValidatorFn[]
    let value: any
    switch (spec.type) {
      case 'string':
        validators = stringValidators(spec)
        if (currentValue !== undefined) {
          value = currentValue
        } else {
          value = spec.default ? getDefaultString(spec.default) : null
        }
        return this.formBuilder.control(value, validators)
      case 'number':
        validators = numberValidators(spec)
        if (currentValue !== undefined) {
          value = currentValue
        } else {
          value = spec.default || null
        }
        return this.formBuilder.control(value, validators)
      case 'object':
        return this.getFormGroup(spec.spec, [], currentValue)
      case 'list':
        validators = listValidators(spec)
        const mapped = (
          Array.isArray(currentValue) ? currentValue : (spec.default as any[])
        ).map(entry => {
          return this.getListItem(spec, entry)
        })
        return this.formBuilder.array(mapped, validators)
      case 'union':
        const currentSelection = currentValue?.[spec.tag.id]
        const isValid = !!spec.variants[currentSelection]

        return this.getUnionObject(
          spec,
          isValid ? currentSelection : spec.default,
          isValid ? currentValue : undefined,
        )
      case 'boolean':
      case 'enum':
        value = currentValue === undefined ? spec.default : currentValue
        return this.formBuilder.control(value)
      default:
        return this.formBuilder.control(null)
    }
  }
}

function getListItemValidators(spec: ValueSpecList) {
  if (isValueSpecListOf(spec, 'string')) {
    return stringValidators(spec.spec)
  } else if (isValueSpecListOf(spec, 'number')) {
    return numberValidators(spec.spec)
  }
}

function stringValidators(
  spec: ValueSpecString | ListValueSpecString,
): ValidatorFn[] {
  const validators: ValidatorFn[] = []

  if (!(spec as ValueSpecString).nullable) {
    validators.push(Validators.required)
  }

  if (spec.pattern) {
    validators.push(Validators.pattern(spec.pattern))
  }

  return validators
}

function numberValidators(
  spec: ValueSpecNumber | ListValueSpecNumber,
): ValidatorFn[] {
  const validators: ValidatorFn[] = []

  validators.push(isNumber())

  if (!(spec as ValueSpecNumber).nullable) {
    validators.push(Validators.required)
  }

  if (spec.integral) {
    validators.push(isInteger())
  }

  validators.push(numberInRange(spec.range))

  return validators
}

function listValidators(spec: ValueSpecList): ValidatorFn[] {
  const validators: ValidatorFn[] = []

  validators.push(listInRange(spec.range))

  validators.push(listItemIssue())

  if (!isValueSpecListOf(spec, 'enum')) {
    validators.push(listUnique(spec))
  }

  return validators
}

function isFullUnion(
  spec: ValueSpecUnion | ListValueSpecUnion,
): spec is ValueSpecUnion {
  return !!(spec as ValueSpecUnion).name
}

export function numberInRange(stringRange: string): ValidatorFn {
  return control => {
    const value = control.value
    if (!value) return null
    try {
      Range.from(stringRange).checkIncludes(value)
      return null
    } catch (e: any) {
      return { numberNotInRange: { value: `Number must be ${e.message}` } }
    }
  }
}

export function isNumber(): ValidatorFn {
  return control =>
    !control.value || control.value == Number(control.value)
      ? null
      : { notNumber: { value: control.value } }
}

export function isInteger(): ValidatorFn {
  return control =>
    !control.value || control.value == Math.trunc(control.value)
      ? null
      : { numberNotInteger: { value: control.value } }
}

export function listInRange(stringRange: string): ValidatorFn {
  return control => {
    try {
      Range.from(stringRange).checkIncludes(control.value.length)
      return null
    } catch (e: any) {
      return { listNotInRange: { value: `List must be ${e.message}` } }
    }
  }
}

export function listItemIssue(): ValidatorFn {
  return parentControl => {
    const { controls } = parentControl as FormArray
    const problemChild = controls.find(c => c.invalid)
    if (problemChild) {
      return { listItemIssue: { value: 'Invalid entries' } }
    } else {
      return null
    }
  }
}

export function listUnique(spec: ValueSpecList): ValidatorFn {
  return control => {
    const list = control.value
    for (let idx = 0; idx < list.length; idx++) {
      for (let idx2 = idx + 1; idx2 < list.length; idx2++) {
        if (listItemEquals(spec, list[idx], list[idx2])) {
          let display1: string
          let display2: string
          let uniqueMessage = isObjectOrUnion(spec.spec)
            ? uniqueByMessageWrapper(
                spec.spec['unique-by'],
                spec.spec,
                list[idx],
              )
            : ''

          if (isObjectOrUnion(spec.spec) && spec.spec['display-as']) {
            display1 = `"${(Mustache as any).render(
              spec.spec['display-as'],
              list[idx],
            )}"`
            display2 = `"${(Mustache as any).render(
              spec.spec['display-as'],
              list[idx2],
            )}"`
          } else {
            display1 = `Entry ${idx + 1}`
            display2 = `Entry ${idx2 + 1}`
          }

          return {
            listNotUnique: {
              value: `${display1} and ${display2} are not unique.${uniqueMessage}`,
            },
          }
        }
      }
    }
    return null
  }
}

function listItemEquals(spec: ValueSpecList, val1: any, val2: any): boolean {
  // TODO: fix types
  switch (spec.subtype) {
    case 'string':
    case 'number':
    case 'enum':
      return val1 == val2
    case 'object':
      const obj: ListValueSpecObject = spec.spec as any

      return listObjEquals(obj['unique-by'], obj, val1, val2)
    case 'union':
      const union: ListValueSpecUnion = spec.spec as any

      return unionEquals(union['unique-by'], union, val1, val2)
    default:
      return false
  }
}

function itemEquals(spec: ValueSpec, val1: any, val2: any): boolean {
  switch (spec.type) {
    case 'string':
    case 'number':
    case 'boolean':
    case 'enum':
      return val1 == val2
    case 'object':
      // TODO: 'unique-by' does not exist on ValueSpecObject, fix types
      return objEquals(
        (spec as any)['unique-by'],
        spec as ValueSpecObject,
        val1,
        val2,
      )
    case 'union':
      // TODO: 'unique-by' does not exist on ValueSpecUnion, fix types
      return unionEquals(
        (spec as any)['unique-by'],
        spec as ValueSpecUnion,
        val1,
        val2,
      )
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

function listObjEquals(
  uniqueBy: UniqueBy,
  spec: ListValueSpecObject,
  val1: any,
  val2: any,
): boolean {
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
  return false
}

function objEquals(
  uniqueBy: UniqueBy,
  spec: ValueSpecObject,
  val1: any,
  val2: any,
): boolean {
  if (uniqueBy === null) {
    return false
  } else if (typeof uniqueBy === 'string') {
    // TODO: fix types
    return itemEquals((spec as any)[uniqueBy], val1[uniqueBy], val2[uniqueBy])
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
  return false
}

function unionEquals(
  uniqueBy: UniqueBy,
  spec: ValueSpecUnion | ListValueSpecUnion,
  val1: any,
  val2: any,
): boolean {
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
  return false
}

function uniqueByMessageWrapper(
  uniqueBy: UniqueBy,
  spec: ListValueSpecObject | ListValueSpecUnion,
  obj: Record<string, string>,
) {
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

function uniqueByMessage(
  uniqueBy: UniqueBy,
  configSpec: ConfigSpec,
  outermost = true,
): string {
  let joinFunc
  const subSpecs: string[] = []
  if (uniqueBy === null) {
    return ''
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
  const ret = subSpecs.filter(Boolean).join(joinFunc)
  return outermost || subSpecs.filter(ss => ss).length === 1
    ? ret
    : '(' + ret + ')'
}

function isObjectOrUnion(
  spec: ListValueSpecOf<any>,
): spec is ListValueSpecObject | ListValueSpecUnion {
  // only lists of objects and unions have unique-by
  return 'unique-by' in spec
}

function isUnion(spec: any): spec is ListValueSpecUnion {
  // only unions have tag
  return !!spec.tag
}

export function convertValuesRecursive(
  configSpec: ConfigSpec,
  group: FormGroup,
) {
  Object.entries(configSpec).forEach(([key, valueSpec]) => {
    const control = group.get(key)

    if (!control) return

    if (valueSpec.type === 'number') {
      control.setValue(control.value ? Number(control.value) : null)
    } else if (valueSpec.type === 'string') {
      if (!control.value) control.setValue(null)
    } else if (valueSpec.type === 'object') {
      convertValuesRecursive(valueSpec.spec, group.get(key) as FormGroup)
    } else if (valueSpec.type === 'union') {
      const formGr = group.get(key) as FormGroup
      const spec = valueSpec.variants[formGr.controls[valueSpec.tag.id].value]
      convertValuesRecursive(spec, formGr)
    } else if (valueSpec.type === 'list') {
      const formArr = group.get(key) as FormArray
      const { controls } = formArr

      if (valueSpec.subtype === 'number') {
        controls.forEach(control => {
          control.setValue(control.value ? Number(control.value) : null)
        })
      } else if (valueSpec.subtype === 'string') {
        controls.forEach(control => {
          if (!control.value) control.setValue(null)
        })
      } else if (valueSpec.subtype === 'object') {
        controls.forEach(formGroup => {
          const objectSpec = valueSpec.spec as ListValueSpecObject
          convertValuesRecursive(objectSpec.spec, formGroup as FormGroup)
        })
      } else if (valueSpec.subtype === 'union') {
        controls.forEach(formGroup => {
          const unionSpec = valueSpec.spec as ListValueSpecUnion
          const spec =
            unionSpec.variants[
              (formGroup as FormGroup).controls[unionSpec.tag.id].value
            ]
          convertValuesRecursive(spec, formGroup as FormGroup)
        })
      }
    }
  })
}

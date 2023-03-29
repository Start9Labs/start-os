import { Injectable } from '@angular/core'
import {
  UntypedFormArray,
  UntypedFormBuilder,
  UntypedFormControl,
  UntypedFormGroup,
  ValidatorFn,
  Validators,
} from '@angular/forms'
import {
  InputSpec,
  isValueSpecListOf,
  ListValueSpecNumber,
  ListValueSpecObject,
  ListValueSpecOf,
  ListValueSpecString,
  UniqueBy,
  ValueSpec,
  ValueSpecSelect,
  ValueSpecMultiselect,
  ValueSpecFile,
  ValueSpecList,
  ValueSpecNumber,
  ValueSpecObject,
  ValueSpecString,
  ValueSpecUnion,
  unionSelectKey,
  ValueSpecTextarea,
} from 'start-sdk/lib/config/config-types'
import { getDefaultString, Range } from '../util/config-utilities'
const Mustache = require('mustache')

@Injectable({
  providedIn: 'root',
})
export class FormService {
  constructor(private readonly formBuilder: UntypedFormBuilder) {}

  createForm(
    spec: InputSpec,
    current: Record<string, any> = {},
  ): UntypedFormGroup {
    return this.getFormGroup(spec, [], current)
  }

  getUnionObject(
    spec: ValueSpecUnion,
    selection: string | null,
  ): UntypedFormGroup {
    const { name, description, warning, variants, nullable } = spec

    const selectSpec: ValueSpecSelect = {
      type: 'select',
      name,
      description,
      warning,
      default: selection,
      nullable,
      values: Object.keys(variants).reduce(
        (prev, curr) => ({
          ...prev,
          [curr]: variants[curr].name,
        }),
        {},
      ),
    }

    const selectedSpec = selection ? variants[selection].spec : {}

    return this.getFormGroup({
      [unionSelectKey]: selectSpec,
      ...selectedSpec,
    })
  }

  getListItem(spec: ValueSpecList, entry: any) {
    const listItemValidators = getListItemValidators(spec)
    if (isValueSpecListOf(spec, 'string')) {
      return this.formBuilder.control(entry, listItemValidators)
    } else if (isValueSpecListOf(spec, 'number')) {
      return this.formBuilder.control(entry, listItemValidators)
    } else if (isValueSpecListOf(spec, 'object')) {
      return this.getFormGroup(spec.spec.spec, listItemValidators, entry)
    }
  }

  private getFormGroup(
    config: InputSpec,
    validators: ValidatorFn[] = [],
    current?: Record<string, any> | null,
  ): UntypedFormGroup {
    let group: Record<
      string,
      UntypedFormGroup | UntypedFormArray | UntypedFormControl
    > = {}
    Object.entries(config).map(([key, spec]) => {
      group[key] = this.getFormEntry(spec, current ? current[key] : undefined)
    })
    return this.formBuilder.group(group, { validators })
  }

  private getFormEntry(
    spec: ValueSpec,
    currentValue?: any,
  ): UntypedFormGroup | UntypedFormArray | UntypedFormControl {
    let value: any
    switch (spec.type) {
      case 'string':
        if (currentValue !== undefined) {
          value = currentValue
        } else {
          value = spec.default ? getDefaultString(spec.default) : null
        }
        return this.formBuilder.control(value, stringValidators(spec))
      case 'textarea':
        value = currentValue || null
        return this.formBuilder.control(value, textareaValidators(spec))
      case 'number':
        if (currentValue !== undefined) {
          value = currentValue
        } else {
          value = spec.default || null
        }
        return this.formBuilder.control(value, numberValidators(spec))
      case 'object':
        return this.getFormGroup(spec.spec, [], currentValue)
      case 'list':
        const mapped = (
          Array.isArray(currentValue) ? currentValue : (spec.default as any[])
        ).map(entry => {
          return this.getListItem(spec, entry)
        })
        return this.formBuilder.array(mapped, listValidators(spec))
      case 'file':
        return this.formBuilder.control(
          currentValue || null,
          fileValidators(spec),
        )
      case 'union':
        const currentSelection = currentValue?.[unionSelectKey]
        const isValid = !!spec.variants[currentSelection]

        return this.getUnionObject(
          spec,
          isValid ? currentSelection : spec.default,
        )
      case 'boolean':
        value = currentValue === undefined ? spec.default : currentValue
        return this.formBuilder.control(value)
      case 'select':
        value = currentValue === undefined ? spec.default : currentValue
        return this.formBuilder.control(value, selectValidators(spec))
      case 'multiselect':
        value = currentValue === undefined ? spec.default : currentValue
        return this.formBuilder.control(value, multiselectValidators(spec))
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

function textareaValidators(spec: ValueSpecTextarea): ValidatorFn[] {
  const validators: ValidatorFn[] = []

  if (!spec.nullable) {
    validators.push(Validators.required)
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

function selectValidators(spec: ValueSpecSelect): ValidatorFn[] {
  const validators: ValidatorFn[] = []

  if (!spec.nullable) {
    validators.push(Validators.required)
  }

  return validators
}

function multiselectValidators(spec: ValueSpecMultiselect): ValidatorFn[] {
  const validators: ValidatorFn[] = []
  validators.push(listInRange(spec.range))
  return validators
}

function listValidators(spec: ValueSpecList): ValidatorFn[] {
  const validators: ValidatorFn[] = []
  validators.push(listInRange(spec.range))
  validators.push(listItemIssue())
  return validators
}

function fileValidators(spec: ValueSpecFile): ValidatorFn[] {
  const validators: ValidatorFn[] = []

  if (!spec.nullable) {
    validators.push(Validators.required)
  }

  return validators
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
    const { controls } = parentControl as UntypedFormArray
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
          const objSpec = spec.spec
          let display1: string
          let display2: string
          let uniqueMessage = isObject(objSpec)
            ? uniqueByMessageWrapper(objSpec.uniqueBy, objSpec, list[idx])
            : ''

          if (isObject(objSpec) && objSpec.displayAs) {
            display1 = `"${(Mustache as any).render(
              objSpec.displayAs,
              list[idx],
            )}"`
            display2 = `"${(Mustache as any).render(
              objSpec.displayAs,
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
      return val1 == val2
    case 'object':
      const obj: ListValueSpecObject = spec.spec as any

      return listObjEquals(obj.uniqueBy, obj, val1, val2)
    default:
      return false
  }
}

function itemEquals(spec: ValueSpec, val1: any, val2: any): boolean {
  switch (spec.type) {
    case 'string':
    case 'textarea':
    case 'number':
    case 'boolean':
    case 'select':
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
  if (!uniqueBy) {
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
  if (!uniqueBy) {
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
  spec: ValueSpecUnion,
  val1: any,
  val2: any,
): boolean {
  const variantSpec = spec.variants[val1[unionSelectKey]].spec
  if (!uniqueBy) {
    return false
  } else if (typeof uniqueBy === 'string') {
    if (uniqueBy === unionSelectKey) {
      return val1[unionSelectKey] === val2[unionSelectKey]
    } else {
      return itemEquals(variantSpec[uniqueBy], val1[uniqueBy], val2[uniqueBy])
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
  spec: ListValueSpecObject,
  obj: Record<string, string>,
) {
  let configSpec = spec.spec

  const message = uniqueByMessage(uniqueBy, configSpec)
  if (message) {
    return ' Must be unique by: ' + message
  }
}

function uniqueByMessage(
  uniqueBy: UniqueBy,
  configSpec: InputSpec,
  outermost = true,
): string {
  let joinFunc
  const subSpecs: string[] = []
  if (!uniqueBy) {
    return ''
  } else if (typeof uniqueBy === 'string') {
    return configSpec[uniqueBy]
      ? (configSpec[uniqueBy] as ValueSpecObject).name
      : uniqueBy
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

function isObject(spec: ListValueSpecOf<any>): spec is ListValueSpecObject {
  // only lists of objects have uniqueBy
  return 'uniqueBy' in spec
}

export function convertValuesRecursive(
  configSpec: InputSpec,
  group: UntypedFormGroup,
) {
  Object.entries(configSpec).forEach(([key, valueSpec]) => {
    const control = group.get(key)

    if (!control) return

    if (valueSpec.type === 'number') {
      control.setValue(
        control.value || control.value === 0 ? Number(control.value) : null,
      )
    } else if (valueSpec.type === 'string' || valueSpec.type === 'textarea') {
      if (!control.value) control.setValue(null)
    } else if (valueSpec.type === 'object') {
      convertValuesRecursive(valueSpec.spec, group.get(key) as UntypedFormGroup)
    } else if (valueSpec.type === 'union') {
      const formGr = group.get(key) as UntypedFormGroup
      const spec =
        valueSpec.variants[formGr.controls[unionSelectKey].value].spec
      convertValuesRecursive(spec, formGr)
    } else if (valueSpec.type === 'list') {
      const formArr = group.get(key) as UntypedFormArray
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
          convertValuesRecursive(objectSpec.spec, formGroup as UntypedFormGroup)
        })
      }
    }
  })
}

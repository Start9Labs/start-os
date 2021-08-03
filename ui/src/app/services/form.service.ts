import { Injectable } from '@angular/core'
import { AbstractControl, FormArray, FormBuilder, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms'
import { ConfigSpec, isValueSpecListOf, ListValueSpecNumber, ListValueSpecObject, ListValueSpecString, ValueSpec, ValueSpecEnum, ValueSpecList, ValueSpecNumber, ValueSpecObject, ValueSpecString, ValueSpecUnion } from '../pkg-config/config-types'
import { Range } from '../pkg-config/config-utilities'

@Injectable({
  providedIn: 'root',
})
export class FormService {
  validationMessages: { [key: string]: { type: string, message: string }[] } = { }

  constructor (
    private readonly formBuilder: FormBuilder,
  ) { }

  createForm (config: ConfigSpec): FormGroup {
    return this.getFormGroup(config)
  }

  getListItemValidators (spec: ValueSpecList, key: string,  index: number) {
    const listKey = `${key}/${index}`
    this.validationMessages[listKey] = []
    if (isValueSpecListOf(spec, 'string')) {
      return this.stringValidators(listKey, spec.spec)
    } else if (isValueSpecListOf(spec, 'number')) {
      return this.numberValidators(listKey, spec.spec)
    } else if (isValueSpecListOf(spec, 'object')) {
      return this.objectValidators(listKey, spec.spec)
    }
  }

  getUnionObject (spec: ValueSpecUnion, type: string): FormGroup {
    const { name, description, changeWarning, variants, tag } = spec
    const typeSpec: ValueSpecEnum = {
      type: 'enum',
      name,
      description,
      changeWarning,
      default: type,
      values: Object.keys(variants),
      valueNames: tag.variantNames,
    }
    return this.getFormGroup({ type: typeSpec, ...spec.variants[type] })
  }

  private getFormGroup (config: ConfigSpec, validators: ValidatorFn[] = []): FormGroup {
    const group = { }
    Object.entries(config).map(([key, spec]) => {
      group[key] = this.getFormEntry(key, spec)
    })
    return this.formBuilder.group(group, { validators })
  }

  private getFormEntry (key: string, spec: ValueSpec): FormGroup | FormArray | FormControl {
    this.validationMessages[key] = []
    let validators: ValidatorFn[]
    switch (spec.type) {
      case 'string':
        validators = this.stringValidators(key, spec)
        return this.formBuilder.control(spec.default || '', validators)
      case 'number':
        validators = this.numberValidators(key, spec)
        return this.formBuilder.control(spec.default, validators)
      case 'object':
        validators = this.objectValidators(key, spec)
        return this.getFormGroup(spec.spec, validators)
      case 'list':
        validators = this.listValidators(key, spec)
        const mapped = (spec.default as any[]).map((entry: any, index) => {
          const listItemValidators = this.getListItemValidators(spec, key, index)
          if (isValueSpecListOf(spec, 'string')) {
            return this.formBuilder.control(entry, listItemValidators)
          } else if (isValueSpecListOf(spec, 'number')) {
            return this.formBuilder.control(entry, listItemValidators)
          } else if (isValueSpecListOf(spec, 'enum')) {
            return this.formBuilder.control(entry)
          } else if (isValueSpecListOf(spec, 'object')) {
            return this.getFormGroup(spec.spec.spec, listItemValidators)
          }
        })
        return this.formBuilder.array(mapped, validators)
      case 'union':
        return this.getUnionObject(spec, spec.default)
      case 'pointer':
        return this.formBuilder.control('System Defined')
      case 'boolean':
      case 'enum':
        return this.formBuilder.control(spec.default)
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
        message: spec.patternDescription,
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

  private objectValidators (key: string, spec: ValueSpecObject | ListValueSpecObject): ValidatorFn[] {
    const validators: ValidatorFn[] = []

    if (!(spec as ValueSpecObject).nullable) {
      validators.push(Validators.required)
      this.validationMessages[key].push({
        type: 'required',
        message: 'Cannot be blank.',
      })
    }

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

export function listUnique (spec: ValueSpec): ValidatorFn {
  return (control: AbstractControl): ValidationErrors | null => {
    for (let idx = 0; idx < control.value.length; idx++) {
      for (let idx2 = idx + 1; idx2 < control.value.length; idx2++) {
        if (equals(spec, control.value[idx], control.value[idx2])) {
          return { listNotUnique: { value: control.value } }
        } else {
          return null
        }
      }
    }
  }
}

export function equals (spec: ValueSpec, val1: any, val2: any): boolean {
  switch (spec.type) {
    case 'string':
    case 'number':
    case 'boolean':
    case 'enum':
      return val1 === val2
    case 'object':
    case 'union':
      // @TODO how to check this
      return false
    default:
      return false
  }
}

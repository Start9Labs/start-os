import { Pipe, PipeTransform } from '@angular/core'
import {
  AbstractControl,
  FormGroup,
  UntypedFormArray,
  ValidationErrors,
} from '@angular/forms'
import { IonicSafeString } from '@ionic/angular'
import { ListValueSpecOf } from 'start-sdk/types/config-types'
import { Range } from 'src/app/util/config-utilities'
import { getElementId } from './form-object/form-object.component'

@Pipe({
  name: 'getError',
})
export class GetErrorPipe implements PipeTransform {
  transform(errors: ValidationErrors, patternDesc?: string): string {
    if (errors['required']) {
      return 'Required'
    } else if (errors['pattern']) {
      return patternDesc || 'Invalid pattern'
    } else if (errors['notNumber']) {
      return 'Must be a number'
    } else if (errors['numberNotInteger']) {
      return 'Must be an integer'
    } else if (errors['numberNotInRange']) {
      return errors['numberNotInRange'].value
    } else if (errors['listNotUnique']) {
      return errors['listNotUnique'].value
    } else if (errors['listNotInRange']) {
      return errors['listNotInRange'].value
    } else if (errors['listItemIssue']) {
      return errors['listItemIssue'].value
    } else {
      return 'Unknown error'
    }
  }
}

@Pipe({
  name: 'toEnumListDisplay',
})
export class ToEnumListDisplayPipe implements PipeTransform {
  transform(arr: string[], spec: ListValueSpecOf<'enum'>): string {
    return arr.map((v: string) => spec['value-names'][v]).join(', ')
  }
}

@Pipe({
  name: 'toWarningText',
})
export class ToWarningTextPipe implements PipeTransform {
  transform(text: string | null): IonicSafeString | string {
    return text
      ? new IonicSafeString(`<ion-text color="warning">${text}</ion-text>`)
      : ''
  }
}

@Pipe({
  name: 'toRange',
})
export class ToRangePipe implements PipeTransform {
  transform(range: string): Range {
    return Range.from(range)
  }
}

@Pipe({
  name: 'toElementId',
})
export class ToElementIdPipe implements PipeTransform {
  transform(objectId: string, key: string, index = 0): string {
    return getElementId(objectId, key, index)
  }
}

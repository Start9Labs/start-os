import { Pipe, PipeTransform } from '@angular/core'
import { Annotation } from '../pkg-config/config-utilities'

@Pipe({
  name: 'annotationStatus',
})
export class AnnotationStatusPipe implements PipeTransform {
  transform (a: Annotation, target: FieldStatus): boolean {
    return target === getStatus(a)
  }
}

function getStatus (a: Annotation): FieldStatus {
  if (isInvalid(a)) return 'Invalid'
  if (isEdited(a)) return 'Edited'
  if (isAdded(a)) return 'Added'
  return 'NoChange'
}

function isInvalid (a: Annotation): boolean {
  return !!a.invalid
}

// edited only registers if its a valid edit
function isEdited (a: Annotation): boolean {
  return a.edited && !a.invalid
}

function isAdded (a: Annotation): boolean {
  return a.added && !a.edited && !a.invalid
}

type FieldStatus = 'Edited' | 'Added' | 'Invalid' | 'NoChange'

import { Pipe, PipeTransform } from '@angular/core';
import { peekProperties, PropertySubject } from '../util/property-subject.util'

@Pipe({
  name: 'peekProperties',
})
export class PeekPropertiesPipe implements PipeTransform {
  transform<T> (value: PropertySubject<T>): T {
    return peekProperties(value)
  }
}

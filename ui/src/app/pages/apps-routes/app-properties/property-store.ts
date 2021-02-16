import { Injectable } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { PackageProperties } from '../../../util/properties.util'

@Injectable({
  providedIn: 'root',
})
export class PropertyStore {
  properties$: BehaviorSubject<PackageProperties> = new BehaviorSubject({ })
  watch$ () { return this.properties$.asObservable() }

  update (properties: PackageProperties): void {
    this.properties$.next(properties)
  }
}
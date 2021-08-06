import { Inject, Injectable, InjectionToken } from '@angular/core'
import { IonNav } from '@ionic/angular'
import { AppConfigComponentMapping } from '../modals/app-config-injectable'
import { ConfigCursor } from '../pkg-config/config-cursor'

export const APP_CONFIG_COMPONENT_MAPPING = new InjectionToken<string>('APP_CONFIG_COMPONENTS')

@Injectable({
  providedIn: 'root',
})
export class SubNavService {
  path: string[]

  constructor (
    @Inject(APP_CONFIG_COMPONENT_MAPPING) private readonly appConfigComponentMapping: AppConfigComponentMapping,
  ) { }

  async push (key: string, cursor: ConfigCursor<any>, nav: IonNav) {
    const component = this.appConfigComponentMapping[cursor.spec().type]
    this.path.push(key)
    nav.push(component, { cursor }, { mode: 'ios' })
  }

  async pop (nav: IonNav) {
    this.path.pop()
    nav.pop({ mode: 'ios' })
  }

  async popTo (index: number, nav: IonNav) {
    this.path = this.path.slice(0, index + 1)
    nav.popTo(index, { mode: 'ios' })
  }
}
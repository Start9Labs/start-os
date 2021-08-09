import { Injectable } from '@angular/core'
import { IonNav } from '@ionic/angular'
import { ConfigCursor } from '../pkg-config/config-cursor'

@Injectable({
  providedIn: 'root',
})
export class SubNavService {
  path: string[]

  async push (key: string, cursor: ConfigCursor<any>, nav: IonNav) {
    this.path.push(key)
    // nav.push(component, { cursor }, { mode: 'ios' })
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
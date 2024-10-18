import { Injectable } from '@angular/core'
import { ControlDirective } from './control.directive'

@Injectable()
export class InvalidService {
  private readonly controls: ControlDirective[] = []

  scrollIntoView() {
    this.controls.find(({ invalid }) => invalid)?.scrollIntoView()
  }

  add(control: ControlDirective) {
    this.controls.push(control)
  }

  remove(control: ControlDirective) {
    this.controls.splice(this.controls.indexOf(control), 1)
  }
}

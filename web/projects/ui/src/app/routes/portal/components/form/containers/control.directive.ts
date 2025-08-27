import { Directive, inject, Injectable, OnDestroy, OnInit } from '@angular/core'
import { ControlContainer, NgControl } from '@angular/forms'
import { tuiInjectElement } from '@taiga-ui/cdk'

@Injectable()
export class InvalidService {
  private readonly controls: ControlDirective[] = []

  scrollIntoView() {
    this.controls.find(d => d.invalid)?.scrollIntoView()
  }

  add(control: ControlDirective) {
    this.controls.push(control)
  }

  remove(control: ControlDirective) {
    this.controls.splice(this.controls.indexOf(control), 1)
  }
}

@Directive()
export class ControlDirective implements OnInit, OnDestroy {
  private readonly service = inject(InvalidService, { optional: true })
  private readonly element = tuiInjectElement()
  private readonly control =
    inject(NgControl, { optional: true }) ||
    inject(ControlContainer, { optional: true })

  get invalid(): boolean {
    return !!this.control?.invalid
  }

  scrollIntoView() {
    this.element.scrollIntoView({ behavior: 'smooth' })
  }

  ngOnInit() {
    this.service?.add(this)
  }

  ngOnDestroy() {
    this.service?.remove(this)
  }
}

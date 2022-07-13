import { Component, EventEmitter, Input, Output } from '@angular/core'
import { capitalizeFirstLetter } from '@start9labs/shared'
import { BaseSlide } from '../wizard-types'

@Component({
  selector: 'complete',
  templateUrl: './complete.component.html',
  styleUrls: ['../app-wizard.component.scss'],
})
export class CompleteComponent implements BaseSlide {
  @Input()
  params!: {
    verb: string // loader verb: '*stopping* ...'
    title: string
    Fn: () => Promise<any>
  }

  @Output() onSuccess: EventEmitter<void> = new EventEmitter()
  @Output() onError: EventEmitter<string> = new EventEmitter()

  message = ''

  loading = true

  async load() {
    this.message =
      capitalizeFirstLetter(this.params.verb || '') + ' ' + this.params.title
    try {
      await this.params.Fn()
      this.onSuccess.emit()
    } catch (e: any) {
      this.onError.emit(`Error: ${e.message || e}`)
    }
  }
}

import { Component, Input } from '@angular/core'
import { BehaviorSubject, from, Subject } from 'rxjs'
import { takeUntil } from 'rxjs/operators'
import { capitalizeFirstLetter } from '@start9labs/shared'
import { markAsLoadingDuring$ } from '../loadable'
import { WizardAction } from '../wizard-types'

@Component({
  selector: 'complete',
  templateUrl: './complete.component.html',
  styleUrls: ['../install-wizard.component.scss'],
})
export class CompleteComponent {
  @Input() params: {
    action: WizardAction
    verb: string // loader verb: '*stopping* ...'
    title: string
    executeAction: () => Promise<any>
  }

  @Input() transitions: {
    cancel: () => any
    next: (prevResult?: any) => any
    final: () => any
    error: (e: Error) => any
  }

  loading$ = new BehaviorSubject(false)
  cancel$ = new Subject<void>()

  message: string

  load() {
    markAsLoadingDuring$(this.loading$, from(this.params.executeAction()))
      .pipe(takeUntil(this.cancel$))
      .subscribe({
        error: e =>
          this.transitions.error(
            new Error(`${this.params.action} failed: ${e.message || e}`),
          ),
        complete: () => this.transitions.final(),
      })
  }

  ngOnInit() {
    this.message = `${capitalizeFirstLetter(this.params.verb)} ${
      this.params.title
    }...`
  }
}

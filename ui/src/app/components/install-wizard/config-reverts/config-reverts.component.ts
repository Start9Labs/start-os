import { Component, Input, OnInit } from '@angular/core'
import { BehaviorSubject, from, Subject } from 'rxjs'
import { map, takeUntil, tap } from 'rxjs/operators'
import { ConfigReverts } from 'src/app/models/app-types'
import { markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { Colorable, Loadable } from '../loadable'
import { WizardAction } from '../wizard-types'

@Component({
  selector: 'config-reverts',
  templateUrl: './config-reverts.component.html',
  styleUrls: ['../install-wizard.component.scss'],
})
export class ConfigRevertsComponent implements OnInit, Loadable, Colorable {
  @Input() params: {
    fetchConfigReverts: () => Promise<ConfigReverts>,
  }
  configReverts: ConfigReverts

  $loading$ = new BehaviorSubject(false)
  $color$ = new BehaviorSubject('light')
  $cancel$ = new Subject<void>()

  load () {
    markAsLoadingDuring$(this.$loading$, from(this.params.fetchConfigReverts())).pipe(
      takeUntil(this.$cancel$),
      map(reverts => this.configReverts = reverts || {}),
    ).subscribe(
      {
        complete: () => {
          this.hasDependentViolation = this.dependentBreakages && this.dependentBreakages.length > 0
          if (this.hasDependentViolation) {
            this.longMessage = `${capitalizeFirstLetter(this.params.verb)} ${this.params.title} will cause the following services to STOP running. Starting them again will require additional actions.`
            this.$color$.next('warning')
          } else if (this.params.skipConfirmationDialogue) {
            this.finished({ })
          } else {
            this.longMessage = `No other services installed on your Embassy will be affected by this action.`
            this.$color$.next('success')
          }
        },
        error: (e: Error) => this.finished({ error: new Error(`Fetching dependent service information failed: ${e.message || e}`) }),
      },
    )
  }

  constructor () { }
  ngOnInit () { }
}

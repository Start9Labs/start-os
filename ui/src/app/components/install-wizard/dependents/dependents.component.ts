import { Component, Input, OnInit } from '@angular/core'
import { BehaviorSubject, from, of, Subject } from 'rxjs'
import { catchError, concatMap, map, takeUntil } from 'rxjs/operators'
import { DependentBreakage } from 'src/app/models/app-types'
import { markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { capitalizeFirstLetter, pauseFor } from 'src/app/util/misc.util'
import { Loadable } from '../loadable'
import { WizardAction } from '../wizard-types'

@Component({
  selector: 'dependents',
  templateUrl: './dependents.component.html',
  styleUrls: ['../install-wizard.component.scss'],
})
export class DependentsComponent implements OnInit, Loadable {
  @Input() params: {
    title: string,
    action: WizardAction, // Are you sure you want to *uninstall*...,
    verb: string, // *Uninstalling* will cause problems...
    fetchBreakages: () => Promise<DependentBreakage[]>,
    skipConfirmationDialogue?: boolean
  }
  @Input() transitions: {
    cancel: () => void
    next: () => void
    final: () => void
    error: (e: Error) => void
  }

  dependentBreakages: DependentBreakage[]
  hasDependentViolation: boolean
  longMessage: string | null = null
  $loading$ = new BehaviorSubject(false)
  $cancel$ = new Subject<void>()

  constructor () { }
  ngOnInit () { }

  load () {
    markAsLoadingDuring$(
      this.$loading$,
      from(Promise.all([
        this.params.fetchBreakages(),
        pauseFor(2000),
      ])),
    )
    .pipe(
      takeUntil(this.$cancel$),
      map(([breakages]) => this.dependentBreakages = breakages || []),
      concatMap(() => {
        this.hasDependentViolation = this.dependentBreakages && this.dependentBreakages.length > 0
        if (this.hasDependentViolation) {
          this.longMessage = `${capitalizeFirstLetter(this.params.verb)} ${this.params.title} will cause the following services to STOP running. Starting them again will require additional actions.`
          return of()
        } else if (!this.hasDependentViolation || this.params.skipConfirmationDialogue) {
          return of(this.transitions.next())
          // return pauseFor(250)
        }
      }),
      catchError((e: Error) => of(this.transitions.error(new Error(`Fetching dependent service information failed: ${e.message || e}`)))),
    )
    .subscribe()
  }
}

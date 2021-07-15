import { Component, Input, OnInit } from '@angular/core'
import { BehaviorSubject, from, Subject } from 'rxjs'
import { takeUntil, tap } from 'rxjs/operators'
import { Breakages } from 'src/app/services/api/api.types'
import { markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { capitalizeFirstLetter, isEmptyObject } from 'src/app/util/misc.util'
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
    action: WizardAction, //Are you sure you want to *uninstall*...,
    verb: string, // *Uninstalling* will cause problems...
    fetchBreakages: () => Promise<Breakages>,
    skipConfirmationDialogue?: boolean
  }
  @Input() transitions: {
    cancel: () => void
    next: () => void
    final: () => void
    error: (e: Error) => void
  }

  dependentBreakages: Breakages
  hasDependentViolation: boolean
  longMessage: string | null = null
  color$ = new BehaviorSubject('medium') // this will display disabled while loading
  loading$ = new BehaviorSubject(false)
  cancel$ = new Subject<void>()

  constructor () { }
  ngOnInit () { }

  load () {
    this.color$.next('medium')
    markAsLoadingDuring$(this.loading$, from(this.params.fetchBreakages())).pipe(
      takeUntil(this.cancel$),
      tap(breakages => this.dependentBreakages = breakages),
    ).subscribe(
      {
        complete: () => {
          this.hasDependentViolation = this.dependentBreakages && !isEmptyObject(this.dependentBreakages)
          if (this.hasDependentViolation) {
            this.longMessage = `${capitalizeFirstLetter(this.params.verb)} ${this.params.title} will cause the following services to STOP running. Starting them again will require additional actions.`
            this.color$.next('warning')
          } else if (this.params.skipConfirmationDialogue) {
            this.transitions.next()
          } else {
            this.longMessage = `No other services installed on your Embassy will be affected by this action.`
            this.color$.next('success')
          }
        },
        error: (e: Error) => this.transitions.error(new Error(`Fetching dependent service information failed: ${e.message || e}`)),
      },
    )
  }
}

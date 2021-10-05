import { Component, Input, OnInit } from '@angular/core'
import { BehaviorSubject, from, Subject } from 'rxjs'
import { takeUntil, tap } from 'rxjs/operators'
import { Breakages } from 'src/app/services/api/api.types'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { capitalizeFirstLetter, isEmptyObject } from 'src/app/util/misc.util'
import { Loadable, markAsLoadingDuring$ } from '../loadable'
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
    cancel: () => any
    next: (prevResult?: any) => any
    final: () => any
    error: (e: Error) => any
  }

  dependentBreakages: Breakages
  hasDependentViolation: boolean
  longMessage: string | null = null
  color$ = new BehaviorSubject('medium') // this will display disabled while loading
  loading$ = new BehaviorSubject(false)
  cancel$ = new Subject<void>()

  constructor (
    public readonly patch: PatchDbService,
  ) { }
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
            this.longMessage = `${capitalizeFirstLetter(this.params.verb)} ${this.params.title} will prohibit the following services from functioning properly and will cause them to stop if they are currently running.`
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

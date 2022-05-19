import { Component, Input } from '@angular/core'
import { BehaviorSubject, from, Subject } from 'rxjs'
import { takeUntil, tap } from 'rxjs/operators'
import { Breakages } from 'src/app/services/api/api.types'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { capitalizeFirstLetter, isEmptyObject } from '@start9labs/shared'
import { markAsLoadingDuring$ } from '../loadable'
import { WizardAction } from '../wizard-types'

@Component({
  selector: 'dependents',
  templateUrl: './dependents.component.html',
  styleUrls: [
    './dependents.component.scss',
    '../install-wizard.component.scss',
  ],
})
export class DependentsComponent {
  @Input() params: {
    title: string
    action: WizardAction //Are you sure you want to *uninstall*...,
    verb: string // *Uninstalling* will cause problems...
    fetchBreakages: () => Promise<Breakages>
  }
  @Input() transitions: {
    cancel: () => any
    next: (prevResult?: any) => any
    final: () => any
    error: (e: Error) => any
  }

  dependentBreakages: Breakages
  dependentViolation: string | undefined

  loading$ = new BehaviorSubject(false)
  cancel$ = new Subject<void>()

  constructor(public readonly patch: PatchDbService) {}

  load() {
    markAsLoadingDuring$(this.loading$, from(this.params.fetchBreakages()))
      .pipe(
        takeUntil(this.cancel$),
        tap(breakages => (this.dependentBreakages = breakages)),
      )
      .subscribe({
        complete: () => {
          if (
            this.dependentBreakages &&
            !isEmptyObject(this.dependentBreakages)
          ) {
            this.dependentViolation = `${capitalizeFirstLetter(
              this.params.verb,
            )} ${
              this.params.title
            } will prohibit the following services from functioning properly and may cause them to stop if they are currently running.`
          } else {
            this.transitions.next()
          }
        },
        error: (e: Error) =>
          this.transitions.error(
            new Error(
              `Fetching dependent service information failed: ${
                e.message || e
              }`,
            ),
          ),
      })
  }
}

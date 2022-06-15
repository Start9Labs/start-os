import { Component, EventEmitter, Input, Output } from '@angular/core'
import { Breakages } from 'src/app/services/api/api.types'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { capitalizeFirstLetter, isEmptyObject } from '@start9labs/shared'
import { BaseSlide } from '../wizard-types'

@Component({
  selector: 'dependents',
  templateUrl: './dependents.component.html',
  styleUrls: ['./dependents.component.scss', '../app-wizard.component.scss'],
})
export class DependentsComponent implements BaseSlide {
  @Input() params: {
    title: string
    verb: string // *Uninstalling* will cause problems...
    Fn: () => Promise<Breakages>
  }

  @Output() onSuccess: EventEmitter<void> = new EventEmitter()
  @Output() onError: EventEmitter<string> = new EventEmitter()

  breakages: Breakages
  warningMessage: string | undefined

  loading = true

  readonly pkgs$ = this.patch.watch$('package-data')

  constructor(public readonly patch: PatchDbService) {}

  async load() {
    try {
      this.breakages = await this.params.Fn()
      if (this.breakages && !isEmptyObject(this.breakages)) {
        this.warningMessage =
          capitalizeFirstLetter(this.params.verb) +
          ' ' +
          this.params.title +
          ' will prohibit the following services from functioning properly.'
      } else {
        this.onSuccess.emit()
      }
    } catch (e: any) {
      this.onError.emit(
        `Error fetching dependent service information: ${e.message || e}`,
      )
    } finally {
      this.loading = false
    }
  }
}

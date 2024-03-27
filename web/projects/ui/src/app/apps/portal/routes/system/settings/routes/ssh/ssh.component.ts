import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { catchError, defer, of } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { SSHInfoComponent } from './info.component'
import { SSHTableComponent } from './table.component'

@Component({
  template: `
    <ssh-info />
    <h3 class="g-title">
      Saved Keys
      <button
        tuiButton
        size="xs"
        iconLeft="tuiIconPlus"
        (click)="table.add.call(table)"
      >
        Add Key
      </button>
    </h3>
    <table #table class="g-table" [keys]="keys$ | async"></table>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiButtonModule, SSHTableComponent, SSHInfoComponent],
})
export class SettingsSSHComponent {
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  readonly keys$ = defer(() => this.api.getSshKeys({})).pipe(
    catchError(e => {
      this.errorService.handleError(e)

      return of([])
    }),
  )
}

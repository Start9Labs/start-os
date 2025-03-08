import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { catchError, defer, of } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TitleDirective } from 'src/app/services/title.service'
import { SSHInfoComponent } from './info.component'
import { SSHTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">Back</a>
      SSH
    </ng-container>
    <ssh-info />
    <section class="g-card">
      <header>
        Saved Keys
        <button
          tuiButton
          size="xs"
          iconStart="@tui.plus"
          [style.margin-inline-start]="'auto'"
          (click)="table.add.call(table)"
        >
          Add Key
        </button>
      </header>
      <table #table tuiTable class="g-table" [keys]="keys$ | async"></table>
    </section>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiButton,
    SSHTableComponent,
    SSHInfoComponent,
    RouterLink,
    TitleDirective,
    TuiTable,
  ],
})
export default class SystemSSHComponent {
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  readonly keys$ = defer(() => this.api.getSshKeys({})).pipe(
    catchError(e => {
      this.errorService.handleError(e)

      return of([])
    }),
  )
}

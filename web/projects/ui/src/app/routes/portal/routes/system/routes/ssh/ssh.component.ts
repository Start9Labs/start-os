import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiLink, TuiTitle } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { TuiHeader } from '@taiga-ui/layout'
import { catchError, defer, of } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TitleDirective } from 'src/app/services/title.service'
import { SSHTableComponent } from './table.component'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">Back</a>
      SSH
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>SSH</h3>
        <p tuiSubtitle>
          Manage your SSH keys to access your server from the command line
          <a
            tuiLink
            href="https://docs.start9.com/@TODO"
            target="_blank"
            rel="noreferrer"
            appearance="action-grayscale"
            iconEnd="@tui.external-link"
            [pseudo]="true"
            [textContent]="'View instructions'"
          ></a>
        </p>
      </hgroup>
    </header>
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
      <div #table [keys]="keys$ | async"></div>
    </section>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiButton,
    SSHTableComponent,
    RouterLink,
    TitleDirective,
    TuiTable,
    TuiHeader,
    TuiTitle,
    TuiLink,
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

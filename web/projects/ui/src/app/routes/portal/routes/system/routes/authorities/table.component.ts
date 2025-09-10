import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { AuthorityItemComponent } from './item.component'
import { AuthorityService } from './authority.service'

@Component({
  selector: 'authorities-table',
  template: `
    <table [appTable]="['Provider', 'URL', 'Contact', null]">
      <tr [authority]="{ name: 'Local Root CA' }"></tr>
      @for (authority of authorityService.authorities(); track $index) {
        <tr [authority]="authority"></tr>
      } @empty {
        <td [attr.colspan]="4">
          <div [tuiSkeleton]="true">{{ 'Loading' | i18n }}</div>
        </td>
      }
    </table>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiSkeleton, i18nPipe, TableComponent, AuthorityItemComponent],
})
export class AuthoritiesTableComponent {
  protected readonly authorityService = inject(AuthorityService)
}

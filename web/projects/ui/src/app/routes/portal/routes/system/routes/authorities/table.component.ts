import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { AuthorityItemComponent } from './item.component'
import { AuthorityService } from './authority.service'

@Component({
  selector: 'authorities-table',
  template: `
    <table [appTable]="['Provider', 'URL', 'Contact', null]">
      <tr [authority]="{ name: 'Root CA' }"></tr>
      @for (authority of authorityService.authorities(); track $index) {
        <tr [authority]="authority"></tr>
      }
    </table>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TableComponent, AuthorityItemComponent],
})
export class AuthoritiesTableComponent {
  protected readonly authorityService = inject(AuthorityService)
}

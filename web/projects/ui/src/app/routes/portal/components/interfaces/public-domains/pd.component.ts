import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { DocsLinkDirective, i18nPipe } from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { PublicDomainsItemComponent } from './pd.item.component'
import { PublicDomain, PublicDomainService } from './pd.service'

@Component({
  selector: 'section[publicDomains]',
  template: `
    <header>
      {{ 'Public Domains' | i18n }}
      <a
        tuiIconButton
        docsLink
        path="/user-manual/connecting-remotely/clearnet.html"
        appearance="icon"
        iconStart="@tui.external-link"
      >
        {{ 'Documentation' | i18n }}
      </a>
      @if (service.data()) {
        <button
          tuiButton
          iconStart="@tui.plus"
          [style.margin-inline-start]="'auto'"
          (click)="service.add()"
        >
          {{ 'Add' | i18n }}
        </button>
      }
    </header>
    @if (publicDomains()?.length === 0) {
      <app-placeholder icon="@tui.globe">
        {{ 'No public domains' | i18n }}
      </app-placeholder>
    } @else {
      <table [appTable]="['Domain', 'Gateway', 'Certificate Authority', null]">
        @for (domain of publicDomains(); track $index) {
          <tr [publicDomain]="domain"></tr>
        } @empty {
          @for (_ of [0]; track $index) {
            <tr>
              <td colspan="4">
                <div [tuiSkeleton]="true">{{ 'Loading' | i18n }}</div>
              </td>
            </tr>
          }
        }
      </table>
    }
  `,
  styles: `
    :host {
      grid-column: span 7;
    }
  `,
  host: { class: 'g-card' },
  providers: [PublicDomainService],
  imports: [
    TuiButton,
    TableComponent,
    PlaceholderComponent,
    i18nPipe,
    DocsLinkDirective,
    PublicDomainsItemComponent,
    TuiSkeleton,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PublicDomainsComponent {
  readonly service = inject(PublicDomainService)

  readonly publicDomains = input.required<readonly PublicDomain[] | undefined>()
}

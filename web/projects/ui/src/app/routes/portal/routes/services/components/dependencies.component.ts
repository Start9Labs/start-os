import { KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ServiceActionRequestsComponent } from './action-requests.component'

@Component({
  selector: 'service-dependencies',
  template: `
    <header>Dependencies</header>
    @for (d of pkg.currentDependencies | keyvalue; track $index) {
      <a
        tuiCell
        [routerLink]="services[d.key] ? ['..', d.key] : ['/portal/marketplace']"
        [queryParams]="services[d.key] ? {} : { id: d.key }"
      >
        <tui-avatar><img alt="" [src]="d.value.icon" /></tui-avatar>
        <span tuiTitle>
          {{ d.value.title }}
          <span tuiSubtitle>{{ d.value.versionRange }}</span>
        </span>
        <tui-icon icon="@tui.arrow-right" />
      </a>
      @if (services[d.key]; as service) {
        <service-action-requests [pkg]="service" />
      }
    } @empty {
      <blockquote>No dependencies</blockquote>
    }
  `,
  styles: `
    a {
      margin: 0 -1rem;

      &::after {
        display: none;
      }
    }

    service-action-requests {
      display: block;
      padding: 1rem 0 0 2.375rem;
      margin: -1rem 0 1rem 1.125rem;
      box-shadow: inset 0.125rem 0 var(--tui-border-normal);
    }

    blockquote {
      text-align: center;
      font: var(--tui-font-text-l);
      color: var(--tui-text-tertiary);
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    KeyValuePipe,
    TuiCell,
    TuiAvatar,
    TuiTitle,
    ServiceActionRequestsComponent,
    RouterLink,
    TuiIcon,
  ],
})
export class ServiceDependenciesComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input({ required: true })
  services: Record<string, PackageDataEntry> = {}
}

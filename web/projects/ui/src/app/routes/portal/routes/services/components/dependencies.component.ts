import { KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ServicePlaceholderComponent } from './placeholder.component'

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
    } @empty {
      <service-placeholder icon="@tui.boxes">
        No dependencies
      </service-placeholder>
    }
  `,
  styles: `
    :host {
      grid-column: span 3;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    KeyValuePipe,
    RouterLink,
    TuiCell,
    TuiAvatar,
    TuiTitle,
    TuiIcon,
    ServicePlaceholderComponent,
  ],
})
export class ServiceDependenciesComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input({ required: true })
  services: Record<string, PackageDataEntry> = {}
}

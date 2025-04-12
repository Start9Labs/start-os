import { KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { PkgDependencyErrors } from 'src/app/services/dep-error.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

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
          @if (getError(d.key); as error) {
            <span tuiSubtitle class="g-warning">{{ error }}</span>
          } @else {
            <span tuiSubtitle class="g-positive">Satisfied</span>
          }
          <span tuiSubtitle>{{ d.value.versionRange }}</span>
        </span>
        <tui-icon icon="@tui.arrow-right" />
      </a>
    } @empty {
      <app-placeholder icon="@tui.boxes">No dependencies</app-placeholder>
    }
  `,
  styles: `
    :host {
      min-height: 12rem;
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
    PlaceholderComponent,
  ],
})
export class ServiceDependenciesComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input({ required: true })
  services: Record<string, PackageDataEntry> = {}

  @Input({ required: true })
  errors: PkgDependencyErrors = {}

  getError(id: string): string {
    const depError = this.errors[id]

    if (!depError) {
      return ''
    }

    switch (depError.type) {
      case 'notInstalled':
        return 'Not installed'
      case 'incorrectVersion':
        return 'Incorrect version'
      case 'notRunning':
        return 'Not running'
      case 'actionRequired':
        return 'Action required'
      case 'healthChecksFailed':
        return 'Required health check not passing'
      case 'transitive':
        return 'Dependency has a dependency issue'
      default:
        return 'Unknown error'
    }
  }
}

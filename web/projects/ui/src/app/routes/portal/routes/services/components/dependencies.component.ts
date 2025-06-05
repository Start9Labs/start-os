import { KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { PkgDependencyErrors } from 'src/app/services/dep-error.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'service-dependencies',
  template: `
    <header>{{ 'Dependencies' | i18n }}</header>
    @for (d of pkg.currentDependencies | keyvalue; track $index) {
      <a
        tuiCell
        [routerLink]="services[d.key] ? ['..', d.key] : ['/portal/marketplace']"
        [queryParams]="services[d.key] ? {} : { id: d.key }"
        [class.error]="getError(d.key)"
      >
        <tui-avatar>
          <img
            alt=""
            [src]="d.value.icon || 'assets/img/service-icons/fallback.png'"
          />
        </tui-avatar>
        <span tuiTitle>
          {{ d.value.title || d.key }}
          @if (getError(d.key); as error) {
            <span tuiSubtitle class="g-warning">
              {{ error | i18n }}
              @if (getHealthCheckName(d.key); as healthCheckName) {
                : {{ getHealthCheckName }}
              }
            </span>
          } @else {
            <span tuiSubtitle class="g-positive">{{ 'Satisfied' | i18n }}</span>
          }
          <span tuiSubtitle>{{ d.value.versionRange }}</span>
        </span>
        <tui-icon icon="@tui.arrow-right" />
      </a>
    } @empty {
      <app-placeholder icon="@tui.boxes">
        {{ 'No dependencies' | i18n }}
      </app-placeholder>
    }
  `,
  styles: `
    :host {
      min-height: 12rem;
      grid-column: span 3;
    }

    .error {
      box-shadow: inset 1.25rem 0 0 -1rem var(--tui-status-warning);
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    KeyValuePipe,
    RouterLink,
    TuiCell,
    TuiAvatar,
    TuiTitle,
    TuiIcon,
    PlaceholderComponent,
    i18nPipe,
  ],
})
export class ServiceDependenciesComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input({ required: true })
  services: Record<string, PackageDataEntry> = {}

  @Input({ required: true })
  errors: PkgDependencyErrors = {}

  getError(id: string): i18nKey | undefined {
    const depError = this.errors[id]

    if (!depError) {
      return undefined
    }

    switch (depError.type) {
      case 'notInstalled':
        return 'Not installed'
      case 'incorrectVersion':
        return 'Incorrect version'
      case 'notRunning':
        return 'Not running'
      case 'actionRequired':
        return 'Task Required'
      case 'healthChecksFailed':
        return 'Required health check not passing'
      case 'transitive':
        return 'Dependency has a dependency issue'
      default:
        return 'Unknown error'
    }
  }

  getHealthCheckName(id: string) {
    const depError = this.errors[id]
    return depError?.type === 'healthChecksFailed'
      ? depError.check.name
      : undefined
  }
}

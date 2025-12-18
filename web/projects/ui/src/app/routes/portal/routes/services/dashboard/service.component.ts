import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  OnChanges,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { tuiPure } from '@taiga-ui/cdk'
import { ServiceUptimeComponent } from 'src/app/routes/portal/routes/services/components/uptime.component'
import { ConnectionService } from 'src/app/services/connection.service'
import { PkgDependencyErrors } from 'src/app/services/dep-error.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import { StatusComponent } from './status.component'

@Component({
  selector: 'tr[appService]',
  template: `
    <td [style.width.rem]="3" [style.grid-area]="'1 / 1 / 4'">
      <img alt="logo" [src]="pkg.icon" />
    </td>
    <td class="title">
      <a [routerLink]="routerLink">{{ manifest.title }}</a>
    </td>
    <td class="status" [style.grid-area]="'3 / 2'">
      <app-status [pkg]="pkg" [hasDepErrors]="hasError(depErrors)" />
    </td>
    <td class="version">{{ manifest.version }}</td>
    <td class="uptime">
      @if (pkg.statusInfo.started; as started) {
        <span>{{ 'Uptime' | i18n }}:</span>
        <service-uptime [started]="started" />
      } @else {
        -
      }
    </td>
  `,
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    :host {
      @include taiga.transition(background);
      cursor: pointer;

      &:hover {
        background: var(--tui-background-neutral-1);
      }
    }

    td::before {
      display: none;
    }

    img {
      display: block;
      height: 2rem;
      width: 2rem;
      border-radius: 100%;
    }

    a {
      color: var(--tui-text-primary);
      font-weight: bold;
    }

    span {
      display: none;
    }

    .title {
      width: 21rem;
    }

    .status {
      width: 21rem;
    }

    .uptime {
      width: 13rem;
    }

    .version {
      width: 21rem;
    }

    :host-context(tui-root._mobile) {
      position: relative;
      display: grid;
      grid-template: 1.25rem 2rem 1.5rem/4rem 1fr;
      align-items: center;
      padding: 1rem;

      &:hover {
        background: none;
      }

      img {
        height: 3rem;
        width: 3rem;
      }

      td {
        padding: 0;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        color: var(--tui-text-secondary);

        &::before {
          display: inline;
        }

        &:empty {
          display: none;
        }
      }

      .title {
        grid-area: 2 / 2;
        font: var(--tui-font-heading-6);
      }

      .version {
        grid-area: 1 / 2;
        font: var(--tui-font-text-s);
      }

      .uptime {
        grid-area: 4 / 2;
        display: flex;
        align-items: baseline;
        gap: 0.5rem;

        &:not(:has(service-uptime)) {
          display: none;
        }

        span {
          display: inline-block;
        }
      }
    }
  `,
  hostDirectives: [RouterLink],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [RouterLink, StatusComponent, ServiceUptimeComponent, i18nPipe],
})
export class ServiceComponent implements OnChanges {
  private readonly link = inject(RouterLink)

  @Input()
  pkg!: PackageDataEntry

  @Input()
  depErrors?: PkgDependencyErrors

  readonly connected$ = inject(ConnectionService)

  get installed(): boolean {
    return this.pkg.stateInfo.state === 'installed'
  }

  get manifest() {
    return getManifest(this.pkg)
  }

  get routerLink() {
    return `/services/${this.manifest.id}`
  }

  ngOnChanges() {
    this.link.routerLink = this.routerLink
  }

  @tuiPure
  hasError(errors: PkgDependencyErrors = {}): boolean {
    return Object.values(errors).some(Boolean)
  }
}

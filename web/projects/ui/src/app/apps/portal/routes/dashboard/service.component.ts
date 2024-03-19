import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { tuiPure } from '@taiga-ui/cdk'
import { ControlsComponent } from 'src/app/apps/portal/routes/dashboard/controls.component'
import { StatusComponent } from 'src/app/apps/portal/routes/dashboard/status.component'
import { ConnectionService } from 'src/app/services/connection.service'
import { PkgDependencyErrors } from 'src/app/services/dep-error.service'
import {
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'

@Component({
  standalone: true,
  selector: 'tr[appService]',
  template: `
    <td><img alt="logo" [src]="appService.icon" /></td>
    <td>
      <a [routerLink]="routerLink">{{ appService.manifest.title }}</a>
    </td>
    <td>{{ appService.manifest.version }}</td>
    <td
      [appStatus]="appService"
      [appStatusError]="hasError(appServiceError)"
    ></td>
    <td [style.text-align]="'center'">
      <fieldset
        [disabled]="!installed || !(connected$ | async)"
        [appControls]="appService"
      ></fieldset>
    </td>
  `,
  styles: `
    img {
      height: 2rem;
      width: 2rem;
      border-radius: 100%;
    }

    td {
      padding: 0.5rem;
    }

    a {
      color: var(--tui-text-01);
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [RouterLink, AsyncPipe, StatusComponent, ControlsComponent],
})
export class ServiceComponent {
  @Input()
  appService!: PackageDataEntry

  @Input()
  appServiceError?: PkgDependencyErrors

  readonly connected$ = inject(ConnectionService).connected$

  get routerLink() {
    return `/portal/service/${this.appService.manifest.id}`
  }

  get installed(): boolean {
    return this.appService.state === PackageState.Installed
  }

  @tuiPure
  hasError(errors: PkgDependencyErrors = {}): boolean {
    return Object.values(errors).some(Boolean)
  }
}

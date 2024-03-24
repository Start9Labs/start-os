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
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/util/get-package-data'

@Component({
  standalone: true,
  selector: 'tr[appService]',
  template: `
    <td><img alt="logo" [src]="pkg.icon" /></td>
    <td>
      <a [routerLink]="routerLink">{{ manifest.title }}</a>
    </td>
    <td>{{ manifest.version }}</td>
    <td appStatus [pkg]="pkg" [hasDepErrors]="hasError(depErrors)"></td>
    <td [style.text-align]="'center'">
      <fieldset
        appControls
        [disabled]="
          this.pkg.stateInfo.state !== 'installed' || !(connected$ | async)
        "
        [pkg]="pkg"
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
  pkg!: PackageDataEntry

  @Input()
  depErrors?: PkgDependencyErrors

  readonly connected$ = inject(ConnectionService).connected$

  get manifest() {
    return getManifest(this.pkg)
  }

  get routerLink() {
    return `/portal/service/${this.manifest.id}`
  }

  @tuiPure
  hasError(errors: PkgDependencyErrors = {}): boolean {
    return Object.values(errors).some(Boolean)
  }
}

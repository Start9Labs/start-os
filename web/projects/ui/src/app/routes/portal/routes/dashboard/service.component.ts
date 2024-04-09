import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { tuiPure } from '@taiga-ui/cdk'
import { ControlsComponent } from 'src/app/routes/portal/routes/dashboard/controls.component'
import { StatusComponent } from 'src/app/routes/portal/routes/dashboard/status.component'
import { ConnectionService } from 'src/app/services/connection.service'
import { PkgDependencyErrors } from 'src/app/services/dep-error.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  standalone: true,
  selector: 'tr[appService]',
  template: `
    <td [style.grid-area]="'1 / 1 / 4'">
      <a [routerLink]="routerLink"><img alt="logo" [src]="pkg.icon" /></a>
    </td>
    <td [style.grid-area]="'1 / 2'">
      <a [routerLink]="routerLink">{{ manifest.title }}</a>
    </td>
    <td [style.grid-area]="'2 / 2'">{{ manifest.version }}</td>
    <td
      [style.grid-area]="'3 / 2'"
      appStatus
      [pkg]="pkg"
      [hasDepErrors]="hasError(depErrors)"
    ></td>
    <td [style.grid-area]="'2 / 3'" [style.text-align]="'center'">
      <fieldset
        appControls
        [disabled]="!installed || !(connected$ | async)"
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
      font-weight: bold;
    }

    .text {
      display: contents;
    }

    :host-context(tui-root._mobile) {
      position: relative;
      display: grid;
      grid-template: 2rem 2rem 2rem/6rem 1fr 2rem;
      align-items: center;
      padding: 1rem;

      img {
        height: 4rem;
        width: 4rem;
        margin: 1rem;
      }

      td {
        padding: 0;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
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

  get installed(): boolean {
    return this.pkg.stateInfo.state !== 'installed'
  }

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

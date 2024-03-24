import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { TuiLetModule, tuiPure } from '@taiga-ui/cdk'
import {
  TuiButtonModule,
  tuiButtonOptionsProvider,
} from '@taiga-ui/experimental'
import { map, Observable } from 'rxjs'
import { UILaunchComponent } from 'src/app/apps/portal/routes/dashboard/ui.component'
import { ActionsService } from 'src/app/apps/portal/services/actions.service'
import { DepErrorService } from 'src/app/services/dep-error.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/util/get-package-data'
import { Manifest } from '@start9labs/marketplace'

@Component({
  standalone: true,
  selector: 'fieldset[appControls]',
  template: `
    @if (pkg.status.main.status === 'running') {
      <button
        tuiIconButton
        iconLeft="tuiIconSquare"
        (click)="actions.stop(manifest)"
      >
        Stop
      </button>

      <button
        tuiIconButton
        iconLeft="tuiIconRotateCw"
        (click)="actions.restart(manifest)"
      >
        Restart
      </button>
    } @else {
      <button
        *tuiLet="hasUnmet(pkg) | async as hasUnmet"
        tuiIconButton
        iconLeft="tuiIconPlay"
        [disabled]="!this.pkg.status.configured"
        (click)="actions.start(manifest, !!hasUnmet)"
      >
        Start
      </button>

      <button
        tuiIconButton
        iconLeft="tuiIconTool"
        (click)="actions.configure(manifest)"
      >
        Configure
      </button>
    }

    <app-ui-launch [pkg]="pkg" />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButtonModule, UILaunchComponent, TuiLetModule, AsyncPipe],
  providers: [tuiButtonOptionsProvider({ size: 's', appearance: 'none' })],
})
export class ControlsComponent {
  private readonly errors = inject(DepErrorService)

  @Input()
  pkg!: PackageDataEntry

  get manifest(): Manifest {
    return getManifest(this.pkg)
  }

  readonly actions = inject(ActionsService)

  @tuiPure
  hasUnmet(pkg: PackageDataEntry): Observable<boolean> {
    const id = getManifest(pkg).id
    return this.errors.getPkgDepErrors$(id).pipe(
      map(errors =>
        Object.keys(pkg.currentDependencies)
          .map(id => !!(errors[id] as any)?.[id]) // @TODO fix
          .some(Boolean),
      ),
    )
  }
}

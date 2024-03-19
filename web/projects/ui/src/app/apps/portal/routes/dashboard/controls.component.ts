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
import { map, of } from 'rxjs'
import { UIComponent } from 'src/app/apps/portal/routes/dashboard/ui.component'
import { ActionsService } from 'src/app/apps/portal/services/actions.service'
import { DepErrorService } from 'src/app/services/dep-error.service'
import {
  PackageDataEntry,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'

@Component({
  standalone: true,
  selector: 'fieldset[appControls]',
  template: `
    @if (isRunning) {
      <button
        tuiIconButton
        iconLeft="tuiIconSquare"
        (click)="actions.stop(appControls)"
      >
        Stop
      </button>

      <button
        tuiIconButton
        iconLeft="tuiIconRotateCw"
        (click)="actions.restart(appControls)"
      >
        Restart
      </button>
    } @else {
      <button
        *tuiLet="hasUnmet(appControls) | async as hasUnmet"
        tuiIconButton
        iconLeft="tuiIconPlay"
        [disabled]="!isConfigured"
        (click)="actions.start(appControls, !!hasUnmet)"
      >
        Start
      </button>

      <button
        tuiIconButton
        iconLeft="tuiIconTool"
        (click)="actions.configure(appControls)"
      >
        Configure
      </button>
    }

    <app-ui [pkg]="appControls" />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButtonModule, UIComponent, TuiLetModule, AsyncPipe],
  providers: [tuiButtonOptionsProvider({ size: 's', appearance: 'none' })],
})
export class ControlsComponent {
  private readonly errors = inject(DepErrorService)

  @Input()
  appControls!: PackageDataEntry

  readonly actions = inject(ActionsService)

  get isRunning(): boolean {
    return (
      this.appControls.installed?.status.main.status ===
      PackageMainStatus.Running
    )
  }

  get isConfigured(): boolean {
    return !!this.appControls.installed?.status.configured
  }

  @tuiPure
  hasUnmet({ installed, manifest }: PackageDataEntry) {
    return installed
      ? this.errors.getPkgDepErrors$(manifest.id).pipe(
          map(errors =>
            Object.keys(installed['current-dependencies'])
              .filter(id => !!manifest.dependencies[id])
              .map(id => !!(errors[manifest.id] as any)?.[id]) // @TODO fix
              .some(Boolean),
          ),
        )
      : of(false)
  }
}

import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { TuiLet } from '@taiga-ui/cdk'
import { TuiButton, tuiButtonOptionsProvider } from '@taiga-ui/core'
import { map } from 'rxjs'
import { ControlsService } from 'src/app/services/controls.service'
import { DepErrorService } from 'src/app/services/dep-error.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'
import { getManifest } from 'src/app/utils/get-package-data'
import { UILaunchComponent } from './ui.component'

const RUNNING = ['running', 'starting', 'restarting']

@Component({
  standalone: true,
  selector: 'fieldset[appControls]',
  template: `
    @if (running()) {
      <button
        tuiIconButton
        iconStart="@tui.square"
        (click)="controls.stop(manifest())"
      >
        Stop
      </button>

      <button
        tuiIconButton
        iconStart="@tui.rotate-cw"
        [disabled]="status().primary !== 'running'"
        (click)="controls.restart(manifest())"
      >
        Restart
      </button>
    } @else {
      <button
        *tuiLet="hasUnmet() | async as hasUnmet"
        tuiIconButton
        iconStart="@tui.play"
        [disabled]="status().primary !== 'stopped'"
        (click)="controls.start(manifest(), !!hasUnmet)"
      >
        Start
      </button>
    }

    <app-ui-launch [pkg]="pkg()" />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, UILaunchComponent, TuiLet, AsyncPipe],
  providers: [tuiButtonOptionsProvider({ size: 's', appearance: 'none' })],
  styles: `
    :host {
      padding: 0;
      border: none;
      cursor: default;
    }

    :host-context(tui-root._mobile) {
      button {
        display: none;
      }
    }
  `,
})
export class ControlsComponent {
  private readonly errors = inject(DepErrorService)

  readonly controls = inject(ControlsService)
  readonly pkg = input.required<PackageDataEntry>()
  readonly status = computed(() => renderPkgStatus(this.pkg()))
  readonly running = computed(() => RUNNING.includes(this.status().primary))
  readonly manifest = computed(() => getManifest(this.pkg()))
  readonly hasUnmet = computed(() =>
    this.errors.getPkgDepErrors$(this.manifest().id).pipe(
      map(errors =>
        Object.keys(this.pkg().currentDependencies)
          .map(id => errors[id])
          .some(Boolean),
      ),
    ),
  )
}

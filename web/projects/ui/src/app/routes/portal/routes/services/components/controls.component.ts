import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  DOCUMENT,
  inject,
  input,
} from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiIcon,
  TuiOptionNew,
} from '@taiga-ui/core'
import { map } from 'rxjs'
import { ControlsService } from 'src/app/services/controls.service'
import { DepErrorService } from 'src/app/services/dep-error.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { getManifest } from 'src/app/utils/get-package-data'
import { InterfaceService } from '../../../components/interfaces/interface.service'

@Component({
  selector: 'service-controls',
  template: `
    @if (['running', 'starting', 'restarting'].includes(status()!)) {
      <button
        tuiButton
        appearance="primary-destructive"
        iconStart="@tui.square"
        (click)="controls.stop(manifest())"
      >
        {{ 'Stop' | i18n }}
      </button>

      @if (status() === 'running') {
        <button
          tuiButton
          iconStart="@tui.rotate-cw"
          (click)="controls.restart(manifest())"
        >
          {{ 'Restart' | i18n }}
        </button>

        @if (interfaces().length > 1) {
          <button
            tuiButton
            appearance="primary-grayscale"
            iconStart="@tui.external-link"
            tuiDropdownOpen
            [tuiDropdown]="content"
          >
            {{ 'Open UI' | i18n }}
          </button>
          <ng-template #content>
            <tui-data-list>
              @for (i of interfaces(); track $index) {
                <a
                  tuiOption
                  new
                  target="_blank"
                  rel="noreferrer"
                  [attr.href]="getHref(i)"
                >
                  {{ i.name }}
                  <tui-icon icon="@tui.external-link" />
                </a>
              }
            </tui-data-list>
          </ng-template>
        } @else if (interfaces()[0]; as i) {
          <button
            tuiButton
            appearance="primary-grayscale"
            iconStart="@tui.external-link"
            (click)="openUI(i)"
          >
            {{ 'Open UI' | i18n }}
          </button>
        }
      }
    }

    @if (status() === 'stopped') {
      @let unmet = hasUnmet() | async;
      <button
        tuiButton
        iconStart="@tui.play"
        (click)="controls.start(manifest(), !!unmet)"
      >
        {{ 'Start' | i18n }}
      </button>
    }
  `,
  styles: `
    :host {
      width: 100%;
      max-width: 18rem;
      display: flex;
      flex-wrap: wrap;
      gap: 1rem;
      justify-content: center;
      margin-block-start: 1rem;

      &:nth-child(3) {
        grid-row: span 2;
      }
    }

    [tuiButton] {
      flex: 1;
      min-width: fit-content;
    }

    :host-context(tui-root._mobile) {
      display: flex;
      margin: 0;

      [tuiButton] {
        font-size: 0;
        gap: 0;
        border-radius: 100%;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiButton,
    i18nPipe,
    AsyncPipe,
    TuiDataList,
    TuiDropdown,
    TuiIcon,
    TuiOptionNew,
  ],
})
export class ServiceControlsComponent {
  private readonly errors = inject(DepErrorService)
  private readonly interfaceService = inject(InterfaceService)
  private readonly document = inject(DOCUMENT)

  readonly pkg = input.required<PackageDataEntry>()
  readonly status = input<PrimaryStatus>()
  readonly manifest = computed(() => getManifest(this.pkg()))
  readonly controls = inject(ControlsService)

  readonly hasUnmet = computed(() =>
    this.errors.getPkgDepErrors$(this.manifest().id).pipe(
      map(errors =>
        Object.keys(this.pkg().currentDependencies)
          .map(id => errors[id])
          .some(Boolean),
      ),
    ),
  )

  readonly interfaces = computed(() =>
    Object.values(this.pkg().serviceInterfaces).filter(
      i =>
        i.type === 'ui' &&
        (i.addressInfo.scheme === 'http' ||
          i.addressInfo.sslScheme === 'https'),
    ),
  )

  getHref(ui: T.ServiceInterface): string {
    const host = this.pkg().hosts[ui.addressInfo.hostId]
    if (!host) return ''
    return this.interfaceService.launchableAddress(ui, host)
  }

  openUI(ui: T.ServiceInterface) {
    this.document.defaultView?.open(this.getHref(ui), '_blank', 'noreferrer')
  }
}

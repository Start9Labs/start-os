import {
  AfterViewInit,
  Component,
  ElementRef,
  inject,
  ViewChild,
  DOCUMENT,
} from '@angular/core'
import { DownloadHTMLService, ErrorService, i18nPipe } from '@start9labs/shared'
import { TuiIcon, TuiLoader, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCardLarge, TuiCell, TuiHeader } from '@taiga-ui/layout'
import { ApiService } from '../services/api.service'
import { StateService } from '../services/state.service'
import { DocumentationComponent } from '../components/documentation.component'
import { MatrixComponent } from '../components/matrix.component'
import { SetupCompleteRes } from '../types'

@Component({
  template: `
    <canvas matrix></canvas>
    <section tuiCardLarge>
      <header tuiHeader>
        <h2 tuiTitle>
          <span class="inline-title">
            <tui-icon icon="@tui.circle-check-big" class="g-positive" />
            {{ 'Setup Complete!' | i18n }}
          </span>
          @if (!stateService.kiosk) {
            <span tuiSubtitle>
              {{
                stateService.setupType === 'restore'
                  ? ('You can unplug your backup drive' | i18n)
                  : stateService.setupType === 'transfer'
                    ? ('You can unplug your transfer drive' | i18n)
                    : ('http://start.local was for setup only. It will no longer work.'
                      | i18n)
              }}
            </span>
          }
        </h2>
      </header>

      @if (!result) {
        <tui-loader />
      } @else {
        <!-- Step: Download Address Info (non-kiosk only) -->
        @if (!stateService.kiosk) {
          <button tuiCell="l" [disabled]="downloaded" (click)="download()">
            <tui-avatar appearance="secondary" src="@tui.download" />
            <div tuiTitle>
              {{ 'Download Address Info' | i18n }}
              <div tuiSubtitle>
                {{
                  "Contains your server's permanent local address and Root CA"
                    | i18n
                }}
              </div>
            </div>
            @if (downloaded) {
              <tui-icon icon="@tui.circle-check" class="g-positive" />
            }
          </button>
        }

        <!-- Step: Remove USB Media (when restart needed) -->
        @if (result.needsRestart) {
          <button
            tuiCell="l"
            [class.disabled]="!stateService.kiosk && !downloaded"
            [disabled]="(!stateService.kiosk && !downloaded) || usbRemoved"
            (click)="usbRemoved = true"
          >
            <tui-avatar appearance="secondary" src="@tui.usb" />
            <div tuiTitle>
              {{ 'USB Removed' | i18n }}
              <div tuiSubtitle>
                {{
                  'Remove the USB installation media from your server' | i18n
                }}
              </div>
            </div>
            @if (usbRemoved) {
              <tui-icon icon="@tui.circle-check" class="g-positive" />
            }
          </button>

          <!-- Step: Restart Server -->
          <button
            tuiCell="l"
            [class.disabled]="!usbRemoved"
            [disabled]="!usbRemoved || rebooted || rebooting"
            (click)="reboot()"
          >
            <tui-avatar appearance="secondary" src="@tui.rotate-cw" />
            <div tuiTitle>
              {{ 'Restart Server' | i18n }}
              <div tuiSubtitle>
                @if (rebooting) {
                  {{ 'Waiting for server to come back online' | i18n }}
                } @else if (rebooted) {
                  {{ 'Server is back online' | i18n }}
                } @else {
                  {{ 'Restart your server to complete setup' | i18n }}
                }
              </div>
            </div>
            @if (rebooting) {
              <tui-loader />
            } @else if (rebooted) {
              <tui-icon icon="@tui.circle-check" class="g-positive" />
            }
          </button>
        }

        <!-- Step: Open Local Address (non-kiosk only) -->
        @if (!stateService.kiosk) {
          <button
            tuiCell="l"
            [class.disabled]="!canOpenAddress"
            [disabled]="!canOpenAddress"
            (click)="openLocalAddress()"
          >
            <tui-avatar appearance="secondary" src="@tui.external-link" />
            <div tuiTitle>
              {{ 'Open Local Address' | i18n }}
              <div tuiSubtitle>{{ lanAddress }}</div>
            </div>
          </button>

          <app-documentation hidden [lanAddress]="lanAddress" />
        }

        <!-- Step: Continue to Login (kiosk only) -->
        @if (stateService.kiosk) {
          <button
            tuiCell="l"
            [class.disabled]="result.needsRestart && !rebooted"
            [disabled]="result.needsRestart && !rebooted"
            (click)="exitKiosk()"
          >
            <tui-avatar appearance="secondary" src="@tui.log-in" />
            <div tuiTitle>
              {{ 'Continue to Login' | i18n }}
              <div tuiSubtitle>
                {{ 'Proceed to the StartOS login screen' | i18n }}
              </div>
            </div>
          </button>
        }
      }
    </section>
  `,
  styles: `
    .inline-title {
      display: inline-flex;
      align-items: center;
      gap: 0.5rem;
    }

    [tuiCell].disabled {
      opacity: var(--tui-disabled-opacity);
      pointer-events: none;
    }
  `,
  imports: [
    TuiCardLarge,
    TuiCell,
    TuiIcon,
    TuiLoader,
    TuiAvatar,
    MatrixComponent,
    DocumentationComponent,
    TuiHeader,
    TuiTitle,
    i18nPipe,
  ],
})
export default class SuccessPage implements AfterViewInit {
  @ViewChild(DocumentationComponent, { read: ElementRef })
  private readonly documentation?: ElementRef<HTMLElement>

  private readonly document = inject(DOCUMENT)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly downloadHtml = inject(DownloadHTMLService)
  private readonly i18n = inject(i18nPipe)

  readonly stateService = inject(StateService)

  result?: SetupCompleteRes
  lanAddress = ''
  downloaded = false
  usbRemoved = false
  rebooting = false
  rebooted = false

  get canOpenAddress(): boolean {
    if (!this.downloaded) return false
    if (this.result?.needsRestart && !this.rebooted) return false
    return true
  }

  ngAfterViewInit() {
    setTimeout(() => this.complete(), 500)
  }

  download() {
    if (this.downloaded) return

    const lanElem = this.document.getElementById('lan-addr')
    if (lanElem) lanElem.innerHTML = this.lanAddress

    this.document
      .getElementById('cert')
      ?.setAttribute(
        'href',
        `data:application/x-x509-ca-cert;base64,${this.result!.rootCa}`,
      )

    const html = this.documentation?.nativeElement.innerHTML || ''

    this.downloadHtml.download('StartOS-info.html', html).then(() => {
      this.downloaded = true
    })
  }

  exitKiosk() {
    this.api.exit()
  }

  openLocalAddress() {
    window.open(this.lanAddress, '_blank')
  }

  async reboot() {
    this.rebooting = true

    try {
      await this.api.exit()
      await this.pollForServer()
      this.rebooted = true
      this.rebooting = false
    } catch (e: any) {
      this.errorService.handleError(e)
      this.rebooting = false
    }
  }

  private async complete() {
    try {
      this.result = await this.api.complete()

      if (!this.stateService.kiosk) {
        this.lanAddress = `http://${this.result.hostname}.local`

        if (!this.result.needsRestart) {
          await this.api.exit()
        }
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  private async pollForServer(): Promise<void> {
    const maxAttempts = 60
    let attempts = 0

    while (attempts < maxAttempts) {
      try {
        await this.api.echo({ message: 'ping' }, this.lanAddress)
        return
      } catch {
        await new Promise(resolve => setTimeout(resolve, 5000))
        attempts++
      }
    }

    throw new Error(
      this.i18n.transform(
        'Server did not come back online. Please check your server and try accessing it manually.',
      ),
    )
  }
}

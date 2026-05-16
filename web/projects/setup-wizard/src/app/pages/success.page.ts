import {
  AfterViewInit,
  Component,
  DOCUMENT,
  ElementRef,
  inject,
  ViewChild,
} from '@angular/core'
import {
  DialogService,
  DownloadHTMLService,
  ErrorService,
  i18nPipe,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiCell, TuiIcon, TuiLoader, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar } from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { DocumentationComponent } from '../components/documentation.component'
import { MatrixComponent } from '../components/matrix.component'
import { MokEnrollmentDialog } from '../components/mok-enrollment.dialog'
import { RemoveMediaDialog } from '../components/remove-media.dialog'
import { ApiService } from '../services/api.service'
import { StateService } from '../services/state.service'

// Runs when the downloaded HTML is opened. iOS Safari ignores Content-Type on
// .crt downloads and saves the file as .crt.html — but installs profiles cleanly
// when given a .mobileconfig. The script swaps the cert link's href/download on
// iOS/iPadOS so the same offline HTML works for both Apple-mobile and everyone else.
const IOS_CERT_SWAP_SCRIPT = `<script>
(function(){var el=document.getElementById('cert');if(!el)return;
var ua=navigator.userAgent;
var isIOS=/iPad|iPhone|iPod/.test(ua)||(navigator.platform==='MacIntel'&&navigator.maxTouchPoints>1);
if(isIOS&&el.dataset.mobileconfigHref){el.href=el.dataset.mobileconfigHref;
if(el.dataset.mobileconfigName)el.download=el.dataset.mobileconfigName;}})();
</script>`

function buildMobileconfigPlist(
  derBase64: string,
  hostname: string,
  certUuid: string,
  profileUuid: string,
): string {
  return `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
\t<key>PayloadContent</key>
\t<array>
\t\t<dict>
\t\t\t<key>PayloadCertificateFileName</key>
\t\t\t<string>${hostname}.crt</string>
\t\t\t<key>PayloadContent</key>
\t\t\t<data>${derBase64}</data>
\t\t\t<key>PayloadDescription</key>
\t\t\t<string>Adds the StartOS root certificate authority for ${hostname}.</string>
\t\t\t<key>PayloadDisplayName</key>
\t\t\t<string>${hostname} Root Certificate</string>
\t\t\t<key>PayloadIdentifier</key>
\t\t\t<string>com.start9.ca.cert.${certUuid}</string>
\t\t\t<key>PayloadType</key>
\t\t\t<string>com.apple.security.root</string>
\t\t\t<key>PayloadUUID</key>
\t\t\t<string>${certUuid}</string>
\t\t\t<key>PayloadVersion</key>
\t\t\t<integer>1</integer>
\t\t</dict>
\t</array>
\t<key>PayloadDescription</key>
\t<string>Trusts the root certificate authority for ${hostname}.</string>
\t<key>PayloadDisplayName</key>
\t<string>StartOS Root CA (${hostname})</string>
\t<key>PayloadIdentifier</key>
\t<string>com.start9.ca.profile.${profileUuid}</string>
\t<key>PayloadType</key>
\t<string>Configuration</string>
\t<key>PayloadUUID</key>
\t<string>${profileUuid}</string>
\t<key>PayloadVersion</key>
\t<integer>1</integer>
</dict>
</plist>
`
}

@Component({
  template: `
    <canvas matrix></canvas>
    <section tuiCardLarge>
      <header tuiHeader>
        <hgroup tuiTitle>
          <h2 tuiCell="m">
            <tui-icon icon="@tui.circle-check-big" class="g-positive" />
            {{ 'Setup Complete!' | i18n }}
          </h2>
          @if (!stateService.kiosk) {
            <p tuiSubtitle>
              {{
                'http://start.local was for setup only. It will no longer work.'
                  | i18n
              }}
            </p>
          }
        </hgroup>
      </header>

      @if (!result) {
        <tui-loader />
      } @else {
        <!-- Step: Download Address Info (non-kiosk only) -->
        @if (!stateService.kiosk) {
          <button tuiCell="l" (click)="download()">
            <span tuiAvatar="@tui.download" appearance="secondary"></span>
            <span tuiTitle>
              <b>{{ 'Download Address Info' | i18n }}</b>
              <span tuiSubtitle>
                {{
                  "Contains your server's permanent local address and Root CA"
                    | i18n
                }}
              </span>
            </span>
            @if (downloaded) {
              <tui-icon icon="@tui.circle-check" class="g-positive" />
            }
          </button>
        }

        <!-- Step: Restart flow -->
        @if (result.needsRestart) {
          <button
            tuiCell="l"
            [class.disabled]="!stateService.kiosk && !downloaded"
            [disabled]="!stateService.kiosk && !downloaded"
            (click)="removeMedia()"
          >
            <span tuiAvatar="@tui.usb" appearance="secondary"></span>
            <span tuiTitle>
              <b>{{ 'Remove Installation Media' | i18n }}</b>
              <span tuiSubtitle>
                {{
                  'Remove USB stick or other installation media from your server'
                    | i18n
                }}
              </span>
            </span>
            @if (usbRemoved) {
              <tui-icon icon="@tui.circle-check" class="g-positive" />
            }
          </button>

          <!-- Step: Secure Boot Enrollment (when MOK enrolled) -->
          @if (stateService.mokEnrolled) {
            <button
              tuiCell="l"
              [class.disabled]="!usbRemoved"
              [disabled]="!usbRemoved"
              (click)="acknowledgeMok()"
            >
              <span tuiAvatar="@tui.shield-check" appearance="secondary"></span>
              <span tuiTitle>
                <b>{{ 'Secure Boot Enrollment' | i18n }}</b>
                <span tuiSubtitle>
                  {{
                    'Prepare for Secure Boot key enrollment on the next reboot'
                      | i18n
                  }}
                </span>
              </span>
              @if (mokAcknowledged) {
                <tui-icon icon="@tui.circle-check" class="g-positive" />
              }
            </button>
          }

          <!-- Step: Restart Server -->
          <button
            tuiCell="l"
            [class.disabled]="
              !usbRemoved || (stateService.mokEnrolled && !mokAcknowledged)
            "
            [disabled]="
              !usbRemoved || (stateService.mokEnrolled && !mokAcknowledged)
            "
            (click)="reboot()"
          >
            <span tuiAvatar="@tui.rotate-cw" appearance="secondary"></span>
            <span tuiTitle>
              <b>{{ 'Restart Server' | i18n }}</b>
              <span tuiSubtitle>
                @if (rebooting) {
                  {{ 'Waiting for server to come back online' | i18n }}
                } @else if (rebooted) {
                  {{ 'Server is back online' | i18n }}
                } @else {
                  {{ 'Restart your server to complete setup' | i18n }}
                }
              </span>
            </span>
            @if (rebooting) {
              <tui-loader />
            } @else if (rebooted) {
              <tui-icon icon="@tui.circle-check" class="g-positive" />
            }
          </button>
        } @else if (stateService.kiosk) {
          <button tuiCell="l" (click)="exitKiosk()">
            <span tuiAvatar="@tui.log-in" appearance="secondary"></span>
            <span tuiTitle>
              <b>{{ 'Continue to Login' | i18n }}</b>
              <span tuiSubtitle>
                {{ 'Proceed to the StartOS login screen' | i18n }}
              </span>
            </span>
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
            <span tuiAvatar="@tui.external-link" appearance="secondary"></span>
            <span tuiTitle>
              <b>{{ 'Open Local Address' | i18n }}</b>
              <span tuiSubtitle>{{ lanAddress }}</span>
            </span>
          </button>

          <app-documentation hidden [lanAddress]="lanAddress" />
        }
      }
    </section>
  `,
  styles: `
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
  private readonly dialogs = inject(DialogService)
  private readonly i18n = inject(i18nPipe)

  readonly stateService = inject(StateService)

  result?: T.SetupResult
  lanAddress = ''
  downloaded = false
  usbRemoved = false
  mokAcknowledged = false
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
    const lanElem = this.document.getElementById('lan-addr')
    if (lanElem) lanElem.innerHTML = this.lanAddress

    const certElem = this.document.getElementById('cert')
    if (certElem) {
      certElem.setAttribute(
        'href',
        `data:application/octet-stream;base64,${this.result!.rootCa}`,
      )
      certElem.setAttribute(
        'data-mobileconfig-href',
        this.buildMobileconfigDataUrl(
          this.result!.rootCa,
          this.result!.hostname,
        ),
      )
      certElem.setAttribute(
        'data-mobileconfig-name',
        `${this.result!.hostname}.mobileconfig`,
      )
    }

    const html = this.documentation?.nativeElement.innerHTML || ''

    this.downloadHtml
      .download('StartOS-info.html', html + IOS_CERT_SWAP_SCRIPT)
      .then(() => {
        this.downloaded = true
      })
  }

  private buildMobileconfigDataUrl(pem: string, hostname: string): string {
    const derBase64 = pem
      .replace(/-----BEGIN CERTIFICATE-----/g, '')
      .replace(/-----END CERTIFICATE-----/g, '')
      .replace(/\s+/g, '')
    const certUuid = crypto.randomUUID()
    const profileUuid = crypto.randomUUID()
    const plist = buildMobileconfigPlist(
      derBase64,
      hostname,
      certUuid,
      profileUuid,
    )
    return `data:application/x-apple-aspen-config;base64,${btoa(plist)}`
  }

  removeMedia() {
    this.dialogs
      .openComponent<boolean>(new PolymorpheusComponent(RemoveMediaDialog), {
        dismissible: false,
        closable: false,
      })
      .subscribe(() => {
        this.usbRemoved = true
      })
  }

  acknowledgeMok() {
    this.dialogs
      .openComponent<boolean>(new PolymorpheusComponent(MokEnrollmentDialog), {
        label: 'Secure Boot',
        size: 'm',
        dismissible: false,
        closable: false,
      })
      .subscribe(() => {
        this.mokAcknowledged = true
      })
  }

  exitKiosk() {
    this.api.exit()
  }

  openLocalAddress() {
    window.open(this.lanAddress, '_blank')
  }

  async reboot() {
    if (this.rebooting || this.rebooted) return

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
        await this.api.echo({ message: 'ping' }, `${this.lanAddress}/rpc/v1`)
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

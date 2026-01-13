import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { DiskInfo, ErrorService, i18nPipe, toGuid } from '@start9labs/shared'
import {
  TuiButton,
  TuiDataList,
  TuiDialogOptions,
  TuiDialogService,
  TuiDropdown,
  TuiIcon,
  TuiLoader,
  TuiTitle,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiConfirmData } from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { ApiService } from '../services/api.service'
import { StateService } from '../services/state.service'

@Component({
  template: `
    <section tuiCardLarge="compact">
      <header tuiHeader>
        <h2 tuiTitle>
          {{ 'Transfer Data' | i18n }}
          <span tuiSubtitle>
            {{
              'Select the drive containing your existing StartOS data' | i18n
            }}
            <a class="refresh" (click)="refresh()">
              <tui-icon icon="@tui.rotate-cw" />
              {{ 'Refresh' | i18n }}
            </a>
          </span>
        </h2>
      </header>

      @if (loading) {
        <tui-loader />
      } @else {
        <button
          tuiButton
          iconEnd="@tui.chevron-down"
          [tuiDropdown]="dropdown"
          [tuiDropdownLimitWidth]="'fixed'"
          [(tuiDropdownOpen)]="open"
          style="width: 100%"
        >
          {{ 'Select Drive' | i18n }}
        </button>

        <ng-template #dropdown>
          <tui-data-list>
            @for (drive of drives; track drive.logicalname) {
              <button tuiOption new (click)="select(drive)">
                <div class="drive-item">
                  <span>{{ drive.vendor }} {{ drive.model }}</span>
                  <small>{{ drive.logicalname }}</small>
                </div>
              </button>
            } @empty {
              <div class="no-items">
                {{ 'No StartOS data drives found' | i18n }}
              </div>
            }
          </tui-data-list>
        </ng-template>
      }
    </section>
  `,
  styles: `
    .refresh {
      display: inline-flex;
      align-items: center;
      gap: 0.25rem;
      cursor: pointer;
      color: var(--tui-text-action);

      tui-icon {
        font-size: 0.875rem;
      }
    }

    .drive-item {
      display: flex;
      flex-direction: column;

      small {
        opacity: 0.7;
      }
    }

    .no-items {
      padding: 0.5rem 0.75rem;
      color: var(--tui-text-secondary);
      font-style: italic;
    }
  `,
  imports: [
    TuiButton,
    TuiCardLarge,
    TuiDataList,
    TuiDropdown,
    TuiIcon,
    TuiLoader,
    TuiTitle,
    TuiHeader,
    i18nPipe,
  ],
})
export default class TransferPage {
  private readonly api = inject(ApiService)
  private readonly router = inject(Router)
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly stateService = inject(StateService)
  private readonly i18n = inject(i18nPipe)

  loading = true
  open = false
  drives: DiskInfo[] = []

  async ngOnInit() {
    await this.loadDrives()
  }

  async refresh() {
    this.loading = true
    await this.loadDrives()
  }

  select(drive: DiskInfo) {
    this.open = false

    const WARNING_OPTIONS: Partial<TuiDialogOptions<TuiConfirmData>> = {
      label: this.i18n.transform('Warning'),
      size: 's',
      data: {
        content: this.i18n.transform(
          'After transferring data from this drive, do not attempt to boot into it again as a Start9 Server. This may result in services malfunctioning, data corruption, or loss of funds.',
        ),
        yes: this.i18n.transform('Continue'),
        no: this.i18n.transform('Cancel'),
      },
    }

    this.dialogs
      .open(TUI_CONFIRM, WARNING_OPTIONS)
      .pipe(filter(Boolean))
      .subscribe(() => {
        const guid = toGuid(drive)
        if (guid) {
          this.stateService.recoverySource = {
            type: 'migrate',
            guid,
          }
          this.router.navigate(['/password'])
        }
      })
  }

  private async loadDrives() {
    try {
      const allDrives = await this.api.getDisks()
      // Filter to only drives with StartOS data (guid)
      this.drives = allDrives.filter(toGuid)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }
}

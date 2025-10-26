import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { LoadingService } from '@start9labs/shared'
import { TuiAutoFocus, tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiError,
  TuiTextfield,
} from '@taiga-ui/core'
import { TuiDialog, TuiDialogService } from '@taiga-ui/experimental'
import { TUI_CONFIRM, TuiFieldErrorPipe } from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { filter, map, tap } from 'rxjs'
import { ApiService } from 'src/app/services/api/api.service'
import { TunnelData } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    <table class="g-table">
      <thead>
        <tr>
          <th>Name</th>
          <th>IP Range</th>
          <th [style.padding-inline-end.rem]="0.625">
            <button tuiButton size="xs" iconStart="@tui.plus" (click)="onAdd()">
              Add
            </button>
          </th>
        </tr>
      </thead>
      <tbody>
        @for (subnet of subnets(); track $index) {
          <tr>
            <td>{{ subnet.name }}</td>
            <td>{{ subnet.range }}</td>
            <td>
              <button
                tuiIconButton
                size="xs"
                tuiDropdown
                tuiDropdownOpen
                appearance="flat-grayscale"
                iconStart="@tui.ellipsis-vertical"
              >
                Actions
                <tui-data-list *tuiTextfieldDropdown size="s">
                  <button
                    tuiOption
                    iconStart="@tui.pencil"
                    new
                    (click)="onEdit(subnet)"
                  >
                    Rename
                  </button>
                  <button
                    tuiOption
                    iconStart="@tui.trash"
                    new
                    (click)="onDelete($index)"
                  >
                    Delete
                  </button>
                </tui-data-list>
              </button>
            </td>
          </tr>
        }
      </tbody>
    </table>
    <ng-template [tuiDialogOptions]="{ label: label() }" [(tuiDialog)]="dialog">
      <form tuiForm [formGroup]="form">
        <tui-textfield>
          <label tuiLabel>Name</label>
          <input tuiTextfield tuiAutoFocus formControlName="name" />
        </tui-textfield>
        <tui-error
          formControlName="name"
          [error]="[] | tuiFieldError | async"
        />
        @if (!editing()) {
          <tui-textfield>
            <label tuiLabel>IP Range</label>
            <input tuiTextfield formControlName="subnet" />
          </tui-textfield>
          <tui-error
            formControlName="subnet"
            [error]="[] | tuiFieldError | async"
          />
        }
        <footer>
          <button tuiButton (click)="onSave()" [disabled]="form.invalid">
            Save
          </button>
        </footer>
      </form>
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiButton,
    TuiDropdown,
    TuiDataList,
    TuiTextfield,
    TuiDialog,
    TuiForm,
    TuiError,
    TuiFieldErrorPipe,
    TuiAutoFocus,
  ],
})
export default class Subnets {
  private readonly dialogs = inject(TuiDialogService)
  private readonly api = inject(ApiService)
  private readonly loading = inject(LoadingService)
  private readonly patch = inject<PatchDB<TunnelData>>(PatchDB)

  protected readonly dialog = signal(false)
  protected readonly editing = signal(false)

  protected readonly subnets = toSignal<MappedSubnet[], []>(
    this.patch.watch$('wg', 'subnets').pipe(
      map(s =>
        Object.entries(s).map(([range, info]) => ({
          range,
          name: info.name,
          hasClients: !!Object.keys(info.clients).length,
        })),
      ),
    ),
    { initialValue: [] },
  )

  protected readonly next = computed(() => {
    const last = Number(
      this.subnets().at(-1)?.range.split('/')[0]?.split('.')[2] || '-1',
    )
    return `10.59.${last + 1}.1/24`
  })

  protected readonly label = computed(() =>
    this.editing() ? 'Rename Subnet' : 'Add Subnet',
  )

  protected readonly form = inject(NonNullableFormBuilder).group({
    name: ['', Validators.required],
    subnet: ['', Validators.required],
  })

  protected onAdd(): void {
    this.editing.set(false)
    this.form.reset({ subnet: this.next() })
    this.dialog.set(true)
  }

  protected onEdit(subnet: MappedSubnet): void {
    this.editing.set(true)
    this.form.reset({ subnet: subnet.range, name: subnet.name })
    this.dialog.set(true)
  }

  protected async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }

    const loader = this.loading.open().subscribe()
    const value = this.form.getRawValue()

    try {
      this.editing()
        ? await this.api.editSubnet(value)
        : await this.api.addSubnet(value)
    } catch (e) {
      console.log(e)
    } finally {
      loader.unsubscribe()
      this.dialog.set(false)
    }
  }

  protected onDelete(index: number): void {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const subnet = this.subnets().at(index)?.range
        if (!subnet) return

        const loader = this.loading.open().subscribe()
        try {
          await this.api.deleteSubnet({ subnet })
        } catch (e) {
          console.log(e)
        } finally {
          loader.unsubscribe()
          this.dialog.set(false)
        }
      })
  }
}

type MappedSubnet = {
  range: string
  name: string
  hasClients: boolean
}

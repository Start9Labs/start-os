import { Directive, ElementRef, inject, input, Output } from '@angular/core'
import { StartOSDiskInfo } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { filter, fromEvent, switchMap } from 'rxjs'
import { PASSWORD } from 'src/app/components/password.component'

@Directive({
  standalone: true,
  selector: 'button[server][password]',
})
export class PasswordDirective {
  private readonly dialogs = inject(TuiDialogService)

  readonly server = input.required<StartOSDiskInfo>()

  @Output()
  readonly password = fromEvent(inject(ElementRef).nativeElement, 'click').pipe(
    switchMap(() =>
      this.dialogs.open<string>(PASSWORD, {
        label: 'Unlock Drive',
        size: 's',
        data: { passwordHash: this.server().passwordHash },
      }),
    ),
    filter(Boolean),
  )
}

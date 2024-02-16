import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { InterfacesComponent } from './interfaces.component'
import { InterfaceComponent } from './interface.component'

@Component({
  standalone: true,
  selector: 'app-interface-local',
  template: `
    <em>
      Local addresses can only be accessed while connected to the same Local
      Area Network (LAN) as your server, either directly or using a VPN.
      <a
        href="https://docs.start9.com/latest/user-manual/interface-addresses#local"
        target="_blank"
        rel="noreferrer"
      >
        <strong>View instructions</strong>
      </a>
    </em>
    <a
      *ngIf="!interfaces.packageContext"
      tuiButton
      iconLeft="tuiIconDownload"
      href="/public/eos/local.crt"
      [download]="interfaces.addressInfo.lanHostname + '.crt'"
      [style.align-self]="'flex-start'"
    >
      Download Root CA
    </a>
    <app-interface
      label="Local"
      [hostname]="interfaces.addressInfo.lanHostname"
      [isUi]="interfaces.isUi"
    ></app-interface>
    <ng-container
      *ngFor="let iface of interfaces.addressInfo.ipInfo | keyvalue"
    >
      <app-interface
        *ngIf="iface.value.ipv4 as ipv4"
        [label]="iface.key + ' (IPv4)'"
        [hostname]="ipv4"
        [isUi]="interfaces.isUi"
      ></app-interface>
      <app-interface
        *ngIf="iface.value.ipv6 as ipv6"
        [label]="iface.key + ' (IPv6)'"
        [hostname]="ipv6"
        [isUi]="interfaces.isUi"
      ></app-interface>
    </ng-container>
  `,
  imports: [InterfaceComponent, CommonModule, TuiButtonModule],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceLocalComponent {
  readonly interfaces = inject(InterfacesComponent)
}

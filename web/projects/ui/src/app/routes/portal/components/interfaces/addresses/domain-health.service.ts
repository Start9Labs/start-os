import { inject, Injectable } from '@angular/core'
import { DialogService, ErrorService } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { DOMAIN_VALIDATION, DnsGateway } from './dns.component'
import { PORT_FORWARD_VALIDATION } from './port-forward.component'
import { PRIVATE_DNS_VALIDATION } from './private-dns.component'

@Injectable({ providedIn: 'root' })
export class DomainHealthService {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly dialog = inject(DialogService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)

  async checkPublicDomain(fqdn: string, gatewayId: string): Promise<void> {
    try {
      const gateway = await this.getGatewayData(gatewayId)
      if (!gateway) return

      const [dnsPass, portPass] = await Promise.all([
        this.api
          .queryDns({ fqdn })
          .then(ip => ip === gateway.ipInfo.wanIp)
          .catch(() => false),
        this.api
          .checkPort({ gateway: gatewayId, port: 443 })
          .then(r => r.reachable)
          .catch(() => false),
      ])

      if (!dnsPass || !portPass) {
        setTimeout(
          () =>
            this.openPublicDomainModal(fqdn, gateway, 443, {
              dnsPass,
              portPass,
            }),
          250,
        )
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  async checkPrivateDomain(gatewayId: string): Promise<void> {
    try {
      const gateway = await this.getGatewayData(gatewayId)
      if (!gateway) return

      const configured = await this.api
        .checkDns({ gateway: gatewayId })
        .catch(() => false)

      if (!configured) {
        setTimeout(
          () => this.openPrivateDomainModal(gateway, { configured }),
          250,
        )
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  async showPublicDomainSetup(fqdn: string, gatewayId: string): Promise<void> {
    try {
      const gateway = await this.getGatewayData(gatewayId)
      if (!gateway) return

      this.openPublicDomainModal(fqdn, gateway, 443)
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  async checkPortForward(gatewayId: string, port: number): Promise<void> {
    try {
      const gateway = await this.getGatewayData(gatewayId)
      if (!gateway) return

      const portPass = await this.api
        .checkPort({ gateway: gatewayId, port })
        .then(r => r.reachable)
        .catch(() => false)

      if (!portPass) {
        setTimeout(
          () => this.openPortForwardModal(gateway, port, { portPass }),
          250,
        )
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  async showPortForwardSetup(gatewayId: string, port: number): Promise<void> {
    try {
      const gateway = await this.getGatewayData(gatewayId)
      if (!gateway) return

      this.openPortForwardModal(gateway, port)
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  async showPrivateDomainSetup(gatewayId: string): Promise<void> {
    try {
      const gateway = await this.getGatewayData(gatewayId)
      if (!gateway) return

      this.openPrivateDomainModal(gateway)
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  private async getGatewayData(gatewayId: string): Promise<DnsGateway | null> {
    const network = await firstValueFrom(
      this.patch.watch$('serverInfo', 'network'),
    )
    const gateway = network.gateways[gatewayId]
    if (!gateway?.ipInfo) return null
    return { id: gatewayId, ...gateway, ipInfo: gateway.ipInfo }
  }

  private openPublicDomainModal(
    fqdn: string,
    gateway: DnsGateway,
    port: number,
    initialResults?: { dnsPass: boolean; portPass: boolean },
  ) {
    this.dialog
      .openComponent(DOMAIN_VALIDATION, {
        label: 'Address Requirements',
        size: 'm',
        data: { fqdn, gateway, port, initialResults },
      })
      .subscribe()
  }

  private openPortForwardModal(
    gateway: DnsGateway,
    port: number,
    initialResults?: { portPass: boolean },
  ) {
    this.dialog
      .openComponent(PORT_FORWARD_VALIDATION, {
        label: 'Address Requirements',
        size: 'm',
        data: { gateway, port, initialResults },
      })
      .subscribe()
  }

  private openPrivateDomainModal(
    gateway: DnsGateway,
    initialResults?: { configured: boolean },
  ) {
    this.dialog
      .openComponent(PRIVATE_DNS_VALIDATION, {
        label: 'Address Requirements',
        size: 'm',
        data: { gateway, initialResults },
      })
      .subscribe()
  }
}

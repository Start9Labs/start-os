import { Injectable } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { Dump } from 'patch-db-client'
import { Observable } from 'rxjs'
import { TunnelData } from '../patch-db/data-model'

@Injectable({
  providedIn: 'root',
})
export abstract class ApiService {
  abstract openWebsocket$<T>(guid: string): Observable<T>
  abstract subscribe(): Promise<SubscribeRes> // db.subscribe
  // auth
  abstract login(params: T.Tunnel.SetPasswordParams): Promise<null> // auth.login
  abstract logout(): Promise<null> // auth.logout
  abstract setPassword(params: T.Tunnel.SetPasswordParams): Promise<null> // auth.set-password
  // subnets
  abstract addSubnet(
    params: T.Tunnel.SubnetParams & T.Tunnel.AddSubnetParams,
  ): Promise<null> // subnet.add
  abstract editSubnet(
    params: T.Tunnel.SubnetParams & T.Tunnel.AddSubnetParams,
  ): Promise<null> // subnet.edit
  abstract deleteSubnet(params: T.Tunnel.SubnetParams): Promise<null> // subnet.remove
  // devices
  abstract addDevice(params: T.Tunnel.AddDeviceParams): Promise<null> // device.add
  abstract editDevice(params: T.Tunnel.AddDeviceParams): Promise<null> // device.edit
  abstract deleteDevice(params: T.Tunnel.RemoveDeviceParams): Promise<null> // device.remove
  abstract showDeviceConfig(
    params: T.Tunnel.RemoveDeviceParams,
  ): Promise<string> // device.show-config
  // forwards
  abstract addForward(params: T.Tunnel.AddPortForwardParams): Promise<null> // port-forward.add
  abstract deleteForward(
    params: T.Tunnel.RemovePortForwardParams,
  ): Promise<null> // port-forward.remove
  abstract updateForwardLabel(
    params: T.Tunnel.UpdatePortForwardLabelParams,
  ): Promise<null> // port-forward.update-label
  abstract setForwardEnabled(
    params: T.Tunnel.SetPortForwardEnabledParams,
  ): Promise<null> // port-forward.set-enabled
  // system
  abstract restart(): Promise<null> // restart
  // update
  abstract checkUpdate(): Promise<T.Tunnel.TunnelUpdateResult> // update.check
  abstract applyUpdate(): Promise<T.Tunnel.TunnelUpdateResult> // update.apply
}

export type SubscribeRes = {
  dump: Dump<TunnelData>
  guid: string
}

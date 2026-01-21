export type Protocol = 'tcp' | 'udp' | 'tcp+udp'

export type PublishedPortStatus =
  | 'active'
  | 'partial' // IPv4 unavailable (e.g., CGNAT)
  | 'paused' // Device offline or identity mismatch
  | 'error' // Failed to apply rule
  | 'disabled'

export interface PublishedPort {
  id: string // Unique identifier
  enabled: boolean
  label: string
  deviceMac: string // Link to device by MAC
  ports: string // Internal port/range (e.g., "8123" or "27015-27030")
  protocol: Protocol
  ipv4: boolean
  ipv6: boolean
  ipv4PublicPort?: string // External port for IPv4 (defaults to internal)
  source: 'any' | string // 'any' or CIDR like "203.0.113.0/24"
}

export interface PublishedPortDisplay extends PublishedPort {
  status: PublishedPortStatus
  statusReason?: string
  deviceName?: string
  deviceIpv4?: string
  deviceIpv6?: string
  endpointIpv4?: string // e.g., "example.ddns.net:8123"
  endpointIpv6?: string // e.g., "[2001:db8::50]:8123"
}

export const STATUS_LABELS: Record<
  PublishedPortStatus,
  { icon: string; label: string; class: string }
> = {
  active: { icon: '🟢', label: 'Active', class: 'status-active' },
  partial: { icon: '⚠️', label: 'Partial', class: 'status-partial' },
  paused: { icon: '⏸', label: 'Paused', class: 'status-paused' },
  error: { icon: '🔴', label: 'Error', class: 'status-error' },
  disabled: { icon: '⚪', label: 'Disabled', class: 'status-disabled' },
}

export const PROTOCOL_LABELS: Record<Protocol, string> = {
  tcp: 'TCP',
  udp: 'UDP',
  'tcp+udp': 'TCP + UDP',
}

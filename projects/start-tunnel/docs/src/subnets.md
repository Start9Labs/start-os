# Subnets

A subnet is an isolated `/24` VLAN (up to 254 devices). All devices on the same subnet can communicate with each other.

StartTunnel comes with a default subnet, which is sufficient for most users. You can create additional subnets to isolate groups of devices from each other.

## Creating a Subnet

1. In StartTunnel, navigate to `Subnets` and click "Add".
1. Name the subnet and accept or customize the IP range.
1. Click "Save".

## Removing a Subnet

1. Navigate to `Subnets`, select the subnet, and click "Remove".

> [!WARNING]
> Removing a subnet disconnects all devices on it. Their WireGuard configs will no longer work.

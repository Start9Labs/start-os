# PCP Hostname Extension for SNI-Demultiplexed Port Mappings

- **Intended status:** Standards Track (extension to RFC 6887)
- **Internet-Draft:** draft-start9-pcp-hostname-00
- **Author:** drbonez (Start9)

## Abstract

This document defines a Port Control Protocol option, HOSTNAME, that
associates one or more fully qualified domain names with a MAP request. A
PCP server implementing this option demultiplexes inbound connections on a
shared external port by inspecting the Server Name Indication presented in
the TLS ClientHello and forwards each connection to the internal host that
holds the binding for the presented name. This allows multiple internal
hosts behind a single external IP address to share well-known ports
(e.g. 443) provided their hostnames differ, while each host retains the
ability to self-provision its mappings via ordinary PCP semantics.
Demultiplexing is defined for TCP-carried TLS, with optional support for
QUIC.

## 1. Introduction

### 1.1 Motivation

A NAT gateway exposes one external IP address, or at most a few. A
conventional PCP MAP grants an entire (protocol, external IP, external
port) tuple to a single internal host, so the number of hosts that can
serve HTTPS on port 443 is limited to the number of external
addresses, typically one. Operators work around this with a manually
configured reverse proxy or SNI proxy on the gateway, which:

1. requires central, out-of-band configuration rather than client
   self-provisioning, and
2. typically replaces the client source address with the proxy address,
   hiding the real peer from the backend.

Since hosts behind a NAT typically serve public Internet domains (the
names are registered in public DNS and resolve to the gateway's external
address), the hostname is a natural demultiplexing key that the internal
host already knows and can assert itself. This extension moves the SNI
routing table into PCP, so each internal host registers its own names, and
the gateway, which is already on the return path for all LAN traffic,
forwards with the original client source address preserved.

### 1.2 Requirements Language

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED", "NOT RECOMMENDED", "MAY", and
"OPTIONAL" in this document are to be interpreted as described in BCP 14
[RFC2119] [RFC8174] when, and only when, they appear in all capitals, as
shown here.

### 1.3 Terminology

This document uses the terminology of [RFC6887] (PCP client, PCP server,
internal host, external address, mapping, mapping nonce). In addition:

- **Hostname binding:** an association, held by the PCP server, from
  (protocol, external IP address, external port, hostname) to an internal
  address and internal port, created by a MAP request carrying a HOSTNAME
  option.
- **Fallback mapping:** a conventional mapping (created by a MAP request
  with no HOSTNAME option) on a port that also has hostname bindings; it
  receives connections that match no hostname binding (Section 3.2).
- **Demultiplexed port:** a (protocol, external IP address, external port)
  tuple holding at least one hostname binding. Inbound connections to a
  demultiplexed port are routed per Section 4.4 or 4.5 rather than by
  conventional destination NAT.

## 2. Overview of Operation

This section is informative; normative requirements appear in Sections 3
through 9.

1. An internal host sends a PCP MAP request for protocol TCP, suggesting
   an external port (e.g. 443), and includes one or more HOSTNAME options,
   each carrying an FQDN such as `git.example.com`.
2. The PCP server checks each requested name against its hostname bindings
   for that (external IP, external port). If no conflicting binding
   exists, it creates bindings with the requested lifetime and returns
   success, echoing the HOSTNAME options.
3. Other internal hosts may map the same external port with different
   hostnames; the port is shared because the mapping key is extended to
   (protocol, external IP, external port, hostname).
4. For each inbound TCP connection to a demultiplexed port, the server
   accepts the connection, reads the TLS ClientHello, extracts the
   server_name extension, selects the matching binding, and splices the
   connection to the bound internal host.
5. Because the PCP server is the default gateway for the internal network,
   it originates the internal leg with the original client's source
   address and port (transparent mode, Section 4.6); return packets
   necessarily traverse the gateway, which steers them back into the
   spliced connection.

A host requests forwards for multiple hostnames either by including
multiple HOSTNAME options in a single MAP request or by issuing multiple
MAP requests; see Section 5.

## 3. The HOSTNAME Option

### 3.1 Format

The option follows the PCP option format ([RFC6887], Section 7.3):

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|  Option Code  |  Reserved     |       Option Length           |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                               |
:                hostname (variable length)                     :
|                                                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

              Figure 1: HOSTNAME Option Format
```

- **Option Code:** TBD1, in the optional-to-process range.
- **Option Length:** length of the hostname field in octets (1 to 255).
  The option data is zero-padded to a 32-bit boundary per [RFC6887];
  padding is not included in Option Length.
- **hostname:** the name to bind, encoded exactly as it appears on the
  wire in the SNI HostName field ([RFC6066], Section 3): ASCII,
  internationalized names as A-labels [RFC5890], no trailing dot, no NUL
  octets, total length 1 to 255 octets. Literal IPv4 and IPv6 addresses
  MUST NOT be used ([RFC6066], Section 3). Comparison is case-insensitive; servers
  SHOULD normalize names to lowercase on receipt. A syntactically invalid
  hostname MUST be rejected with MALFORMED_OPTION.

Because the option code is in the optional-to-process range, a server
that does not implement this option ignores it and processes the request
as a conventional MAP, granting an unrestricted port forward. This
degrades gracefully: if the requesting host is the only one using the
port, observable behavior is identical, and a second host receives
CANNOT_PROVIDE_EXTERNAL (or an alternate port) per standard
MAP processing. Clients detect whether hostname scoping was applied by
checking for the echoed HOSTNAME option(s) in the response; see Section 5.

### 3.2 Validity and the Fallback Mapping

The HOSTNAME option is valid only for the MAP opcode. A server
implementing this option MUST reject any other opcode carrying it with
MALFORMED_OPTION.

The option is valid for protocol TCP, and for protocol UDP only on
servers implementing QUIC demultiplexing (Section 4.5). A server
implementing this option MUST reject a HOSTNAME option for any other
protocol, or for UDP when QUIC demultiplexing is not implemented, with
UNSUPP_HOSTNAME (Section 8). Clients MUST NOT request hostname bindings
for protocols whose servers send application data before the client
(e.g. SMTP, IMAP); a server cannot detect this misuse, and such
connections will stall waiting for a ClientHello and then be handled
as in Section 4.4 step 4.

A HOSTNAME option MAY appear multiple times in a single MAP request, each
occurrence creating one binding, subject to the atomic failure semantics
of Section 4.2.

There is no in-option fallback syntax. A conventional MAP request (no
HOSTNAME option) for the same (protocol, external IP, external port)
creates the fallback mapping: connections whose ClientHello carries no
server_name extension, presents a name with no matching binding, or whose
initial bytes do not parse as TLS (or as QUIC per Section 4.5), are
forwarded to its holder. At most one fallback mapping exists per
(protocol, external IP, external port), enforced by normal MAP
exclusivity.

### 3.3 Wildcards

A hostname whose first label is `*` (e.g. `*.example.com`) requests a
wildcard binding matching exactly one left-most label, with the same
semantics as certificate wildcard matching. Wildcard support is OPTIONAL;
servers not implementing it respond with UNSUPP_HOSTNAME (Section 8). An
exact binding takes precedence over a wildcard binding.

## 4. Server Behavior

### 4.1 Mapping Table

The server maintains, per (protocol, external IP, external port), a set of
hostname bindings, each comprising: hostname, internal address, internal
port, lifetime, and the mapping nonce of the creating client. All standard
MAP processing of [RFC6887] applies (lifetime decay, renewal by matching
nonce, ANNOUNCE/epoch recovery).

### 4.2 Conflicts

If a requested hostname is already bound on that (protocol, external IP,
external port) under a different mapping nonce, the server MUST NOT create
or alter the binding and MUST return result code HOSTNAME_TAKEN
(Section 8).

If any HOSTNAME option in a request fails validation or conflicts, the
entire request fails and no bindings from that request are created or
modified; PCP responses are atomic per request.

A conventional MAP for a (protocol, external IP, external port) that has
hostname bindings is permitted and serves as the fallback mapping
(Section 3.2). It is subject to ordinary MAP exclusivity. Hostname
bindings and the fallback mapping have independent lifetimes; expiry of
one does not affect the others.

### 4.3 Deletion

A MAP request with requested lifetime 0 carrying HOSTNAME options deletes
only the named bindings, subject to the nonce-matching rules of
[RFC6887], Section 15. A lifetime-0 request without HOSTNAME options
deletes only the requester's conventional (fallback) mapping, if any.
Other bindings on the port are unaffected in both cases.

### 4.4 TCP Demultiplexing

For TCP connections arriving at a demultiplexed port, the server:

1. Completes the TCP handshake with the external client.
2. Reads until it has the full ClientHello or a parse failure, subject to
   a buffer cap (16384 octets, the maximum TLS plaintext record fragment,
   is RECOMMENDED) and a timeout (5 seconds is RECOMMENDED). ClientHellos
   can span multiple TCP segments and, rarely, multiple TLS records;
   servers SHOULD handle multi-record ClientHellos.
3. Extracts server_name, selects the binding (exact match, then wildcard,
   then the fallback mapping if present), and connects to the bound
   internal host and port, replaying the buffered bytes before splicing.
4. If no binding matches and no fallback mapping exists, the server
   SHOULD close the connection. Whether to reset or send a TLS alert is
   implementation defined; the server MUST NOT forward the connection to
   an arbitrary binding.

Demultiplexing engages only on demultiplexed ports. A port holding only a
conventional mapping is forwarded as plain destination NAT per [RFC6887],
with no ClientHello inspection. When the first hostname binding is added
to a port with an existing conventional mapping, the port transitions to
demultiplexed mode; established connections are unaffected, and the
conventional mapping's holder begins receiving only fallback traffic for
new connections. The reverse transition occurs when the last hostname
binding expires. The server does not terminate TLS; it requires no keys
and forwards the ClientHello bytes verbatim.

### 4.5 QUIC Demultiplexing (OPTIONAL)

A server MAY additionally implement demultiplexing of QUIC [RFC9000] for
hostname bindings with protocol UDP. Servers implementing this section
accept HOSTNAME options with protocol UDP; servers that do not MUST
reject them per Section 3.2, so a client can discover support by the
result code.

For UDP datagrams arriving at a demultiplexed port:

1. **Initial packets.** A datagram beginning with a QUIC long header
   ([RFC8999]) of a version the server supports is processed as a
   candidate Initial. The server derives the Initial secrets from the
   client's Destination Connection ID and the version-specific salt
   ([RFC9001], Section 5.2), decrypts the packet, and reassembles the
   CRYPTO stream to obtain the TLS ClientHello. The ClientHello can span
   multiple Initial packets; the server MUST buffer and reassemble across
   datagrams, subject to a cap (10 datagrams or 65535 octets of CRYPTO
   data is RECOMMENDED) and a timeout (5 seconds is RECOMMENDED).
2. **Binding selection.** The server extracts server_name and selects a
   binding exactly as in Section 4.4 step 3. It then creates flow state
   keyed on the connection's 4-tuple, forwards the buffered datagrams
   verbatim to the bound internal host, and forwards subsequent datagrams
   matching the flow in both directions. No decryption is performed after
   binding selection, and the server never holds handshake or 1-RTT keys.
3. **Flow lifetime.** Flow state is removed after an idle period
   consistent with the gateway's UDP mapping timeout. Version
   Negotiation and Retry exchanges initiated by the internal host
   traverse the established flow and require no special handling.
4. **Non-matching datagrams.** Datagrams that are not parseable Initials
   of a supported version (including short-header packets matching no
   flow, long headers of unknown versions, and non-QUIC UDP payloads) are
   forwarded to the fallback mapping if one exists and otherwise dropped.

Two consequences follow from routing on flow state. First, active
connection migration and NAT rebinding both move the connection to a
4-tuple the server has no state for; such packets carry connection IDs
opaque to the server and arrive as non-matching datagrams. Internal hosts
serving QUIC behind a demultiplexing gateway SHOULD send the
disable_active_migration transport parameter ([RFC9000], Section 18.2) to
suppress client-initiated migration; this does not prevent NAT rebinding
([RFC9000], Section 9.3), which can still strand a connection on an
unknown 4-tuple unless coordinated connection-ID routing is in use.
Coordinated connection-ID routing (e.g. QUIC-LB
[I-D.ietf-quic-load-balancers]) could lift this restriction and is out of
scope. Second, a client whose first flight uses a QUIC version unknown to
the server cannot be demultiplexed; servers SHOULD support all current
standard versions and treat unknown-version long headers as non-matching
rather than attempting Version Negotiation themselves.

### 4.6 Source Address Preservation

Servers SHOULD originate the internal leg of demultiplexed TCP
connections using the external client's source address and port (e.g. via
IP_TRANSPARENT on Linux), relying on their position as the internal
network's gateway to capture return traffic. When operating in this mode
the forwarded connection is, from the internal host's perspective,
indistinguishable from a direct destination-NAT forward. A server that
cannot operate transparently uses its own address on the internal leg;
this behavior is visible to internal hosts and operators should document
it. QUIC demultiplexing (Section 4.5) forwards datagrams at the IP layer
and preserves the source address inherently.

## 5. Client Behavior

- Clients include one HOSTNAME option per name. Given the atomic failure
  semantics of Section 4.2, clients SHOULD use one MAP request per name
  when independent success matters, and MAY batch multiple options in one
  request when the names succeed or fail together (e.g. a certificate SAN
  set).
- Clients MUST examine the response for echoed HOSTNAME options. Absence
  means the server did not process the option and the granted mapping is
  unrestricted; clients that require hostname scoping MAY delete such a
  mapping. Against a server that ignores the option, both request
  patterns degrade safely: a batched request yields one
  unrestricted mapping covering all names, and per-name requests for the
  same internal port resolve to that same mapping ([RFC6887] keys
  mappings on the internal tuple), acting as refreshes rather than
  conflicts.
- On gateways with multiple external addresses, hostname bindings are
  scoped per external address: the same name may be bound on two external
  IPs to different internal hosts. Clients SHOULD set the Suggested
  External IP Address field ([RFC6887], Section 11.1) to the address
  their public DNS resolves to and SHOULD include PREFER_FAILURE so the
  server fails rather than assigning a different address. The Assigned
  External IP Address in the response is authoritative; clients
  publishing DNS records from PCP state SHOULD use it rather than the
  suggested address.
- Clients renew exactly as in [RFC6887], re-sending the same MAP request
  (same nonce, same options) before lifetime expiry.
- Clients MUST be prepared to receive HOSTNAME_TAKEN at any renewal
  (e.g. after server state loss and a race with another host) and SHOULD
  surface the condition rather than silently retrying.

## 6. Interaction with Other PCP Options

- **FILTER** ([RFC6887]): composes normally; filters restrict which
  remote peers may connect and are evaluated before demultiplexing.
- **PREFER_FAILURE** ([RFC6887]): composes normally; see Section 5 for
  its use on multihomed gateways.
- **THIRD_PARTY** ([RFC6887]): a request combining THIRD_PARTY and
  HOSTNAME options creates hostname bindings on behalf of the third
  party, subject to the server's THIRD_PARTY authorization policy; the
  conflict rules of Section 4.2 apply to the third party's bindings
  identically.
- **PORT_SET** ([RFC7753]): a request combining PORT_SET and HOSTNAME
  options is not meaningful (a hostname binding targets a single internal
  port) and MUST be rejected with MALFORMED_OPTION.

## 7. Interaction with Encrypted ClientHello

ECH [I-D.ietf-tls-esni] encrypts the true server name; the outer
ClientHello carries the public name of the client-facing server. A
demultiplexing server sees only the outer SNI, for both TLS over TCP and
QUIC. Deployments using this extension with ECH MUST bind the public name
(the name published in the HTTPS/SVCB ech parameter), and the internal
host performs inner-name routing itself if needed. Names hidden by ECH
cannot serve as demultiplexing keys by design; this is a property of ECH,
not a defect of this extension.

## 8. New Result Codes

- **HOSTNAME_TAKEN (TBD2):** the requested hostname is already bound on
  this external IP address and port by another client. This is a short
  lifetime error; the condition clears when the conflicting binding
  expires or is deleted.
- **UNSUPP_HOSTNAME (TBD3):** the hostname is syntactically valid but the
  request uses a feature this server does not support (e.g. wildcard
  bindings, or protocol UDP without QUIC demultiplexing support). This is
  a long lifetime error.

Syntactically invalid hostnames are rejected with the existing
MALFORMED_OPTION ([RFC6887]).

## 9. Security Considerations

The security model is that of [RFC6887]: any host on the internal network
may create mappings, so any host may claim any hostname, first come first
served. This is equivalent in power to the existing ability of any
internal host to claim an entire external port; the granularity is finer
but the trust assumption is unchanged. Deployments requiring stronger
guarantees SHOULD deploy PCP authentication [RFC7652] or restrict the
option via policy (e.g. per-host hostname allowlists).

A server MAY verify, at binding time, that the requested name resolves in
public DNS to one of its external addresses, rejecting names that do not.
DNS can change after binding and split-horizon deployments may
legitimately fail such a check, so it MUST be possible to disable; it is
a policy mechanism, not a protocol requirement.

ClientHello collection (Sections 4.4 and 4.5) creates state per pending
connection. Servers MUST cap buffered bytes and the number of pending
connections to bound memory under handshake-and-stall attacks, and SHOULD
subject pending connections to the same limits as their existing
connection tracking.

The hostname presented in SNI is attacker controlled and unauthenticated.
The server uses it only to select among pre-registered bindings; it MUST
NOT be used to construct file paths or commands, and MUST be sanitized
before inclusion in log output.

For QUIC demultiplexing, an attacker can cheaply elicit Initial-secret
derivation and decryption work with spoofed datagrams. The per-version
key derivation uses no secret state and the cost is bounded per datagram,
but servers SHOULD rate-limit Initial processing per source address.

## 10. Applicability and Limitations

Demultiplexing requires a client-first protocol carrying SNI: TLS over
TCP (Section 4.4) and QUIC (Section 4.5). Non-TLS protocols with a
cleartext hostname early in the stream (e.g. HTTP/1.1 Host) are out of
scope; the fallback mapping covers them coarsely. Server-first protocols
cannot be demultiplexed at all (Section 3.2).

## 11. IANA Considerations

### 11.1 PCP Option Code

IANA is requested to assign the following option code in the "PCP
Options" registry of the "Port Control Protocol (PCP) Parameters" group,
from the Specification Required range (192-223); values in this range
are optional-to-process:

- **Value:** TBD1
- **Name:** HOSTNAME
- **Purpose:** Associates a fully qualified domain name with a MAP
  request; inbound connections on the mapped external port are forwarded
  to the internal host whose binding matches the SNI presented in the TLS
  or QUIC ClientHello.
- **Valid for Opcodes:** MAP
- **Length:** variable; 1 to 255 octets
- **May Appear in:** Request. May appear in response only if it appeared
  in the associated request.
- **Maximum Occurrences:** As many as fit within maximum PCP message
  size.
- **Reference:** this document

### 11.2 PCP Result Codes

IANA is requested to assign two result codes in the "PCP Result Codes"
registry of the same group, from the Specification Required range
(128-191):

- **Value:** TBD2; **Name:** HOSTNAME_TAKEN; **Description:** The
  requested hostname is already bound on the assigned external address
  and port by another client. This is a short lifetime error.
  **Reference:** this document
- **Value:** TBD3; **Name:** UNSUPP_HOSTNAME; **Description:** The
  HOSTNAME option requests a feature not supported by this server. This
  is a long lifetime error. **Reference:** this document

## 12. References

### 12.1 Normative References

- [RFC2119] Bradner, S., "Key words for use in RFCs to Indicate
  Requirement Levels", BCP 14, RFC 2119, March 1997.
- [RFC8174] Leiba, B., "Ambiguity of Uppercase vs Lowercase in RFC 2119
  Key Words", BCP 14, RFC 8174, May 2017.
- [RFC6887] Wing, D., Ed., Cheshire, S., Boucadair, M., Penno, R., and
  P. Selkirk, "Port Control Protocol (PCP)", RFC 6887, April 2013.
- [RFC6066] Eastlake 3rd, D., "Transport Layer Security (TLS) Extensions:
  Extension Definitions", RFC 6066, January 2011.
- [RFC5890] Klensin, J., "Internationalized Domain Names for Applications
  (IDNA): Definitions and Document Framework", RFC 5890, August 2010.
- [RFC8999] Thomson, M., "Version-Independent Properties of QUIC",
  RFC 8999, May 2021.
- [RFC9000] Iyengar, J., Ed. and M. Thomson, Ed., "QUIC: A UDP-Based
  Multiplexed and Secure Transport", RFC 9000, May 2021.
- [RFC9001] Thomson, M., Ed. and S. Turner, Ed., "Using TLS to Secure
  QUIC", RFC 9001, May 2021.
- [I-D.ietf-tls-esni] Rescorla, E., Oku, K., Sullivan, N., and C. Wood,
  "TLS Encrypted Client Hello", Work in Progress, draft-ietf-tls-esni.
- [RFC7753] Sun, Q., Boucadair, M., Sivakumar, S., Zhou, C., Tsou, T.,
  and S. Perreault, "Port Control Protocol (PCP) Extension for Port-Set
  Allocation", RFC 7753, February 2016.

### 12.2 Informative References

- [RFC7652] Cullen, M., Hartman, S., Zhang, D., and T. Reddy, "Port
  Control Protocol (PCP) Authentication Mechanism", RFC 7652,
  September 2015.
- [I-D.ietf-quic-load-balancers] Duke, M., Banks, N., and C. Huitema,
  "QUIC-LB: Generating Routable QUIC Connection IDs", Work in Progress,
  draft-ietf-quic-load-balancers.

## Authors' Addresses

drbonez
Start9
Email: TBD

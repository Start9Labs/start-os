# Makefile Build System

StartOS packages use a two-file Makefile system that separates reusable build logic from project-specific configuration.

## File Structure

```
my-service-startos/
├── Makefile     # Project-specific configuration (minimal)
└── s9pk.mk      # Shared build logic (copy from template)
```

## s9pk.mk

The `s9pk.mk` file contains all the common build logic shared across StartOS packages. Copy this file from `hello-world-startos/s9pk.mk` without modification.

### Targets

| Target               | Description                                          |
| -------------------- | ---------------------------------------------------- |
| `make` or `make all` | Build for all architectures (default)                |
| `make x86`           | Build for x86_64 only                                |
| `make arm`           | Build for aarch64 only                               |
| `make riscv`         | Build for riscv64 only                               |
| `make universal`     | Build a single package containing all architectures  |
| `make install`       | Install the most recent .s9pk to your StartOS server |
| `make clean`         | Remove build artifacts                               |

### Variables

| Variable  | Default         | Description                              |
| --------- | --------------- | ---------------------------------------- |
| `ARCHES`  | `x86 arm riscv` | Architectures to build by default        |
| `TARGETS` | `arches`        | Default build target                     |
| `VARIANT` | (unset)         | Optional variant suffix for package name |

## Makefile

The project `Makefile` is minimal and just includes `s9pk.mk`:

```makefile
include s9pk.mk
```

### Adding Custom Targets

For services with variants (e.g., GPU support), extend the Makefile:

```makefile
TARGETS := generic rocm
ARCHES := x86 arm

include s9pk.mk

.PHONY: generic rocm

generic:
	$(MAKE) all_arches VARIANT=generic

rocm:
	ROCM=1 $(MAKE) all_arches VARIANT=rocm ARCHES=x86_64
```

This produces packages named `myservice_generic_x86_64.s9pk` and `myservice_rocm_x86_64.s9pk`.

> [!WARNING]
> Each variant must declare a **distinct** hardware requirement in the manifest (with at most one empty fallback), or publishing the second variant fails with a registry metadata mismatch. See [GPU/Hardware Acceleration](./manifest.md#hardware-requirements-and-variants).

### Overriding Defaults

Override variables _before_ `include s9pk.mk`:

```makefile
# Build only for x86 and arm
ARCHES := x86 arm

include s9pk.mk
```

## Build Commands

```bash
# Build for all architectures
make

# Build for a specific architecture
make x86
make arm

# Install to StartOS server (requires ~/.startos/config.yaml)
make install

# Clean build artifacts
make clean
```

### Chaining Commands

You can chain multiple targets in a single invocation:

```bash
make clean arm          # Clean, then build ARM package
make clean x86 install  # Clean, build x86 package, then install
make clean install      # Clean, build universal, then install
```

## Prerequisites

The build system checks for:

- `start-cli` -- StartOS CLI tool
- `npm` -- Node.js package manager
- `~/.startos/developer.key.pem` -- Developer key (auto-initialized if missing)

See [Environment Setup](./environment-setup.md) for installation instructions.

## Installation

To install a package directly to your StartOS server, configure the server address in `~/.startos/config.yaml`:

```yaml
host: http://your-server.local
```

Then run:

```bash
make install
```

This builds the package and sideloads it to your device.

### Example Output

**Building an ARM package:**

```
$ make arm
   Re-evaluating ingredients...
   Packing 'albyhub_aarch64.s9pk'...
Build Complete!

  Alby Hub   v1.19.3:1
  Filename:   albyhub_aarch64.s9pk
  Size:       7M
  Arch:       aarch64
  SDK:        0.4.0-beta.36
  Git:        78c30ec776f6a9d55be3701e9b82093c866a382c
```

> [!NOTE]
> If you have uncommitted changes, the Git hash will be shown in red.

**Installing a package:**

```
$ make arm install

Installing to working-finalist.local ...
Sideloading 100%
  Uploading...
  Validating Headers...
  Unpacking...
```

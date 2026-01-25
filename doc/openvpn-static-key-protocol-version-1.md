# OpenVPN Static Key Protocol (Version 1)

*Informational – protocol description derived from interoperable implementations*

---

## 1. Overview

OpenVPN Static Key mode (also known as **v1**) is a symmetric-key VPN protocol that predates the TLS-based OpenVPN v2 protocol. Unlike v2, it does **not** use TLS, certificates, or control channels. Instead, it relies on:

* A pre-shared static key file
* Symmetric encryption (e.g. AES)
* HMAC-based authentication
* Packet IDs for replay protection
* Encapsulation of arbitrary payloads

This document describes the **wire protocol**, independent of any particular implementation.

---

## 2. Transport

Static Key mode operates over:

* **UDP (datagram)** — most common
* **TCP (stream)** — packets are length-prefixed

This document focuses on the **packet format**; framing differences are described in §11.

---

## 3. Cryptographic Primitives (Examples)

> **Important:** The algorithms and sizes below are *examples*, not protocol requirements.

| Purpose            | Example Algorithm |
| ------------------ | ----------------- |
| Encryption         | AES-256-CBC       |
| Cipher block size  | 16 bytes          |
| Authentication     | HMAC-SHA256       |
| HMAC output length | 32 bytes          |
| Padding            | PKCS#7            |

---

## 4. Static Key File Format

The static key file contains **256 bytes** of key material encoded as hexadecimal.

```
-----BEGIN OpenVPN Static key V1-----
<16 bytes hex>
<16 bytes hex>
...
<16 bytes hex>
-----END OpenVPN Static key V1-----
```

* 16 lines × 16 bytes = **256 bytes total**
* Parsed as a contiguous byte array `K[0..255]`

### 4.1 Unused Key Material

Not all bytes of the static key are necessarily used by a given cipher/HMAC combination.

This is **intentional and normal**.

The static key size was designed to:

* Support **multiple independent keys**
* Allow for **larger cipher keys or digest sizes**
* Preserve backward compatibility as algorithms evolve

As a result, some portions of the static key may remain unused depending on:

* Cipher key size
* HMAC digest length
* Key direction mode

---

## 5. Key Direction and Offsets

Static Key mode supports three keying modes:

* **Bidirectional** (default)
* **Normal** (direction = 0)
* **Inverse** (direction = 1)

The static key is partitioned into **four independent keys**:

* Cipher Encrypt Key
* Cipher Decrypt Key
* HMAC Encrypt Key
* HMAC Decrypt Key

### 5.1 Offset Table (Byte Offsets into Static Key)

#### Bidirectional

| Purpose            | Offset |
| ------------------ | ------ |
| Cipher Encrypt Key | 0      |
| Cipher Decrypt Key | 0      |
| HMAC Encrypt Key   | 64     |
| HMAC Decrypt Key   | 64     |

#### Direction = 0 (“Normal”)

| Purpose            | Offset |
| ------------------ | ------ |
| Cipher Encrypt Key | 0      |
| Cipher Decrypt Key | 128    |
| HMAC Encrypt Key   | 64     |
| HMAC Decrypt Key   | 192    |

#### Direction = 1 (“Inverse”)

| Purpose            | Offset |
| ------------------ | ------ |
| Cipher Encrypt Key | 128    |
| Cipher Decrypt Key | 0      |
| HMAC Encrypt Key   | 192    |
| HMAC Decrypt Key   | 64     |

### 5.2 Key Lengths

* Cipher key length = cipher key size (e.g. 32 bytes for AES-256)
* HMAC key length = digest length (e.g. 32 bytes for SHA-256)

---

## 6. Packet Structure (Outer)

Each OpenVPN v1 packet has the following structure **on the wire**:

```
+----------------------+------------------+----------------------+
| HMAC (32 bytes)     | IV (16 bytes)    | Ciphertext (var)     |
+----------------------+------------------+----------------------+
```

### Notes

* HMAC covers **IV + Ciphertext**
* IV is transmitted in plaintext
* Ciphertext is padded (PKCS#7)

---

## 7. HMAC Computation

### 7.1 Sender

```
HMAC = HMAC(key, IV || Ciphertext)
```

### 7.2 Receiver

1. Read HMAC, IV, Ciphertext
2. Recompute HMAC over `IV || Ciphertext`
3. Constant-time compare
4. Reject packet if verification fails

---

## 8. Ciphertext Payload (Inner Structure)

After decryption, the plaintext has the following structure:

```
+------------------+----------------+----------------------+
| Packet ID (8)   | Compress (1)   | Payload (var)        |
+------------------+----------------+----------------------+
```

---

## 9. Inner Fields

### 9.1 Packet ID

```
+----------------------+----------------------+
| Packet ID (uint32)  | Timestamp (uint32)   |
+----------------------+----------------------+
```

* Big-endian
* Packet ID is monotonically increasing
* Timestamp is seconds since UNIX epoch
* Used for replay protection (implementation-dependent)

---

Yep — that’s a much better, *truer-to-OpenVPN* justification.
Below is **only the revised compression subsection**, rewritten to reflect OpenVPN’s own rationale and tone, while keeping the rest of the document unchanged.

You can drop this in verbatim to replace **Section 9.2 (Compression Byte → OpenVPN Recommendation)**.

---

### 9.2 Compression Byte

| Value | Meaning         |
| ----: | --------------- |
|  0xFA | No compression  |
|  0x66 | LZO compression |
|  0x69 | LZ4 compression |

#### OpenVPN Recommendation

OpenVPN **strongly recommends disabling compression** in almost all cases.

The rationale is as follows:

* **The vast majority of data sent across the Internet is already compressed** *before* it enters a VPN tunnel (e.g. HTTPS, TLS, video, audio, archives).
* The **VORACLE attack** demonstrated that mixing compression and encryption without extreme care can introduce serious security vulnerabilities.
* OpenVPN is a **single-threaded process** that already spends significant CPU time encrypting and decrypting traffic. Adding compression and decompression to the same process increases CPU load for little or no benefit.
* For already-compressed or entropy-dense data, compression provides **no size reduction** and may even increase packet size, while still consuming CPU resources.

The general consensus is that **compression should not be used**, except under *unusual circumstances*.

This effectively translates to:

> *“You do not need compression, unless you know exactly why you need it.”*

##### Unusual Circumstances

Compression may be beneficial **only if all of the following are true**:

* You control **both the client and the server**
* You know that the traffic consists largely of **uncompressed data**
* The data benefits significantly from compression (e.g. raw or lightly encoded streams)

An example might be a VPN transporting uncompressed or poorly compressed video from a low-end security camera.

Outside of such scenarios, enabling compression generally wastes CPU time and increases complexity without providing meaningful benefit.

As a result, modern OpenVPN configurations typically use:

```
Compression byte = 0xFA
```

---

## 10. Ping Packet (Keepalive)

Ping packets are used to maintain NAT bindings and detect connectivity.

```
Magic value (16 bytes):
2a 18 7b f3 64 1e b4 cb 07 ed 2d 0a 98 1f c7 48
```

```
+------------------------------------------------+
| 16-byte fixed magic constant                  |
+------------------------------------------------+
```

#### Ping Interval

Implementations typically send ping packets at a **regular interval**, for example:

* **Every 10 seconds (default)**

The interval is not fixed by the protocol.

---

## 11. Example Payload: IPv4 (Non-Authoritative)

IPv4 packets are a **common** payload carried by OpenVPN Static Key mode, but the protocol itself does not mandate any specific payload format.

As an example, an IPv4 packet typically begins with:

```
0x45  => Version 4, header length 5
```

```
+----------------------+
| IPv4 Header + Data  |
+----------------------+
```

The IPv4 packet is transmitted verbatim after the compression byte.

> This section is illustrative only. Static Key mode may carry other payload types depending on the deployment.

---

## 12. Supported Algorithms (OpenVPN)

The following algorithms are supported by OpenVPN and may be used with Static Key mode, subject to configuration and build options.

### 12.1 Ciphers

* AES-128-CBC
* AES-192-CBC
* AES-256-CBC
* ARIA-128-CBC
* ARIA-192-CBC
* ARIA-256-CBC
* CAMELLIA-128-CBC
* CAMELLIA-192-CBC
* CAMELLIA-256-CBC

All supported ciphers use CBC (Cipher Block Chaining) mode.

### 12.2 Message Digests / HMAC Algorithms

* MD4
* MD5
* SHA1
* SHA224
* SHA256
* SHA384
* SHA512
* RIPEMD160
* Whirlpool
* BLAKE2s-256
* SHA3-224
* SHA3-256
* SHA3-384
* SHA3-512
* SHAKE128
* SHAKE256
* SM3

---

## 13. Transport Framing

### 13.1 UDP

Each OpenVPN packet is sent as a **single datagram**.

```
[ OpenVPN Packet ]
```

---

### 13.2 TCP

Each packet is prefixed with a 2-byte length field:

```
+------------------+----------------------+
| Length (uint16) | OpenVPN Packet       |
+------------------+----------------------+
```

* Big-endian
* Length does **not** include the length field itself

---

## 14. Encryption Process (Summary)

### Sender

1. Construct plaintext:

   * Packet ID
   * Compression byte
   * Payload
2. Generate random IV
3. Encrypt plaintext with cipher + IV
4. Compute HMAC over `IV || Ciphertext`
5. Send `HMAC || IV || Ciphertext`

### Receiver

1. Split packet into HMAC, IV, Ciphertext
2. Verify HMAC
3. Decrypt Ciphertext
4. Parse Packet ID
5. Read compression byte
6. Process payload

---

## 15. Status

This document serves as a **wire-level reference** for OpenVPN Static Key (v1), a legacy but still interoperable OpenVPN protocol mode.

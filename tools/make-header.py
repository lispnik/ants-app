#!/usr/bin/env python3
"""make-header.py -- write docs/header.png, a 2400x800 banner for the web
page and the App Store: two of the game's paintings on their plates, a few
cubes gone, a large ant crossing between them, and a line of small ants
marching off with the missing cubes toward the title.

The pictures are read from src/model.lisp.  The ants are drawn as in
make-icon.py, from signed distance functions; the title is set in Avenir
Next by tools/header-title.swift.

    python3 tools/make-header.py
"""
import math, os, re, struct, subprocess, tempfile, zlib

W, H = 2400, 800
here = os.path.dirname(os.path.abspath(__file__))
root = os.path.normpath(os.path.join(here, ".."))

PALETTE = {c: tuple(round(v * 255) for v in rgb) for c, rgb in {
    "R": (0.91, 0.24, 0.22), "O": (0.98, 0.55, 0.16), "Y": (0.99, 0.84, 0.22),
    "G": (0.33, 0.72, 0.30), "C": (0.45, 0.82, 0.93), "B": (0.20, 0.45, 0.90),
    "P": (0.96, 0.50, 0.74), "T": (0.95, 0.84, 0.66), "N": (0.52, 0.33, 0.19),
    "W": (0.97, 0.97, 0.95), "K": (0.20, 0.18, 0.22)}.items()}
GROUND = (66, 87, 56)
PLATE = (204, 199, 189)
PLATE_EDGE = (140, 135, 128)

def pictures():
    source = open(os.path.join(root, "src", "model.lisp")).read()
    found = {}
    for m in re.finditer(r':name "([^"]+)".*?:rows \'\(([^)]*)\)', source, re.S):
        found[m.group(1)] = re.findall(r'"([^"]*)"', m.group(2))
    return found

def shade(colour, k):
    return tuple(max(0, min(255, c * k)) for c in colour)

def mix(a, b, t):
    return tuple(x + (y - x) * t for x, y in zip(a, b))

# ------------------------------------------------------------------
# the ground, darker toward the edges

pixels = []
for y in range(H):
    row = []
    for x in range(W):
        d = math.hypot((x - W * 0.55) / W, (y - H * 0.5) / H)
        row.append(shade(GROUND, 1.08 - 0.45 * d * d))
    pixels.append(row)

def fill(x0, y0, x1, y1, colour):
    for y in range(max(0, round(y0)), min(H, round(y1))):
        row = pixels[y]
        for x in range(max(0, round(x0)), min(W, round(x1))):
            row[x] = colour

def darken(x0, y0, x1, y1, k):
    for y in range(max(0, round(y0)), min(H, round(y1))):
        row = pixels[y]
        for x in range(max(0, round(x0)), min(W, round(x1))):
            row[x] = shade(row[x], k)

def plate(rows, left, top, cell, missing=()):
    """A picture on its plate, as in the game; MISSING lists (row, column)s taken."""
    w, h = len(rows[0]), len(rows)
    pad = cell * 0.45
    darken(left - pad + 14, top - pad + 18, left + w * cell + pad + 14, top + h * cell + pad + 18, 0.72)
    fill(left - pad - 3, top - pad - 3, left + w * cell + pad + 3, top + h * cell + pad + 3, PLATE_EDGE)
    fill(left - pad, top - pad, left + w * cell + pad, top + h * cell + pad, PLATE)
    for r, line in enumerate(rows):
        for c, char in enumerate(line):
            if char == "." or (r, c) in missing:
                continue
            x, y, s = left + c * cell + 1.5, top + r * cell + 1.5, cell - 3
            colour = PALETTE[char]
            fill(x, y, x + s, y + s, colour)
            fill(x, y, x + s, y + s * 0.22, shade(colour, 1.15) if char != "W" else (255, 255, 255))
            fill(x, y + s * 0.84, x + s, y + s, shade(colour, 0.78))

def cube_at(cx, cy, s, colour):
    fill(cx - s / 2, cy - s / 2, cx + s / 2, cy + s / 2, colour)
    fill(cx - s / 2, cy - s / 2, cx + s / 2, cy - s / 2 + s * 0.22, shade(colour, 1.15))
    fill(cx - s / 2, cy + s / 2 - s * 0.16, cx + s / 2, cy + s / 2, shade(colour, 0.78))

# ------------------------------------------------------------------
# ants: in the ant's frame u runs to the head, v across; sizes are for an
# ant about 950 long, scaled

BODY = [(-235, 0, 205, 150), (-35, 0, 38, 30), (55, 0, 105, 72), (215, 0, 98, 92)]
LIMBS = []
for side in (-1, 1):
    for root_u, knee, foot in ((95, (175, 205), (300, 330)),
                               (55, (40, 240), (-10, 380)),
                               (15, (-110, 205), (-250, 330))):
        LIMBS.append(((root_u, side * 40), (knee[0], side * knee[1]), 17))
        LIMBS.append(((knee[0], side * knee[1]), (foot[0], side * foot[1]), 13))
    LIMBS.append(((280, side * 45), (360, side * 135), 8))
    LIMBS.append(((360, side * 135), (475, side * 120), 7))
    LIMBS.append(((285, side * 45), (330, side * 40), 13))
CARRIED = (372, 0, 58, 12)

def ellipse(u, v, cu, cv, ru, rv):
    nu, nv = (u - cu) / ru, (v - cv) / rv
    return (math.hypot(nu, nv) - 1) * min(ru, rv), (nu, nv)

def capsule(u, v, a, b, r):
    pu, pv = u - a[0], v - a[1]
    du, dv = b[0] - a[0], b[1] - a[1]
    t = max(0, min(1, (pu * du + pv * dv) / (du * du + dv * dv)))
    return math.hypot(pu - du * t, pv - dv * t) - r

def box(u, v, cu, cv, half, radius):
    qu, qv = abs(u - cu) - half + radius, abs(v - cv) - half + radius
    return math.hypot(max(qu, 0), max(qv, 0)) + min(max(qu, qv), 0) - radius

def ant(cx, cy, degrees, scale, colour, cube_colour):
    """Draw an ant centred at (CX, CY), heading DEGREES (0 is right, counterclockwise
    up), SCALE times the icon's, carrying a cube of CUBE_COLOUR (or none)."""
    a = math.radians(-degrees)
    cu, su = math.cos(a), math.sin(a)
    dark = shade(colour, 0.42)
    # The light from up-left and the viewer, turned into the ant's frame.
    lu, lv = -cu - su, su - cu
    n = math.hypot(lu, lv)
    light = (lu / n * 0.65, lv / n * 0.65, 0.76)
    def at(x, y):
        dx, dy = (x - cx) / scale, (y - cy) / scale
        u, v = dx * cu + dy * su, -dx * su + dy * cu
        best, col = 1e9, None
        for bu, bv, ru, rv in BODY:
            d, (nu, nv) = ellipse(u, v, bu, bv, ru, rv)
            if d < best:
                nz = math.sqrt(max(0.0, 1 - min(1.0, nu * nu + nv * nv)))
                lit = max(0.0, nu * light[0] + nv * light[1] + nz * light[2])
                base = mix(dark, colour, min(1.0, 0.25 + 0.95 * lit))
                spec = max(0.0, lit - 0.82) / 0.18
                best, col = d, mix(base, (255, 240, 235), spec * 0.7)
        for p, q, r in LIMBS:
            d = capsule(u, v, p, q, r)
            if d < best:
                best, col = d, shade(dark, 0.8)
        if cube_colour:
            d = box(u, v, *CARRIED)
            if d < best:
                best, col = d, (shade(cube_colour, 1.1) if v < -CARRIED[2] * 0.45
                                else shade(cube_colour, 0.8) if v > CARRIED[2] * 0.6 else cube_colour)
        return best * scale, col
    reach = 520 * scale + 40
    for y in range(max(0, int(cy - reach)), min(H, int(cy + reach))):
        row = pixels[y]
        for x in range(max(0, int(cx - reach)), min(W, int(cx + reach))):
            sx, sy = 16 * scale + 3, 20 * scale + 4           # shadow
            d, _ = at(x - sx, y - sy)
            soft = 30 * scale + 4
            if d < soft:
                row[x] = shade(row[x], 1 - 0.45 * max(0.0, min(1.0, (soft - d) / (soft * 1.3))))
            d, col = at(x, y)
            edge = 4.5 * scale + 1.2
            outline = max(0.0, min(1.0, edge - d))
            if outline > 0:
                row[x] = mix(row[x], (28, 14, 16), outline)
                body = max(0.0, min(1.0, 0.5 - d))
                if body > 0:
                    row[x] = mix(row[x], col, body)

# ------------------------------------------------------------------
# the scene

art = pictures()
CELL = 19
MONA = (1060, 150)
STARRY = (1700, 150)
# Cubes taken from the bottom edges, carried by the marching ants.
mona_missing = {(25, 3), (25, 4), (24, 4), (25, 9)}
starry_missing = {(25, 1), (25, 2), (24, 2), (25, 6), (23, 0), (25, 0)}
plate(art["Mona Lisa"], MONA[0], MONA[1], CELL, mona_missing)
plate(art["Starry Night"], STARRY[0], STARRY[1], CELL, starry_missing)

# A line of small ants, from under the paintings toward the title, along a
# gentle curve; each carries a cube of its own colour.
march = ["Y", "B", "N", "G", "K", "C", "N", "O", "B"]
for i, char in enumerate(march):
    t = i / (len(march) - 1)
    x = 2080 - t * 1300
    y = 712 + 22 * math.sin(t * math.pi * 1.6)
    heading = 180 - 9 * math.cos(t * math.pi * 1.6) * 1.6
    colour = PALETTE[char] if char not in "K" else (60, 54, 66)
    ant(x, y, heading, 0.135, colour if char != "W" else (220, 220, 214), PALETTE[char])

# The large ant, crossing from the Mona Lisa to the Starry Night.
ant(1665, 420, 32, 0.42, PALETTE["R"], PALETTE["Y"])

# ------------------------------------------------------------------

def png(path):
    raw = b"".join(b"\x00" + bytes(round(v) for p in row for v in p) for row in pixels)
    def chunk(kind, data):
        body = kind + data
        return struct.pack(">I", len(data)) + body + struct.pack(">I", zlib.crc32(body) & 0xffffffff)
    with open(path, "wb") as out:
        out.write(b"\x89PNG\r\n\x1a\n")
        out.write(chunk(b"IHDR", struct.pack(">IIBBBBB", W, H, 8, 2, 0, 0, 0)))
        out.write(chunk(b"IDAT", zlib.compress(raw, 9)))
        out.write(chunk(b"IEND", b""))

out = os.path.join(root, "docs", "header.png")
art_only = os.path.join(tempfile.mkdtemp(), "header-art.png")
png(art_only)
# The title, in the space left of the paintings: set by the system, whose
# Avenir Next ImageMagick cannot open.
subprocess.run(["swift", os.path.join(here, "header-title.swift"), art_only, out], check=True)
subprocess.run(["magick", out, "-alpha", "off", out], check=True)       # no alpha, for the App Store
print("wrote", os.path.relpath(out, root))

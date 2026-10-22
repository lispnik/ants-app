#!/usr/bin/env python3
"""make-icon.py -- write res/Ants.xcassets, the app icon, with the standard
library alone: the Starry Night level's picture as cubes, edge to edge, and
over it a large red ant walking diagonally up and to the right with a yellow
cube in its jaws.  1024x1024, no alpha, as iOS requires.

The ant is drawn from signed distance functions in its own frame (u along
the body, head first; v across it), so its edges are antialiased without
supersampling; its body parts are lit as if round.

    python3 tools/make-icon.py
"""
import json, math, os, struct, zlib

SIZE = 1024

# The Starry Night level's rows (src/model.lisp), 30 wide and 26 tall; the
# icon shows 26 of the columns, square cells edge to edge.
PICTURE = ["BBBBBBCCCBBBBBWWWBBWCCCBBBBBBB",
           "BBBBBGKCWWWBBWWYWWBBCCCWOOOOOB",
           "BBBBBKKWWYWWBWYYYWBBWCCOOYYYOO",
           "BBWWWCGWYYYWBWWYWWBBBCCOYYYYYO",
           "BWWYWWKWWYWWBBWWWBBBBCCOYYYYYO",
           "BWYYYKKBWWWBBBBBWWWWBWCOYYYYYO",
           "BWWYKKGBBBBBBBBBWWYWWBCOOYYYOO",
           "BBWWKGKBBBBBBBBWWYYYWBCWWWOOOB",
           "WBBBGKBBBBBBBBBWWWYWWWWWYWWBBB",
           "CBBBKKBBBBBWWWBBBWWWBCWYYYWBBB",
           "CBBKKGKBBBWWYWWBBBBBWCWWYWWBBB",
           "CBBKGKKBBBWYYYWBBBBWCCBWWWBBBB",
           "CCBGKKGBBBWWYWWBBBCCCCBBBBBBBB",
           "CCBBKGKBBBBWWWBBCCCCCBBBBBBBBB",
           "CCBBGKKGBBBBBCCCCCCCBBBBBBBBBB",
           "CCWBKKGKKBBBWCCCCCCBBBBBBBBBBB",
           "BCWKKGKKGBBBBWCCCBBBBBBBBBBBBB",
           "BBBBGKKGGGGGGGGGGGBBBBBBBBBBBG",
           "BBBBKKGKGGGGGGGGGGGGBBBBBBBGGG",
           "GGGKKGKKGGGGGGGGGGGGGGGGGGGGGG",
           "NNKKGKKGNNNNNNNNNNNNNNNNNNNNNN",
           "NKKGKKGKNYWYNNNNNNNYWYNNNNNNNN",
           "NNGKKGKNNNNNNYWYWNNNNNNYWYWNNN",
           "NNKKGKKNNNNNNNNNNNNNNNNNNNNYWN",
           "NNKGKKGKNNNNNNNNNNNNNNNNNNNNNN",
           "NNGKKGKKGNNNNNNNNNNNNNNNNNNNNN"]
FIRST_COLUMN = 3

# The game's palette (src/model.lisp *palette*).
PALETTE = {c: tuple(round(v * 255) for v in rgb) for c, rgb in {
    "R": (0.91, 0.24, 0.22), "O": (0.98, 0.55, 0.16), "Y": (0.99, 0.84, 0.22),
    "G": (0.33, 0.72, 0.30), "C": (0.45, 0.82, 0.93), "B": (0.20, 0.45, 0.90),
    "P": (0.96, 0.50, 0.74), "T": (0.95, 0.84, 0.66), "N": (0.52, 0.33, 0.19),
    "W": (0.97, 0.97, 0.95), "K": (0.20, 0.18, 0.22)}.items()}
GAP = (40, 44, 60)

def shade(colour, k):
    return tuple(max(0, min(255, c * k)) for c in colour)

def mix(a, b, t):
    return tuple(x + (y - x) * t for x, y in zip(a, b))

# ------------------------------------------------------------------
# the painting

pixels = [[GAP] * SIZE for _ in range(SIZE)]

def fill(x0, y0, x1, y1, colour):
    for y in range(max(0, round(y0)), min(SIZE, round(y1))):
        row = pixels[y]
        for x in range(max(0, round(x0)), min(SIZE, round(x1))):
            row[x] = colour

cell = SIZE / len(PICTURE)
for r, row in enumerate(PICTURE):
    for c, char in enumerate(row[FIRST_COLUMN:FIRST_COLUMN + len(PICTURE)]):
        x, y, s = c * cell + 2, r * cell + 2, cell - 4
        colour = PALETTE[char]
        fill(x, y, x + s, y + s, colour)
        fill(x, y, x + s, y + s * 0.22, shade(colour, 1.15) if char != "W" else (255, 255, 255))
        fill(x, y + s * 0.84, x + s, y + s, shade(colour, 0.78))

# ------------------------------------------------------------------
# the ant

ANGLE = math.radians(-40)                 # head up and to the right
CENTRE = (535, 530)
CU, SU = math.cos(ANGLE), math.sin(ANGLE)

def local(x, y):
    """Image point (x, y) in the ant's frame: u toward the head, v across."""
    dx, dy = x - CENTRE[0], y - CENTRE[1]
    return dx * CU + dy * SU, -dx * SU + dy * CU

def ellipse(u, v, cu, cv, ru, rv):
    """Signed distance (approximate) to an ellipse, and its unit-sphere normal."""
    nu, nv = (u - cu) / ru, (v - cv) / rv
    q = math.hypot(nu, nv)
    return (q - 1) * min(ru, rv), (nu, nv)

def capsule(u, v, a, b, r):
    au, av = a
    bu, bv = b
    pu, pv = u - au, v - av
    du, dv = bu - au, bv - av
    t = max(0, min(1, (pu * du + pv * dv) / (du * du + dv * dv)))
    return math.hypot(pu - du * t, pv - dv * t) - r

def box(u, v, cu, cv, half, radius):
    qu, qv = abs(u - cu) - half + radius, abs(v - cv) - half + radius
    return math.hypot(max(qu, 0), max(qv, 0)) + min(max(qu, qv), 0) - radius

BODY = [  # centre u, v, radii u, v
    (-235, 0, 205, 150),                  # abdomen
    (-35, 0, 38, 30),                     # petiole
    (55, 0, 105, 72),                     # thorax
    (215, 0, 98, 92),                     # head
]
LIMBS = []                                # (a, b, radius)
for side in (-1, 1):
    # Legs: from the thorax, a thigh out and a shin down, splayed.
    for root_u, knee, foot in ((95, (175, 205), (300, 330)),
                               (55, (40, 240), (-10, 380)),
                               (15, (-110, 205), (-250, 330))):
        LIMBS.append(((root_u, side * 40), (knee[0], side * knee[1]), 17))
        LIMBS.append(((knee[0], side * knee[1]), (foot[0], side * foot[1]), 13))
    # Antennae: elbowed, out over the cube.
    LIMBS.append(((280, side * 45), (360, side * 135), 8))
    LIMBS.append(((360, side * 135), (475, side * 120), 7))
    # Jaws, closing on the cube.
    LIMBS.append(((285, side * 45), (330, side * 40), 13))
CARRIED = (372, 0, 58, 12)                # the cube: centre u, v, half side, corner radius

RED = PALETTE["R"]
DARK = shade(RED, 0.42)
# The light, from up-left and toward the viewer, in the ant's frame (u v z).
lu, lv = local(CENTRE[0] - 1, CENTRE[1] - 1)
lu, lv = lu / math.hypot(lu, lv) * 0.65, lv / math.hypot(lu, lv) * 0.65
LIGHT = (lu, lv, 0.76)

def ant_at(x, y):
    """(coverage, colour) of the ant at the image point (x, y), or None."""
    u, v = local(x, y)
    best, colour = 1e9, None
    for cu, cv, ru, rv in BODY:
        d, (nu, nv) = ellipse(u, v, cu, cv, ru, rv)
        if d < best:
            nz = math.sqrt(max(0.0, 1 - min(1.0, nu * nu + nv * nv)))
            lit = max(0.0, nu * LIGHT[0] + nv * LIGHT[1] + nz * LIGHT[2])
            base = mix(DARK, RED, min(1.0, 0.25 + 0.95 * lit))
            spec = max(0.0, lit - 0.82) / 0.18
            best, colour = d, mix(base, (255, 235, 225), spec * 0.75)
    for a, b, r in LIMBS:
        d = capsule(u, v, a, b, r)
        if d < best:
            best, colour = d, shade(DARK, 0.85)
    d = box(u, v, CARRIED[0], CARRIED[1], CARRIED[2], CARRIED[3])
    if d < best:
        yellow = PALETTE["Y"]
        best, colour = d, (shade(yellow, 1.08) if v - CARRIED[1] < -CARRIED[2] * 0.45
                           else shade(yellow, 0.8) if v - CARRIED[1] > CARRIED[2] * 0.6 else yellow)
    return best, colour

# Every point within reach of the ant: a bound on u and v.
def near(x, y, margin):
    u, v = local(x, y)
    return -470 - margin < u < 470 + margin and abs(v) < 400 + margin

SHADOW = (18, 22)                         # offset, down and right
for y in range(SIZE):
    row = pixels[y]
    for x in range(SIZE):
        if not near(x, y, 60):
            continue
        # A soft shadow under the ant, darkening the painting.
        d, _ = ant_at(x - SHADOW[0], y - SHADOW[1])
        if d < 30:
            row[x] = shade(row[x], 1 - 0.45 * max(0.0, min(1.0, (30 - d) / 40)))
        # A thin dark outline, then the ant.
        d, colour = ant_at(x, y)
        outline = max(0.0, min(1.0, 4.5 - d))
        if outline > 0:
            row[x] = mix(row[x], (28, 14, 16), outline)
            body = max(0.0, min(1.0, 0.5 - d))
            if body > 0:
                row[x] = mix(row[x], colour, body)

# ------------------------------------------------------------------

def png(path):
    raw = b"".join(b"\x00" + bytes(round(v) for p in row for v in p) for row in pixels)
    def chunk(kind, data):
        body = kind + data
        return struct.pack(">I", len(data)) + body + struct.pack(">I", zlib.crc32(body) & 0xffffffff)
    with open(path, "wb") as out:
        out.write(b"\x89PNG\r\n\x1a\n")
        out.write(chunk(b"IHDR", struct.pack(">IIBBBBB", SIZE, SIZE, 8, 2, 0, 0, 0)))  # RGB, no alpha
        out.write(chunk(b"IDAT", zlib.compress(raw, 9)))
        out.write(chunk(b"IEND", b""))

here = os.path.dirname(os.path.abspath(__file__))
catalogue = os.path.join(here, "..", "res", "Ants.xcassets")
iconset = os.path.join(catalogue, "AppIcon.appiconset")
os.makedirs(iconset, exist_ok=True)
info = {"author": "ants-app", "version": 1}
json.dump({"info": info}, open(os.path.join(catalogue, "Contents.json"), "w"), indent=2)
json.dump({"images": [{"filename": "icon.png", "idiom": "universal", "platform": "ios", "size": "1024x1024"}],
           "info": info}, open(os.path.join(iconset, "Contents.json"), "w"), indent=2)
png(os.path.join(iconset, "icon.png"))
print("wrote", os.path.normpath(iconset))

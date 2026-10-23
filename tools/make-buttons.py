#!/usr/bin/env python3
"""make-buttons.py -- draw the round's settings, hint and restart buttons, in
the header's style: a glossy cube of one of the game's colours, outlined and
shadowed, with a white symbol on it.  Each is 150x150 with alpha, a 50x50
point button at 3x; the cube is 42 points across.

The cube, the gear and the arrow are signed distance functions, so their
edges are antialiased; the ? is set in Avenir Next by header-title.swift.

    python3 tools/make-buttons.py      # writes res/buttons/button-*.png
"""
import json, math, os, struct, subprocess, tempfile, zlib

here = os.path.dirname(os.path.abspath(__file__))
root = os.path.normpath(os.path.join(here, ".."))
SIZE = 150
CX, CY = 75, 72                       # the cube's centre, a little up for its shadow
HALF, RADIUS = 63, 30                 # the cube's half side and corner radius
OUTLINE = (28, 14, 16)

PALETTE = {"R": (232, 61, 56), "B": (51, 115, 230), "G": (84, 184, 77)}

def clamp(x, lo=0.0, hi=1.0):
    return max(lo, min(hi, x))

def rounded_box(x, y, cx, cy, half, radius):
    qx, qy = abs(x - cx) - half + radius, abs(y - cy) - half + radius
    return math.hypot(max(qx, 0), max(qy, 0)) + min(max(qx, qy), 0) - radius

def gear(x, y):
    dx, dy = x - CX, y - CY
    r = math.hypot(dx, dy)
    a = math.atan2(dy, dx)
    tooth = clamp((math.cos(8 * a + math.pi / 8) - 0.05) * 2.2)
    body = r - (23 + 9 * tooth)
    hole = 9.5 - r
    return max(body, hole)

def triangle(px, py, a, b, c):
    """Signed distance to the triangle ABC."""
    def edge(p, q):
        ex, ey = q[0] - p[0], q[1] - p[1]
        wx, wy = px - p[0], py - p[1]
        t = clamp((wx * ex + wy * ey) / (ex * ex + ey * ey))
        return math.hypot(wx - ex * t, wy - ey * t), ex * wy - ey * wx
    d = min(edge(a, b)[0], edge(b, c)[0], edge(c, a)[0])
    signs = [edge(a, b)[1], edge(b, c)[1], edge(c, a)[1]]
    inside = all(s >= 0 for s in signs) or all(s <= 0 for s in signs)
    return -d if inside else d

def restart_arrow(x, y):
    """A clockwise arrow round the centre, open at the top right; the head at
    the top, pointing on round."""
    dx, dy = x - CX, y - CY
    R, w = 25, 6.5
    a = math.degrees(math.atan2(dy, dx))          # y down: clockwise is increasing
    start, end = -35, -105                        # the gap, from end round to start
    in_gap = end < a < start
    if in_gap:
        ends = [(CX + R * math.cos(math.radians(t)), CY + R * math.sin(math.radians(t))) for t in (start, end)]
        ring = min(math.hypot(x - ex, y - ey) for ex, ey in ends) - w
    else:
        ring = abs(math.hypot(dx, dy) - R) - w
    # The head, at the arc's end (angle END), pointing along increasing angle.
    t = math.radians(end)
    px, py = CX + R * math.cos(t), CY + R * math.sin(t)
    tx, ty = -math.sin(t), math.cos(t)            # the way round
    nx, ny = math.cos(t), math.sin(t)             # outward
    tip = (px + tx * 17, py + ty * 17)
    back = (px - tx * 5, py - ty * 5)
    head = triangle(x, y, tip, (back[0] + nx * 15, back[1] + ny * 15), (back[0] - nx * 15, back[1] - ny * 15))
    return min(ring, head)

def over(dst, colour, alpha):
    """Composite COLOUR at ALPHA over the premultiplied-free RGBA DST."""
    if alpha <= 0:
        return dst
    r, g, b, a = dst
    out_a = alpha + a * (1 - alpha)
    if out_a <= 0:
        return (0, 0, 0, 0)
    mixc = [(c * alpha + d * a * (1 - alpha)) / out_a for c, d in zip(colour, (r, g, b))]
    return (*mixc, out_a)

def draw(colour, symbol):
    pixels = [[(0, 0, 0, 0)] * SIZE for _ in range(SIZE)]
    light = tuple(min(255, c * 1.25 + 25) for c in colour)
    dark = tuple(c * 0.62 for c in colour)
    for y in range(SIZE):
        for x in range(SIZE):
            p = pixels[y][x]
            # The soft shadow, down and right.
            d = rounded_box(x - 4, y - 7, CX, CY, HALF, RADIUS)
            p = over(p, (0, 0, 0), 0.42 * clamp((8 - d) / 16))
            d = rounded_box(x, y, CX, CY, HALF, RADIUS)
            # The outline, then the cube's face: lit from the top, darker below,
            # a gloss across its upper part and a bevel round its edge.
            p = over(p, OUTLINE, clamp(0.5 - (d - 4.5)))
            body = clamp(0.5 - d)
            if body > 0:
                t = (y - (CY - HALF)) / (2 * HALF)
                base = tuple(l + (k - l) * clamp(t * 1.15) for l, k in zip(light, dark))
                bevel = clamp((d + 10) / 10)                    # 0 inside, 1 at the edge
                base = tuple(c * (1 - 0.18 * bevel * clamp(t * 2)) for c in base)
                if t < 0.12:
                    base = tuple(c + (255 - c) * 0.25 * bevel for c in base)
                gloss = 0.3 * clamp(1 - t / 0.5) ** 1.5 * clamp(-d / 8)
                base = tuple(c + (255 - c) * gloss for c in base)
                p = over(p, base, body)
                # The symbol, white, with a dark shadow.
                if symbol:
                    sd = symbol(x - 2, y - 3)
                    p = over(p, OUTLINE, 0.35 * clamp(0.5 - sd / 2.5) * body)
                    p = over(p, (255, 255, 255), clamp(0.5 - symbol(x, y)) * body)
            pixels[y][x] = p
    return pixels

def png(pixels, path):
    raw = b"".join(b"\x00" + bytes(round(clamp(v / 255 if i < 3 else v) * 255)
                                   for p in row for i, v in enumerate(p)) for row in pixels)
    def chunk(kind, data):
        body = kind + data
        return struct.pack(">I", len(data)) + body + struct.pack(">I", zlib.crc32(body) & 0xffffffff)
    with open(path, "wb") as out:
        out.write(b"\x89PNG\r\n\x1a\n")
        out.write(chunk(b"IHDR", struct.pack(">IIBBBBB", SIZE, SIZE, 8, 6, 0, 0, 0)))  # RGBA
        out.write(chunk(b"IDAT", zlib.compress(raw, 9)))
        out.write(chunk(b"IEND", b""))

out = os.path.join(root, "res", "buttons")
os.makedirs(out, exist_ok=True)
png(draw(PALETTE["B"], gear), os.path.join(out, "button-settings.png"))
png(draw(PALETTE["R"], restart_arrow), os.path.join(out, "button-restart.png"))
# The ?, set by the system in Avenir Next over a plain green cube.
plain = os.path.join(tempfile.mkdtemp(), "hint.png")
png(draw(PALETTE["G"], None), plain)
spec = os.path.join(os.path.dirname(plain), "spec.json")
json.dump([dict(text="?", font="AvenirNext-Heavy", colour="#FFFFFF", size=78, baseline=CY + 27, x=None)],
          open(spec, "w"))
subprocess.run(["swift", os.path.join(here, "header-title.swift"), plain,
                os.path.join(out, "button-hint.png"), spec], check=True)
print("wrote", os.path.relpath(out, root))

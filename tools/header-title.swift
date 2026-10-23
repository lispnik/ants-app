// header-title.swift -- set the header's title over the drawn art, with the
// system's Avenir Next, which ImageMagick cannot open by face.  SPEC is a
// JSON list of lines: text, font, colour ("#RRGGBB" or "#RRGGBBAA"), size,
// baseline from the top, and x (null centres the line), all in pixels.
//
//     swift tools/header-title.swift art.png out.png spec.json
import AppKit

let args = CommandLine.arguments
guard args.count == 4, let art = NSImage(contentsOfFile: args[1]),
      let rep = art.representations.first,
      let data = FileManager.default.contents(atPath: args[3]),
      let spec = try? JSONSerialization.jsonObject(with: data) as? [[String: Any]] else {
    FileHandle.standardError.write("usage: header-title.swift art.png out.png spec.json\n".data(using: .utf8)!)
    exit(1)
}
let width = rep.pixelsWide, height = rep.pixelsHigh
let canvas = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: width, pixelsHigh: height,
                              bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
                              colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
canvas.size = NSSize(width: width, height: height)        // one point a pixel
NSGraphicsContext.saveGraphicsState()
NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: canvas)
art.draw(in: NSRect(x: 0, y: 0, width: width, height: height))

func colour(_ hex: String) -> NSColor {
    var value: UInt64 = 0
    Scanner(string: String(hex.dropFirst())).scanHexInt64(&value)
    let alpha = hex.count == 9 ? CGFloat(value & 0xff) / 255 : 1
    let rgb = hex.count == 9 ? value >> 8 : value
    return NSColor(red: CGFloat((rgb >> 16) & 0xff) / 255, green: CGFloat((rgb >> 8) & 0xff) / 255,
                   blue: CGFloat(rgb & 0xff) / 255, alpha: alpha)
}

for line in spec {
    let size = line["size"] as! Double
    let font = NSFont(name: line["font"] as! String, size: size)!
    let shadow = NSShadow()
    shadow.shadowColor = NSColor(white: 0, alpha: 0.35)
    shadow.shadowOffset = NSSize(width: size * 0.024, height: -size * 0.032)
    shadow.shadowBlurRadius = size * 0.055
    let text = NSAttributedString(string: line["text"] as! String,
                                  attributes: [.font: font, .foregroundColor: colour(line["colour"] as! String),
                                               .shadow: shadow])
    let x = (line["x"] as? Double) ?? (Double(width) - text.size().width) / 2
    text.draw(at: NSPoint(x: x, y: Double(height) - (line["baseline"] as! Double) + font.descender))
}
NSGraphicsContext.restoreGraphicsState()
try! canvas.representation(using: .png, properties: [:])!.write(to: URL(fileURLWithPath: args[2]))

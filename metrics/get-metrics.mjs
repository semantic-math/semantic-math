import fs from "fs";
import opentype from "opentype.js";

const font = opentype.loadSync("./Comic Sans MS.ttf");
const glyphs = {};

for (let i = 0; i < font.glyphs.length; i++) {
    const glyph = font.glyphs.glyphs[i];

    if (glyph.unicode) {
        glyphs[glyph.unicode] = {
            advance: glyph.advanceWidth,
            bearingX: glyph.xMin,
            bearingY: glyph.yMax,
            width: glyph.xMax - glyph.xMin,
            height: glyph.yMax - glyph.yMin,
        }
    }
}

const metrics = {
    unitsPerEm: font.unitsPerEm,
    glyphMetrics: glyphs,
}

fs.writeFileSync("comic-sans.json", JSON.stringify(metrics, null, 4));

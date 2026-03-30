# MGT100 Slide Reformatting Plan

## Goal
Reformat all mgt100-*.qmd files to use the Grant McDermott clean-revealjs template from `../../slidestemplate/`.

## Reference Implementation
The authoritative style reference is `admeas.qmd` at https://github.com/kennethcwilbur/admeas. All formatting decisions should follow the patterns in that file.

## Template Source
`/mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/slidestemplate/`

## Step 1: Copy template support files into mgt100_gh

One-time setup (DONE). These are now in the mgt100_gh directory:
- `_extensions/grantmcdermott/clean/` (clean.scss, _extension.yml, mathjax-config.js)
- `custom.css`
- `gtag.html` (includes Google Analytics + script to inject audio player on title slide)

Note: `gtag.html` contains a DOMContentLoaded script that appends an `<audio controls>` widget for `mgt100_theme_song.mp3` to the title slide. This applies automatically to all decks that use `include-in-header: gtag.html`. No per-file changes needed — the audio player appears on every deck's title slide.

## Step 2: Rewrite YAML front matter

Replace the entire YAML block with the structured template below. Preserve existing title, subtitle, and author name content but reformat into this structure:

```yaml
---
title: "**Slide Deck Title**"
subtitle: "MGT 100 Week N"
author:
  - name: "<a href='https://kennethcwilbur.com' target='_blank'>Kenneth C. Wilbur</a>"
    affiliations:
      - name: Professor of Marketing and Analytics
      - name: University of California, San Diego
  - name: "<a href='https://www.linkedin.com/in/dyavorsky/' target='_blank'>Daniel Yavorsky</a>"
    affiliations:
      - name: SVP-Analytics
      - name: GBK Collective
date: today
date-format: "[This version: ]MMMM YYYY[ | License: CC BY 4.0 | We use javascript to track readership.<br>We welcome reuse with attribution. Please share widely.]"
format:
  clean-revealjs:
    slide-number: c
    chalkboard:
      buttons: false
    css: custom.css
    include-in-header: gtag.html
---
```

Key details:
- **Title** must be wrapped in `**bold**` markdown
- **Subtitle** should be `"MGT 100 Week N"` matching the existing week number
- **Author** uses the structured `- name:` / `affiliations:` format with HTML anchor tags, not markdown links
- **Date** uses `date: today` with the dynamic date-format string
- Remove obsolete keys: `auto-stretch`, `theme: default`, `pdf-max-pages-per-slide`, `pdf-separate-fragments`, `output: html_document`, `editor_options`, `slide-number` (when outside the `format:` block)

## Step 3: Convert `::: aside` blocks

Each `::: aside` serves one of two purposes — convert accordingly:
- **Source citations** → `::: {.source-url}` blocks
- **Supplementary notes/commentary** → `::: {.callout-note appearance="minimal"}` blocks

Use judgment per instance. ~20 occurrences per file.

## Step 4: Ensure near-universal callout notes

**Rule: Every content slide (`##`) MUST have a `::: {.callout-note appearance="minimal"}` block.** Section headers (`#`) never get callouts. Recap, "Class script," "Going further," and similar wrap-up slides do not need callouts.

### Callout style (targeting undergrad audience)
- **1-3 sentences** that engage the student, state the main takeaway, and ask a discussion question
- Explain what the image shows in plain language
- Connect the image to the broader course concept when possible
- End with a **discussion question** when natural ("Why?", "What does this imply?", "What else do you see?")
- Never duplicate the slide title or repeat an image caption verbatim
- Never place the same content in both callout and notes

### Content sourcing hierarchy

1. **Existing `::: aside` text** — Convert directly to callout. Expand if terse and surrounding context supports a fuller treatment.
2. **Existing `::: notes` text that is audience-appropriate** — Promote the most substantive sentences from notes into the callout. Leave speaker-only logistics (e.g., "Bring name tents") in notes only.
3. **Contextual suggestion** — When the image or surrounding bullets provide enough context to draft a reasonable callout, write one and prefix the callout text with `SUGGESTION:` so the author knows it needs verification. The goal is to engage the student, state the main takeaway, and ask a question that could prompt further thinking or discussion.
4. **Blank placeholder** — When contextual clues are absent, insert:
   ```
   ::: {.callout-note appearance="minimal"}
   <!-- TODO: Add callout note for this slide -->
   :::
   ```

## Step 5: Standardize image sizing

Convert from `width="Xin"` to `height="Npx"` (fixed pixel heights). The default is `height="400px"`.

- **Default** (most images): `height="400px"`
- **Large full-slide images** (width >= 8in or hero images): `height="450px"` or `height="500px"`
- **Small inline/decorative images** (1-3in): `height="150px"` to `height="250px"`
- **Small corner images** using `.absolute` positioning: `height="120px"` to `height="200px"`

~25 images per file. Requires per-image judgment, but when in doubt use `400px`.

**Never change an image's aspect ratio.** Always set only `height` OR `width`, never both. The browser will scale the other dimension proportionally. When placing an image in a narrow column, use `height` and let the column constrain the width naturally.

### Crop whitespace from images

When adding external images (e.g., from OWID, screenshots, exported slides), crop unnecessary whitespace margins before placing them. Use ImageMagick:
```bash
# Check how much whitespace exists
convert image.png -trim -fuzz 5% info:
# Crop with ~10px padding preserved on each side
convert image.png -crop WxH+X+Y +repage image.png
```
This maximizes the chart/content area within the slide's height budget and prevents callouts or source URLs from being pushed too far down.

### PDF height budget (critical)

The slide coordinate space is 1050×700px. Content that exceeds 700px total height will split across multiple PDF pages. Budget the total height of all slide elements to stay under ~650px (leaving margin). Approximate element heights:

| Element | Height |
|---------|--------|
| `## Title` | ~50-60px |
| Bullet line | ~30px |
| Callout (1-3 sentences) | ~70-80px |
| Source-url | ~25-30px |
| Image | as specified |

**Height limits by slide composition:**

| Slide has... | Max image height |
|--------------|-----------------|
| Image only (no title, no callout) | 500px |
| Image + callout | 450px |
| Image + callout + source-url | 400px |
| Title + image + callout + source-url | 350px |
| Title + bullets + image + callout | 280px |

When a slide has both bullets AND an image (e.g., bullets above, image right-aligned), the image must be small enough that total content fits. Use `{.smaller}` on text-heavy slides with many sub-bullets to reduce text height.

**After rendering, always verify the PDF page count matches the slide count.** If pages exceed slides, identify and fix the overflowing slides.

## Step 6: Convert indented code-block sub-bullets

The current files use 4+ space indentation to create fake sub-bullets rendered as code blocks:
```
-   Main point
        - Sub point styled as code
```
Convert these to proper nested markdown lists (4-space indent) as appropriate. ~15 instances per file.

## Step 7: Audit links for `{target="_blank"}`

Ensure all external links include `{target="_blank"}`. Most already do; fix any that don't.

## Step 8: Enforce consistent slide element ordering

Follow the admeas.qmd canonical order on every slide:
1. `## Slide Title`
2. Image (if any)
3. `::: {.callout-note appearance="minimal"}` (if any)
4. `::: {.source-url}` (if any)
5. `::: notes` (if any)

Never place notes before source-url. Never place callout after notes.

Exception: On bullet-heavy slides, bullets come after the title and before any image. If both bullets and an image appear, the image follows the bullets, then callout, source-url, notes.

**Bullets + portrait-shaped image slides should use two-column layout.** When a slide has bullet text AND a portrait-shaped (tall) image, use `:::: {.columns}` to place the bullets + callout in the left column (55-65%) and the image in the right column (35-45%). For landscape-shaped (wide) images, stack bullets above or below the image instead of using columns.

## Step 9: Pipe separator rules

- **Callout-note blocks**: Do NOT use `|` to join items. Use flowing sentences, periods, or `<br>` line breaks.
- **Source-url blocks**: `|` IS the standard separator for multiple citations. Example:
  ```
  ::: {.source-url}
  [Author1 (Year)](url){target="_blank"} | [Author2 (Year)](url){target="_blank"}
  :::
  ```

## Step 10: Add `{.smaller}` to text-heavy slides

Apply `{.smaller}` class to slides with dense text or long bullet lists (e.g., slides with 5+ bullets each having sub-bullets). Follow admeas.qmd practice. Do not add it to image-focused slides.

## Step 11: No `.scrollable` class

Never use `{.scrollable}`. If a slide overflows, use `{.smaller}` or split the content across two slides.

## Source formatting conventions

- **Bullet syntax**: Use compact `- ` (dash + single space), not `-   ` (dash + 3 spaces). Sub-bullets use 4-space indent:
  ```
  - Main point
      - Sub-point
  ```
- **Section headers** (`#`): May include a subtitle as a bullet underneath. No callouts on section headers.
- **Slide headers** (`##`): Prefer bare `##` for untitled image slides. Do not use `## <!--#hidden title-->` comment syntax.
- **Image paths** (`images/...`): Already correct, leave as-is.
- **`::: notes` blocks**: Leave existing notes as-is unless promoting content to a callout (Step 4). Empty `::: notes` / `:::` blocks are acceptable as placeholders.
- **Source-url citations**: Prefer `[Author (Year)](url){target="_blank"}` format when the source has an identifiable author. Use `[source](url){target="_blank"}` only when no author is identifiable.

## Advanced layout techniques

These patterns from admeas.qmd are available when needed:

### Two-column layouts
Use `:::: {.columns}` / `::: {.column width="X%"}` for side-by-side content. Preserve existing column layouts; do not introduce new ones during reformatting unless specifically requested.
```
:::: {.columns}

::: {.column width="60%"}
![](images/image.png){fig-align="center" height="400px"}
:::

::: {.column width="40%"}
::: {.callout-note appearance="minimal"}
Explanatory text.
:::
:::

::::
```

### Standard recurring images

These images appear on the same slide types across all decks. Always use the exact sizing and placement shown:

| Slide | Image | Markup |
|-------|-------|--------|
| Intermission | `intermission.jpg` | `![](images/intermission.jpg){fig-align="center" height="400px"}` |
| Class script | `scroll.jpg` | `![](images/scroll.jpg){.absolute bottom="20" right="20" height="100px"}` |
| Competition | `competition.png` | `![](images/competition.png){.absolute bottom="20" right="20" height="120px"}` |
| Recap | `recap.png` | `![](images/recap.png){.absolute bottom="20" right="20" height="100px"}` |
| Going further | `takingoff.png` | `![](images/takingoff.png){.absolute bottom="20" right="20" height="100px"}` |

### Absolute positioning for corner images
Use `.absolute` for small decorative/supplementary images that should not disrupt content flow:
```
![](images/small.png){.absolute bottom="20" right="20" height="180px"}
```

### Inline style overrides
Available as edge-case tools when standard classes are insufficient:
- `style="font-size: 0.75em;"` on a callout with unusually long text
- `style="margin: 0; line-height: 1;"` for tightly stacked image groups

## Files to reformat
- mgt100-01.qmd (v2 draft exists; needs fixes per steps 2, 4, 8-11, and source formatting)
- mgt100-02.qmd
- mgt100-03.qmd
- mgt100-05.qmd
- mgt100-06.qmd
- mgt100-07.qmd
- mgt100-09.qmd
- mgt100-10.qmd
- mgt100-conjoint.qmd

## Preferences (synced across machines via Google Drive)

- **No silent content changes**: Never modify slide deck content (words, numbers, links) unless explicitly asked. Formatting changes must preserve every word.
- **Use absolute paths**: In Bash commands, use absolute paths instead of `cd` to avoid unnecessary permission prompts.
- **Re-render after changes**: Always re-render HTML/PDF and verify after non-trivial changes (layout, images, structural edits).
- **Push workflow**: Use `scripts/push_to_github.sh` to push to GitHub. Never run git directly on Google Drive — the FUSE filesystem doesn't support it reliably.
- **Two machines**: The user works from two machines. Machine-local setup (GitHub token, packages, permissions) must be repeated on each.
- **PDF naming convention**: Slide deck PDFs must be named `mgt100-NN.pdf` (e.g., `mgt100-01.pdf`), matching the links in `index.html`. Never use `-slides` or other suffixes.

# PDF Rendering Diagnostic Guide

This guide diagnoses why `chromium --headless --print-to-pdf` may produce different (broken) results on a second machine. Run each step in order. The reference values below come from the machine where rendering works correctly.

## Known-good configuration (reference machine)

| Component | Version |
|-----------|---------|
| Debian | 12.11 (bookworm) |
| Kernel | 6.6.99-09128-g14e87a8a9b71 x86_64 |
| Chromium | 146.0.7680.177-1~deb12u1 |
| Quarto | 1.8.25 |
| Fonts | 448 files, 190+ families |
| poppler-utils | installed (pdfinfo, pdftoppm) |
| ImageMagick | installed |

## Step 1: Check versions

Run each command and compare to the reference values above.

```bash
cat /etc/debian_version
chromium --version
quarto --version
```

**Chromium version is the most critical.** Even minor version differences can change headless PDF layout. If versions differ, fix Chromium first (step 1a) before proceeding.

### Step 1a: Fix Chromium version mismatch

```bash
sudo apt-get update
sudo apt-get install --only-upgrade chromium
```

If the version in apt is older than 146.x, check whether the Debian security repo is enabled:
```bash
grep -r "security.debian.org" /etc/apt/sources.list /etc/apt/sources.list.d/
```

### Step 1b: Fix Quarto version mismatch

```bash
quarto update
```

Or install a specific version:
```bash
wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.8.25/quarto-1.8.25-linux-amd64.deb
sudo dpkg -i quarto-1.8.25-linux-amd64.deb
```

## Step 2: Check fonts

Font differences are the second most common cause of layout divergence. Missing fonts cause text reflow, which causes page overflows.

```bash
fc-list | wc -l
```

Reference: **448** font files. If significantly fewer, fonts are missing.

### Check critical font packages

```bash
dpkg -l cros-host-fonts fonts-dejavu-core fonts-liberation fonts-noto-mono fonts-urw-base35 gsfonts 2>/dev/null | grep -E '^ii|^un'
```

`cros-host-fonts` is a ChromeOS-specific package that provides Arial, Arial Black, Arial Narrow, Comic Sans MS, Georgia, Garamond, Tahoma, Trebuchet MS, Verdana, Webdings, Wingdings (1/2/3), and Google Sans. These are the fonts most likely used by reveal.js slides.

If `cros-host-fonts` is missing:
```bash
sudo apt-get install cros-host-fonts
```

If that package is unavailable (not ChromeOS), install equivalent Microsoft core fonts:
```bash
sudo apt-get install fonts-liberation fonts-dejavu
```

### Check specific font families used by reveal.js

The clean-revealjs template primarily uses system sans-serif fonts. Verify these exist:
```bash
fc-list : family | sort -u | grep -iE "^(Liberation Sans|Noto Sans$|DejaVu Sans$|Arial$|Roboto$|Google Sans$)"
```

Reference machine has all of these. If any are missing, install the corresponding package.

After installing fonts, rebuild the font cache:
```bash
fc-cache -fv
```

## Step 3: Check PDF support tools

```bash
which pdfinfo pdftoppm
dpkg -l poppler-utils imagemagick 2>/dev/null | grep -E '^ii|^un'
```

Install if missing:
```bash
sudo apt-get install poppler-utils imagemagick
```

## Step 4: Test render

Run the exact two-step rendering pipeline on mgt100-02.qmd:

### Step 4a: Render HTML
```bash
quarto render /mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/mgt100-02.qmd
```

### Step 4b: Render PDF
```bash
chromium --headless --disable-gpu \
  --print-to-pdf="/mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/mgt100-02.pdf" \
  --print-to-pdf-no-header \
  --virtual-time-budget=180000 \
  "file:///mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/mgt100-02.html?print-pdf"
```

### Step 4c: Check page count
```bash
pdfinfo /mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/mgt100-02.pdf | grep Pages
```

**Expected: 62 pages.** If more pages appear, content is overflowing and splitting across pages.

### Step 4d: Visual check
```bash
mkdir -p /tmp/mgt100-02-check
pdftoppm -png -r 150 /mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/mgt100-02.pdf /tmp/mgt100-02-check/page
ls /tmp/mgt100-02-check/ | wc -l
```

Then screenshot and visually inspect all pages. Look for:
- Pages that are split (content continues onto a blank or partial next page)
- Missing images (blank rectangles)
- Broken math rendering (raw LaTeX instead of formatted equations)
- Wrong fonts (monospace where sans-serif expected)
- Missing callout box styling (plain text instead of blue-bordered boxes)

## Step 5: Diagnose specific symptoms

### Symptom: Too many PDF pages (e.g., 70+ instead of 62)
**Cause:** Content overflow. Some slides are too tall and split across multiple PDF pages.
**Root cause:** Usually font differences causing text to render larger/taller, or missing `custom.css` `@media print` rules.
**Fix:** Check fonts (step 2). Also verify `custom.css` exists and contains `@media print` rules:
```bash
grep -c "@media print" /mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/custom.css
```
Should return at least 1.

### Symptom: Blank or white pages
**Cause:** `--virtual-time-budget` too low. Chromium didn't finish rendering before capturing.
**Fix:** Increase the budget:
```bash
--virtual-time-budget=300000
```

### Symptom: Missing images
**Cause:** File paths. The `file:///` protocol requires exact absolute paths. Google Drive FUSE mount paths may differ between machines.
**Fix:** Check the mount point:
```bash
ls /mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/images/ | head -5
```
If that path doesn't work, find the actual mount:
```bash
mount | grep -i google
```

### Symptom: MathJax not rendering (raw $...$ visible)
**Cause:** MathJax loads from CDN; needs network access during render, or `--virtual-time-budget` too low.
**Fix:** Increase virtual-time-budget. If offline, this is expected — MathJax requires internet.

### Symptom: Callout boxes missing blue border / wrong styling
**Cause:** `custom.css` or `_extensions/` not found during quarto render.
**Fix:** Verify the extension exists:
```bash
ls /mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/_extensions/grantmcdermott/clean/
```
Should contain: `clean.scss`, `_extension.yml`

### Symptom: Landscape/portrait orientation wrong
**Cause:** Missing `@media print` rules in `custom.css`.
**Fix:** The `custom.css` file should contain print rules forcing 11in x 8.5in landscape. Since both machines share the file via Google Drive, this should be identical — but verify:
```bash
grep "11in" /mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/custom.css
```

## Step 6: Nuclear option — match packages exactly

If the above doesn't resolve it, install the exact same package set:

```bash
# On the working machine, export package list:
dpkg --get-selections | grep -iE 'chromium|font|poppler|imagemagick|quarto' > /tmp/working_packages.txt

# On the broken machine, compare:
dpkg --get-selections | grep -iE 'chromium|font|poppler|imagemagick|quarto' > /tmp/local_packages.txt
diff /tmp/working_packages.txt /tmp/local_packages.txt
```

Install any missing packages from the diff.

## Reference: the exact render commands that produce correct output

```bash
# Step 1: QMD -> HTML
quarto render /mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/mgt100-02.qmd

# Step 2: HTML -> PDF (landscape, no headers)
chromium --headless --disable-gpu \
  --print-to-pdf="/mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/mgt100-02.pdf" \
  --print-to-pdf-no-header \
  --virtual-time-budget=180000 \
  "file:///mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/mgt100-02.html?print-pdf"

# Step 3: Verify
pdfinfo /mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/mgt100-02.pdf | grep Pages
# Expected: 62
```
